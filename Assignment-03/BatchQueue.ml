(** Batch Bounded Blocking Queue

    A bounded blocking queue where enqueue and dequeue operate on
    batches of elements atomically, with strict FIFO fairness and
    head-of-line blocking for both enqueue and dequeue waiters.

    Uses a single mutex with per-waiter condition variables. *)

(** A blocked enqueuer waiting for enough free space. *)
type 'a enq_waiter = {
  items : 'a array;       (** The batch of items this thread wants to enqueue *)
  cond : Condition.t;     (** Private condition variable — signaled when this
                              waiter reaches the head and space may be available *)
}

(** A blocked dequeuer waiting for enough items. *)
type 'a deq_waiter = {
  amount : int;           (** Number of items this thread wants to dequeue *)
  cond : Condition.t;     (** Private condition variable — signaled when this
                              waiter reaches the head and items may be available *)
}

type 'a t = {
  mutex : Mutex.t;
  buffer : 'a Queue.t;                   (** Items currently in the queue *)
  capacity : int;
  enq_waiters : 'a enq_waiter Queue.t;   (** FIFO queue of blocked enqueuers *)
  deq_waiters : 'a deq_waiter Queue.t;   (** FIFO queue of blocked dequeuers *)
}

(** [create capacity] initializes a new queue. Validate capacity, then
    initialize all fields of the ['a t] record. *)
let create capacity = 
  if capacity <= 0 then invalid_arg "The capacity of the queue must be greater than 0";
  let mutex = Mutex.create () in
  let buffer = Queue.create () in
  let enq_waiters = Queue.create () in
  let deq_waiters = Queue.create () in
  { mutex; buffer; capacity; enq_waiters; deq_waiters }

let validate_enq_count q n =
  if n <= 0 then
    invalid_arg "BatchQueue: batch size must be positive";
  if n > q.capacity then
    invalid_arg "BatchQueue: batch size exceeds capacity"

let validate_deq_count q n =
  if n <= 0 then
    invalid_arg "BatchQueue: dequeue count must be positive";
  if n > q.capacity then
    invalid_arg "BatchQueue: dequeue count exceeds capacity"

let free_space q = q.capacity - Queue.length q.buffer

(** [notify q] checks the head of each waiter queue and signals it if
    its request can now be satisfied. Call after every enqueue or dequeue. *)
let notify q = 
  begin
  let new_enq = Queue.peek_opt q.enq_waiters in
  match new_enq with
  | Some enq_op -> if free_space q >= Array.length enq_op.items then Condition.signal enq_op.cond else ()
  | None -> ()
  end;
  begin
  let new_deq = Queue.peek_opt q.deq_waiters in
  match new_deq with
  | Some deq_op -> if Queue.length q.buffer >= deq_op.amount then Condition.signal deq_op.cond else ()
  | None -> ()
  end

(** [enq q items] atomically enqueues all items. Algorithm:
    1. Validate and lock the mutex (use [Fun.protect] for safe unlock).
    2. If [enq_waiters] is non-empty OR not enough free space:
       - Create a waiter, push it to [enq_waiters], and loop on
         [Condition.wait] until this waiter is at the head of
         [enq_waiters] AND there is enough space.
       - Pop self from [enq_waiters].
    3. Push all items into [buffer].
    4. Call [notify]. *)
let enq q items =
  validate_enq_count q (Array.length items);
  Mutex.lock q.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock q.mutex)(fun () ->
    if ((free_space q < Array.length items) || (Queue.length q.enq_waiters > 0)) then begin
      let new_waiter = {items = items; cond = Condition.create ()} in
      Queue.push new_waiter q.enq_waiters;
      while ((Queue.peek q.enq_waiters != new_waiter) || (free_space q < Array.length items)) do
        Condition.wait new_waiter.cond q.mutex
      done;
      ignore (Queue.pop q.enq_waiters);
    end

    for i = 0 to (Array.length items - 1) do
      Queue.push items.(i) q.buffer
    done;
    notify q      
  )

(** [deq q n] atomically dequeues [n] items. Symmetric to [enq]:
    wait on [deq_waiters] until at head AND enough items available. *)
let deq q n = 
  validate_deq_count q n;
  Mutex.lock q.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock q.mutex)(fun () ->
    if ((Queue.length q.buffer < n) || (Queue.length q.deq_waiters > 0)) then begin
      let new_waiter = {amount = n; cond = Condition.create ()} in
      Queue.push new_waiter q.deq_waiters;
      while ((Queue.peek q.deq_waiters != new_waiter) || (Queue.length q.buffer < n)) do
        Condition.wait new_waiter.cond q.mutex
      done;
      ignore (Queue.pop q.deq_waiters);
    end;

    let a = Array.init n (fun _ -> Queue.pop q.buffer) in
    notify q;
    a 
  )

(** [try_enq q items] non-blocking enqueue. If no enqueuers are waiting
    ahead AND enough free space, enqueue and return [true]. Otherwise
    return [false] immediately (do not create a waiter). *)
let try_enq q items = 
  validate_enq_count q (Array.length items);
  Mutex.lock q.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock q.mutex)(fun () ->    
    if ((free_space q < Array.length items) || (Queue.length q.enq_waiters > 0)) then false
    else begin
      for i = 0 to (Array.length items - 1) do
        Queue.push items.(i) q.buffer
      done;
      notify q;
      true
    end
  )


(** [try_deq q n] non-blocking dequeue. If no dequeuers are waiting
    ahead AND enough items, dequeue and return [Some items]. Otherwise
    return [None] immediately (do not create a waiter). *)
let try_deq q n =
  validate_deq_count q n;
  Mutex.lock q.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock q.mutex)(fun () ->    
    if ((Queue.length q.buffer < n) || (Queue.length q.deq_waiters > 0)) then 
      None
    else begin
      let a = Array.init n (fun _ -> Queue.pop q.buffer) in
      notify q;
      Some a
    end
  )

let size q =
  Mutex.lock q.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock q.mutex)(fun () ->
    Queue.length q.buffer
  )

let capacity q = q.capacity