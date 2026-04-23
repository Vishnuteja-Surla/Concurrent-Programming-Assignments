(** A fiber-level mutex.

    Invariants:
    - [waiters] is non-empty  ⇒  [locked = true].  (If the mutex were
      unlocked with a waiter present, {!unlock} would have handed it
      off directly.)
    - Each trigger in [waiters] is signaled at most once.  Stale
      entries (signaled via a select race on another case) may linger
      and are harmlessly skipped by {!find_live_waiter}.

    The shape mirrors [Chan] in [golike_unicore_select]: a state field
    plus a FIFO queue of [(slot, trigger)] pairs.  [slot := Some ()] is
    the flag an unlocker writes before signaling, so that when the
    waiter wakes it can distinguish "lock granted to me" from "select
    chose a different case".
*)

type t = {
  mutable locked : bool;
  waiters : (unit option ref * Trigger.t) Queue.t;
}

let create () = {locked = false; waiters = Queue.create ()}

(** Pop waiters until one is signaled successfully.  A failed signal
    means the waiter was already woken by another case of its
    [Select.select], so we simply try the next one.  Returns [true] if
    a live waiter was found (in which case the lock has been handed
    off), [false] if the queue was drained. *)
let rec find_live_waiter waiters = 
  if Queue.is_empty waiters then false
  else
    let (slot, trigger) = Queue.pop waiters in
    slot := Some ();
    if Trigger.signal trigger then true
    else find_live_waiter waiters

let try_lock m = 
  match m.locked with
  | true -> false
  | false -> m.locked <- true; true

let lock m =
  match try_lock m with
  | true -> ()
  | false -> 
      let slot = ref None in
      let trigger = Trigger.create () in
      Queue.push (slot, trigger) m.waiters;
      Trigger.await trigger;
      ()


let unlock m = 
  if not m.locked then failwith "Mutex.unlock: already unlocked"
  else
    match find_live_waiter m.waiters with
    | true -> ()
    | false -> m.locked <- false; ()

let lock_evt m = Select.Evt {
  try_complete = (fun () ->
    if try_lock m then Some ()
    else None
  );
  offer = (fun slot trigger ->
    Queue.push (slot, trigger) m.waiters
  );
  wrap = Fun.id;
}