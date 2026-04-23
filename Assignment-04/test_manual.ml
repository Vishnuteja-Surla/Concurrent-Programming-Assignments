(** Manual tests for fiber-level Mutex / Condition / Semaphore / Barrier.

    Each test function returns [(ok, msg)] — [ok] is pass/fail, [msg]
    is a short description printed in the report.  Most tests run
    their fibers inside [Sched.run (fun () -> ...)]; remember that the
    scheduler is cooperative unicore, so you'll want [Sched.yield] to
    force interleavings. *)

open Golike_unicore_select

let passed = ref 0
let failed = ref 0

let report name ok msg =
  if ok then begin
    incr passed;
    Printf.printf "[ PASS ] %s — %s\n%!" name msg
  end else begin
    incr failed;
    Printf.printf "[ FAIL ] %s — %s\n%!" name msg
  end

let run_test name f =
  try
    let ok, msg = f () in
    report name ok msg
  with e ->
    incr failed;
    Printf.printf "[ EXN  ] %s — %s\n%!" name (Printexc.to_string e)

(** Test [try_lock] / [unlock] on a single mutex, sequentially. *)
let test_mutex_basic () =let m = Mutex.create () in
  let b1 = Mutex.try_lock m in
  let b2 = Mutex.try_lock m in
  Mutex.unlock m;
  let b3 = Mutex.try_lock m in
  (b1 && not b2 && b3, "basic sequential try_lock/unlock")

(** Test that blocked waiters are served in FIFO order. *)
let test_mutex_fifo () = 
  let m = Mutex.create () in
  let order = ref [] in
  Sched.run(fun () ->
    Mutex.lock m;
    Sched.fork(fun () ->
      Mutex.lock m;
      order := 1 :: !order;
      Mutex.unlock m
    );
    Sched.fork(fun () ->
      Mutex.lock m;
      order := 2 :: !order;
      Mutex.unlock m
    );
    Sched.yield ();
    Mutex.unlock m
  );
  (!order = [2; 1], "waiters are woken in strict FIFO order")

(** The [Bounded_buffer] module below is PROVIDED — a classic
    Mutex + two-condvar implementation of a bounded FIFO queue.
    Do not modify it; use it in [test_bounded_buffer]. *)

module Bounded_buffer = struct
  type 'a t = {
    m : Mutex.t;
    not_empty : Condition.t;
    not_full : Condition.t;
    buf : 'a Queue.t;
    capacity : int;
  }
  let create capacity = {
    m = Mutex.create ();
    not_empty = Condition.create ();
    not_full = Condition.create ();
    buf = Queue.create ();
    capacity;
  }
  let put b x =
    Mutex.lock b.m;
    while Queue.length b.buf = b.capacity do
      Condition.wait b.not_full b.m
    done;
    Queue.push x b.buf;
    Condition.signal b.not_empty;
    Mutex.unlock b.m
  let get b =
    Mutex.lock b.m;
    while Queue.is_empty b.buf do
      Condition.wait b.not_empty b.m
    done;
    let x = Queue.pop b.buf in
    Condition.signal b.not_full;
    Mutex.unlock b.m;
    x
end

(** Test bounded-buffer throughput — no items lost, no duplicates,
    under multiple concurrent producers and consumers. *)
let test_bounded_buffer () =
  let b = Bounded_buffer.create 2 in
  let out = ref [] in
  
  Sched.run (fun () ->
    Sched.fork (fun () ->
      Bounded_buffer.put b 1;
      Bounded_buffer.put b 2;
      Bounded_buffer.put b 3;
    );
    
    Sched.fork (fun () ->
      let v1 = Bounded_buffer.get b in
      let v2 = Bounded_buffer.get b in
      let v3 = Bounded_buffer.get b in
      out := [v1; v2; v3]
    )
  );
  
  (!out = [1; 2; 3], "bounded buffer respects capacity and signals")

(** The [Rw_lock] module below is PROVIDED — writer-priority R/W lock.
    Do not modify it; use it in [test_readers_writers]. *)

module Rw_lock = struct
  type t = {
    m : Mutex.t;
    can_read : Condition.t;
    can_write : Condition.t;
    mutable readers : int;
    mutable writer : bool;
    mutable waiting_writers : int;
  }
  let create () = {
    m = Mutex.create ();
    can_read = Condition.create ();
    can_write = Condition.create ();
    readers = 0; writer = false; waiting_writers = 0;
  }
  let read_lock r =
    Mutex.lock r.m;
    while r.writer || r.waiting_writers > 0 do
      Condition.wait r.can_read r.m
    done;
    r.readers <- r.readers + 1;
    Mutex.unlock r.m
  let read_unlock r =
    Mutex.lock r.m;
    r.readers <- r.readers - 1;
    if r.readers = 0 then Condition.signal r.can_write;
    Mutex.unlock r.m
  let write_lock r =
    Mutex.lock r.m;
    r.waiting_writers <- r.waiting_writers + 1;
    while r.writer || r.readers > 0 do
      Condition.wait r.can_write r.m
    done;
    r.waiting_writers <- r.waiting_writers - 1;
    r.writer <- true;
    Mutex.unlock r.m
  let write_unlock r =
    Mutex.lock r.m;
    r.writer <- false;
    if r.waiting_writers > 0 then Condition.signal r.can_write
    else Condition.broadcast r.can_read;
    Mutex.unlock r.m
end

(** Test the R/W exclusion invariants: at most one writer,
    readers and writers never coexist. *)
let test_readers_writers () =
  let rw = Rw_lock.create () in
  let readers_active = ref 0 in
  let writers_active = ref 0 in
  let invariant_failed = ref false in

  let check_invariant () =
    if !writers_active > 1 || (!writers_active = 1 && !readers_active > 0) then
      invariant_failed := true
  in

  Sched.run (fun () ->
    (* Spawn 3 Readers *)
    for _ = 1 to 3 do
      Sched.fork (fun () ->
        Rw_lock.read_lock rw;
        readers_active := !readers_active + 1;
        check_invariant ();
        
        Sched.yield (); (* Try to let someone else sneak in! *)
        
        readers_active := !readers_active - 1;
        Rw_lock.read_unlock rw
      )
    done;
    
    (* Spawn 2 Writers *)
    for _ = 1 to 2 do
      Sched.fork (fun () ->
        Rw_lock.write_lock rw;
        writers_active := !writers_active + 1;
        check_invariant ();
        
        Sched.yield (); (* Try to let someone else sneak in! *)
        
        writers_active := !writers_active - 1;
        Rw_lock.write_unlock rw
      )
    done
  );
  
  (not !invariant_failed, "R/W lock maintains exclusion invariants")

(** Test reusable N-party barrier: no fiber is more than one round
    ahead of any other across multiple barrier crossings. *)
let test_barrier () = 
  let n = 3 in
  let b = Barrier.create n in
  let count_round1 = ref 0 in
  let count_round2 = ref 0 in
  
  Sched.run (fun () ->
    for _ = 1 to n do
      Sched.fork (fun () ->
        (* --- Round 1 --- *)
        Barrier.wait b;
        incr count_round1;
        
        (* --- Round 2 --- *)
        Barrier.wait b;
        incr count_round2
      )
    done
  );
  
  (!count_round1 = n && !count_round2 = n, "barrier synchronizes multiple rounds")

(** Test that a semaphore with [k] permits never allows more than
    [k] fibers in the critical section simultaneously. *)
let test_semaphore () =
  let k = 2 in
  let sem = Semaphore.create k in
  let inside = ref 0 in
  let max_inside = ref 0 in

  Sched.run(fun () ->
    for _ = 1 to 4 do
      Sched.fork(fun () ->
        Semaphore.acquire sem;
        
        inside := !inside + 1;
        if !inside > !max_inside then max_inside := !inside;

        Sched.yield ();

        inside := !inside - 1;

        Semaphore.release sem;
      )
    done
  );

  (!max_inside = k, "semaphore respects permit limit")

(** Test that [Select.select] picks an already-free mutex in phase 1
    (the fast path). *)
let test_lock_evt_fastpath () =
  let m = Mutex.create () in
  let ok = ref false in
  
  Sched.run (fun () ->
    Select.select [Mutex.lock_evt m];
    
    Mutex.unlock m;
    ok := true
  );
  
  (!ok, "lock_evt fastpath acquires free mutex instantly")

(** Test [Select.select] over two held mutexes — it should block until
    one is unlocked, then take that case; stale waiter on the other
    mutex must be tolerated. *)
let test_lock_evt_blocking () =
  let m1 = Mutex.create () in
  let m2 = Mutex.create () in
  let won_m1 = ref false in
  
  Sched.run (fun () ->
    (* 1. Main grabs both locks *)
    Mutex.lock m1;
    Mutex.lock m2;
    
    (* 2. Selector Fiber tries to grab whichever becomes free first *)
    Sched.fork (fun () ->
      Select.select [
        Mutex.lock_evt m1 |> Select.wrap (fun () -> won_m1 := true; Mutex.unlock m1);
        Mutex.lock_evt m2 |> Select.wrap (fun () -> won_m1 := false; Mutex.unlock m2)
      ]
    );
    
    (* 3. Yield to let Selector enqueue on both mutexes *)
    Sched.yield ();
    
    (* 4. Unlock m1. Selector wakes up and wins m1! *)
    Mutex.unlock m1;
    
    (* 5. Yield to let Selector actually run its winning case *)
    Sched.yield ();
    
    (* 6. Unlock m2. Selector left a stale trigger here. 
          Your mutex must silently skip it without crashing. *)
    Mutex.unlock m2
  );
  
  (!won_m1, "select blocks and tolerates stale waiters")

(** Test the load-balancer pattern from Lecture 10's [_scratch/test1.ml]:
    many clients race to claim any of several slot mutexes via
    [Select.select] over [lock_evt]. *)
let test_load_balancer () =
  let num_slots = 3 in
  let num_clients = 10 in
  
  (* Create an array of 3 distinct Mutexes *)
  let slots = Array.init num_slots (fun _ -> Mutex.create ()) in
  let jobs_done = ref 0 in
  
  Sched.run (fun () ->
    for _i = 1 to num_clients do
      Sched.fork (fun () ->
        
        (* Build a list of events: "try to lock slot 0", "try to lock slot 1", etc. 
           We wrap the event to simply return the mutex that was won. *)
        let events = 
          Array.to_list slots
          |> List.map (fun m -> 
               Mutex.lock_evt m |> Select.wrap (fun () -> m)
             )
        in
        
        (* Block until ANY slot becomes available *)
        let won_mutex = Select.select events in
        
        (* --- CRITICAL SECTION --- *)
        (* Yield to simulate work and force the scheduler to interleave clients *)
        Sched.yield ();
        
        jobs_done := !jobs_done + 1;
        (* ------------------------ *)
        
        (* Release the slot back to the hungry mob *)
        Mutex.unlock won_mutex
      )
    done
  );
  
  (!jobs_done = num_clients, "load balancer safely serves all clients")

(** Test that [Condition.wait] re-acquires the mutex before returning
    (POSIX semantics). *)
let test_wait_reacquires () = 
  let m = Mutex.create () in
  let c = Condition.create () in
  let ok = ref false in
  
  Sched.run (fun () ->
    Sched.fork (fun () ->
      Mutex.lock m;
      Condition.wait c m;
      Mutex.unlock m;
      ok := true
    );
    
    Sched.yield ();
    
    Mutex.lock m;
    Condition.signal c;
    Mutex.unlock m
  );
  
  (!ok, "Condition.wait reacquires the mutex before returning")

let () =
  Printf.printf "=== Manual tests (fiber-level Mutex/Cond/Sem/Bar) ===\n%!";
  run_test "mutex_basic"           test_mutex_basic;
  run_test "mutex_fifo"            test_mutex_fifo;
  run_test "bounded_buffer"        test_bounded_buffer;
  run_test "readers_writers"       test_readers_writers;
  run_test "barrier"               test_barrier;
  run_test "semaphore"             test_semaphore;
  run_test "lock_evt_fastpath"     test_lock_evt_fastpath;
  run_test "lock_evt_blocking"     test_lock_evt_blocking;
  run_test "load_balancer"         test_load_balancer;
  run_test "wait_reacquires"       test_wait_reacquires;
  Printf.printf "\n%d passed, %d failed\n%!" !passed !failed;
  if !failed > 0 then exit 1