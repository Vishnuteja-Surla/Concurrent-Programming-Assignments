(** A fiber-level condition variable.

    Implementation: a plain FIFO queue of triggers.  {!wait} relies on
    the unicore invariant that "no context switch happens unless we
    perform an effect (await / yield / a channel op)" — this is what
    makes the enqueue-then-unlock sequence atomic, without needing any
    extra state-machine gymnastics.
*)

type t = {
  waiters : Trigger.t Queue.t;
}

let create () = {waiters = Queue.create ()}

let wait c m = 
  let trigger = Trigger.create () in
  Queue.push trigger c.waiters;
  Mutex.unlock m;
  Trigger.await trigger;
  Mutex.lock m;
  ()


(** Pop triggers until one is signalled successfully (skipping stale
    ones — unused here because [Condition] does not participate in
    [Select], but kept for symmetry and future extension). *)
let rec signal_one waiters =
  if Queue.is_empty waiters then ()
  else
    let trigger = Queue.pop waiters in
    if not (Trigger.signal trigger) then signal_one waiters

let signal c = signal_one c.waiters

let broadcast c =
  while not (Queue.is_empty c.waiters) do
    signal_one c.waiters
  done