(** QCheck-Lin Linearizability Test for Batch Bounded Blocking Queue

    This test verifies that the non-blocking try_enq/try_deq operations
    are linearizable under concurrent access.

    == Your Task ==

    Complete the BatchQueueSig module below:
    1. Define wrapper functions for batch operations with fixed sizes
    2. Define the [api] list using Lin's DSL ([val_] combinator)

    == Design Note ==

    try_enq takes an int array argument and try_deq returns int array option.
    The Lin DSL works best with primitive types, so we define wrapper functions
    for small fixed batch sizes (1, 2, 3) and register each as a separate
    operation. This tests the core batch logic with manageable complexity.

    == Expected Result ==

    This test should PASS. The mutex-based implementation ensures all
    operations are linearizable.
*)

module BQ = BatchQueue

(** Lin API specification for the batch queue *)
module BatchQueueSig = struct
  type t = int BQ.t

  let init () = BQ.create 6

  let cleanup _ = ()

  open Lin
  let int_small = nat_small
  let _ = (t, int_small)

  (* Enqueue Wrappers *)
  let try_enq1 q x = BQ.try_enq q [| x |]
  let try_enq2 q x y = BQ.try_enq q [| x; y |]
  let try_enq3 q x y z = BQ.try_enq q [| x; y; z |]

  (* Dequeue Wrappers *)
  let try_deq1 q = BQ.try_deq q 1
  let try_deq2 q = BQ.try_deq q 2
  let try_deq3 q = BQ.try_deq q 3

  let api = [
    val_ "try_enq1" try_enq1 (t @-> int_small @-> returning bool);
    val_ "try_deq1" try_deq1 (t @-> returning (option (array int_small)));
    val_ "try_enq2" try_enq2 (t @-> int_small @-> int_small @-> returning bool);
    val_ "try_deq2" try_deq2 (t @-> returning (option (array int_small)));
    val_ "try_enq3" try_enq3 (t @-> int_small @-> int_small @-> int_small @-> returning bool);
    val_ "try_deq3" try_deq3 (t @-> returning (option (array int_small)));
    val_ "size" BQ.size (t @-> returning int);
    val_ "capacity" BQ.capacity (t @-> returning int);
  ]
end

(** Generate the linearizability test from the specification *)
module BQ_domain = Lin_domain.Make(BatchQueueSig)

(** Run 1000 test iterations *)
let () =
  QCheck_base_runner.run_tests_main [
    BQ_domain.lin_test ~count:1000 ~name:"Batch queue linearizability";
  ]