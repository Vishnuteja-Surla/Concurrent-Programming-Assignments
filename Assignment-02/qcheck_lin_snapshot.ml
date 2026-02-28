(* QCheck-Lin Linearizability Test for Atomic Snapshot

    This test verifies that the atomic snapshot implementation is linearizable
    under concurrent access.

    == Your Task ==

    Implement the QCheck-Lin specification for the atomic snapshot.
    Follow the examples from Lecture 3:
    - qcheck_lin_bounded.ml
    - qcheck_lin_lockfree.ml

    You need to:
    1. Define the Lin API specification module (SnapshotSig)
    2. Specify init() and cleanup() functions
    3. Define the api list using Lin's DSL (val_ combinator)
    4. Generate and run the linearizability test

    == Lin DSL Type Descriptors ==

    The Snapshot.scan function returns 'int array'. Use:
      returning (array int)

    The others are standard and follow the API from the lectures.

    == Expected Result ==

    This test should PASS. The double-collect algorithm ensures linearizability:
    every scan returns a consistent snapshot that corresponds to some actual
    state that existed during the scan operation. *)


open Lin

(** Lin API specification for the Snapshot queue *)
module SnapshotSig = struct
  type t = int Snapshot.t

  (* Create a snapshot of size 3 with 0s for testing *)
  let init () = Snapshot.create 3 0
  
  (* No cleanup is needed *)
  let cleanup _ = ()

  (* API description using Lin's combinator DSL *)
  let api =
    [
      (* update takes a snapshot (t), an index 0-2 (int_bound 2), a value (int), and returns unit *)
      val_ "update" Snapshot.update (t @-> int_bound 2 @-> int @-> returning_or_exc unit);
      
      (* scan takes a snapshot (t) and returns an int array *)
      val_ "scan" Snapshot.scan (t @-> returning (array int));
    ]
end

(* Generate the linearizability test from the specification *)
module SnapshotLin = Lin_domain.Make(SnapshotSig)

let () =
  QCheck_base_runner.run_tests_main [
    SnapshotLin.lin_test ~count:200 ~name:"Atomic Snapshot Linearizability"
  ]