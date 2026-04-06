(** Manual concurrent tests for Batch Bounded Blocking Queue *)

let printf = Printf.printf

let assert_array_eq a b msg =
  if a <> b then begin
    printf "FAIL: %s\n  expected: [|%s|]\n  got:      [|%s|]\n" msg
      (Array.to_list a |> List.map string_of_int |> String.concat "; ")
      (Array.to_list b |> List.map string_of_int |> String.concat "; ");
    exit 1
  end

(** Test create, enq, deq, size, capacity in a single thread. *)
let test_sequential_basic () = 
  let q = BatchQueue.create 10 in
  assert(BatchQueue.capacity q = 10);
  assert(BatchQueue.size q = 0);
  BatchQueue.enq q [|1;2;3|];
  assert(BatchQueue.size q = 3);
  let result = BatchQueue.deq q 2 in
  assert_array_eq result [|1;2|] "Basic Deque Mismatch";
  assert(BatchQueue.size q = 1);
  ()

(** Test that invalid arguments raise [Invalid_argument]. *)
let test_error_handling () =
  try
    let _ = BatchQueue.create 0 in
    failwith "FAIL: create 0 should have raised an Invalid Argument!"
  with Invalid_argument e -> printf "SUCCESS: Caught the exception - %s\n" e;
  try
    let _ = BatchQueue.create (-5) in
    failwith "FAIL: create -5 should have raised an Invalid Argument!"
  with Invalid_argument e -> printf "SUCCESS: Caught the exception - %s\n" e;
  try
    let q = BatchQueue.create 5 in
    BatchQueue.enq q [|1;2;3;4;5;6;7|];
    failwith "FAIL: Enqueuing more than capacity should have raised an Invalid Argument!"
  with Invalid_argument e -> printf "SUCCESS: Caught the exception - %s\n" e;
  try
    let q = BatchQueue.create 5 in
    BatchQueue.enq q [||];
    failwith "FAIL: Enqueuing 0 sized array should have raised an Invalid Argument!"
  with Invalid_argument e -> printf "SUCCESS: Caught the exception - %s\n" e;
  try
    let q = BatchQueue.create 5 in
    BatchQueue.deq q 7;
    failwith "FAIL: Dequeue count more than capacity should have raised an Invalid Argument!"
  with Invalid_argument e -> printf "SUCCESS: Caught the exception - %s\n" e;
  try
    let q = BatchQueue.create 5 in
    BatchQueue.deq q 0;
    failwith "FAIL: Dequeue count 0 should have raised an Invalid Argument!"
  with Invalid_argument e -> printf "SUCCESS: Caught the exception - %s\n" e;
  try
    let q = BatchQueue.create 5 in
    BatchQueue.deq q (-3);
    failwith "FAIL: Dequeue count < 0 should have raised an Invalid Argument!"
  with Invalid_argument e -> printf "SUCCESS: Caught the exception - %s\n" e;


(** Test that deq blocks until items arrive (and/or enq blocks until space frees). *)
let test_blocking_enq_deq () =
  let q = BatchQueue.create 5 in
  let consumer = Domain.spawn(fun () ->
    BatchQueue.deq q 3
  ) in
  Unix.sleepf 0.1;
  BatchQueue.enq q [|1;2;3|];
  let result1 = Domain.join consumer in
  assert_array_eq result [|1;2;3|] "Deque sleep condition is faulty";
  BatchQueue.enq q [|1;2;3;4;5|]
  let producer = Domain.spawn(fun () ->
    BatchQueue.enq q [|6;7|]  
  ) in
  Unix.sleepf 0.1;
  let _result2 = BatchQueue.deq q 2 in
  Domain.join producer;
  assert(BatchQueue.size q = 5)

(** Test that a single producer/consumer pair sees items in FIFO order. *)
let test_fifo_single_producer_consumer () = failwith "TODO: implement"

(** Test dequeuer head-of-line blocking: deq(5) arrives before deq(2);
    even when 6 items are enqueued, deq(5) must be served first. *)
let test_dequeuer_head_of_line_blocking () = failwith "TODO: implement"

(** Test enqueuer head-of-line blocking: enq(3) arrives before enq(1);
    freeing 1 slot must NOT let enq(1) jump ahead. *)
let test_enqueuer_head_of_line_blocking () = failwith "TODO: implement"

(** Test that no items are lost or duplicated under concurrent access. *)
let test_no_lost_items () = failwith "TODO: implement"

(** Test that a batch enqueue is not interleaved with another batch. *)
let test_batch_atomicity () = failwith "TODO: implement"

(** Stress test: multiple producers and consumers with many operations. *)
let test_stress () = failwith "TODO: implement"

let () =
  test_sequential_basic ();
  test_error_handling ();
  test_blocking_enq_deq ();
  test_fifo_single_producer_consumer ();
  test_dequeuer_head_of_line_blocking ();
  test_enqueuer_head_of_line_blocking ();
  test_no_lost_items ();
  test_batch_atomicity ();
  test_stress ();
  printf "\nAll manual tests passed!\n"