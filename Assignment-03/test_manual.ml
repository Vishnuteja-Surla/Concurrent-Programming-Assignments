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
  assert_array_eq result1 [|1;2;3|] "FAIL: Blocking Enq-Deq Test failed!";
  BatchQueue.enq q [|1;2;3;4;5|];
  let producer = Domain.spawn(fun () ->
    BatchQueue.enq q [|6;7|]
  ) in
  Unix.sleepf 0.1;
  let _result2 = BatchQueue.deq q 2 in
  Domain.join producer;
  assert(BatchQueue.size q = 5)

(** Test that a single producer/consumer pair sees items in FIFO order. *)
let test_fifo_single_producer_consumer () =
  let q = BatchQueue.create 10 in
  let producer = Domain.spawn(fun () ->
    BatchQueue.enq q [|1;2;3|];
    BatchQueue.enq q [|4;5|];
    BatchQueue.enq q [|6;7;8|];
    BatchQueue.enq q [|9;10|]
  ) in
  let consumer = Domain.spawn(fun () ->
    let chunk1 = BatchQueue.deq q 4 in
    let chunk2 = BatchQueue.deq q 6 in
    Array.append chunk1 chunk2
  ) in
  Domain.join producer;
  let result = Domain.join consumer in
  assert_array_eq result [|1;2;3;4;5;6;7;8;9;10|] "FAIL: FIFO Single Producer-Consumer test failed!";

(** Test dequeuer head-of-line blocking: deq(5) arrives before deq(2);
    even when 6 items are enqueued, deq(5) must be served first. *)
let test_dequeuer_head_of_line_blocking () = 
  let q = BatchQueue.create 6 in
  let step = Atomic.make 0 in
  let threadA = Domain.spawn(fun () ->
    Atomic.set step 1;
    BatchQueue.deq q 5
  ) in  
  while Atomic.get step < 1 do
    Domain.cpu_relax ()
  done;
  Unix.sleepf 0.05;

  let threadB = Domain.spawn(fun () -> 
    Atomic.set step 2;
    BatchQueue.deq q 2
  ) in  
  while Atomic.get step < 2 do
    Domain.cpu_relax ()
  done;
  Unix.sleepf 0.05;

  BatchQueue.enq q [|1;2;3;4;5;6|];
  Unix.sleepf 0.05;
  BatchQueue.enq q [|7|];
  let resA = Domain.join threadA in
  let resB = Domain.join threadB in
  assert(BatchQueue.size q = 0);
  assert_array_eq resA [|1;2;3;4;5|] "FAIL: Unexpected Deque ordering!";
  assert_array_eq resB [|6;7|] "FAIL: Unexpected Deque ordering!"

(** Test enqueuer head-of-line blocking: enq(3) arrives before enq(1);
    freeing 1 slot must NOT let enq(1) jump ahead. *)
let test_enqueuer_head_of_line_blocking () =
  let q = BatchQueue.create 3 in
  let step = Atomic.make 0 in
  BatchQueue.enq q [|1;2;3|];
  let threadA = Domain.spawn(fun () ->
    Atomic.set step 1;
    BatchQueue.enq q [|4;5;6|]
  ) in
  while Atomic.get step < 1 do
    Domain.cpu_relax ()
  done;
  Unix.sleepf 0.05;
  let threadB = Domain.spawn(fun () ->
    Atomic.set step 2;
    BatchQueue.enq q [|7|]
  ) in
  while Atomic.get step < 2 do
    Domain.cpu_relax ()
  done;
  Unix.sleepf 0.05;

  let _d1 = BatchQueue.deq q 1 in
  Unix.sleepf 0.05;
  assert(BatchQueue.size q = 2);

  let _d2 = BatchQueue.deq q 2 in
  Unix.sleepf 0.05;
  Domain.join threadA;

  let _d3 = BatchQueue.deq q 3 in
  Unix.sleepf 0.05;
  Domain.join threadB;

  let _d4 = BatchQueue.deq q 1 in
  assert(BatchQueue.size q = 0)

(** Test that no items are lost or duplicated under concurrent access. *)
let test_no_lost_items () =
  let total_items = 400 in
  let q = BatchQueue.create 50 in
  let make_chunk start size = Array.init size (fun i -> start + i) in
  let producers = Array.init 4 (fun i ->
    Domain.spawn (fun () ->
      for j = 0 to 9 do
        let start_val = (i * 100) + (j * 10) + 1 in
        BatchQueue.enq q (make_chunk start_val 10)
      done
    )
  ) in
  let consumers = Array.init 4 (fun _ ->
    Domain.spawn (fun () ->
      let acc = ref [||] in
      let count = ref 0 in
      while !count < 100 do
        let chunk = BatchQueue.deq q 5 in
        acc := Array.append !acc chunk;
        count := !count + 5
      done;
      !acc 
    )
  ) in
  Array.iter Domain.join producers;
  let results = Array.map Domain.join consumers in
  let massive_array = Array.concat (Array.to_list results) in
  Array.sort compare massive_array;
  assert (Array.length massive_array = total_items);
  for i = 0 to (total_items - 1) do
    if massive_array.(i) <> i + 1 then begin
      Printf.printf "FAIL: Lost or duplicated item! Expected %d at index %d but got %d\n" 
        (i + 1) i massive_array.(i);
      assert false
    end
  done

(** Test that a batch enqueue is not interleaved with another batch. *)
let test_batch_atomicity () =
  let q = BatchQueue.create 10 in
  let producerA = Domain.spawn(fun () ->
    BatchQueue.enq q [|1;1;1|]
  ) in
  let producerB = Domain.spawn(fun () ->
    BatchQueue.enq q [|2;2;2|]
  ) in
  let producerC = Domain.spawn(fun () ->
    BatchQueue.enq q [|3;3;3|]
  ) in
  Domain.join producerA;
  Domain.join producerB;
  Domain.join producerC;

  let r1 = BatchQueue.deq q 3 in
  let r2 = BatchQueue.deq q 3 in
  let r3 = BatchQueue.deq q 3 in
  assert(r1.(0) = r1.(1) && r1.(1) = r1.(2));
  assert(r2.(0) = r2.(1) && r2.(1) = r2.(2));
  assert(r3.(0) = r3.(1) && r3.(1) = r3.(2))

(** Stress test: multiple producers and consumers with many operations. *)
let test_stress () =
  let q = BatchQueue.create 10 in
  let iterations = 1000 in
  let num_threads = 10 in
  let producers = Array.init num_threads (fun _ ->
    Domain.spawn (fun () ->
      for _ = 1 to iterations do
        BatchQueue.enq q [|1; 2|];
        BatchQueue.enq q [|3; 4; 5|]
      done
    )
  ) in

  let consumers = Array.init num_threads (fun _ ->
    Domain.spawn (fun () ->
      for _ = 1 to iterations do
        ignore (BatchQueue.deq q 4);
        ignore (BatchQueue.deq q 1)
      done
    )
  ) in
  Array.iter Domain.join producers;
  Array.iter Domain.join consumers;

  assert (BatchQueue.size q = 0)

let () =
  test_sequential_basic ();
  Printf.printf "Sequential Basic test passed successfully!\n%!";
  test_error_handling ();
  Printf.printf "Error handling test passed successfully!\n%!";
  test_blocking_enq_deq ();
  Printf.printf "Blocking Enq-Deq test passed successfully!\n%!";
  test_fifo_single_producer_consumer ();
  Printf.printf "FIFO Single Producer-Consumer test passed successfully!\n%!";
  test_dequeuer_head_of_line_blocking ();
  Printf.printf "Dequeuer Head of Line Blocking test passed successfully!\n%!";
  test_enqueuer_head_of_line_blocking ();
  Printf.printf "Enqueuer Head of Line Blocking test passed successfully!\n%!";
  test_no_lost_items ();
  Printf.printf "No Lost items test passed successfully!\n%!";
  test_batch_atomicity ();
  Printf.printf "Batch Atomicity test passed successfully!\n%!";
  test_stress ();
  Printf.printf "Stress test passed successfully!\n%!";
  printf "\nAll manual tests passed!\n%!"