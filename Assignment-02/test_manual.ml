(** Manual Concurrent Tests for Atomic Snapshot

    These tests verify basic correctness of the snapshot implementation
    using manual concurrent scenarios.

    Each test includes clear expectations - read the comments above each
    test function to understand what behavior is required.
*)

(** Test 1: Sequential operations

    WHAT TO IMPLEMENT:
    - Snapshot.create should initialize n registers with the given value
    - Snapshot.update should set a register to a new value
    - Snapshot.scan should return an array containing all register values

    EXPECTED BEHAVIOR:
    - Create a snapshot with 4 registers initialized to 0
    - Update each register to a different value (10, 20, 30, 40)
    - Scan should return exactly [| 10; 20; 30; 40 |]

    This test should PASS if your basic operations work correctly.
*)
let test_sequential () =
  Printf.printf "Running Sequential test...\n%!";
  let s = Snapshot.create 4 0 in
  Snapshot.update s 0 10;
  Snapshot.update s 1 20;
  Snapshot.update s 2 30;
  Snapshot.update s 3 40;
  let result = Snapshot.scan s in
  assert(result = [|10; 20; 30; 40|]);
  ()

(** Test 2: Concurrent updates, single scanner

    WHAT TO IMPLEMENT:
    - Snapshot.update must be thread-safe (use Atomic.set, not ref)
    - Multiple threads can update different registers simultaneously

    EXPECTED BEHAVIOR:
    - 4 domains each update their own register 100 times
    - Domain 0 writes values 0..100 to register 0
    - Domain 1 writes values 1000..1100 to register 1, etc.
    - Final scan should see a valid state where:
      * Register 0 has a value between 0 and 100
      * Register 1 has a value between 1000 and 1100
      * Register 2 has a value between 2000 and 2100
      * Register 3 has a value between 3000 and 3100

    This test should PASS if you use Atomic.t correctly (no data races).
*)
let test_concurrent_updates () =
  Printf.printf "Running concurrent updates test...\n%!";
  let n = 4 in
  let s = Snapshot.create n 0 in

  let domains = 
    Array.init n (fun i -> 
      Domain.spawn (fun () ->
          for j = 0 to 99 do
            Snapshot.update s i ((i * 1000) + j)
          done
        )
      )
    in
    Array.iter Domain.join domains;
  
  let result = Snapshot.scan s in
    for i = 0 to (n - 1) do
      let min_val = (i*1000) in
      let max_val = (i*1000) + 100 in
      assert(result.(i) >= min_val && result.(i) < max_val)
    done;
    ()



(** Test 3: Multiple concurrent scanners - THE CRITICAL TEST FOR DOUBLE-COLLECT

    WHAT TO IMPLEMENT:
    - Snapshot.scan must use the DOUBLE-COLLECT algorithm
    - This ensures every scan returns a LINEARIZABLE (consistent) snapshot

    EXPECTED BEHAVIOR:
    - One updater continuously writes i, i*10, i*100 to registers 0, 1, 2
    - 4 scanner threads each perform 50 scans while updates happen
    - EVERY scan must see a consistent state:
      * The iteration number visible in each register must be non-increasing
        left to right: r0 >= r1/10 >= r2/100
      * Example valid states: [0,0,0], [5,50,500], [23,230,2300]
      * Scans can see partially-written states (registers updated left to right)
      * Example valid states: [5,40,400], [5,50,400]
      * Example INVALID state: [5,50,600] (r2/100=6 > r0=5: never existed!)

    WHY THIS MATTERS:
    - Without double-collect, you might see [5, 50, 600] - a state that
      NEVER actually existed atomically
    - Double-collect guarantees you only see states that truly existed

    This test should PASS (all 200 scans consistent) ONLY if you implement
    the double-collect algorithm correctly. A naive scan will fail here.
*)
let test_concurrent_scans () =
  Printf.printf "Running concurrent scan test...\n%!";
  let s = Snapshot.create 3 0 in

  (* One Updater Domain *)
  let updater = Domain.spawn (fun () ->
      for i = 0 to 100 do
        Snapshot.update s 0 i;
        Snapshot.update s 1 (i*10);
        Snapshot.update s 2 (i*100);
        ()
      done
    ) in

  (* 4 Scanner Domains *)
    let scanners = 
      Array.init 4 (fun _ ->
        Domain.spawn(fun () ->
          for _ = 1 to 50 do
            let arr = Snapshot.scan s in
            let r0 = arr.(0) in
            let r1 = arr.(1) in
            let r2 = arr.(2) in
            assert((r0 >= (r1/10)) && (r1 >= (r2/10)))
          done
        )
      )
        in
        Domain.join updater;
        Array.iter Domain.join scanners;
        ()

(** Test 4: High contention stress test

    WHAT TO IMPLEMENT:
    - Your implementation must handle many threads reading/writing simultaneously
    - No deadlocks, no crashes, no data races

    EXPECTED BEHAVIOR:
    - 8 threads run simultaneously for 1000 iterations each
    - Even-numbered threads write to registers
    - Odd-numbered threads scan continuously
    - Test should complete without hanging or crashing

    This test should PASS if your atomic operations are correct and your
    double-collect handles high contention gracefully.
*)
let test_high_contention () =
  Printf.printf "Running high contention test...\n%!";
  let n = 3 in
  let s = Snapshot.create n 0 in
  let domains = 
    Array.init 8 (fun id ->
        Domain.spawn(fun () ->
          for i = 0 to 1000 do
            if id mod 2 = 0 then
              Snapshot.update s (i mod n) i
            else
              ignore (Snapshot.scan s)
          done
        )
      )
        in
        Array.iter Domain.join domains;
        ()

(** Main test runner *)
let () =
  test_sequential ();
  Printf.printf "Sequential test passed successfully!\n%!";
  test_concurrent_updates ();
  Printf.printf "Concurrent Updates test passed successfully!\n%!";
  test_concurrent_scans ();
  Printf.printf "Concurrent Scans test passed successfully!\n%!";
  test_high_contention ();
  Printf.printf "High Contention test passed successfully!\n%!";
  Printf.printf "All manual tests passed!\n%!"