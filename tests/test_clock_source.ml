(* Test for different clock sources *)

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ())

let test_timeout_with_clock t clock =
  let timeout_ns = 100_000_000L in (* 100ms *)
  let start = Unix.gettimeofday () in
  let job = Uring.timeout t clock timeout_ns () in
  assert (job <> None);
  let res = Uring.submit t in
  Printf.eprintf "[%s] Submitted %d timeout request(s)\n%!"
    (match clock with
     | Uring.Monotonic -> "Monotonic"
     | Uring.Boottime -> "Boottime"
     | Uring.Realtime -> "Realtime")
    res;

  (* Wait for timeout completion *)
  let rec wait_completion () =
    match Uring.wait t with
    | None -> wait_completion ()
    | Some { result; data = () } ->
        let elapsed = Unix.gettimeofday () -. start in
        Printf.eprintf "  Timeout completed with result %d after %.3f seconds\n%!" result elapsed;
        assert (result = -62); (* -ETIME *)
        assert (elapsed >= 0.09 && elapsed <= 0.15) (* Allow some tolerance *)
  in
  wait_completion ()

let test_register_clock t =
  Printf.eprintf "\nTesting clock registration...\n%!";

  (* Try registering each clock type *)
  let test_register clock_name clock =
    try
      Uring.register_clock t clock;
      Printf.eprintf "  Successfully registered %s clock\n%!" clock_name
    with Unix.Unix_error (err, _, _) ->
      Printf.eprintf "  Failed to register %s clock: %s (kernel may not support IORING_REGISTER_CLOCK)\n%!"
        clock_name (Unix.error_message err)
  in

  test_register "Monotonic" Uring.Monotonic;
  test_register "Boottime" Uring.Boottime;
  test_register "Realtime" Uring.Realtime

let test_absolute_timeout t =
  Printf.eprintf "\nTesting absolute timeout with Realtime clock...\n%!";

  (* Get current time and add 100ms *)
  (* Note: io_uring expects seconds and nanoseconds in the timespec *)
  let current_time = Unix.gettimeofday () in
  let target_time = current_time +. 0.1 in (* 100ms from now *)
  let target_sec = Int64.of_float target_time in
  let target_nsec = Int64.of_float ((target_time -. float_of_int (Int64.to_int target_sec)) *. 1e9) in
  (* Combine seconds and nanoseconds for the absolute timeout value *)
  let target_ns = Int64.add (Int64.mul target_sec 1_000_000_000L) target_nsec in

  let start = Unix.gettimeofday () in
  let job = Uring.timeout ~absolute:true t Uring.Realtime target_ns () in
  assert (job <> None);
  let res = Uring.submit t in
  Printf.eprintf "  Submitted %d absolute timeout request(s)\n%!" res;

  let rec wait_completion () =
    match Uring.wait t with
    | None -> wait_completion ()
    | Some { result; data = () } ->
        let elapsed = Unix.gettimeofday () -. start in
        Printf.eprintf "  Absolute timeout completed with result %d after %.3f seconds\n%!" result elapsed;
        assert (result = -62); (* -ETIME *)
        (* For absolute timeouts, the timing might be slightly different *)
        assert (elapsed >= 0.08 && elapsed <= 0.20) (* Allow more tolerance for absolute *)
  in
  wait_completion ()

let () =
  let t = Uring.create ~queue_depth:8 () in

  Printf.eprintf "Testing timeouts with different clock sources...\n%!";

  (* Test relative timeouts with each clock type *)
  test_timeout_with_clock t Uring.Monotonic;
  test_timeout_with_clock t Uring.Boottime;
  test_timeout_with_clock t Uring.Realtime;

  (* Test clock registration *)
  test_register_clock t;

  (* Test absolute timeout *)
  test_absolute_timeout t;

  Printf.eprintf "\nAll clock source tests completed successfully!\n%!";
  Uring.exit t