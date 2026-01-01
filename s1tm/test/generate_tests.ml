
let write_file filename content =
  let oc = open_out filename in
  Printf.fprintf oc "%s" content;
  close_out oc

let tm_anbn = "M:
  states: [s, q1, q2, q3, q4, qA, qR]
  input_alphabet: [a, b]
  tape_alphabet_extra: [X, Y, _]
  start_state: s
  accept_state: qA
  reject_state: qR
  delta:
    s:
      a: [q1, X, R]
      _: [qA, _, S] # Empty input accepted
      Y: [qA, Y, S] # All matched
    q1:
      a: [q1, a, R]
      b: [q2, Y, L]
      Y: [q1, Y, R]
    q2:
      a: [q2, a, L]
      X: [s, X, R]
      Y: [q2, Y, L]
    q3: 
      # Cleanup check (not strictly needed if logical flow is correct for anbn)
      _: [qR, _, R]
"

let tm_palindrome = "M:
  # Palindromes over {0,1}
  states: [s, q0, q1, q2, q3, q4, qA, qR]
  input_alphabet: [0, 1]
  tape_alphabet_extra: [_, X]
  start_state: s
  accept_state: qA
  reject_state: qR
  delta:
    s:
      0: [q1, _, R] # Match 0
      1: [q2, _, R] # Match 1
      _: [qA, _, S] # Empty/Done
    q1: # Find mating 0 at right end
      0: [q1, 0, R]
      1: [q1, 1, R]
      _: [q3, _, L]
    q2: # Find mating 1 at right end
      0: [q2, 0, R]
      1: [q2, 1, R]
      _: [q4, _, L]
    q3: # Check for 0
      0: [q0, _, L] # Found, go back
      _: [qA, _, S] # Single 0, done
    q4: # Check for 1
      1: [q0, _, L] # Found, go back
      _: [qA, _, S] # Single 1, done
    q0: # Return to start
      0: [q0, 0, L]
      1: [q0, 1, L]
      _: [s, _, R]
"

let tm_101 = "M:
  # Contains substring 101
  states: [s, q1, q2, qA, qR]
  input_alphabet: [0, 1]
  tape_alphabet_extra: [_]
  start_state: s
  accept_state: qA
  reject_state: qR
  delta:
    s: # Start state, look for 1
      0: [s, 0, R]
      1: [q1, 1, R]
    q1: # Have 1, look for 0
      0: [q2, 0, R]
      1: [q1, 1, R] # 11... stay in q1
    q2: # Have 10, look for 1
      0: [s, 0, R] # 100... restart
      1: [qA, 1, S] # 101 Found!
"

let tm_even0 = "M:
  # Even number of 0s
  states: [s, q1, qA, qR]
  input_alphabet: [0, 1]
  tape_alphabet_extra: [_]
  start_state: s
  accept_state: qA
  reject_state: qR
  delta:
    s: # Even so far
      1: [s, 1, R]
      0: [q1, 0, R]
      _: [qA, _, S]
    q1: # Odd so far
      1: [q1, 1, R]
      0: [s, 0, R]
      _: [qR, _, S]
"

let tests = [
  (* anbn *)
  ("test05", tm_anbn, "");
  ("test06", tm_anbn, "ab");
  ("test07", tm_anbn, "aabb");
  ("test08", tm_anbn, "a");
  ("test09", tm_anbn, "b");
  ("test10", tm_anbn, "ba");
  ("test11", tm_anbn, "aabbb");
  
  (* Palindrome *)
  ("test12", tm_palindrome, "0");
  ("test13", tm_palindrome, "00");
  ("test14", tm_palindrome, "101");
  ("test15", tm_palindrome, "11011");
  ("test16", tm_palindrome, "10");
  ("test17", tm_palindrome, "100");
  
  (* Contains 101 *)
  ("test18", tm_101, "101");
  ("test19", tm_101, "0010100");
  ("test20", tm_101, "10");
  ("test21", tm_101, "000000");
  
  (* Even 0s *)
  ("test22", tm_even0, "");
  ("test23", tm_even0, "111");
  ("test24", tm_even0, "00");
  ("test25", tm_even0, "010");
  ("test26", tm_even0, "0");
  ("test27", tm_even0, "000");
]

let () =
  List.iter (fun (name, tm, w) ->
    let infile = name ^ ".in" in
    let expfile = name ^ ".exp" in
    let content = tm ^ "\nw: \"" ^ w ^ "\"\n" in
    write_file infile content;
    let cmd = Printf.sprintf "cat %s | ../_build/default/s1tm.exe > %s" infile expfile in
    let exit_code = Sys.command cmd in
    if exit_code <> 0 then Printf.printf "Failed to run test %s\n" name
    else Printf.printf "Generated %s\n" name
  ) tests;

  let dune_oc = open_out "dune_extra" in
  List.iter (fun (name, _, _) ->
    Printf.fprintf dune_oc "
(rule
  (with-outputs-to %s.out
  (with-accepted-exit-codes 0
    (run bash -c \"cat %%{dep:%s.in} | %%{exe:../s1tm.exe}\")
)))

(rule
  (alias runtest)
  (action (diff %s.exp %s.out))
)\n" name name name name
  ) tests;
  close_out dune_oc
