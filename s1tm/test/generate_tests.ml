
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

let tm_odd_binary = "M:
  states: [s, qA, qR]
  input_alphabet: [0, 1]
  tape_alphabet_extra: [_]
  start_state: s
  accept_state: qA
  reject_state: qR
  delta:
    s: # Move to end
      0: [s, 0, R]
      1: [s, 1, R]
      _: [qA, _, L] # Check last digit (cheat: L from blank is last char)
    
    # Actually, simpler: verify if ends with 1
    # State qA will check
    # Wait, need specific states.
    # Let's rewrite: s scans right. q1 is 'last saw 0', q2 is 'last saw 1'.
    
  states: [s, q0, q1, qA, qR]
  start_state: s
  delta:
    s: # Empty
      0: [q0, 0, R]
      1: [q1, 1, R]
      _: [qR, _, S] # Empty string not odd
    q0: # Saw 0
      0: [q0, 0, R]
      1: [q1, 1, R]
      _: [qR, _, S] # Ends in 0 -> Even
    q1: # Saw 1
      0: [q0, 0, R]
      1: [q1, 1, R]
      _: [qA, _, S] # Ends in 1 -> Odd
"

let tm_unary_gt = "M:
  # Accepts 0^m 1 0^n where m > n
  states: [s, q1, q2, q3, q4, qA, qR]
  input_alphabet: [0, 1]
  tape_alphabet_extra: [_, x]
  start_state: s
  accept_state: qA
  reject_state: qR
  delta:
    s:
      0: [q1, x, R] # Mark left 0
      1: [qR, 1, S] # No left 0s or started with 1 (fail if m=0)
      x: [s, x, R] # Skip marked
    q1:
      0: [q1, 0, R]
      1: [q2, 1, R] # Found separator
      x: [q1, x, R]
    q2:
      x: [q2, x, R] # Skip marked right 0s
      0: [q3, x, L] # Mark right 0
      _: [qA, _, S] # No right 0s left! m > n verified (since we marked a left 0 and failed to find a right 0)
    q3:
      x: [q3, x, L]
      0: [q3, 0, L]
      1: [q4, 1, L] # Back passed separator
    q4:
      0: [q4, 0, L] # Go back to start
      x: [s, x, R] # Found marker, restart s
"

let tm_0101 = "M:
  # Contains 0101
  states: [s, q1, q2, q3, qA, qR]
  input_alphabet: [0, 1]
  tape_alphabet_extra: [_]
  start_state: s
  accept_state: qA
  reject_state: qR
  delta:
    s:
      0: [q1, 0, R]
      1: [s, 1, R]
    q1: # Have 0
      0: [q1, 0, R]
      1: [q2, 1, R] # Have 01
    q2: # Have 01
      0: [q3, 0, R] # Have 010
      1: [s, 1, R] # 011 -> Reset (fail sequence)
    q3: # Have 010
      0: [q1, 0, R] # 0100 -> Have 0
      1: [qA, 1, S] # 0101 -> Accept
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

  (* Binary Increment (Accepts valid binary strings, output not checked by simple YES/NO runner but logic runs)
     Wait, s1tm only prints YES/NO. So let's test a DECIDER problem.
     Problem: Decide if w is a valid binary string representing an ODD number.
  *)
  ("test28", tm_odd_binary, "1");
  ("test29", tm_odd_binary, "101");
  ("test30", tm_odd_binary, "11");
  ("test31", tm_odd_binary, "0");
  ("test32", tm_odd_binary, "10");
  ("test33", tm_odd_binary, "000");
  ("test34", tm_odd_binary, "110");
  ("test35", tm_odd_binary, "");

  (* Unary Subtraction (Decider: Accept if length m > length n in 0^m 1 0^n) *)
  ("test36", tm_unary_gt, "0010");
  ("test37", tm_unary_gt, "01");
  ("test38", tm_unary_gt, "000100");
  ("test39", tm_unary_gt, "010");
  ("test40", tm_unary_gt, "0100");
  ("test41", tm_unary_gt, "1");
  ("test42", tm_unary_gt, "00100");

  (* Pattern 0101 *)
  ("test43", tm_0101, "0101");
  ("test44", tm_0101, "001011");
  ("test45", tm_0101, "111010100");
  ("test46", tm_0101, "010");
  ("test47", tm_0101, "011001");
  ("test48", tm_0101, "0000000");
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
