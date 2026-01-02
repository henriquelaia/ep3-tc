type estado = string [@@deriving yaml]

type simbolo = string

let simbolo_to_yaml s = `String s

let simbolo_of_yaml = function
  | `String s -> Ok s
  | `Float f -> Ok (string_of_int (int_of_float f))
  | `Bool b -> Ok (if b then "1" else "0")
  | v -> Error (`Msg ("Invalid simbolo in YAML: " ^ Yaml.to_string_exn v))

type direction = L | R | S

(* Custom serialization/deserialization for direction *)
let direction_to_yaml = function
  | L -> `String "L"
  | R -> `String "R"
  | S -> `String "S"

let direction_of_yaml = function
  | `String "L" -> Ok L
  | `String "R" -> Ok R
  | `String "S" -> Ok S
  | v -> Error (`Msg ("Invalid direction in YAML: " ^ Yaml.to_string_exn v))

type transition = estado * simbolo * direction [@@deriving yaml]

type delta = (string, (string, transition) Hashtbl.t) Hashtbl.t

(* Custom serialization/deserialization for delta *)
let delta_to_yaml delta =
  `O
    (Hashtbl.fold
       (fun key inner acc ->
         let inner_lst =
           Hashtbl.fold
             (fun k v acc2 -> (k, transition_to_yaml v) :: acc2)
             inner []
         in
         (key, `O inner_lst) :: acc )
       delta [] )

let delta_of_yaml = function
  | `O lst ->
      let outer_tbl = Hashtbl.create (List.length lst) in
      List.iter
        (fun (key, value) ->
          match value with
          | `O inner_lst ->
              let inner_tbl = Hashtbl.create (List.length inner_lst) in
              List.iter
                (fun (k, v) ->
                  let transition =
                    match transition_of_yaml v with
                    | Ok t -> t
                    | Error (`Msg m) -> failwith m
                  in
                  Hashtbl.add inner_tbl k transition )
                inner_lst ;
              Hashtbl.add outer_tbl key inner_tbl
          | v ->
              failwith ("Invalid inner delta YAML: " ^ Yaml.to_string_exn v) )
        lst ;
      Ok outer_tbl
  | v -> failwith ("Invalid delta YAML: " ^ Yaml.to_string_exn v)

type tm =
  { states: estado list
  ; input_alphabet: simbolo list
  ; tape_alphabet_extra: simbolo list
  ; start_state: estado
  ; accept_state: estado
  ; reject_state: estado
  ; delta: delta }
[@@deriving yaml]

type tm_w = {m: tm [@key "M"]; w: string} [@@deriving yaml]

(* --- LEITURA SEGURA (BUFFER) --- *)
let read_multiplelines () =
  let buf = Buffer.create 1024 in
  try
    while true do
      let line = input_line stdin in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n'
    done;
    "" (* Nunca atingido *)
  with End_of_file -> Buffer.contents buf

let yaml_val = function
  | Ok v -> v (* v has type Yaml.value *)
  | Error (`Msg m) -> failwith m

(* string para tm_w *)
let tm_w_de_string s = s |> Yaml.of_string_exn |> tm_w_of_yaml |> yaml_val

let blank = "_"
let max_steps = 200

let hashtbl_find_opt tbl k =
  try Some (Hashtbl.find tbl k) with Not_found -> None

let symbols_of_word w =
  List.init (String.length w) (fun i -> String.make 1 w.[i])

let mem x xs = List.exists ((=) x) xs

(* Alfabeto da fita é a união dos dois (duplicados não importam para o 'mem') *)
let tape_alphabet m = 
  if mem blank m.tape_alphabet_extra 
  then m.input_alphabet @ m.tape_alphabet_extra
  else m.input_alphabet @ (blank :: m.tape_alphabet_extra)

let word_valid m w =
  symbols_of_word w |> List.for_all (fun s -> mem s m.input_alphabet)

(* --- REMOVIDO: disjoint --- *)

let tm_valid m =
  mem m.start_state m.states
  && mem m.accept_state m.states
  && mem m.reject_state m.states
  (* && mem blank (tape_alphabet m) <-- Garantido pela função tape_alphabet acima *)
  && m.accept_state <> m.reject_state
  (* REMOVIDO: && disjoint m.input_alphabet m.tape_alphabet_extra *)

let delta_valid m =
  let tape = tape_alphabet m in
  Hashtbl.fold (fun q inner acc ->
    acc && mem q m.states &&
    Hashtbl.fold (fun a (q', b, _) acc__in ->
      acc__in && mem a tape && mem q' m.states && mem b tape
    ) inner true
  ) m.delta true

type tape = simbolo list * simbolo list

let tape_init w =
  match symbols_of_word w with
  | [] -> ([], [ blank ])
  | rs -> ([], rs)

let tape_read (_, right) =
  match right with
  | h :: _ -> h
  | [] -> blank

let tape_write sym (left, right) =
  match right with
  | _ :: rs -> (left, sym :: rs)
  | [] -> (left, [ sym ])

let tape_move_left (left, right) =
  match left, right with
  | [], rs -> ([], blank :: rs)
  | l :: ls, rs -> (ls, l :: rs)

let tape_move_right (left, right) =
  match right with
  | [] -> (blank :: left, [ blank ])
  | h :: [] -> (h :: left, [ blank ])
  | h :: r :: rs -> (h :: left, r :: rs)

let tape_move d t =
  match d with
  | L -> tape_move_left t
  | R -> tape_move_right t
  | S -> t

let rec drop_blanks = function
  | [] -> []
  | x :: xs when x = blank -> drop_blanks xs
  | xs -> xs

let tape_to_string (left, right) =
  let all = List.rev left @ right in
  let no_leading = drop_blanks all in
  let no_trailing = List.rev (drop_blanks (List.rev no_leading)) in
  String.concat "" (no_trailing @ [ blank ])

let delta_lookup m q a =
  match hashtbl_find_opt m.delta q with
  | None -> None
  | Some inner -> hashtbl_find_opt inner a

type sim_result = Accept of tape | Reject of tape | DontKnow | Invalid

let simulate m w =
  if not (tm_valid m) then Invalid
  else if not (delta_valid m) then Invalid
  else if not (word_valid m w) then Invalid
  else
    let rec loop steps q t =
      if q = m.accept_state then Accept t
      else if q = m.reject_state then Reject t
      else if steps >= max_steps then DontKnow
      else
        let a = tape_read t in
        match delta_lookup m q a with
        | None -> Reject t
        | Some (q', write_sym, dir) ->
            let t' = t |> tape_write write_sym |> tape_move dir in
            loop (steps + 1) q' t'
    in
    loop 0 m.start_state (tape_init w)

let s1tm m w =
  match simulate m w with
  | Accept _ -> Some true
  | Reject _ -> Some false
  | DontKnow | Invalid -> None

let print_result r =
  match r with
  | Invalid -> print_endline "INVALID"
  | DontKnow -> print_endline "DON'T KNOW"
  | Accept t ->
      print_endline "YES";
      print_endline (tape_to_string t)
  | Reject t ->
      print_endline "NO";
      print_endline (tape_to_string t)

let () =
  ignore s1tm;
  let s = read_multiplelines () |> String.trim in
  try
    let tw = tm_w_de_string s in
    print_result (simulate tw.m tw.w)
  with _ ->
    print_endline "INVALID"