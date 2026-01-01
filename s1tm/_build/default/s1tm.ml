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

let rec read_multiplelines () =
  try
    let line = read_line () in
    line ^ "\n" ^ read_multiplelines ()
  with End_of_file -> ""

let yaml_val = function
  | Ok v -> v (* v has type Yaml.value *)
  | Error (`Msg m) -> failwith m

(* string para tm_w *)
let tm_w_de_string s = s |> Yaml.of_string_exn |> tm_w_of_yaml |> yaml_val

(* tm_w para string *)
let tm_w_para_string tm_w = tm_w |> tm_w_to_yaml |> Yaml.to_string_exn
[@@warning "-32"]



(* ------------------------------------------------------------ *)
(* Simulação (até 200 passos)                                    *)
(* ------------------------------------------------------------ *)

let blank : simbolo = "_"
let max_steps = 200

let hashtbl_find_opt (tbl : ('a, 'b) Hashtbl.t) (k : 'a) : 'b option =
  try Some (Hashtbl.find tbl k) with Not_found -> None

let symbols_of_word (w : string) : simbolo list =
  let n = String.length w in
  let rec go i acc =
    if i < 0 then acc else go (i - 1) (String.make 1 w.[i] :: acc)
  in
  go (n - 1) []

let tape_alphabet (m : tm) : simbolo list = m.input_alphabet @ m.tape_alphabet_extra

let mem (x : 'a) (xs : 'a list) = List.exists (( = ) x) xs

let tm_valid (m : tm) : bool =
  mem m.start_state m.states
  && mem m.accept_state m.states
  && mem m.reject_state m.states
  && mem blank (tape_alphabet m)

let word_valid (m : tm) (w : string) : bool =
  symbols_of_word w |> List.for_all (fun s -> mem s m.input_alphabet)

let delta_valid (m : tm) : bool =
  let ok = ref true in
  let tape = tape_alphabet m in
  Hashtbl.iter
    (fun q inner ->
      if not (mem q m.states) then ok := false ;
      Hashtbl.iter
        (fun a (q', b, _dir) ->
          if not (mem a tape) then ok := false ;
          if not (mem q' m.states) then ok := false ;
          if not (mem b tape) then ok := false )
        inner )
    m.delta ;
  !ok

(* Fita: zipper (left, head, right) *)
type tape = {left: simbolo list; head: simbolo; right: simbolo list}

let tape_init (w : string) : tape =
  match symbols_of_word w with
  | [] -> {left= []; head= blank; right= []}
  | h :: t -> {left= []; head= h; right= t}

let tape_write (sym : simbolo) (t : tape) : tape = {t with head= sym}

let tape_move_left (t : tape) : tape =
  match t.left with
  | [] -> {left= []; head= blank; right= t.head :: t.right}
  | l :: ls -> {left= ls; head= l; right= t.head :: t.right}

let tape_move_right (t : tape) : tape =
  match t.right with
  | [] -> {left= t.head :: t.left; head= blank; right= []}
  | r :: rs -> {left= t.head :: t.left; head= r; right= rs}

let tape_move (d : direction) (t : tape) : tape =
  match d with L -> tape_move_left t | R -> tape_move_right t | S -> t

let drop_while (p : 'a -> bool) (xs : 'a list) : 'a list =
  let rec go = function
    | [] -> []
    | x :: tl when p x -> go tl
    | rest -> rest
  in
  go xs

let trim_trailing (p : 'a -> bool) (xs : 'a list) : 'a list =
  xs |> List.rev |> drop_while p |> List.rev

let tape_to_string (t : tape) : string =
  let all = List.rev t.left @ (t.head :: t.right) in
  let core =
    all
    |> drop_while (fun s -> s = blank)
    |> trim_trailing (fun s -> s = blank)
  in
  String.concat "" (core @ [blank])

let delta_lookup (m : tm) (q : estado) (a : simbolo) : transition option =
  match hashtbl_find_opt m.delta q with
  | None -> None
  | Some inner -> hashtbl_find_opt inner a

type sim_result = Accept of tape | Reject of tape | DontKnow | Invalid

let simulate (m : tm) (w : string) : sim_result =
  if not (tm_valid m) then Invalid
  else if not (delta_valid m) then Invalid
  else if not (word_valid m w) then Invalid
  else
    let rec loop steps (q : estado) (t : tape) =
      if q = m.accept_state then Accept t
      else if q = m.reject_state then Reject t
      else if steps >= max_steps then DontKnow
      else
        match delta_lookup m q t.head with
        | None -> Reject t
        | Some (q', write_sym, dir) ->
            let t' = t |> tape_write write_sym |> tape_move dir in
            loop (steps + 1) q' t'
    in
    loop 0 m.start_state (tape_init w)

(* Função pedida no enunciado *)
let s1tm (m : tm) (w : string) : bool option =
  match simulate m w with
  | Accept _ -> Some true
  | Reject _ -> Some false
  | DontKnow | Invalid -> None

let print_result (r : sim_result) : unit =
  match r with
  | Invalid -> print_endline "INVALID"
  | DontKnow -> print_endline "DON'T KNOW"
  | Accept t ->
      print_endline "YES" ;
      print_endline (tape_to_string t)
  | Reject t ->
      print_endline "NO" ;
      print_endline (tape_to_string t)

let () =
  ignore s1tm; (* Silence unused warning *)
  let s = read_multiplelines () |> String.trim in
  try
    let tw = tm_w_de_string s in
    tw.m |> ignore ;
    print_result (simulate tw.m tw.w)
  with _ ->
    (* YAML inválido, ou falhas de parsing *)
    print_endline "INVALID"
