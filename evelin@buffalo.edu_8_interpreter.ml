type stackValue = 
  | BOOL of bool 
  | INT of int 
  | ERROR 
  | STRING of string 
  | NAME of string 
  | UNIT

type command =
  | ADD
  | SUB
  | MUL
  | DIV
  | REM
  | NEG
  | SWAP
  | TOSTRING
  | PRINTLN
  | QUIT
  | POP
  | PUSH of stackValue
  | CAT
  | AND
  | OR
  | NOT
  | EQUAL
  | LESSTHAN
  | BIND
  | IF
  | LET
  | END
  | FUN of string  * string 
  | FUNEND
  | RETURN
  | CALL
  | INOUTFUN of string  * string 


type binding = (string * stackValue)  (* (name, value) pair *)

let bind_table = Hashtbl.create 20 (*gloabal bind hashtable*)
let fun_commands_dict : (string, command list * string *  (string, stackValue) Hashtbl.t * bool) Hashtbl.t = Hashtbl.create 10
let add_binding name value bt =
  Hashtbl.replace bt name value
  
let lookup_value name bt =
  try
    Some (Hashtbl.find bt name)
  with Not_found ->
    None

let copy_fun_commands_dict (dict : (string, command list * string * (string, stackValue) Hashtbl.t * bool) Hashtbl.t) : (string, command list * string * (string, stackValue) Hashtbl.t * bool) Hashtbl.t =
  let new_dict = Hashtbl.create (Hashtbl.length dict) in
  Hashtbl.iter (fun key (cmds, str, bt, isIO) ->
    let new_bt = Hashtbl.copy bt in
    Hashtbl.add new_dict key (cmds, str, new_bt, isIO)
  ) dict;
  new_dict

let copy_stack stack = List.map (fun x -> x) stack

let interpreter (input, output) =
  let ic = open_in input in
  let oc = open_out output in

  let rec loop_read acc =
    try
      let l = String.trim(input_line ic) in
      loop_read (l :: acc)
    with
      | End_of_file -> 
        close_in ic;
        List.rev acc
  in

  let strList = loop_read [] in

  (* converting string to stackValue *)
  let is_numeric str =
    let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9' in
    let rec is_numeric_helper idx =
      if idx < String.length str then
        if is_digit str.[idx] then
          is_numeric_helper (idx + 1)
        else
          false
      else
        true
    in
    is_numeric_helper 0
  in

  let is_quoted_string str =
    String.length str > 0 && str.[0] = '"' && str.[String.length str - 1] = '"'
  in

  let is_valid_name name =
    let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
    let is_digit c = c >= '0' && c <= '9' in
    let is_underscore c = c = '_' in
    
    let rec check_chars index =
      match index with
      | i when i >= String.length name -> true
      | i ->
        let c = String.get name i in
        match c with
        | c when is_letter c || is_underscore c || is_digit c -> check_chars (i + 1)
        | _ -> false
    in
    
    match String.length name with
    | 0 -> false
    | _ ->
      match String.get name 0 with
      | c when is_letter c || is_underscore c -> check_chars 1
      | _ -> false  
  in 

  let str2sv s =
    let contains_dot str = String.contains str '.' in
  
    match s with
    | _ ->
      let len = String.length s in
      if len > 0 && String.sub s 0 5 = "push " then
        let arg = String.trim (String.sub s 5 (len - 5)) in
        if is_quoted_string arg then
          STRING (String.sub arg 1 (String.length arg - 2)) (* strip quotes *)
        else if contains_dot arg then
          ERROR  (* reject floating-point numbers *)
        else if String.length arg > 0 && arg.[0] = '-' && String.length (String.trim arg) > 1 then
          let num_arg = String.sub arg 1 (String.length arg - 1) in
          if String.length num_arg > 0 && num_arg.[0] <> '0' then
            INT (-int_of_string num_arg)
          else
            INT 0  (*"-0" as integer 0 *)
        else if is_numeric arg then
            INT (int_of_string arg)
        else if String.length arg > 0 && arg.[0] <> '-' then
          if arg = ":true:" then
            BOOL true
          else if arg = ":false:" then
            BOOL false
          else if arg = ":error:" then
            ERROR
          else if arg = ":unit:" then
            UNIT
          else if is_valid_name arg then  (* valid name *)
            NAME arg
          else
            ERROR  
        else
          ERROR
      else
        ERROR  
  in

  (* list of commands *)
  let str2com s =
    match s with
    | "add" -> ADD
    | "sub" -> SUB
    | "mul" -> MUL
    | "div" -> DIV
    | "rem" -> REM
    | "neg" -> NEG
    | "swap" -> SWAP
    | "toString" -> TOSTRING
    | "println" -> PRINTLN
    | "quit" -> QUIT
    | "pop" -> POP
    | "cat" -> CAT
    | "and" -> AND
    | "or" -> OR
    | "not" -> NOT
    | "equal" -> EQUAL
    | "lessThan" -> LESSTHAN
    | "bind" -> BIND
    | "if" -> IF
    | "let" -> LET
    | "end" -> END
    | "funEnd" -> FUNEND
    | "call" -> CALL
    | "return" -> RETURN
    | _ -> 
      let words = String.split_on_char ' ' s in
      match words with
      | "fun" :: name1 :: name2 :: [] -> FUN (name1, name2)
      | "inOutFun" :: name1 :: name2 :: [] -> INOUTFUN (name1, name2)
      | _ -> PUSH (str2sv s)

  in

  let comList = List.map str2com strList in

  let rec write_stack oc = function
    | [] -> close_out oc
    | top :: rest ->
      begin
        match top with
        | BOOL b -> Printf.fprintf oc ":%s:\n" (string_of_bool b)
        | INT i -> Printf.fprintf oc "%d\n" i
        | ERROR -> Printf.fprintf oc ":error:\n"
        | STRING s -> Printf.fprintf oc "%s\n" s
        | NAME n -> Printf.fprintf oc "%s\n" n
        | UNIT -> Printf.fprintf oc ":unit:\n"
      end;
      write_stack oc rest
  in

  let rec processor cl stack bt print =
    match (cl, stack) with
    (*do variable binding first*)
    | (BIND::restOfCommands, value::NAME(name)::restofStack) ->
        (match value with
        | INT _ | STRING _ | BOOL _ | UNIT -> add_binding name value bt; processor restOfCommands (UNIT::restofStack) bt print
        | NAME n -> 
          (match lookup_value n bt with
          | Some v -> 
              add_binding name v bt; (* bind name to the found value *)
              processor restOfCommands (UNIT::restofStack) bt print
          | None -> (* error: No value found or found a name *)
              processor restOfCommands (ERROR::stack) bt print) 
        | _ -> processor restOfCommands (ERROR::stack) bt print) 
    
    | (ADD::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match lookup_value name1 bt, lookup_value name2 bt with
        | Some (INT a), Some (INT b) ->
            processor restOfCommands (INT (a + b) :: restofStack) bt print
        | _, _ -> processor restOfCommands (ERROR::stack) bt print)
    | (ADD::restOfCommands, NAME(name)::INT(b)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT a) -> processor restOfCommands (INT (a + b) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print)
    | (ADD::restOfCommands, INT(a)::NAME(name)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT b) -> processor restOfCommands (INT (a + b) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print) 
    
    | (SUB::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match (lookup_value name1 bt, lookup_value name2 bt) with
        | (Some (INT a), Some (INT b)) -> processor restOfCommands (INT (b - a) :: restofStack) bt print
        | (_, _) -> processor restOfCommands (ERROR::stack) bt print) 
    | (SUB::restOfCommands, NAME(name)::INT(b)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT a) -> processor restOfCommands (INT (b - a) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print)
    | (SUB::restOfCommands, INT(a)::NAME(name)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT b) -> processor restOfCommands (INT (b - a) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print)
 
    | (MUL::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match (lookup_value name1 bt, lookup_value name2 bt) with
        | (Some (INT a), Some (INT b)) -> processor restOfCommands (INT (a * b) :: restofStack) bt print
        | (_, _) -> processor restOfCommands (ERROR::stack) bt print)
    | (MUL::restOfCommands, NAME(name)::INT(b)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT a) -> processor restOfCommands (INT (a * b) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print)
    | (MUL::restOfCommands, INT(a)::NAME(name)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT b) -> processor restOfCommands (INT (a * b) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print)
      
    | (DIV::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match (lookup_value name1 bt, lookup_value name2 bt) with
        | (Some (INT a), Some (INT b)) ->
            (match a with
            | 0 -> processor restOfCommands (ERROR::stack) bt print
            | _ -> processor restOfCommands (INT (b/a) :: restofStack) bt print)
        | (_, _) -> processor restOfCommands (ERROR::stack) bt print)
    | (DIV::restOfCommands, NAME(name)::INT(b)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT a) ->
            (match a with
            | 0 -> processor restOfCommands (ERROR::stack) bt print
            | _ -> processor restOfCommands (INT (b/a) :: restofStack) bt print)
        | _ -> processor restOfCommands (ERROR::stack) bt print)
    | (DIV::restOfCommands, INT(a)::NAME(name)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT b) ->
            (match a with
            | 0 -> processor restOfCommands (ERROR::stack) bt print
            | _ -> processor restOfCommands (INT (b/a) :: restofStack)bt print)
        | _ -> processor restOfCommands (ERROR::stack) bt print)

    | (REM::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match (lookup_value name1 bt, lookup_value name2 bt) with
        | (Some (INT a), Some (INT b)) ->
            (match a with
            | 0 -> processor restOfCommands (ERROR::stack)bt print
            | _ -> processor restOfCommands (INT (b mod a) :: restofStack)bt print)
        | (_, _) -> processor restOfCommands (ERROR::stack)bt print)
    | (REM::restOfCommands, NAME(name)::INT(b)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT a) ->
            (match a with
            | 0 -> processor restOfCommands (ERROR::stack)bt print
            | _ -> processor restOfCommands (INT (b mod a) :: restofStack) bt print)
        | _ -> processor restOfCommands (ERROR::stack)bt print)
    | (REM::restOfCommands, INT(a)::NAME(name)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT b) ->
            (match a with
            | 0 -> processor restOfCommands (ERROR::stack)bt print
            | _ -> processor restOfCommands (INT (b mod a) :: restofStack)bt print)
        | _ -> processor restOfCommands (ERROR::stack)bt print)

    | (NEG::restOfCommands, NAME(name)::restofStack) ->
        (match (lookup_value name bt) with
        | Some (INT a) -> processor restOfCommands (INT(-a) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print)

    | (AND::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match (lookup_value name1 bt, lookup_value name2 bt) with
        | (Some (BOOL a), Some (BOOL b)) -> processor restOfCommands (BOOL (a && b) :: restofStack)bt print
        | (_, _) -> processor restOfCommands (ERROR::stack)bt print)
    | (AND::restOfCommands, NAME(name)::BOOL(b)::restofStack) ->
        (match lookup_value name bt with
        | Some (BOOL a) -> processor restOfCommands (BOOL (a && b) :: restofStack)bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print)
    | (AND::restOfCommands, BOOL(a)::NAME(name)::restofStack) ->
        (match lookup_value name bt with
        | Some (BOOL b) -> processor restOfCommands (BOOL (a && b) :: restofStack)bt print
        | _ -> processor restOfCommands (ERROR::stack)bt print)

    | (NOT::restOfCommands, NAME(name)::restofStack) ->
        (match (lookup_value name bt) with
        | Some (BOOL b1) -> processor restOfCommands (BOOL (not b1) :: restofStack)bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print)

    | (OR::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match (lookup_value name1 bt, lookup_value name2 bt) with
        | (Some (BOOL a), Some (BOOL b)) -> processor restOfCommands (BOOL (a || b) :: restofStack)bt print
        | (_, _) -> processor restOfCommands (ERROR::stack)bt print)
    | (OR::restOfCommands, NAME(name)::BOOL(b)::restofStack) ->
        (match lookup_value name bt with
        | Some (BOOL a) -> processor restOfCommands (BOOL (a || b) :: restofStack)bt print
        | _ -> processor restOfCommands (ERROR::stack)bt print)
    | (OR::restOfCommands, BOOL(a)::NAME(name)::restofStack) ->
        (match lookup_value name bt with
        | Some (BOOL b) -> processor restOfCommands (BOOL (a || b) :: restofStack)bt print
        | _ -> processor restOfCommands (ERROR::stack)bt print)

    | (EQUAL::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match (lookup_value name1 bt, lookup_value name2 bt) with
        | (Some INT(v1), Some INT(v2)) -> processor restOfCommands (BOOL (v1 = v2) :: restofStack) bt print
        | (_, _) -> processor restOfCommands (ERROR::stack) bt print)
    | (EQUAL::restOfCommands, NAME(name)::INT(b)::restofStack) ->
        (match lookup_value name bt with
        | Some INT(a) -> processor restOfCommands (BOOL (a = b) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack)bt print)
    | (EQUAL::restOfCommands, INT(a)::NAME(name)::restofStack) ->
        (match lookup_value name bt with
        | Some INT(b) -> processor restOfCommands (BOOL (a = b) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack)bt print)

    | (LESSTHAN::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match (lookup_value name1 bt, lookup_value name2 bt) with
        | (Some (INT a), Some (INT b)) -> processor restOfCommands (BOOL (b < a) :: restofStack) bt print
        | (_, _) -> processor restOfCommands (ERROR::stack) bt print)
    | (LESSTHAN::restOfCommands, NAME(name)::INT(b)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT a) -> processor restOfCommands (BOOL (b < a) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack)bt print)
    | (LESSTHAN::restOfCommands, INT(a)::NAME(name)::restofStack) ->
        (match lookup_value name bt with
        | Some (INT b) -> processor restOfCommands (BOOL (b < a) :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR::stack) bt print)
    
    | (CAT::restOfCommands, STRING(s1)::sv::restofStack) ->
        (match sv with
        | STRING(s2) -> processor restOfCommands (STRING(s2 ^ s1) :: restofStack) bt print
        | NAME(name) ->
            (match lookup_value name bt with
            | Some (STRING(s2)) -> processor restOfCommands (STRING(s2 ^ s1) :: restofStack) bt print
            | _ -> processor restOfCommands (ERROR::stack) bt print)
        | _ -> processor restOfCommands (ERROR::stack) bt print)
    | (CAT::restOfCommands, sv::STRING(s2)::restofStack) ->
        (match sv with
        | STRING(s1) -> processor restOfCommands (STRING(s2 ^ s1) :: restofStack) bt print
        | NAME(name) ->
            (match lookup_value name bt with
            | Some (STRING(s1)) -> processor restOfCommands (STRING(s2 ^ s1) :: restofStack) bt print
            | _ -> processor restOfCommands (ERROR::stack) bt print)
        | _ -> processor restOfCommands (ERROR::stack) bt print)
    | (CAT::restOfCommands, NAME(name1)::NAME(name2)::restofStack) ->
        (match (lookup_value name1 bt, lookup_value name2 bt) with
        | (Some (STRING(s1)), Some (STRING(s2))) -> processor restOfCommands (STRING(s2 ^ s1) :: restofStack) bt print
        | (_, _) -> processor restOfCommands (ERROR::stack) bt print)

    | (IF::restOfCommands, x::y::con::restofStack) ->
        (match con with
        | NAME name -> (
            match lookup_value name bt with
            | Some (BOOL b) -> if b then processor restOfCommands (x :: restofStack) bt print
                              else processor restOfCommands (y :: restofStack) bt print
            | _ -> processor restOfCommands (ERROR :: stack) bt print
          )
        | BOOL b -> if b then processor restOfCommands (x :: restofStack) bt print
                    else processor restOfCommands (y :: restofStack) bt print
        | _ -> processor restOfCommands (ERROR :: stack) bt print)
    | (BIND::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print

    (*regular integars / no lookup*)
    | (PUSH sv :: restOfCommands, _) -> processor restOfCommands (sv :: stack) bt print
    | (POP :: restOfCommands, _ :: restofStack) -> processor restOfCommands restofStack bt print
    | (POP :: restOfCommands, _) -> processor restOfCommands (ERROR :: stack) bt print
    | (SUB::restOfCommands, INT(a)::INT(b)::restofStack) -> processor restOfCommands (INT(b-a)::restofStack) bt print
    | (SUB::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    | (ADD::restOfCommands, INT(a)::INT(b)::restofStack) -> processor restOfCommands (INT(a+b)::restofStack) bt print
    | (ADD::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    | (MUL::restOfCommands, INT(a)::INT(b)::restofStack) -> processor restOfCommands (INT(a*b)::restofStack) bt print
    | (MUL::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    | (DIV::restOfCommands, INT(a)::INT(b)::restofStack) ->
        (match a with
        | 0 -> processor restOfCommands (ERROR::stack) bt print
        | _ -> processor restOfCommands (INT(b/a)::restofStack) bt print) 
    | (DIV::restOfCommands, _) -> processor restOfCommands (ERROR::stack)  bt print
    | (REM::restOfCommands, INT(a)::INT(b)::restofStack) ->
        (match a with
        | 0 -> processor restOfCommands (ERROR::stack) bt print
        | _ -> processor restOfCommands (INT(b mod a)::restofStack) bt print)
    | (REM::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    | (NEG::restOfCommands, INT(a)::restofStack) -> processor restOfCommands (INT(-a)::restofStack) bt print
    | (NEG::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    | (SWAP::restOfCommands, a::b::restofStack) -> processor restOfCommands (b::a::restofStack) bt print
    | (SWAP::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    | (CAT::restOfCommands, _) -> processor restOfCommands (ERROR :: stack) bt print
    | (AND::restOfCommands, BOOL(b)::BOOL(a)::restofStack) -> processor restOfCommands (BOOL (a && b) :: restofStack) bt print  
    | (AND::restOfCommands, _) -> processor restOfCommands (ERROR :: stack) bt print
    | (OR::restOfCommands, BOOL(b)::BOOL(a)::restofStack) -> processor restOfCommands (BOOL (a || b) :: restofStack) bt print
    | (OR::restOfCommands, _) -> processor restOfCommands (ERROR :: stack) bt print
    | (NOT::restOfCommands, BOOL(a)::restofStack) -> processor restOfCommands (BOOL (not a) :: restofStack) bt print
    | (NOT::restOfCommands, _) -> processor restOfCommands (ERROR :: stack) bt print
    | (EQUAL::restOfCommands, INT(a)::INT(b)::restofStack) -> processor restOfCommands (BOOL (a = b) :: restofStack) bt print
    | (EQUAL::restOfCommands, _) -> processor restOfCommands (ERROR :: stack) bt print
    | (LESSTHAN::restOfCommands, INT(a)::INT(b)::restofStack) -> processor restOfCommands (BOOL (b < a) :: restofStack) bt print
    | (LESSTHAN::restOfCommands, _) -> processor restOfCommands (ERROR :: stack) bt print
    | (IF::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print(* less than 3 values *)
    | (TOSTRING::restOfCommands, top::restofStack) ->
      (match top with
      | BOOL b -> processor restOfCommands (STRING (if b then ":true:" else ":false:")::restofStack) bt print
      | INT i -> processor restOfCommands (STRING (string_of_int i)::restofStack) bt print
      | ERROR -> processor restOfCommands (STRING ":error:"::restofStack) bt print
      | UNIT -> processor restOfCommands (STRING ":unit:"::restofStack) bt print
      | STRING s -> processor restOfCommands (STRING s::restofStack) bt print
      | NAME n -> processor restOfCommands (STRING n::restofStack) bt print)
    | (TOSTRING::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    | (PRINTLN::restOfCommands, top::restofStack) ->
        (match top with
        | STRING s -> processor restOfCommands restofStack bt (STRING(s)::print)
        | _ -> processor restOfCommands (ERROR::stack) bt print) 
    | (PRINTLN::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    | (QUIT::_, _) -> 
        (stack, print, bt)
    
    (*LET*)
    | (LET::restOfCommands, _) ->
      (* new binding table for local bindings *)
      let local_bind_table = Hashtbl.copy bt in
      let copied_dict = copy_fun_commands_dict fun_commands_dict in
      let new_stack = copy_stack stack in
      let rec extract_until_end cmds acc stack =
        match cmds with
        | END::rest -> List.rev acc, rest, stack
        | LET::rest -> 
            let local_let_table = Hashtbl.copy bt in
            let let_cmds, remaining_cmds, let_stk = extract_until_end rest [] new_stack in
            let let_result_stack, let_result_print, let_result_bt = processor let_cmds [] bt print in
            let first_value = 
              match let_result_stack with
              | [] -> [ERROR]  
              | x :: _ -> [x]  
            in
            let new_stack = first_value @ new_stack in
            (* remove bindings made within the block *)
            Hashtbl.reset bt;
            Hashtbl.iter (fun name value -> Hashtbl.replace bt name value) local_let_table;
            Hashtbl.clear fun_commands_dict;
            Hashtbl.iter (fun key value -> Hashtbl.add fun_commands_dict key value) copied_dict;
            extract_until_end remaining_cmds acc new_stack
        | command::rest -> extract_until_end rest (command::acc) new_stack
        | [] -> [], [], []
      in
      let let_commands, remaining_commands, let_stack = extract_until_end restOfCommands [] new_stack in
    
      (* process commands with local binding table *)
      let let_result_stack, let_result_print, let_result_bt = processor let_commands [] bt print in
      let first_value = 
        match let_result_stack with
        | [] -> [ERROR]  
        | x :: _ -> [x]  
      in
      let new_stack = first_value @ let_stack in
      (* remove bindings made within the block *)
      Hashtbl.reset bt;
      Hashtbl.iter (fun name value -> Hashtbl.replace bt name value) local_bind_table;
      Hashtbl.clear fun_commands_dict;
      Hashtbl.iter (fun key value -> Hashtbl.add fun_commands_dict key value) copied_dict;
      processor remaining_commands (new_stack) bt print
      
    | (END::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    
    (* part3 functions *)
    | (FUN(name1, name2)::restOfCommands, _) -> 
      let prev_funBind_table = Hashtbl.copy bt in
      let rec extract_fun_cmds cmds acc =
        match cmds with
        | FUNEND::rest -> List.rev acc, rest
        | command::rest -> extract_fun_cmds rest (command::acc)
        | [] -> [], []
      in
      let fun_cmds, remaining_cmds = extract_fun_cmds restOfCommands [] in
      add_binding name1 (fun_cmds, name2, prev_funBind_table, false) fun_commands_dict;
      processor remaining_cmds (UNIT::stack) bt print;

      | (INOUTFUN(name1, name2)::restOfCommands, _) -> 
        let prev_funBind_table = Hashtbl.copy bt in
        let rec extract_fun_cmds cmds acc =
          match cmds with
          | FUNEND::rest -> List.rev acc, rest
          | command::rest -> extract_fun_cmds rest (command::acc)
          | [] -> [], []
        in
        let fun_cmds, remaining_cmds = extract_fun_cmds restOfCommands [] in
        add_binding name1 (fun_cmds, name2, prev_funBind_table, true) fun_commands_dict;
        processor remaining_cmds (UNIT::stack) bt print;

    | (FUNEND::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print

    | (CALL::restOfCommands, arg::NAME(name1)::restofStack) ->
      begin
        match lookup_value name1 fun_commands_dict with
        | Some(fun_cmds, name2, local_funBind_table, isIO) ->
          (* name2 to arg *)
          (match arg with
            | NAME(n) -> 
                if Hashtbl.mem fun_commands_dict n then 
                  (add_binding name2 (STRING("blahhardcode")) local_funBind_table)
                else
                (match lookup_value n bt with
                  | Some(value) -> add_binding name2 value local_funBind_table
                  | _ -> add_binding name2 (NAME(n)) local_funBind_table)
            | INT(n) -> add_binding name2 (INT(n)) local_funBind_table
            | STRING(n) -> add_binding name2 (STRING(n)) local_funBind_table
            | BOOL(n) -> add_binding name2 (BOOL(n)) local_funBind_table
            | UNIT -> add_binding name2 UNIT local_funBind_table
            | ERROR -> add_binding name2 ERROR local_funBind_table);

          if lookup_value name2 local_funBind_table = Some(STRING("blahhardcode")) then 
              processor restOfCommands (arg::restofStack) bt print 
          else if lookup_value name2 local_funBind_table = Some(ERROR) then
              processor restOfCommands (ERROR::stack) bt print 
          else
            (* run processor on the function's command list *)
            let test = Hashtbl.copy local_funBind_table in
            let let_result_stack, let_result_print, let_result_bt = processor fun_cmds [] test print in
            let first_value = 
              match let_result_stack with
              | [] -> ERROR
              | NAME(n)::_ -> (
                match lookup_value n test with
                  | Some(value) -> value
                  | _ -> NAME(n)
                )
              | x:: _ -> x
            in
            if isIO 
              then (
                (match arg with
                  | NAME(n) -> (
                    match lookup_value name2 let_result_bt with
                      | Some(value) -> add_binding n value bt; processor restOfCommands (first_value :: restofStack) bt print
                      | _ -> processor restOfCommands (first_value :: restofStack) bt print
                    )
                  | _ -> processor restOfCommands (first_value :: restofStack) bt print))
              else processor restOfCommands (first_value :: restofStack) bt print
        | None -> processor restOfCommands (ERROR::stack) bt print
      end
    | (CALL::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print 
    | (RETURN::restOfCommands, top::restofStack) ->  ([top], print, bt)
    | (RETURN::restOfCommands, _) -> processor restOfCommands (ERROR::stack) bt print
    | (_,_) -> (stack, print, bt)
  in

  let (result_stack, result_print, result_bt) = processor comList [] bind_table [] in

  write_stack oc (List.rev result_print);
  (*write_stack oc (result_stack);*)
  close_out oc