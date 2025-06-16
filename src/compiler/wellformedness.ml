open Batteries;;
open HatchLanguage;;
open Environment;;
open Printf;;
open Asts;;

exception IllFormed of string list;;

(* Create helper functions to do checking *)
let rec contains str lst = 
  match lst with 
  | [] -> false
  | value :: tail -> if str = value then true else contains str tail


(* Checks for duplicate function parameter  *)
let rec check_param_duplicate (name: string) (params: string list) = 
  match params with 
  | [] -> []
  | value :: tail ->  
    let err = if contains value tail then ["Function " ^ name ^ " declares a duplicate parameter " ^ value ] else [] 
    in err @ check_param_duplicate name tail
;;

let rec check_all_param_duplicate (declaration: declaration list): string list =
  match declaration with 
  |  [] -> []
  | DFunction(name, params, _) :: tail ->  
    check_param_duplicate name params @ check_all_param_duplicate tail
;;

let rec declared_function_names (decls: declaration list) : string list =
  match decls with
  | [] -> []
  | DFunction(name, _, _) :: tail -> name :: declared_function_names tail
;;



(* Checks for duplicate function definition *)
let rec check_duplicate_function_def (declarations: declaration list): string list = 
  match declarations with 
  | [] -> []
  | DFunction(name, _, _) :: tail ->
    let rec helper name ds = 
      match ds with 
      | [] -> false
      | DFunction(name2, _, _) :: rest -> if name2 = name then true else helper name rest
    in 
    let err = if helper name tail then ["Duplicate function definition " ^ name] else [] in
    err @ check_duplicate_function_def tail
;;


let rec get_length lst =
  match lst with
  | [] -> 0
  | _ :: tail -> 1 + get_length tail
;;

let rec find_function fname decls =
  match decls with
  | [] -> None
  | DFunction(n, params, _) :: tail ->
    if n = fname then Some params else find_function fname tail
;;

let rec check_function (fname: string) (declarations: declaration list): bool = 
  match declarations with
  | [] -> false
  | DFunction(name, _, _) :: tail -> if name = fname then true else check_function (fname) (tail) 
;;

let rec check_function_not_defined_and_argument (expr: expr) (declarations: declaration list) : string list =
  match expr with
  | EUnaryOp(_, expr) -> check_function_not_defined_and_argument expr declarations
  | EBinaryOp(_, expr1, expr2) -> check_function_not_defined_and_argument expr1 declarations @ check_function_not_defined_and_argument expr2 declarations
  | ELet (_, expr2, expr3) -> check_function_not_defined_and_argument expr2 declarations @ check_function_not_defined_and_argument expr3 declarations
  | EIf (expr1, expr2, expr3) -> check_function_not_defined_and_argument expr1 declarations @ check_function_not_defined_and_argument expr2 declarations @ check_function_not_defined_and_argument expr3 declarations
  (* | ECall(fname, parameters) -> 
    let rec check_params (params: expr list): string list =
      match params with
      | [] -> []
      | value :: tail -> 
        check_function_not_defined_and_argument value declarations @ 
        check_params tail
    in
    let errors_from_args = check_params parameters in
    let arg_count_error =
      match find_function fname declarations with
      | None -> []  
      | Some params ->
        if get_length parameters = get_length params then [] 
        else ["Function " ^ fname ^ " is called with an incorrect number of arguments."]
    in
    let function_undefined_error = if check_function fname declarations then [] else ["Function " ^ fname ^ " is not defined."] in
    errors_from_args @ function_undefined_error @ arg_count_error *)
  | ETuple(expr_list) ->
    let rec check_list lst =
      match lst with
      | [] -> []
      | value :: tail ->
        check_function_not_defined_and_argument value declarations @ check_list tail
    in
    check_list expr_list
  | EInt _ | EBool _ | EVar _  | EAppl _ | ESet _-> []
;;

let check_function_body_undefined_and_argument (declarations: declaration list) = 
  let rec check_functions_unbound decl = 
    match decl with
    | [] -> []
    | DFunction(_, _, body) :: tail -> check_function_not_defined_and_argument body declarations @ check_functions_unbound tail
  in
  check_functions_unbound declarations
;;

let rec check_unbound_variable  (expr: expr)  (env: string list): string list =
  match expr with
  | EVar var_name -> if contains var_name env then [] else ["Unbound variable "^ var_name^"."]
  | EUnaryOp(_, expr) -> check_unbound_variable expr env
  | EBinaryOp(_, expr1, expr2) -> check_unbound_variable expr1 env @ check_unbound_variable expr2 env
  | ELet (var, expr2, expr3) -> check_unbound_variable expr2 env @ check_unbound_variable expr3 (var :: env)
  | EIf (expr1, expr2, expr3) -> check_unbound_variable expr1 env @ check_unbound_variable expr2 env @ check_unbound_variable expr3 env
  (* | ECall(_, arguments) -> 
    let rec check_args args =
      match args with
      | [] -> []
      | arg_expr :: tail -> check_unbound_variable arg_expr env  @ check_args tail
    in
    check_args arguments *)
  | ETuple(expr_list) ->
    let rec check_list lst =
      match lst with
      | [] -> []
      | value :: tail ->
        check_unbound_variable value env @ check_list tail
    in
    check_list expr_list
    | EAppl(expr1, expr2) -> check_unbound_variable expr1 env @ check_unbound_variable expr2 env
    | ESet(tuple, index, value) -> check_unbound_variable tuple env @ check_unbound_variable index env @ check_unbound_variable value env
  | EInt _ | EBool _  -> []
;;

let check_all_unbound_variable_errors (declarations: declaration list) = 
  let fun_names = declared_function_names declarations in

  let rec check_functions_unbound decl = 
    match decl with
    | [] -> []
    | DFunction(name, params, body) :: tail -> 
      let env_for_body = name :: (params @ fun_names) in
      check_unbound_variable body env_for_body @ check_functions_unbound tail
  in
  check_functions_unbound declarations
;;


(* This function produces a list of compile-time errors found in a program. *)
let check_program_for_errors (p : program) : string list =
  match p with 
  | Program(declaration, expr) -> 

    (* So that it knows declared function variable names are legal *)
    let fun_names = declared_function_names declaration in
    let duplicate_param_errors = check_all_param_duplicate declaration in 
    let duplicate_function_def_errors = check_duplicate_function_def declaration in
    let check_function_not_defined = check_function_not_defined_and_argument expr declaration in
    let check_function_body_undefined_and_argument = check_function_body_undefined_and_argument declaration in
    let unbound_errors = check_all_unbound_variable_errors declaration  in
    let main_unbound_errors = check_unbound_variable expr fun_names in
    main_unbound_errors @ unbound_errors @ duplicate_param_errors @ duplicate_function_def_errors  @ check_function_body_undefined_and_argument @ check_function_not_defined
;;

(* This function will check a program for compile-time errors.  If any errors
   are found, an IllFormed exception is thrown.  Otherwise, unit is returned. *)
let check_well_formed (p : program) : unit =
  let errors = check_program_for_errors p in
  if List.is_empty errors then () else raise (IllFormed errors);
;;
