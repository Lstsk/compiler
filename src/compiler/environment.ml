(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;


open Assembly;;

(* This data type represents the kind of environment we will use in our
   compiler.  This is a pair between the next free stack address and a
   dictionary mapping variable names to their offsets. *)
type environment = int * argument Map.String.t;;

(* This defines an exception which should be thrown if we try to read from an
   environment something that doesn't exist. *)
exception UnboundVariable of string * environment;;

(* The value representing an empty environment.  The map is empty and zero
   temporary locations are in use. *)
let empty_environment : environment = (-8, Map.String.empty);;

(* A function to allocate a space for a named variable from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)
let allocate_named_variable (name : string) (env : environment) : environment =
   match env with
   | (num, arg) -> (num - 8, Map.String.add (name) (ArgMemory(AddrByRegisterOffset(RBP, num))) (arg))
;;



(* A function to preallocate parameters.
   Returns a non-empty environment consisting of the paramater values while having
   num set to -8
*)
let environment_with_parameters (env: environment) (params: string list) : environment =
   let rec helper p offset map =
     match p with
     | [] -> (-8, map)
     | curr :: next ->
         helper next (offset + 8) (Map.String.add curr (ArgMemory (AddrByRegisterOffset (RBP, offset))) map)
   in
   helper params 16 (snd env)
 ;;


(* A function to find a previously-allocated space in an environment based upon
   the name of the variable for which that space was allocated.  If the variable
   was not previously allocated, then UnboundVariable is raised.
*)
let find_named_variable (name : string) (env : environment) : argument =
   let (_, arg) = env in
      match Map.String.find_opt name arg with 
      | Some arg -> arg
      | None -> raise (UnboundVariable (name, env))
;;

(* A function to allocate space for a temporary variable from the environment.
   This function does not require a variable name because the temporary space is
   being allocated for the compiler and will not be associated with a name.
   Given an environment, it returns a new, temporary address as well as a new
   environment after that allocation.
*)
let allocate_temp_variable (env : environment) : argument * environment =
   let newEnv = allocate_named_variable "temp" env in
      ((find_named_variable "temp" newEnv), newEnv)
;;

(* A function to print an environment.  This is provided to you for debugging
   purposes. *)
let string_of_environment (env : environment) : string =
  let (next_free, dict) = env in
  let mappings = List.of_enum (Map.String.enum dict) in
  let rec string_of_mappings ms =
    match ms with
    | [] -> ""
    | (name, address)::ms' ->
      (Printf.sprintf
         "%s stored at %s\n"
         name
         (code_of_argument address)
      ) ^ string_of_mappings ms'
  in
  (Printf.sprintf "Next free offset: %d\n" next_free) ^
  string_of_mappings mappings
;;
