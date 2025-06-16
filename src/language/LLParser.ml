(*
This file defines an LL parser for Pyrrhuloxia.
*)

open Batteries;;

open Asts;;
open LLLexer;;

(** This exception should be raised if an error occurs during parsing. *)
exception ParserError of string;;

(** A helper function which attempts to parse the prefix of a token stream into
    an expression.  It takes as an argument a list of mini-parsers: functions
    which *try* to recognize a specific expression and raise a ParserError if
    they fail. *)
let rec parse_first_of
    (parsers : (token list -> expr * token list) list)
    (input : token list)
  : expr * token list =
  match parsers with
  | [] ->
    (* We have no routines to use to find a parser.  Give up, producing an error
       message to hint to the user where we were in the file. *)
    let max_error_length = 4 in
    let first_part_of_token_stream = List.take max_error_length input in
    let stream_string =
      String.join ", " (List.map show_token first_part_of_token_stream)
    in
    raise (ParserError(
        Printf.sprintf "Failed to parse expression from tokens: %s"
          stream_string))
  | parser :: parsers' ->
    try
      (* If this parser successfully produces a result, use it! *)
      parser input
    with
    | ParserError _ ->
      (* This parser failed.  Let's try the next one. *)
      parse_first_of parsers' input
;;

(** A routine which attempts to parse a general expression. *)
let rec parse_expr (input : token list) : expr * token list = 
  match input with
  | TokLet :: TokIdentifer id :: TokEqual :: rest ->
    let (rhs, rest1) = parse_expr rest in
    (match rest1 with
     | TokIn :: rest2 ->
       let (body, rest3) = parse_expr rest2 in
       (ELet(id, rhs, body), rest3)
     | _ -> raise (ParserError "let is missing in"))
  | TokIf :: rest ->
    let (cond, r1) = parse_expr rest in
    (match r1 with
     | TokThen :: r2 ->
       let (tbranch, r3) = parse_expr r2 in
       (match r3 with
        | TokElse :: r4 ->
          let (ebranch, r5) = parse_expr r4 in
          (EIf(cond, tbranch, ebranch), r5)
        | _ -> raise (ParserError "if is missing else"))
     | _ -> raise (ParserError "if is missing then"))
  | toks ->
    parse_or_expr toks

(** A routine which attempts to parse an additive expression. *)
(* and parse_additive_expr (input : token list) : expr * token list =
   (* Read the first primary expr. *)
   let (e, input') = parse_primary_expr input in
   (* In a loop, find each following "+expr" or "-expr".  Put them in a list and
     return that along with the remaining tokens. *)
   let rec loop (loop_input : token list)
    : (binary_operator * expr) list * token list =
    match loop_input with
    | TokPlus :: loop_input' ->
      (* A + is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_primary_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpPlus, e) :: rest_exprs, rest_input)
    | _ ->
      (* We couldn't find a "+ expr", but that's okay; we'll just use what we
         have. *)
      ([], loop_input)
   in
   (* Find all of the operations we want to attach to this expr. *)
   let (op_exprs, input'') = loop input' in
   (* For each "+expr" or "-expr", build an AST from left to right which
     describes these operations. *)
   let result =
    List.fold_left
      (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
      e
      op_exprs
   in
   (result, input'') *)

(** A routine which attempts to parse a primary expression. *)
and parse_primary_expr (tokens: token list) :  expr * token list = 
  match tokens with 
  | TokTrue  :: rest -> (EBool true,  rest)
  | TokFalse :: rest -> (EBool false, rest)
  | TokInt n :: rest -> (EInt n,      rest)
  | TokIdentifer name :: rest -> (EVar name,  rest)

  (* after(expr) *)
  | TokAfter :: TokOpenParen :: rest ->
    let (expr, rest1) = parse_expr rest in
    begin 
      match rest1 with
      | TokCloseParen :: rest2 -> (EUnaryOp(OpAfter, expr), rest2)
      | _ -> raise (ParserError "after missing ')'")
    end

  (* before(expr) *)
  | TokBefore :: TokOpenParen :: rest ->
    let (expr, rest1) = parse_expr rest in
    begin
      match rest1 with
      | TokCloseParen :: rest2 -> (EUnaryOp(OpBefore, expr), rest2)
      | _ -> raise (ParserError "before missing ')'")
    end

  (* print(expr) *)
  | TokPrint :: TokOpenParen :: rest ->
    let (expr, rest1) = parse_expr rest in
    begin
      match rest1 with
      | TokCloseParen :: rest2 -> (EUnaryOp(OpPrint, expr), rest2)
      | _ -> raise (ParserError "print missing ')'")
    end

  (* isbool(expr) *)
  | TokIsBool :: TokOpenParen :: rest ->
    let (expr, rest1) = parse_expr rest in
    begin
      match rest1 with
      | TokCloseParen :: rest2 -> (EUnaryOp(OpIsBool, expr), rest2)
      | _ -> raise (ParserError "isbool missing ')'")
    end

  (* isint(expr) *)
  | TokIsInt :: TokOpenParen :: rest ->
    let (expr, rest1) = parse_expr rest in
    begin
      match rest1 with
      | TokCloseParen :: rest2 -> (EUnaryOp(OpIsInt, expr), rest2)
      | _ -> raise (ParserError "isint missing ')'")
    end

  (* parenthesized sub-expr *)
  | TokOpenParen :: rest ->
    let (expr, rest1) = parse_expr rest in
    begin
      match rest1 with
      | TokCloseParen :: rest2 -> (expr, rest2)
      | _ -> raise (ParserError "missing ')'")
    end

  | [] ->
    raise (ParserError "Unexpected end of input")

  | tok::_ ->
    raise (ParserError ("Unexpected token: " ^ show_token tok))

(** A routine which attempts to parse the integer production of a primary
    expression. *)
and parse_int_expr (input : token list) : expr * token list =
  match input with
  | TokInt n :: rest ->
    (EInt n, rest)
  | TokMinus :: TokInt n :: rest ->
    (EInt (-n), rest)
  | _ ->
    raise (ParserError("Failed to parse integer"))

(** A routine which attempts to parse the parenthesis production of a primary
    expression. *)
and parse_paren_expr (input : token list) : expr * token list =
  match input with
  | TokOpenParen :: rest ->
    begin
      let (expr, rest2) = parse_expr rest in
      match rest2 with
      | TokCloseParen :: rest3 ->
        (expr, rest3)
      | _ ->
        raise (ParserError("Failed to parse expression, missing close parenthesis"))
    end
  | _ ->
    raise (ParserError("Failed to parse expression, missing open parenthesis"))

(* TODO: define your parsing helper functions here *)
and parse_or_expr (tokens : token list) : expr * token list =
  let (left_expr, rest) = parse_and_expr tokens in
  let rec loop current_expr remaining_tokens =
    match remaining_tokens with
    | TokOr :: next ->
      let (right_expr, tail_after) = parse_and_expr next in
      loop (EBinaryOp(OpOr, current_expr, right_expr))
        tail_after
    | other_tokens -> (current_expr, other_tokens)
  in
  loop left_expr rest

and parse_and_expr (tokens : token list) : expr * token list =
  (* first, parse a comparison expression *)
  let (left_expr, rest) = parse_compare_expr tokens in

  (* loop to pick up any && right chains *)
  let rec loop current_expr remaining_tokens =
    match remaining_tokens with
    | TokAnd :: tail ->
      let (right_expr, tail_after) = parse_compare_expr tail in
      loop (EBinaryOp(OpAnd, current_expr, right_expr)) tail_after
    | other ->
      (* no more && just return what we have *)
      (current_expr, other)
  in
  loop left_expr rest

and parse_compare_expr (tokens : token list) : expr * token list =
  (* parse the left side as an additive expression *)
  let (left_expr, rest) = parse_additive_expr tokens in
  (* loop to handle any of <, >, = comparisons *)
  let rec loop current_expr remaining_tokens =
    match remaining_tokens with
    | TokLess :: tokens_after_less ->
      let (right_expr, tokens_after_right) =
        parse_additive_expr tokens_after_less
      in
      loop (EBinaryOp(OpLessThan, current_expr, right_expr))
        tokens_after_right

    | TokGreater :: tokens_after_greater ->
      let (right_expr, tokens_after_right) =
        parse_additive_expr tokens_after_greater
      in
      loop (EBinaryOp(OpGreaterThan, current_expr, right_expr))
        tokens_after_right

    | TokEqual :: tokens_after_equal ->
      let (right_expr, tokens_after_right) =
        parse_additive_expr tokens_after_equal
      in
      loop (EBinaryOp(OpEqualTo, current_expr, right_expr))
        tokens_after_right

    | other_tokens ->
      (* No more comparison operators; return the accumulated expression *)
      (current_expr, other_tokens)
  in
  loop left_expr rest

and parse_additive_expr (tokens : token list) : expr * token list =
  (* parse the left‐hand side as a multiplicative expression *)
  let (left_expr, rest) = parse_multiplicative_expr tokens in
  let rec loop accumulated_expr remaining_tokens =
    match remaining_tokens with
    | TokPlus :: tokens_after_plus ->
      (* we saw a +, so parse the next factor *)
      let (right_expr, tokens_after_right) =
        parse_multiplicative_expr tokens_after_plus
      in
      (* combine and continue *)
      loop (EBinaryOp(OpPlus, accumulated_expr, right_expr))
        tokens_after_right

    | TokMinus :: tokens_after_minus ->
      (* we saw a -, so parse the next factor *)
      let (right_expr, tokens_after_right) =
        parse_multiplicative_expr tokens_after_minus
      in
      loop (EBinaryOp(OpMinus, accumulated_expr, right_expr))
        tokens_after_right

    | other_tokens ->
      (* no more + or -, return what we have *)
      (accumulated_expr, other_tokens)
  in
  loop left_expr rest


and parse_multiplicative_expr (tokens : token list) : expr * token list =
  let (left_expr, rest) = parse_primary_expr tokens in
  let rec loop accumulated_expr remaining_tokens =
    match remaining_tokens with
    | TokStar :: tokens_after_star ->
      let (right_expr, tokens_after_right) =
        parse_primary_expr tokens_after_star
      in
      loop (EBinaryOp(OpTimes, accumulated_expr, right_expr))
        tokens_after_right

    | other_tokens ->
      (accumulated_expr, other_tokens)
  in
  loop left_expr rest

(** This function attempts to transform a list of tokens into a program.  If
    this process is unsuccessful, a ParserError is raised. *)
let parse (tokens : token list) : program =
  let (e, extras) = parse_expr tokens in
  if List.is_empty extras then
    Program([],e)
  else
    let stream_string = String.join ", " (List.map show_token extras) in
    raise (ParserError(Printf.sprintf "Extraneous tokens during parse: %s"
                         stream_string))
;;
