(*
This file defines a lexer for an LL parser generator for Pyrrhuloxia.
*)

open Batteries;;

(** An exception type which should be raised when lexing fails. *)
exception LexerError of string;;

(** The type of tokens produced by this lexer. *)
type token =
  | TokInt of int
  | TokPlus
  | TokMinus
  | TokStar
  | TokOpenParen
  | TokCloseParen
  | TokLet
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokTrue
  | TokFalse
  | TokSet
  | TokLess
  | TokGreater
  | TokEqual
  | TokAnd
  | TokOr
  | TokAfter
  | TokBefore
  | TokPrint
  | TokIsInt
  | TokIsBool
  | TokIsTuple
  | TokTupleIndex
  | TokIdentifer of string
  (* TODO: add more token types here *)
[@@deriving eq, ord, show];;

(** A helper function which attempts to lex the prefix of a string into a token.
    It takes as an argument a list of mini-lexers: functions which *try* to
    recognize a specific token and raise a LexerError if they fail. *)
let rec tokenize_first_of
    (tokenizers : (char list -> token * char list) list)
    (input : char list)
  : token * char list =
  match tokenizers with
  | [] ->
    (* We have no routines to use to find a token.  Give up, producing an error
       message to hint to the user where we were in the file. *)
    let max_error_length = 20 in
    let first_part_of_string =
      String.of_list (List.take max_error_length input)
    in
    raise (LexerError(Printf.sprintf "Unrecognized token starting at: \"%s\""
                        first_part_of_string))
  | tokenizer :: tokenizers' ->
    try
      (* If this tokenizer successfully produces a result, use it! *)
      tokenizer input
    with
    | LexerError _ ->
      (* This tokenizer failed.  Let's try the next one. *)
      tokenize_first_of tokenizers' input
;;

(** This routine determines whether a particular character is an identifier
    character. *)
let is_identifier_character (ch : char) : bool =
  Char.is_digit ch || Char.is_letter ch || ch == '_' || ch == '\''
;;

(** This routine discards whitespace from the input stream. *)
let discard_whitespace (input : char list) : char list =
  List.drop_while Char.is_whitespace input
;;

(** This routine attempts to lex a single numeric token from the input.
    Note that this routine does NOT handle negative numbers. *)
let tokenize_int (input : char list) : token * char list =
  (* Get all of the digits from the front of the input that we can. *)
  let digits = List.take_while Char.is_digit input in
  let rest = List.drop_while Char.is_digit input in
  (* If we didn't get any digits, then the next token isn't an integer. *)
  if List.is_empty digits then
    raise (LexerError "Could not tokenize integer")
  else
    (* We error on "123abc" since it is not a valid integer or identifier. *)
    match rest with
    | ch::_ when is_identifier_character ch ->
      raise (LexerError ("Identifier character following integer"))
    | _ ->
      (* Convert the list of digits into a string. *)
      let digit_string = String.of_list digits in
      (* Turn that into a number. *)
      let number = int_of_string digit_string in
      (* Return a token with that number along with the rest of the input. *)
      (TokInt number, rest)
;;

(** This routine attempts to lex a single plus token from the input. *)
let tokenize_plus (input : char list) : token * char list =
  match input with
  | '+'::rest -> (TokPlus, rest)
  | _ -> raise (LexerError "Could not tokenize plus")
;;

(** This routine attempts to lex a single minus token from the input. *)
let tokenize_minus (input : char list) : token * char list =
  match input with
  | '-'::rest -> (TokMinus, rest)
  | _ -> raise (LexerError "Could not tokenize minus")
;;

(** This routine attemps to lex a single open parenthesis from the input. *)
let tokenize_open_paren (input : char list) : token * char list =
  match input with
  | '('::rest -> (TokOpenParen, rest)
  | _ -> raise (LexerError "Could not tokenize open parenthesis")
;;

(** This routine attemps to lex a single close parenthesis from the input. *)
let tokenize_close_paren (input : char list) : token * char list =
  match input with
  | ')'::rest -> (TokCloseParen, rest)
  | _ -> raise (LexerError "Could not tokenize close parenthesis")
;;

(* TODO: write more lexer routines here *)
let tokenize_identifer (input: char list) : token * char list = 
  match input with 
  | ch ::_ when Char.is_letter ch || ch = '_' ->
    let chars = List.take_while is_identifier_character input in
    let rest  = List.drop_while is_identifier_character input in
    let str = String.of_list chars in
    let tok =
      match str with
      | "let"     -> TokLet
      | "in"      -> TokIn
      | "if"      -> TokIf
      | "then"    -> TokThen
      | "else"    -> TokElse
      | "true"    -> TokTrue
      | "false"   -> TokFalse
      | "after"   -> TokAfter
      | "before"  -> TokBefore
      | "print"   -> TokPrint
      | "isint"   -> TokIsInt
      | "isbool"  -> TokIsBool
      | _         -> TokIdentifer str
    in
    (tok, rest)
  | _ ->
    raise (LexerError "Not an identifier")
;;

(*  for multiplication *)
let tokenize_star  (input : char list) : token * char list  = 
  match input with
  | '*'::rest -> (TokStar, rest)
  | _ -> raise (LexerError "Could not tokenize *")
;;

(* for && *)
let tokenize_and (input : char list) : token * char list  = 
  match input with
  | '&'::'&'::rest -> (TokAnd, rest)
  | _ -> raise (LexerError "Could not tokenize &&")
;;

(* for || *)
let tokenize_or (input : char list) : token * char list = 
  match input with
  | '|'::'|'::rest -> (TokOr, rest)
  | _ -> raise (LexerError "Could not tokenize ||")
;;

(* for < *)
let tokenize_less (input : char list) : token * char list = 
  match input with
  | '<'::rest -> (TokLess, rest)
  | _ -> raise (LexerError "Could not tokenize <")
;;

(* for > *)
let tokenize_greater  (input : char list) : token * char list = 
  match input with
  | '>'::rest -> (TokGreater, rest)
  | _ -> raise (LexerError "Could not tokenize >")
;;

(* for = *)
let tokenize_equal (input : char list) : token * char list =
  match input with
  | '='::rest -> (TokEqual, rest)
  | _ -> raise (LexerError "Could not tokenize =")
;;

(** This routine attempts to take a single token from the input stream.  If an
    unrecoverable error occurs, a LexerError is raised. *)
let tokenize (input : char list) : token * char list =
  (* TODO: modify this function as you write more lexer routines. *)
  tokenize_first_of
    [ tokenize_int;
      tokenize_and;
      tokenize_or;
      tokenize_star;
      tokenize_less;
      tokenize_greater;
      tokenize_equal;
      tokenize_identifer;
      tokenize_plus;
      tokenize_minus;
      tokenize_open_paren;
      tokenize_close_paren;
    ]
    input
;;

(** A function to lex a string.  If lexing is successful, a list of tokens is
    returned.  Otherwise, a LexerError is raised. *)
let lex (text : string) : token list =
  let input = String.to_list text in
  (*
    This function recursively takes a token from the input stream.  It builds up
    a list in *reverse* order because this allows it to tail recurse; the
    list is reversed once the routine is complete.
  *)
  let rec take_tokens (tokens_so_far : token list) (input : char list)
    : token list =
    (* Get rid of any leading whitespace. *)
    let input' = discard_whitespace input in
    (* If we're out of input, then return the list of tokens we have. *)
    if List.is_empty input' then
      tokens_so_far
    else
      (* Take a token from the input. *)
      let (token, input'') = tokenize input' in
      (* Recurse to take more tokens.  Note that the new token is put on the
         front of the list, resulting in the list being constructed in backwards
         order.  This is reversed later.  Doing things this way allows us to
         tail recurse here!  :-D *)
      take_tokens (token :: tokens_so_far) input''
  in
  List.rev (take_tokens [] input)
;;
