(** This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;

(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
  (* Bluebird *)
  | R10
  (* Cardinal *)
  | R11
  | RDI
  | RSI
  | RDX
  | RCX
  | R8
  | R9
  | RBP
;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
  | AddrByRegisterProductOffset of register * register * int
  | AddressByLabel of string
  
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
  | ArgLabelOffset of string * int  
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument
  | AsmIMul of argument * argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmRet
  (* Bluebird *)
  | AsmShl of argument * argument
  | AsmShr of argument * argument
  | AsmSal of argument * argument
  | AsmSar of argument * argument
  | AsmAnd of argument * argument
  | AsmOr of argument * argument
  | AsmXor of argument * argument
  | AsmLabel of string
  | AsmCmp of argument * argument
  | AsmJmp of string
  | AsmJe of string
  | AsmJne of string
  | AsmJl of string
  | AsmJg of string
  | AsmJge of string
  | AsmJa of string
  | AsmJae of string
  | AsmJle of string
  | AsmJbe of string
  | AsmJz of string
  (* Cardinal *)
  | AsmCall of string (* name of C function *)
  | AsmPush of argument
  | AsmPop of argument
  (* Eagle *)
  | AsmSection of string
  | AsmAlign of int
  | AsmDq of string list
  | AsmInc of argument
  | AsmRepMovsq
  | AsmRepStosq
;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =
  match register with
  | RAX -> "rax"
  | RSP -> "rsp"
  | R10 -> "r10"
  | R11 -> "r11"
  | RDI -> "rdi"
  | RSI -> "rsi"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | R8 -> "r8"
  | R9 -> "r9"
  | RBP -> "rbp"
;;

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  match address with
  | AddrByRegister(register) -> "[" ^ code_of_register register ^ "]"
  | AddrByRegisterOffset (register, num) -> 
    let offset = 
      if num >= 0 then  "+" ^  string_of_int num 
      else string_of_int(num) 
    in "[" ^ code_of_register register ^ offset ^ "]"
  | AddressByLabel(str) -> "[" ^ str ^ "]"
  | AddrByRegisterProductOffset (register1, register2, num) ->
    "[" ^ code_of_register register1 ^ " + " ^ code_of_register register2 ^ " * " ^ string_of_int num ^ "]"
(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  match argument with
  | ArgConstant(str) -> str
  | ArgRegister(register) -> code_of_register register
  | ArgMemory(address) -> code_of_address address 
  | ArgLabelOffset(str, num) ->  str ^ " + " ^ string_of_int (num)
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
*)
let code_of_instruction (instruction : instruction) : string =
  match instruction with
  | AsmAdd(arg1, arg2) -> "add " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2 
  | AsmIMul(arg1, arg2) -> "imul " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2 
  | AsmMov(arg1, arg2) -> "mov " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2 
  | AsmSub(arg1, arg2) -> "sub " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2 
  | AsmRet -> "ret\n"
  (* Bluebird *)
  | AsmShr(arg1, arg2) -> "shr " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | AsmShl(arg1, arg2) -> "shl " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | AsmSal(arg1, arg2) -> "sal " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | AsmSar(arg1, arg2) -> "sar " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | AsmAnd(arg1, arg2) -> "and " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | AsmOr(arg1, arg2) -> "or " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | AsmXor(arg1, arg2) -> "xor " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | AsmLabel(arg1) ->  "\n" ^ arg1 ^ ":"
  | AsmCmp(arg1, arg2) -> "cmp " ^ code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  | AsmJmp(arg1) -> "jmp " ^ arg1
  | AsmJe(arg1) -> "je " ^ arg1
  | AsmJne(arg1) -> "jne " ^ arg1
  | AsmJl(arg1) -> "jl " ^ arg1
  | AsmJg(arg1) -> "jg " ^ arg1
  | AsmJge(arg1) -> "jge " ^ arg1
  | AsmJa(arg1) -> "ja " ^ arg1
  | AsmJae(arg1) -> "jae " ^ arg1
  | AsmJz(arg1) -> "jz " ^ arg1
  | AsmJle(arg1) -> "jle " ^ arg1
  | AsmJbe(arg1) -> "jbe " ^ arg1
  (* Cardinal *)
  | AsmCall(arg1) -> "call " ^ arg1
  | AsmPush(arg1) -> "push " ^ code_of_argument arg1 
  | AsmPop(arg1) -> "pop " ^ code_of_argument arg1
  | AsmSection(str) -> "section ." ^ str
  | AsmAlign(int) -> "align " ^ string_of_int int
  | AsmDq(args) -> "\tdq " ^ (String.concat ", " args)
  | AsmInc(arg1) -> "inc " ^ code_of_argument arg1
  | AsmRepMovsq -> "rep movsq"
  | AsmRepStosq -> "rep stosq"
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let rec code_of_instruction_list (instruction_list : instruction list) : string =
  match instruction_list with
  | [] -> ""
  | value :: tail ->
    code_of_instruction value ^ "\n" ^ code_of_instruction_list tail
;;
