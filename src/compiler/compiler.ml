(** This file contains the definition of the compiler: the tool that translates
    an AST in our language into assembly language. *)

open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Freshening;;
open Wellformedness;;

let string_of_twice_int (n : int) : string =
  Int64.to_string (Int64.mul (Int64.of_int n) (Int64.of_int 2))

let check_rax_int (): instruction list =
  (* @ [AsmCall("stopWithError")] *)
  (* [AsmMov(ArgRegister R11, ArgRegister RAX)] *)
  let freshContinue = fresh_name "continue_here" in
  [AsmMov(ArgRegister R11, ArgRegister RAX)] @
  [AsmAnd(ArgRegister R11, ArgConstant "1")] @
  [AsmCmp(ArgRegister R11, ArgConstant "0")] @
  [AsmJe(freshContinue)] @ 
  [AsmMov(ArgRegister RDI, ArgConstant "1")] @
  [AsmCall("stopWithError")] @
  [AsmLabel(freshContinue)] 
;;

let check_rax_bool (): instruction list =
  let freshContinue = fresh_name "continue_here" in
  [AsmMov(ArgRegister R11, ArgRegister RAX)]
  @ [AsmAnd(ArgRegister R11, ArgConstant "3")]   
  @ [AsmCmp(ArgRegister R11, ArgConstant "3")]   
  @ [AsmJe(freshContinue)]
  @ [AsmMov(ArgRegister RDI, ArgConstant "2")]
  @ [AsmCall("stopWithError")]
  @ [AsmLabel(freshContinue)]
;;

let check_both_int (arg: argument) : instruction list = 
  let label_error = fresh_name "label_error" in
  let label_ok = fresh_name "label_ok" in
  [AsmMov(ArgRegister R11, ArgRegister RAX)] @
  [AsmAnd(ArgRegister R11, ArgConstant "1")] @
  [AsmCmp(ArgRegister R11, ArgConstant "0")] @
  [AsmJne(label_error)]  @

  [AsmMov(ArgRegister R11, arg)] @
  [AsmAnd(ArgRegister R11, ArgConstant "1")] @
  [AsmCmp(ArgRegister R11, ArgConstant "0")] @
  [AsmJne(label_error)] @

  [AsmJmp(label_ok)] @

  [AsmLabel(label_error)] @
  [AsmMov(ArgRegister RDI, ArgConstant "1")] @
  [AsmCall("stopWithError")] @

  [AsmLabel(label_ok)]
;;

let check_both_bool (arg: argument) : instruction list = 
  let label_error = fresh_name "label_error" in
  let label_ok = fresh_name "label_ok" in
  [AsmMov(ArgRegister R11, ArgRegister RAX)] @
  [AsmAnd(ArgRegister R11, ArgConstant "3")] @
  [AsmCmp(ArgRegister R11, ArgConstant "3")] @
  [AsmJne(label_error)]  @

  [AsmMov(ArgRegister R11, arg)] @
  [AsmAnd(ArgRegister R11, ArgConstant "3")] @
  [AsmCmp(ArgRegister R11, ArgConstant "3")] @
  [AsmJne(label_error)] @

  [AsmJmp(label_ok)] @

  [AsmLabel(label_error)] @
  [AsmMov(ArgRegister RDI, ArgConstant "2")] @
  [AsmCall("stopWithError")] @

  [AsmLabel(label_ok)]
;;

let check_is_tuple (arg: argument) : instruction list =
  let label_notTuple = fresh_name "label_notTuple" in
  let label_end      = fresh_name "label_end" in
  [ 
    AsmMov (ArgRegister R11, arg);
    AsmAnd (ArgRegister R11, ArgConstant "7");
    AsmCmp (ArgRegister R11, ArgConstant "1");
    AsmJne (label_notTuple);
    AsmMov (ArgRegister R10, arg);
    AsmSub (ArgRegister R10, ArgConstant "1");
    AsmMov (ArgRegister R10, ArgMemory (AddrByRegisterOffset (R10, 0)));
    AsmMov (ArgRegister R11, ArgConstant "0x8000000000000000");
    AsmAnd (ArgRegister R10, ArgRegister R11);
    AsmCmp (ArgRegister R10, ArgConstant "0");
    AsmJne (label_notTuple);
    AsmJmp (label_end);
    AsmLabel (label_notTuple);
    AsmMov(ArgRegister RDI, ArgConstant "3");
    AsmCall("stopWithError");
    AsmLabel (label_end)
  ]
;;

let rec count_expression (expr: expr list) : int =
  match expr with
  | [] -> 0
  | _::tail -> count_expression tail + 1
;;

let zero_temp (temp: argument) : instruction list =
  [AsmMov(ArgRegister R10, ArgConstant "0");
   AsmMov(temp, ArgRegister R10)]
;;

let rec compile_expression (env : environment) (e : expr)
  : instruction list =
  match e with
  | EInt(n) -> [AsmMov (ArgRegister RAX, ArgConstant (string_of_twice_int n))]
  | EBool(b) -> if b then [AsmMov(ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF")] else [AsmMov(ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF")]
  | EVar(s) -> let args = (find_named_variable s env) in [AsmMov (ArgRegister RAX, args)]
  | EUnaryOp(unary, expr) ->
    begin
      match unary with 
      | OpAfter -> compile_expression (env) (expr) @ check_rax_int() @[AsmAdd(ArgRegister RAX, ArgConstant (string_of_twice_int 1))]
      | OpBefore -> compile_expression env expr @ check_rax_int() @ [AsmSub(ArgRegister RAX, ArgConstant (string_of_twice_int 1))]
      | OpIsInt -> 
        compile_expression env expr @ 
        [AsmAnd(ArgRegister RAX, ArgConstant ("1"))] @ 
        [AsmXor(ArgRegister RAX, ArgConstant ("1"))] @
        [AsmShl(ArgRegister RAX, ArgConstant ("63"))] @ 
        [AsmMov(ArgRegister R10, ArgConstant "0x7FFFFFFFFFFFFFFF")] @
        [AsmOr(ArgRegister RAX, ArgRegister R10)]
      | OpIsBool -> 
        compile_expression env expr @ 
        [AsmAnd(ArgRegister RAX, ArgConstant "3")] @ 
        [AsmAdd(ArgRegister RAX, ArgConstant "1")] @ 
        [AsmShr(ArgRegister RAX, ArgConstant "2")] @ 
        [AsmShl(ArgRegister RAX, ArgConstant "63")] @ 
        [AsmMov(ArgRegister R10, ArgConstant "0x7FFFFFFFFFFFFFFF")] @ 
        [AsmOr(ArgRegister RAX, ArgRegister R10)]
      | OpIsTuple -> 
        let label_isTuple   = fresh_name "done_istuple" in
        let label_notTuple  = fresh_name "not_tuple_error" in
        let label_end       = fresh_name "istuple_end" in
        compile_expression env expr @
        [ 
          AsmMov (ArgRegister R10, ArgRegister RAX);
          AsmAnd (ArgRegister R10, ArgConstant "7");
          AsmCmp (ArgRegister R10, ArgConstant "1");
          AsmJne (label_notTuple);
          AsmMov (ArgRegister R10, ArgRegister RAX);
          AsmSub (ArgRegister R10, ArgConstant "1");
          AsmMov (ArgRegister R10, ArgMemory (AddrByRegisterOffset (R10, 0)));

          AsmMov (ArgRegister R11, ArgConstant "0x8000000000000000");
          AsmAnd (ArgRegister R10, ArgRegister R11);
          AsmCmp (ArgRegister R10, ArgConstant "0");
          AsmJne (label_notTuple)  ;

          AsmLabel label_isTuple;
          AsmMov (ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF");  
          AsmJmp (label_end);
          AsmLabel label_notTuple;
          AsmMov (ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF") ;
          AsmLabel label_end 
        ]         
      | OpPrint ->
        compile_expression env expr @
        [AsmMov(ArgRegister RDI, ArgRegister RAX)] @
        [AsmPush(ArgRegister RAX)] @
        [AsmCall("printValue")] @
        [AsmPop(ArgRegister RAX)]
    end
  | EBinaryOp(binary_operator, expr1, expr2) -> 
    begin 
      let (arg, newEnv) = allocate_temp_variable env in
      match binary_operator with 
      | OpPlus -> 
        compile_expression (newEnv) (expr1) @ 
        [AsmMov(arg, ArgRegister RAX)] @ 
        compile_expression (newEnv) (expr2) @ 
        check_both_int (arg) @ 
        [AsmAdd(ArgRegister RAX, arg)] @

        (* Zero Out temporary variables *)
        zero_temp arg

      | OpMinus -> 
        compile_expression (newEnv) (expr1) @ 
        [AsmMov(arg, ArgRegister RAX)] @ 
        compile_expression (newEnv) (expr2) @ 
        check_both_int (arg) @
        [AsmSub(arg, ArgRegister RAX)]  @ 
        [AsmMov(ArgRegister RAX, arg)] @

        (* Zero out temporary variable *)
        zero_temp arg

      (* We need to perform check because we do the shift would change tag bit *)
      | OpTimes -> 
        compile_expression (newEnv) (expr1) @ 
        [AsmMov(arg, ArgRegister RAX)] @
        compile_expression (newEnv) (expr2) @ 
        check_both_int(arg) @
        [AsmSar(ArgRegister RAX, ArgConstant "1")]  @ 
        [AsmIMul(ArgRegister RAX, arg)] @

        (* Zero out temporary variable *)
        zero_temp arg

      | OpLessThan ->  
        let label_false = fresh_name "lt_false" in
        let label_end = fresh_name "lt_end" in
        compile_expression (newEnv) (expr1) 
        @ [AsmMov(arg, ArgRegister RAX)] 
        @ compile_expression (newEnv) (expr2) 
        @ [AsmMov(ArgRegister R10, arg)]
        @ check_both_int (arg)
        @ [AsmCmp(ArgRegister R10, ArgRegister RAX);
           AsmJge(label_false);
           AsmMov(ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF");
           AsmJmp(label_end);
           AsmLabel(label_false);
           AsmMov(ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");
           AsmLabel(label_end);
          ]
        @ zero_temp arg
      | OpGreaterThan -> 
        let label_false = fresh_name "gt_false" in
        let label_end = fresh_name "gt_end" in
        compile_expression (newEnv) (expr1) 
        @ [AsmMov(arg, ArgRegister RAX)] 
        @ compile_expression (newEnv) (expr2) 
        @ [AsmMov(ArgRegister R10, arg)] 
        @ check_both_int (arg)
        @ [AsmCmp(ArgRegister R10, ArgRegister RAX)]
        @ [AsmJle(label_false);
           AsmMov(ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF");
           AsmJmp(label_end);
           AsmLabel(label_false);
           AsmMov(ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");
           AsmLabel(label_end)
          ] 
        @ zero_temp arg
      | OpEqualTo -> 
        let label_false = fresh_name "eq_false" in
        let label_end = fresh_name "eq_end" in
        compile_expression (newEnv) (expr1) 
        @ [AsmMov(arg, ArgRegister RAX)] 
        @ compile_expression (newEnv) (expr2) 
        @ [AsmMov(ArgRegister R10, arg)] 
        @ [AsmCmp(ArgRegister R10, ArgRegister RAX);
           AsmJne(label_false);
           AsmMov(ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF");
           AsmJmp(label_end);
           AsmLabel(label_false);
           AsmMov(ArgRegister RAX, ArgConstant "0x7FFFFFFFFFFFFFFF");
           AsmLabel(label_end)
          ] 
        @ zero_temp arg
      | OpAnd -> 
        compile_expression (newEnv) (expr1) 
        @ [AsmMov(arg, ArgRegister RAX)] 
        @ compile_expression (newEnv) (expr2) 
        @ [AsmMov(ArgRegister R10, arg)]     
        @ check_both_bool arg 
        @ [AsmAnd(ArgRegister RAX, ArgRegister R10)] 
        @ zero_temp arg

      | OpOr -> 
        compile_expression (newEnv) (expr1) 
        @ [AsmMov(arg, ArgRegister RAX)] 
        @ compile_expression (newEnv) (expr2)
        @ [AsmMov(ArgRegister R10, arg)]
        @ check_both_bool arg
        @ [AsmOr(ArgRegister RAX, ArgRegister R10)]
        @ zero_temp arg

      | OpTupleIndex ->
        let label_error = fresh_name "tuple_index_error" in
        let label_continue = fresh_name "tuple_index_continue" in

        (* Compile first expression and check if its tuple *)
        compile_expression newEnv expr1
        @ check_is_tuple (ArgRegister RAX)         
        @ [AsmSub (ArgRegister RAX, ArgConstant (string_of_int (1)))]
        @ [AsmMov(arg, ArgRegister RAX)]   

        (* Compile second expression check if its int *)
        @ compile_expression newEnv expr2
        @ check_rax_int()                             
        @ [AsmSar(ArgRegister RAX, ArgConstant "1")] 
        @ [AsmMov(ArgRegister R10, arg)]  

        (* Check if index is negative *)
        @ [AsmCmp(ArgRegister RAX, ArgConstant "0")]
        @ [AsmJl(label_error)]

        (* Check bounds *)
        @ [AsmMov(ArgRegister R11, ArgMemory(AddrByRegisterOffset(R10, 0)))] 
        @ [AsmCmp(ArgRegister RAX, ArgRegister R11)]
        @ [AsmJge(label_error)]          

        (* add two to skip over the size and GC word *)
        @ [AsmAdd(ArgRegister RAX, ArgConstant "2")]               
        @ [AsmMov(ArgRegister RAX, ArgMemory(AddrByRegisterProductOffset(R10, RAX, 8)))]
        @ [AsmJmp(label_continue)]
        @ [AsmLabel(label_error)]
        @ [AsmMov(ArgRegister RDI, ArgConstant "4")]     
        @ [AsmCall("stopWithError")]
        @ [AsmLabel(label_continue)]
        @ zero_temp arg
    end
  | ELet(str, expr1, expr2) -> 
    begin
      let newEnv = allocate_named_variable str env in
      compile_expression (env) (expr1) @ 
      [AsmMov(find_named_variable str newEnv, ArgRegister RAX)] @ 
      compile_expression (newEnv) (expr2) @
      (* zero out the memory for the variable after it is no longer needed *)
      [
        AsmMov(ArgRegister R10, ArgConstant "0");
        AsmMov(find_named_variable str newEnv, ArgRegister R10)
      ]
    end
  | EIf(expr1, expr2, expr3) -> 
    let label_false = fresh_name ("if_false") in 
    let label_end = fresh_name ("if_end") 
    in compile_expression (env) (expr1) 
       @ check_rax_bool()  
       @ [AsmCmp(ArgRegister RAX, ArgConstant "0xFFFFFFFFFFFFFFFF")]
       @ [AsmJne(label_false)]
       @ compile_expression (env) (expr2) 
       @ [AsmJmp(label_end)] 
       @ [AsmLabel(label_false)] 
       @ compile_expression (env) (expr3) 
       @ [AsmLabel(label_end)]
  (* | ECall(name, parameters) -> 
     let rec compile_params (params: expr list): instruction list =
      match params with 
      | [] -> []
      | head :: tail -> 
        compile_expression env head @ [AsmPush (ArgRegister RAX)] @ compile_params tail
     in 
     (* Reverse the arguments on the stack here *)
     let rec swap_values (i: int) (n: int): instruction list =
      if i >= n/2 then []
      else 
        (* Not stack top/bottom but actual top / bottom *)
        let top = i * 8 in
        let bottom = (n-1-i) * 8 in
        [AsmMov(ArgRegister RAX, ArgMemory(AddrByRegisterOffset (RSP, bottom)))] @
        [AsmMov(ArgRegister R10, ArgMemory(AddrByRegisterOffset (RSP, top)))] @
        [AsmMov(ArgMemory(AddrByRegisterOffset (RSP, bottom)), ArgRegister R10)] @
        [AsmMov(ArgMemory(AddrByRegisterOffset (RSP, top)), ArgRegister RAX)] @
        swap_values (i + 1) (n)
     in
     let rec count_params params acc =
      match params with
      | [] -> acc
      | _ :: tail -> count_params tail (acc + 1)
     in
     let evaluate = compile_params parameters in
     let param_count = count_params parameters 0 in
     let reverse_push = swap_values 0 param_count in
     (* Push all caller-saved register -- maybe *)
     evaluate @ 
     reverse_push @
     [AsmCall(name)] @ 
     (* 1. Remove Argument from stack *)
     [AsmAdd(ArgRegister RSP, ArgConstant (string_of_int(param_count * 8)))] @
     (* 2. Pop all caller-saved registers *)
     [] *)
  | ETuple(expr_list) ->
    let size = count_expression expr_list in
    let (temp_ptr, env) = allocate_temp_variable env in
    let bytes =  8 * (size + 2) in
    let ok_label = fresh_name "tuple_alloc_ok" in

    let rec recurse (expr_list : expr list) (curr_index: int) : instruction list =
      match expr_list with
      | [] -> []
      | value :: tail ->
        compile_expression env value 
        @ [AsmMov(ArgRegister R11, temp_ptr)] 
        @ [AsmSub(ArgRegister R11, ArgConstant "1")]
        @ [AsmMov(ArgMemory(AddrByRegisterOffset(R11, curr_index * 8)), ArgRegister RAX)]
        @ recurse tail (curr_index + 1)
    in
    [AsmMov(ArgRegister R10, ArgMemory(AddressByLabel("heap_cursor")))]
    @ [AsmMov(ArgRegister R11, ArgConstant (string_of_int bytes))]
    @ [AsmAdd(ArgRegister R11, ArgRegister R10)]

    (* compare new cursor vs end_of_heap *)
    @ [AsmCmp(ArgMemory(AddressByLabel("end_of_heap")), ArgRegister R11)]
    @ [AsmJae(ok_label)] (* If its still <= end then skip the GC call*)

    (* Not enough space so we need to invoce GC *)
    @ [AsmMov(ArgMemory(AddressByLabel "end_of_stack"), ArgRegister RSP)]
    @ [AsmMov(ArgRegister RDI, ArgConstant (string_of_int bytes))]
    @ [AsmCall("gc")]

    (* Now we can continue with the allocation *)
    @ [AsmLabel(ok_label)]

    @ [AsmMov(ArgRegister R10, ArgMemory(AddressByLabel "heap_cursor"))] (* Reload the new heap_cursor*)

    (* Compute the new cursor *)
    @ [AsmMov(ArgRegister R11, ArgConstant (string_of_int bytes))]
    @ [AsmAdd(ArgRegister R11, ArgRegister R10)]

    (* Update the heap cursor *)
    @ [AsmMov(ArgMemory(AddressByLabel "heap_cursor"), ArgRegister R11)]
    @ [AsmMov(ArgRegister RAX, ArgRegister R10)]
    @ [AsmInc(ArgRegister RAX)]
    @ [AsmMov(temp_ptr, ArgRegister RAX)] (* store the old location *)

    (* Moving in the header *)
    @ [AsmMov(ArgRegister RAX, ArgConstant (string_of_int(size)))]
    @ [AsmMov(ArgMemory(AddrByRegisterOffset(R10, 0)), ArgRegister RAX)]

    (* Now add the GC word *)
    @ [AsmMov (ArgRegister RAX, ArgConstant "0")]
    @ [AsmMov (ArgMemory (AddrByRegisterOffset (R10, 8)), ArgRegister RAX)]

    (* Instead of starting at index 1, we will start at 2, the third place *)
    @ recurse expr_list 2

    (* Return the memory address and inc 1 to make a it a bird pointer *)
    @ [AsmMov(ArgRegister RAX, temp_ptr)]
    (* Zero out variable on stack that is no longer being used *)
    @ zero_temp temp_ptr
  | EAppl (left_expression, right_expression) -> 
    (* Generate fresh labels *)
    let grow_closure_label = fresh_name "grow_closure" in
    let grow_ok_label = fresh_name "grow_ok" in
    let call_function_label = fresh_name "call_function" in
    let closure_error_label = fresh_name "closure_error" in
    let continue_here_label = fresh_name "continue_here" in

    (* Allocate temporary variables *)
    let (temp_closure, env1) = allocate_temp_variable env in
    let (temp_arguments, env2) = allocate_temp_variable env1 in

    (* Evaluate the left expression *)
    compile_expression env2 left_expression @
    [
      AsmMov(temp_closure, ArgRegister RAX);

      (* Added checks for verifying that the first argument to a function application is a pointer *)
      AsmMov(ArgRegister R11, temp_closure);
      AsmAnd(ArgRegister R11, ArgConstant "7");
      AsmCmp(ArgRegister R11, ArgConstant "1");
      AsmJne(closure_error_label);
    ] @

    (* Evaluate the right expresion :D *)
    compile_expression env2 right_expression @
    [AsmMov(temp_arguments, ArgRegister RAX)] @

    (* Checking and comparing closure header *)
    [
      AsmMov(ArgRegister RAX, temp_closure);
      AsmSub(ArgRegister RAX, ArgConstant "1");
      AsmMov(ArgRegister R11, ArgMemory(AddrByRegisterOffset(RAX, 0)));
      AsmMov(ArgRegister R10, ArgConstant "0x8000000000000000");
      AsmAnd(ArgRegister R11, ArgRegister R10);
      AsmCmp(ArgRegister R11, ArgRegister R10);
      AsmJne(closure_error_label);
    ] @ 


    (* Compares the applied count to the total parameter and either grow closure or call function *)
    [
      AsmMov(ArgRegister RAX, temp_closure);
      AsmSub(ArgRegister RAX, ArgConstant "1");
      AsmMov(ArgRegister R10, ArgMemory(AddrByRegisterOffset(RAX, 0)));

      AsmMov(ArgRegister R11, ArgConstant "0x7FFFFFFFFFFFFFFF");
      AsmAnd(ArgRegister R10, ArgRegister R11);   (* to clear tag bit 8*)

      AsmMov(ArgRegister R11, ArgMemory(AddrByRegisterOffset(RAX, 16)));
      AsmAdd(ArgRegister R10, ArgConstant "1");
      AsmCmp(ArgRegister R10, ArgRegister R11);

      (* if not equal  then we will grow the closure *)
      AsmJne(grow_closure_label);

      (* Else we call function *)
      AsmJmp(call_function_label)
    ] @

    (* Grow Closure Part *)
    [
      AsmLabel(grow_closure_label);

      (* Calculate number of things to copy *)
      AsmMov(ArgRegister RAX, temp_closure); 
      AsmSub(ArgRegister RAX, ArgConstant "1"); (* Convert falcon pointer to raw pointer *)
      AsmMov(ArgRegister RAX, ArgMemory(AddrByRegisterOffset(RAX, 0))); (* get the applied element *)
      AsmMov(ArgRegister R11, ArgConstant "0x7FFFFFFFFFFFFFFF");  
      AsmAnd(ArgRegister RAX, ArgRegister R11); (* untag it *)
      AsmAdd(ArgRegister RAX, ArgConstant "4"); (* add 4 to account for: applied, GC word, total, func*)
      AsmMov(ArgRegister RCX, ArgRegister RAX); (* store it in RCX to prepare for rep movsq*)

      (* Check if there is enough space in heap *)
      AsmMov(ArgRegister RAX, ArgRegister RCX);
      AsmShl(ArgRegister RAX, ArgConstant "3"); 
      
      AsmMov(ArgRegister R11, ArgMemory(AddressByLabel("heap_cursor")));
      AsmAdd(ArgRegister R11, ArgRegister RAX);                        
      AsmCmp(ArgMemory(AddressByLabel("end_of_heap")), ArgRegister R11 );
      AsmJae(grow_ok_label);    

      (* Not enough space do call gc *)
      AsmMov(ArgMemory(AddressByLabel "end_of_stack"), ArgRegister RSP);
      AsmPush(ArgRegister RCX) ;  (* Caller saved registers *)
      AsmMov(ArgRegister RDI, ArgRegister RAX);                      
      AsmCall("gc");
      AsmPop (ArgRegister RCX);

      AsmLabel(grow_ok_label);
      (* new heap cursor chanegd by GC potentially*)
      AsmMov(ArgRegister RAX, ArgMemory(AddressByLabel("heap_cursor")));
      AsmMov(ArgRegister RDX, ArgRegister RAX); (* store base of new copy *)

      (* Copy closure using AsmRepMovsq *)
      AsmMov(ArgRegister RSI, temp_closure);
      AsmSub(ArgRegister RSI, ArgConstant "1");  (* falcon pointer to raw pointer *)
      AsmMov(ArgRegister RDI, ArgRegister RAX); (* set the destination to be the heap_cursor location *)
      AsmRepMovsq; (* copy rcx words from rsi to rdi *)

      (* Copy the new argument (the result the second subexpression) to the end of the copy you just created. *)
      AsmMov(ArgRegister RAX, temp_arguments);
      AsmMov(ArgMemory(AddrByRegisterOffset(RDI, 0)), ArgRegister RAX);   

      (* In the copy, increase the number of arguments by one to account for new argument *)
      AsmMov(ArgRegister RAX, ArgMemory(AddrByRegisterOffset(RDX, 0))); (* get the applied element *)
      AsmInc(ArgRegister RAX); (* increase the number of arguments by one *)
      AsmMov(ArgRegister R11, ArgConstant "0x8000000000000000");
      AsmOr(ArgRegister RAX, ArgRegister R11);  (*reapply the tag*)
      AsmMov(ArgMemory(AddrByRegisterOffset(RDX, 0)), ArgRegister RAX); (* set the new applied element to be the old one *)

      (* Update heap cursor to reflect the fact that this new memory has been allocated by your copy. *)
      AsmMov(ArgRegister RAX, ArgRegister RDI);
      AsmAdd(ArgRegister RAX, ArgConstant "8");
      AsmMov(ArgMemory(AddressByLabel("heap_cursor")), ArgRegister RAX);

      (* Produce in rax a Falcon pointer to the new closure. *)
      AsmMov(ArgRegister RAX, ArgRegister RDX); 
      AsmAdd(ArgRegister RAX, ArgConstant "1");  
      AsmJmp(continue_here_label)
    ]@

    (* Call Function Part *)
    [
      AsmLabel(call_function_label);

      (* Get number of arguments *)
      AsmMov(ArgRegister RAX, temp_closure);
      AsmSub(ArgRegister RAX, ArgConstant "1");
      AsmMov(ArgRegister RAX, ArgMemory(AddrByRegisterOffset(RAX, 16))); 

      AsmMov(ArgRegister R10, temp_closure);

      (* Compute size to size in bytes and allocate stack space *)
      AsmIMul(ArgRegister RAX, ArgConstant "8");
      AsmSub(ArgRegister RSP, ArgRegister RAX);

      (* set RSI to point to the first argument in the closure *)
      AsmSub(ArgRegister R10, ArgConstant "1");
      AsmMov(ArgRegister RSI, ArgRegister R10);
      AsmAdd(ArgRegister RSI, ArgConstant "32");

      (* set RDI to the allocated space *)
      AsmMov(ArgRegister RDI, ArgRegister RSP);

      (* set RCX to be the number of arguments *)
      AsmMov(ArgRegister RCX, ArgMemory(AddrByRegisterOffset(R10, 0))); (* uses the applied paramater*)
      AsmMov(ArgRegister R11, ArgConstant "0x7FFFFFFFFFFFFFFF");
      AsmAnd(ArgRegister RCX, ArgRegister R11);

      (* Copy, aka pushing arguments to stack*)
      AsmRepMovsq;

      (* Now push the new argument into the stack *)
      AsmMov(ArgRegister RAX, temp_arguments);
      AsmMov(ArgMemory(AddrByRegisterOffset(RDI, 0)), ArgRegister RAX);

      (* Call function *)
      AsmMov(ArgRegister RAX, ArgMemory(AddrByRegisterOffset(R10, 24)));
      AsmCall("rax");


      (* Remove arguments from stack *)
      AsmMov(ArgRegister R10, temp_closure);
      AsmSub(ArgRegister R10, ArgConstant "1");
      AsmMov(ArgRegister R11, ArgMemory(AddrByRegisterOffset(R10, 16)));
      AsmIMul(ArgRegister R11, ArgConstant "8");
      AsmAdd(ArgRegister RSP, ArgRegister R11);
      AsmJmp(continue_here_label);
    ] @

    (* closure error *)
    [
      AsmLabel(closure_error_label);
      AsmMov(ArgRegister RDI, ArgConstant "5");
      AsmCall("stopWithError")
    ] @

    (* continue here *)
    [AsmLabel(continue_here_label)]@
    (* Zero out temporary variables *)
    zero_temp temp_closure @ zero_temp temp_arguments
  | ESet (tuple_expr, index_expr, value_expr) -> 
    let label_error = fresh_name "tuple_index_error" in
    let label_continue = fresh_name "tuple_index_continue" in
    let (temp_ptr, newEnv1) = allocate_temp_variable env in
    let (temp_index, newEnv2) = allocate_temp_variable newEnv1 in

    (* Compile the tuple expression and adjust the pointer *)
    compile_expression newEnv2 tuple_expr
    @ check_is_tuple (ArgRegister RAX)
    @ [AsmMov(temp_ptr, ArgRegister RAX)]

    (* Compile the index expression and verify it's an int *)
    @ compile_expression newEnv2 index_expr
    @ check_rax_int()
    @ [AsmSar(ArgRegister RAX, ArgConstant "1")] 
    @ [AsmMov(ArgRegister R10, temp_ptr)]
    @ [AsmSub(ArgRegister R10, ArgConstant "1")] (* Convert falcon pointer to raw pointer *)

    (* Check if index is negative *)
    @ [AsmCmp(ArgRegister RAX, ArgConstant "0")]
    @ [AsmJl(label_error)]

    (* Check if index is within bounds *)
    @ [AsmMov(ArgRegister R11, ArgMemory(AddrByRegisterOffset(R10, 0)))]
    @ [AsmCmp(ArgRegister RAX, ArgRegister R11)]
    @ [AsmJge(label_error)]
    @ [AsmMov(temp_index, ArgRegister RAX)]


    (* Compile the value expression to be stored *)
    @ compile_expression newEnv2 value_expr
    @ [AsmMov(ArgRegister R10, temp_index)]

    (* Add 2 to skip size and GC *)
    @ [AsmAdd(ArgRegister R10, ArgConstant "2")] 
    @ [AsmMov(ArgRegister R11, temp_ptr)]
    @ [AsmSub(ArgRegister R11, ArgConstant "1")] (* Convert falcon pointer to raw pointer *)
    @ [AsmMov(ArgMemory(AddrByRegisterProductOffset(R11, R10, 8)), ArgRegister RAX)]


    @ [AsmJmp(label_continue)]
    @ [AsmLabel(label_error)]
    @ [AsmMov(ArgRegister RDI, ArgConstant "4")]  
    @ [AsmCall("stopWithError")]
    @ [AsmLabel(label_continue)] @ 
    zero_temp temp_index @ zero_temp temp_ptr
;;

let rec get_memory_size_in_instruction (l : instruction list) (currentMax: int) : int =
  match l with 
  | [] -> currentMax
  | value :: tail -> 
    begin
      match value with
      | AsmMov(arg1, _) ->
        begin
          match arg1 with
          | ArgMemory(address) ->
            begin
              match address with
              | AddrByRegisterOffset(_, int) -> 
                if int < currentMax then get_memory_size_in_instruction tail int
                else get_memory_size_in_instruction tail currentMax
              | _ -> get_memory_size_in_instruction tail currentMax
            end
          | _ -> get_memory_size_in_instruction tail currentMax
        end
      | _ -> get_memory_size_in_instruction tail currentMax
    end
;;


let rec generate_zero_closure (declarations: declaration list ) : instruction list = 
  match declarations with 
  | [] -> []
  | value :: tail -> 
    let DFunction(name, arguments, _) =  value in 
    let closure_label =  "closure_of_" ^ name in 
    let function_label = name in
    let num_of_arguments = string_of_int (List.length arguments) in 
    [
      AsmAlign 8 ;
      AsmLabel closure_label;

      (* Added GC word here *)
      AsmDq ["0x8000000000000000"; "0x0000000000000000"; num_of_arguments; function_label]
    ] @ generate_zero_closure tail
;;


(* 
   Binds main expression and function body expression to closures.
*)
let rec bind_all_function_closures (env: environment) (decls: declaration list) : environment =
  match decls with
  | [] -> env
  | DFunction(name, _arguments, _expr) :: tail ->
    let closure_label = "closure_of_" ^ name in
    let arg = ArgLabelOffset(closure_label, 1) in
    let (_, map) = env in
    let new_env = (-8, Map.String.add name arg map) in
    bind_all_function_closures new_env tail
;;


(*
  A helper function that zeros out memory on the stack.
  Specificially for functions including bird main using rep stosq.
*)

let zero_stack_memory (memory_size: int) : instruction list = 
  let word_count =  memory_size / 8  in 
  [
    (* RSP and not RBP because we need to start from lower address when using rep stosq *)
    AsmMov(ArgRegister RDI, ArgRegister RSP);
    AsmMov(ArgRegister RCX, ArgConstant (string_of_int (word_count)));
    AsmMov(ArgRegister RAX, ArgConstant "0");
    AsmRepStosq;
  ]
;;


let rec compile_declaration (env: environment) (declaration_list : declaration list) : instruction list =
  begin
    match declaration_list with
    | [] -> []
    | value :: tail -> 
      let DFunction(name, arguments, expr) = value in
      let preallocated_environment = environment_with_parameters (env) (arguments) in      
      let instructions = compile_expression preallocated_environment expr in
      let memorySize = abs(get_memory_size_in_instruction instructions 0) in

      [AsmLabel(name)] @
      [AsmPush(ArgRegister RBP)] @
      [AsmMov(ArgRegister RBP, ArgRegister RSP)] @
      [AsmSub(ArgRegister RSP, ArgConstant (string_of_int  memorySize))] @



      (* Write zeros to all of the memory between rbp and rsp *)
      zero_stack_memory memorySize @

      (* 1. Push all callee-saved registers here *)
      instructions @
      (* 2.  Pop All callee-saved registers here *)
      [AsmMov(ArgRegister RSP, ArgRegister RBP)] @
      [AsmPop(ArgRegister RBP)] @
      [AsmRet]
      @ compile_declaration env tail
  end
;;

let compile_program (program : Asts.program) : instruction list =
  check_well_formed program;
  match program with
  | Program(declarationList, expr) ->
    let global_env = bind_all_function_closures empty_environment declarationList in 
    let instructions = compile_expression global_env expr in
    let declaration_instruction = compile_declaration global_env declarationList in
    let memorySize = abs(get_memory_size_in_instruction (instructions) 0) in
    [AsmPush(ArgRegister RBP)] @
    [AsmMov(ArgRegister RBP, ArgRegister RSP)] @
    [AsmSub(ArgRegister RSP, ArgConstant (string_of_int  memorySize))] @

    (* Setting gglobal variables at the start of bird main *)
    [AsmMov(ArgMemory(AddressByLabel("heap_cursor")), ArgRegister RDI)] @
    [AsmMov(ArgMemory(AddressByLabel "start_of_heap"), ArgRegister RDI)] @
    [AsmMov(ArgMemory(AddressByLabel "end_of_heap"), ArgRegister RSI)] @
    [AsmMov(ArgMemory(AddressByLabel "start_of_stack"), ArgRegister RBP)] @

    (* zero out stack memory after allocated RSP *)
    zero_stack_memory memorySize @

    instructions @
    [AsmMov(ArgRegister RSP, ArgRegister RBP)] @
    [AsmPop(ArgRegister RBP)] @
    [AsmRet] @
    declaration_instruction 
;;

let compile_to_assembly_code (program : Asts.program) : string =
  let Program(declarationList, _) = program in
  let instructions = compile_program program in
  let instruction_code = code_of_instruction_list instructions in
  let zero_closure_code = code_of_instruction_list (generate_zero_closure declarationList) in

  (* Declare labels as global in order for linker to connect gc.c file's extern declaration to these labels.*)
  "global start_of_stack\n" ^
  "global end_of_stack\n" ^
  "global start_of_heap\n" ^
  "global end_of_heap\n" ^
  "global heap_cursor\n" ^
  "section .data\n" ^
  "align 8\n" ^
  "start_of_stack:\n" ^
  "  dq 0\n" ^
  "end_of_stack:\n" ^
  "  dq 0\n" ^
  "start_of_heap:\n" ^
  "  dq 0\n" ^
  "end_of_heap:\n" ^
  "  dq 0\n" ^
  "heap_cursor:\n" ^
  "  dq 0\n" ^
  zero_closure_code ^ "\n" ^
  "section .text\n" ^
  "global bird_main\n" ^
  "extern printValue\n" ^
  "extern stopWithError\n" ^
  "extern gc\n" ^
  "bird_main:\n" ^
  instruction_code ^ "\n"
;;
