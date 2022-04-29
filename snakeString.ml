open Printf
open Exprs
open Assembly

let string_tag = 0x0000000000000007L
let string_tag_mask = 0x000000000000000FL
let err_VAL_NOT_STRING = 18L

let string_native_bindings =
  [ "snakeStringCmp", (Native, 2)
  ; "snakeStringConcat", (Native, 2)
  ; "snakeStringSubstring", (Native, 3)
  ; "snakeStringToUpper", (Native, 1)
  ; "snakeStringToLower", (Native, 1)
  ; "snakeStringTrim", (Native, 1)
  ]
;;

(** This function will take the string length and return the number of words needed to fit the chars.
    Takes into account the 1 word needed for the string length and will pad so the address is 16byte aligned *)
let reserve_words_from_str_len (len : int) : int =
  let num_whole_words = (len / word_size) + 1 in
  (* + 1 here is to store the length of the string *)
  let rem_word =
    let rem = len mod word_size in
    rem / rem
  in
  let padding_word =
    let word_len = num_whole_words + rem_word in
    if word_len mod 2 = 0 then 0 else 1
  in
  num_whole_words + rem_word + padding_word
;;

let compile_string_literal
    (s : string)
    (tag : int)
    (reserve_fn : int -> int -> instruction list)
    : instruction list
  =
  let len = String.length s in
  let words_to_reserve = reserve_words_from_str_len len in
  let reserve_instrs = reserve_fn (words_to_reserve * word_size) tag in
  let char_lst = s |> String.to_seq |> List.of_seq in
  let len_instr =
    [ IMov (Reg R11, Const (Int64.of_int len)); IMov (RegOffset (0, R15), Reg R11) ]
  in
  let char_instrs =
    List.mapi
      (fun i c ->
        [ IMov (Reg R11, Const (Int64.of_int (Char.code c)))
          (* Characters are 1 byte wide so we increment by one byte at a time and then read from the 0th byte of r11*)
        ; IMov (RegOffset (i + word_size, R15), Reg R11b)
        ])
      char_lst
    |> List.flatten
  in
  let padding_instrs =
    [ IAdd (Reg R15, Const (Int64.of_int (words_to_reserve * word_size))) ]
  in
  let tag_string_instrs =
    [ IMov (Reg RAX, Reg R15); IAdd (Reg RAX, HexConst string_tag) ]
  in
  reserve_instrs @ len_instr @ char_instrs @ tag_string_instrs @ padding_instrs
;;

let check_is_string =
  [ IMov (Reg R11, Reg RAX)
  ; IAnd (Reg R11, HexConst string_tag_mask)
  ; ICmp (Reg R11, HexConst string_tag)
  ]
;;

let new_snake_string_of_size_fn (native_call : arg -> arg list -> instruction list)
    : instruction list
  =
  let label = [ ILabel "create_empty_snake_str" ] in
  let fn_preamble = [ IPush (Reg RBP); IMov (Reg RBP, Reg RSP) ] in
  let ok = "$memcheck_empty_snake_str" in
  (*-1 here since we know that any other tag will be positive*)
  let reserve_instrs =
    [ IMov (Reg RAX, LabelContents "?HEAP_END")
      (* rdi contains the number of bytes to reserve *)
    ; ISub (Reg RAX, Reg RDI)
    ; ICmp (Reg RAX, Reg R15)
    ; IJge (Label ok)
    ]
    @ native_call
        (Label "?try_gc")
        [ Sized (QWORD_PTR, Reg R15)
        ; (* alloc_ptr in C *)
          Sized (QWORD_PTR, Reg RDI)
        ; (* bytes_needed in C *)
          Sized (QWORD_PTR, Reg RBP)
        ; (* first_frame in C *)
          Sized (QWORD_PTR, Reg RSP) (* stack_top in C *)
        ]
    @ [ IInstrComment
          ( IMov (Reg R15, Reg RAX)
          , "assume gc success if returning here, so RAX holds the new heap_reg value" )
      ; ILabel ok
      ]
  in
  let load_str_len_in_first_slot =
    [ (* rsi contains the length of the string in a machine integer *)
      IMov (RegOffset (0, R15), Reg RSI)
    ; IMov (Reg RAX, Reg R15)
    ; IAdd (Reg R15, Reg RDI)
    ]
  in
  label
  @ fn_preamble
  @ reserve_instrs
  @ load_str_len_in_first_slot
  @ [ IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet ]
;;
