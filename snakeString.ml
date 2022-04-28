open Assembly

let string_tag = 0x0000000000000007L
let string_tag_mask = 0x0000000000000007L
let err_VAL_NOT_STRING = 18L

let compile_string_literal
    (s : string)
    (tag : int)
    (reserve_fn : int -> int -> instruction list)
    : instruction list
  =
  let len = String.length s in
  let reserve_instrs =
    let num_words_to_reserve = if len mod 2 = 0 then len + 2 else len + 1 in
    reserve_fn (num_words_to_reserve * word_size) tag
  in
  let char_lst = s |> String.to_seq |> List.of_seq in
  let len_instr =
    [ IMov (Reg R11, Const (Int64.of_int len)); IMov (RegOffset (0, R15), Reg R11) ]
  in
  let char_instrs =
    List.mapi
      (fun i c ->
        [ IMov (Reg R11, Const (Int64.of_int (Char.code c)))
        ; IMov (RegOffset ((i + 1) * word_size, R15), Reg R11)
        ])
      char_lst
    |> List.flatten
  in
  let padding_instrs =
    if len mod 2 = 0
    then [ IAdd (Reg R15, Const (Int64.of_int ((len + 2) * word_size))) ]
    else [ IAdd (Reg R15, Const (Int64.of_int ((len + 1) * word_size))) ]
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
