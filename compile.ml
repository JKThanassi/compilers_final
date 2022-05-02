open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors
open Graph
open SnakeString
module StringSet = Set.Make (String)
module ArgSet = Set.Make (Arg)

type 'a name_envt = (string * 'a) list
type 'a tag_envt = (tag * 'a) list

let print_env env how =
  ExtString.String.join
    "\n"
    (List.map (fun (id, bind) -> sprintf "%s -> %s" id (how bind)) env)
;;

let padding_word = HexConst 0x8BADF000000000DEL
let const_true = HexConst 0xFFFFFFFFFFFFFFFFL
let const_false = HexConst 0x7FFFFFFFFFFFFFFFL
let bool_mask = HexConst 0x8000000000000000L
let bool_tag = 0x000000000000000FL
let bool_tag_mask = 0x000000000000000FL
let num_tag = 0x0000000000000000L
let num_tag_mask = 0x0000000000000001L
let closure_tag = 0x0000000000000005L
let closure_tag_mask = 0x000000000000000FL
let forwarding_ptr_tag = 0x0000000000000003L
let tuple_tag = 0x0000000000000001L
let tuple_tag_mask = 0x000000000000000FL
let const_nil = HexConst tuple_tag
let err_COMP_NOT_NUM = 1L
let err_ARITH_NOT_NUM = 2L
let err_LOGIC_NOT_BOOL = 3L
let err_IF_NOT_BOOL = 4L
let err_OVERFLOW = 5L
let err_GET_NOT_TUPLE = 6L
let err_GET_LOW_INDEX = 7L
let err_GET_HIGH_INDEX = 8L
let err_GET_NOT_NUM = 9L
let err_ACCESS_NIL = 10L
let err_OUT_OF_MEMORY = 11L
let err_SET_NOT_TUPLE = 12L
let err_SET_LOW_INDEX = 13L
let err_SET_NOT_NUM = 14L
let err_SET_HIGH_INDEX = 15L
let err_CALL_NOT_CLOSURE = 16L
let err_CALL_ARITY_ERR = 17L
let heap_reg = R15
let scratch_reg = R11
let dummy_srcspan = Lexing.dummy_pos, Lexing.dummy_pos
let first_six_args_registers = [ RDI; RSI; RDX; RCX; R8; R9 ]

type compile_metadata =
  { fun_name : string
  ; is_recursive : bool
  }

let caller_saved_regs : arg list =
  [ Reg RDI; Reg RSI; Reg RDX; Reg RCX; Reg R8; Reg R9; Reg R10 ]
;;

let callee_saved_regs : arg list = [ Reg R12; Reg R14; Reg RBX ]
let heap_reg = R15
let scratch_reg = R11

(* you can add any functions or data defined by the runtime here for future use *)

let prim_bindings = []

let native_fun_bindings =
  [ "print", (Native, 1); "input", (Native, 0); "equal", (Native, 2) ]
  @ string_native_bindings
;;

let initial_val_env = native_fun_bindings
let initial_fun_env = prim_bindings

(* You may find some of these helpers useful *)

let rec find ls x =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" x))
  | (y, v) :: rest -> if y = x then v else find rest x
;;

let count_vars e =
  let rec helpA e =
    match e with
    | ASeq (e1, e2, _) -> max (helpC e1) (helpA e2)
    | ALet (_, bind, body, _) -> 1 + max (helpC bind) (helpA body)
    | ALetRec (binds, body, _) ->
      List.length binds
      + List.fold_left max (helpA body) (List.map (fun (_, rhs) -> helpC rhs) binds)
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf (_, t, f, _, _) -> max (helpA t) (helpA f)
    | _ -> 0
  in
  helpA e
;;

let rec replicate x i = if i = 0 then [] else x :: replicate x (i - 1)

let rec find_decl (ds : 'a decl list) (name : string) : 'a decl option =
  match ds with
  | [] -> None
  | (DFun (fname, _, _, _) as d) :: ds_rest ->
    if name = fname then Some d else find_decl ds_rest name
;;

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
  | [] -> false
  | x :: xs -> elt = x || find_one xs elt
;;

let rec find_dup (l : 'a list) : 'a option =
  match l with
  | [] -> None
  | [ _ ] -> None
  | x :: xs -> if find_one xs x then Some x else find_dup xs
;;

let rec find_opt (env : 'a name_envt) (elt : string) : 'a option =
  match env with
  | [] -> None
  | (x, v) :: rst -> if x = elt then Some v else find_opt rst elt
;;

(* Prepends a list-like env onto an name_envt *)
let merge_envs list_env1 list_env2 = list_env1 @ list_env2

(* Combines two name_envts into one, preferring the first one *)
let prepend env1 env2 =
  let rec help env1 env2 =
    match env1 with
    | [] -> env2
    | ((k, _) as fst) :: rst ->
      let rst_prepend = help rst env2 in
      if List.mem_assoc k env2 then rst_prepend else fst :: rst_prepend
  in
  help env1 env2
;;

let env_keys e = List.map fst e

(* Scope_info stores the location where something was defined,
   and if it was a function declaration, then its type arity and argument arity *)
type scope_info = sourcespan * int option * int option

let is_well_formed (p : sourcespan program) : sourcespan program fallible =
  let rec wf_E e (env : scope_info name_envt) =
    debug_printf "In wf_E: %s\n" (ExtString.String.join ", " (env_keys env));
    match e with
    | ESeq (e1, e2, _) -> wf_E e1 env @ wf_E e2 env
    | ETuple (es, _) -> List.concat (List.map (fun e -> wf_E e env) es)
    | EGetItem (e, idx, _) -> wf_E e env @ wf_E idx env
    | ESetItem (e, idx, newval, _) -> wf_E e env @ wf_E idx env @ wf_E newval env
    | ENil _ -> []
    | EBool _ -> []
    | EString _ -> []
    | ENumber (n, loc) ->
      if n > Int64.div Int64.max_int 2L || n < Int64.div Int64.min_int 2L
      then [ Overflow (n, loc) ]
      else []
    | EId (x, loc) -> if find_one (List.map fst env) x then [] else [ UnboundId (x, loc) ]
    | EPrim1 (_, e, _) -> wf_E e env
    | EPrim2 (_, l, r, _) -> wf_E l env @ wf_E r env
    | EIf (c, t, f, _) -> wf_E c env @ wf_E t env @ wf_E f env
    | ELet (bindings, body, _) ->
      let rec find_locs x (binds : 'a bind list) : 'a list =
        match binds with
        | [] -> []
        | BBlank _ :: rest -> find_locs x rest
        | BName (y, _, loc) :: rest ->
          if x = y then loc :: find_locs x rest else find_locs x rest
        | BTuple (binds, _) :: rest -> find_locs x binds @ find_locs x rest
      in
      let rec find_dupes (binds : 'a bind list) : exn list =
        match binds with
        | [] -> []
        | BBlank _ :: rest -> find_dupes rest
        | BName (x, _, def) :: rest ->
          List.map (fun use -> DuplicateId (x, use, def)) (find_locs x rest)
          @ find_dupes rest
        | BTuple (binds, _) :: rest -> find_dupes (binds @ rest)
      in
      let dupeIds = find_dupes (List.map (fun (b, _, _) -> b) bindings) in
      let rec process_binds (rem_binds : 'a bind list) (env : scope_info name_envt) =
        match rem_binds with
        | [] -> env, []
        | BBlank _ :: rest -> process_binds rest env
        | BTuple (binds, _) :: rest -> process_binds (binds @ rest) env
        | BName (x, allow_shadow, xloc) :: rest ->
          let shadow =
            if allow_shadow
            then []
            else (
              match find_opt env x with
              | None -> []
              | Some (existing, _, _) -> [ ShadowId (x, xloc, existing) ])
          in
          let new_env = (x, (xloc, None, None)) :: env in
          let newer_env, errs = process_binds rest new_env in
          newer_env, shadow @ errs
      in
      let rec process_bindings bindings (env : scope_info name_envt) =
        match bindings with
        | [] -> env, []
        | (b, e, _) :: rest ->
          let errs_e = wf_E e env in
          let env', errs = process_binds [ b ] env in
          let env'', errs' = process_bindings rest env' in
          env'', errs @ errs_e @ errs'
      in
      let env2, errs = process_bindings bindings env in
      dupeIds @ errs @ wf_E body env2
    (* | EApp (_, _, Native, _) -> [] *)
    | EApp (func, args, _, loc) ->
      let rec_errors = List.concat (List.map (fun e -> wf_E e env) (func :: args)) in
      (match func with
      | EId (funname, _) ->
        (match find_opt env funname with
        | Some (_, _, Some arg_arity) ->
          let actual = List.length args in
          if actual != arg_arity then [ Arity (arg_arity, actual, loc) ] else []
        | _ -> [])
      | _ -> [])
      @ rec_errors
    | ELetRec (binds, body, _) ->
      let nonfuns =
        List.find_all
          (fun b ->
            match b with
            | BName _, ELambda _, _ -> false
            | _ -> true)
          binds
      in
      let nonfun_errs =
        List.map (fun (b, _, where) -> LetRecNonFunction (b, where)) nonfuns
      in
      let rec find_locs x (binds : 'a bind list) : 'a list =
        match binds with
        | [] -> []
        | BBlank _ :: rest -> find_locs x rest
        | BName (y, _, loc) :: rest ->
          if x = y then loc :: find_locs x rest else find_locs x rest
        | BTuple (binds, _) :: rest -> find_locs x binds @ find_locs x rest
      in
      let rec find_dupes (binds : 'a bind list) : exn list =
        match binds with
        | [] -> []
        | BBlank _ :: rest -> find_dupes rest
        | BName (x, _, def) :: rest ->
          List.map (fun use -> DuplicateId (x, use, def)) (find_locs x rest)
        | BTuple (binds, _) :: rest -> find_dupes (binds @ rest)
      in
      let dupeIds = find_dupes (List.map (fun (b, _, _) -> b) binds) in
      let rec process_binds
          (rem_binds : sourcespan bind list)
          (env : scope_info name_envt)
        =
        match rem_binds with
        | [] -> env, []
        | BBlank _ :: rest -> process_binds rest env
        | BTuple (binds, _) :: rest -> process_binds (binds @ rest) env
        | BName (x, allow_shadow, xloc) :: rest ->
          let shadow =
            if allow_shadow
            then []
            else (
              match find_opt env x with
              | None -> []
              | Some (existing, _, _) ->
                if xloc = existing then [] else [ ShadowId (x, xloc, existing) ])
          in
          let new_env = (x, (xloc, None, None)) :: env in
          let newer_env, errs = process_binds rest new_env in
          newer_env, shadow @ errs
      in
      let env, bind_errs = process_binds (List.map (fun (b, _, _) -> b) binds) env in
      let rec process_bindings bindings env =
        match bindings with
        | [] -> env, []
        | (b, e, _) :: rest ->
          let env, errs = process_binds [ b ] env in
          let errs_e = wf_E e env in
          let env', errs' = process_bindings rest env in
          env', errs @ errs_e @ errs'
      in
      let new_env, binding_errs = process_bindings binds env in
      let rhs_problems = List.map (fun (_, rhs, _) -> wf_E rhs new_env) binds in
      let body_problems = wf_E body new_env in
      nonfun_errs
      @ dupeIds
      @ bind_errs
      @ binding_errs
      @ List.flatten rhs_problems
      @ body_problems
    | ELambda (binds, body, _) ->
      let rec dupe x args =
        match args with
        | [] -> None
        | BName (y, _, loc) :: _ when x = y -> Some loc
        | BTuple (binds, _) :: rest -> dupe x (binds @ rest)
        | _ :: rest -> dupe x rest
      in
      let rec process_args rem_args =
        match rem_args with
        | [] -> []
        | BBlank _ :: rest -> process_args rest
        | BName (x, _, loc) :: rest ->
          (match dupe x rest with
          | None -> []
          | Some where -> [ DuplicateId (x, where, loc) ])
          @ process_args rest
        | BTuple (binds, _) :: rest -> process_args (binds @ rest)
      in
      let rec flatten_bind (bind : sourcespan bind) : (string * scope_info) list =
        match bind with
        | BBlank _ -> []
        | BName (x, _, xloc) -> [ x, (xloc, None, None) ]
        | BTuple (args, _) -> List.concat (List.map flatten_bind args)
      in
      process_args binds
      @ wf_E body (merge_envs (List.concat (List.map flatten_bind binds)) env)
  and wf_D d (env : scope_info name_envt) (tyenv : StringSet.t) =
    match d with
    | DFun (_, args, body, _) ->
      let rec dupe x args =
        match args with
        | [] -> None
        | BName (y, _, loc) :: _ when x = y -> Some loc
        | BTuple (binds, _) :: rest -> dupe x (binds @ rest)
        | _ :: rest -> dupe x rest
      in
      let rec process_args rem_args =
        match rem_args with
        | [] -> []
        | BBlank _ :: rest -> process_args rest
        | BName (x, _, loc) :: rest ->
          (match dupe x rest with
          | None -> []
          | Some where -> [ DuplicateId (x, where, loc) ])
          @ process_args rest
        | BTuple (binds, _) :: rest -> process_args (binds @ rest)
      in
      let rec arg_env args (env : scope_info name_envt) =
        match args with
        | [] -> env
        | BBlank _ :: rest -> arg_env rest env
        | BName (name, _, loc) :: rest -> (name, (loc, None, None)) :: arg_env rest env
        | BTuple (binds, _) :: rest -> arg_env (binds @ rest) env
      in
      process_args args @ wf_E body (arg_env args env)
  and wf_G (g : sourcespan decl list) (env : scope_info name_envt) (tyenv : StringSet.t) =
    let add_funbind (env : scope_info name_envt) d =
      match d with
      | DFun (name, args, _, loc) ->
        (name, (loc, Some (List.length args), Some (List.length args))) :: env
    in
    let env = List.fold_left add_funbind env g in
    let errs = List.concat (List.map (fun d -> wf_D d env tyenv) g) in
    errs, env
  in
  match p with
  | Program (decls, body, _) ->
    let initial_env = initial_val_env in
    let initial_env =
      List.fold_left
        (fun env (name, (_, arg_count)) ->
          (name, (dummy_srcspan, Some arg_count, Some arg_count)) :: env)
        initial_fun_env
        initial_env
    in
    let rec find name (decls : 'a decl list) =
      match decls with
      | [] -> None
      | DFun (n, _, _, loc) :: _ when n = name -> Some loc
      | _ :: rest -> find name rest
    in
    let rec dupe_funbinds decls =
      match decls with
      | [] -> []
      | DFun (name, _, _, loc) :: rest ->
        (match find name rest with
        | None -> []
        | Some where -> [ DuplicateFun (name, where, loc) ])
        @ dupe_funbinds rest
    in
    let all_decls = List.flatten decls in
    let initial_tyenv = StringSet.of_list [ "Int"; "Bool" ] in
    let help_G (env, exns) g =
      let g_exns, funbinds = wf_G g env initial_tyenv in
      List.fold_left (fun xs x -> x :: xs) env funbinds, exns @ g_exns
    in
    let env, exns = List.fold_left help_G (initial_env, dupe_funbinds all_decls) decls in
    debug_printf "In wf_P: %s\n" (ExtString.String.join ", " (env_keys env));
    let exns = exns @ wf_E body env in
    (match exns with
    | [] -> Ok p
    | _ -> Error exns)
;;

(* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;; DESUGARING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *)

let desugar (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    fun name ->
      next := !next + 1;
      sprintf "%s_%d" name !next
  in
  let rec helpP (p : sourcespan program) =
    match p with
    | Program (decls, body, tag) ->
      (* This particular desugaring will convert declgroups into ELetRecs *)
      let merge_sourcespans ((s1, _) : sourcespan) ((_, s2) : sourcespan) : sourcespan =
        s1, s2
      in
      let wrap_G g body =
        match g with
        | [] -> body
        | f :: r ->
          let span =
            List.fold_left merge_sourcespans (get_tag_D f) (List.map get_tag_D r)
          in
          ELetRec (helpG g, body, span)
      in
      Program ([], List.fold_right wrap_G decls (helpE body), tag)
  and helpG g = List.map helpD g
  and helpD d =
    match d with
    | DFun (name, args, body, tag) ->
      let helpArg a =
        match a with
        | BTuple (_, tag) ->
          let name = gensym "argtup" in
          let newbind = BName (name, false, tag) in
          newbind, [ a, EId (name, tag), tag ]
        | _ -> a, []
      in
      let newargs, argbinds = List.split (List.map helpArg args) in
      let newbody = ELet (List.flatten argbinds, body, tag) in
      BName (name, false, tag), ELambda (newargs, helpE newbody, tag), tag
  and helpBE bind =
    let b, e, btag = bind in
    let e = helpE e in
    match b with
    | BTuple (binds, ttag) ->
      (match e with
      | EId _ -> expandTuple binds ttag e
      | _ ->
        let newname = gensym "tup" in
        (BName (newname, false, ttag), e, btag)
        :: expandTuple binds ttag (EId (newname, ttag)))
    | _ -> [ b, e, btag ]
  and expandTuple binds tag source : sourcespan binding list =
    let tupleBind i b =
      match b with
      | BBlank _ -> []
      | BName (_, _, btag) ->
        [ b, EGetItem (source, ENumber (Int64.of_int i, dummy_srcspan), tag), btag ]
      | BTuple (binds, tag) ->
        let newname = gensym "tup" in
        let newexpr = EId (newname, tag) in
        ( BName (newname, false, tag)
        , EGetItem (source, ENumber (Int64.of_int i, dummy_srcspan), tag)
        , tag )
        :: expandTuple binds tag newexpr
    in
    (*let size_check =
        EPrim2
          ( CheckSize,
            source,
            ENumber (Int64.of_int (List.length binds), dummy_srcspan),
            dummy_srcspan )
      in
      let size_check_bind = (BBlank dummy_srcspan, size_check, dummy_srcspan) in
      size_check_bind ::*)
    List.flatten (List.mapi tupleBind binds)
  and helpE e =
    match e with
    | ESeq (e1, e2, tag) -> ELet ([ BBlank tag, helpE e1, tag ], helpE e2, tag)
    | ETuple (exprs, tag) -> ETuple (List.map helpE exprs, tag)
    | EGetItem (e, idx, tag) -> EGetItem (helpE e, helpE idx, tag)
    | ESetItem (e, idx, newval, tag) -> ESetItem (helpE e, helpE idx, helpE newval, tag)
    | EId (x, tag) -> EId (x, tag)
    | EString (s, tag) -> EString (s, tag)
    | ENumber (n, tag) -> ENumber (n, tag)
    | EBool (b, tag) -> EBool (b, tag)
    | ENil (t, tag) -> ENil (t, tag)
    | EPrim1 (op, e, tag) -> EPrim1 (op, helpE e, tag)
    | EPrim2 (op, e1, e2, tag) -> EPrim2 (op, helpE e1, helpE e2, tag)
    | ELet (binds, body, tag) ->
      let newbinds = List.map helpBE binds in
      List.fold_right (fun binds body -> ELet (binds, body, tag)) newbinds (helpE body)
    | ELetRec (bindexps, body, tag) ->
      (* ASSUMES well-formed letrec, so only BName bindings *)
      let newbinds = List.map (fun (bind, e, tag) -> bind, helpE e, tag) bindexps in
      ELetRec (newbinds, helpE body, tag)
    | EIf (cond, thn, els, tag) -> EIf (helpE cond, helpE thn, helpE els, tag)
    | EApp (name, args, native, tag) -> EApp (helpE name, List.map helpE args, native, tag)
    | ELambda (binds, (ELambda _ as el), pos) ->
      let newSym = gensym "lam_body_lam" in
      helpE
        (ELambda
           ( binds
           , ELet ([ BName (newSym, false, pos), el, pos ], EId (newSym, pos), pos)
           , pos ))
    | ELambda (binds, body, tag) ->
      let expandBind bind =
        match bind with
        | BTuple (_, btag) ->
          let newparam = gensym "tuparg" in
          BName (newparam, false, btag), helpBE (bind, EId (newparam, btag), btag)
        | _ -> bind, []
      in
      let params, newbinds = List.split (List.map expandBind binds) in
      let newbody =
        List.fold_right (fun binds body -> ELet (binds, body, tag)) newbinds (helpE body)
      in
      ELambda (params, newbody, tag)
  in
  helpP p
;;

(* ASSUMES desugaring is complete *)
let rename_and_tag (p : tag program) : tag program =
  let rec rename env p =
    match p with
    | Program (decls, body, tag) ->
      Program
        (List.map (fun group -> List.map (helpD env) group) decls, helpE env body, tag)
  and helpD env decl =
    match decl with
    | DFun (name, args, body, tag) ->
      let newArgs, env' = helpBS env args in
      DFun (name, newArgs, helpE env' body, tag)
  and helpB env b =
    match b with
    | BBlank _ -> b, env
    | BName (name, allow_shadow, tag) ->
      let name' = sprintf "%s_%d" name tag in
      BName (name', allow_shadow, tag), (name, name') :: env
    | BTuple (binds, tag) ->
      let binds', env' = helpBS env binds in
      BTuple (binds', tag), env'
  and helpBS env (bs : tag bind list) =
    match bs with
    | [] -> [], env
    | b :: bs ->
      let b', env' = helpB env b in
      let bs', env'' = helpBS env' bs in
      b' :: bs', env''
  and helpBG env (bindings : tag binding list) =
    match bindings with
    | [] -> [], env
    | (b, e, a) :: bindings ->
      let b', env' = helpB env b in
      let e' = helpE env e in
      let bindings', env'' = helpBG env' bindings in
      (b', e', a) :: bindings', env''
  and helpE env e =
    match e with
    | ESeq (e1, e2, tag) -> ESeq (helpE env e1, helpE env e2, tag)
    | ETuple (es, tag) -> ETuple (List.map (helpE env) es, tag)
    | EGetItem (e, idx, tag) -> EGetItem (helpE env e, helpE env idx, tag)
    | ESetItem (e, idx, newval, tag) ->
      ESetItem (helpE env e, helpE env idx, helpE env newval, tag)
    | EPrim1 (op, arg, tag) -> EPrim1 (op, helpE env arg, tag)
    | EPrim2 (op, left, right, tag) -> EPrim2 (op, helpE env left, helpE env right, tag)
    | EIf (c, t, f, tag) -> EIf (helpE env c, helpE env t, helpE env f, tag)
    | EString _ -> e
    | ENumber _ -> e
    | EBool _ -> e
    | ENil _ -> e
    | EId (name, tag) ->
      (try EId (find env name, tag) with
      | InternalCompilerError _ -> e)
    | EApp (name, args, Native, tag) -> EApp (name, List.map (helpE env) args, Native, tag)
    | EApp (func, args, _, tag) ->
      let func = helpE env func in
      EApp (func, List.map (helpE env) args, Snake, tag)
    | ELet (binds, body, tag) ->
      let binds', env' = helpBG env binds in
      let body' = helpE env' body in
      ELet (binds', body', tag)
    | ELetRec (bindings, body, tag) ->
      let revbinds, env =
        List.fold_left
          (fun (revbinds, env) (b, e, t) ->
            let b, env = helpB env b in
            (b, e, t) :: revbinds, env)
          ([], env)
          bindings
      in
      let bindings' =
        List.fold_left
          (fun bindings (b, e, tag) -> (b, helpE env e, tag) :: bindings)
          []
          revbinds
      in
      let body' = helpE env body in
      ELetRec (bindings', body', tag)
    | ELambda (binds, body, tag) ->
      let binds', env' = helpBS env binds in
      let body' = helpE env' body in
      ELambda (binds', body', tag)
  in
  rename [] p
;;

(* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;; ANFING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; *)

type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram =
    match p with
    | Program ([], body, _) -> AProgram (helpA body, ())
    | Program _ -> raise (InternalCompilerError "decls should have been desugared away")
  and helpC (e : tag expr) : unit cexpr * unit anf_bind list =
    match e with
    | EPrim1 (op, arg, _) ->
      let arg_imm, arg_setup = helpI arg in
      CPrim1 (op, arg_imm, ()), arg_setup
    | EPrim2 (And, left, right, _) ->
      let left_imm, left_setup = helpI left in
      ( CIf (left_imm, helpA right, ACExpr (CImmExpr (ImmBool (false, ()))), OnlyBool, ())
      , left_setup )
    | EPrim2 (Or, left, right, _) ->
      let left_imm, left_setup = helpI left in
      ( CIf (left_imm, ACExpr (CImmExpr (ImmBool (true, ()))), helpA right, OnlyBool, ())
      , left_setup )
    | EPrim2 (op, left, right, _) ->
      let left_imm, left_setup = helpI left in
      let right_imm, right_setup = helpI right in
      CPrim2 (op, left_imm, right_imm, ()), left_setup @ right_setup
    | EIf (cond, _then, _else, _) ->
      let cond_imm, cond_setup = helpI cond in
      CIf (cond_imm, helpA _then, helpA _else, Any, ()), cond_setup
    | ELet ([], body, _) -> helpC body
    | ELet ((BBlank _, exp, _) :: rest, body, pos) ->
      let exp_ans, exp_setup = helpC exp in
      let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
      body_ans, exp_setup @ [ BSeq exp_ans ] @ body_setup
    | ELet ((BName (bind, _, _), exp, _) :: rest, body, pos) ->
      let exp_ans, exp_setup = helpC exp in
      let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
      body_ans, exp_setup @ [ BLet (bind, exp_ans) ] @ body_setup
    | ELetRec (binds, body, _) ->
      let processBind (bind, rhs, _) =
        match bind with
        | BName (name, _, _) -> name, helpC rhs
        | _ ->
          raise
            (InternalCompilerError
               (sprintf
                  "Encountered a non-simple binding in ANFing a let-rec: %s"
                  (string_of_bind bind)))
      in
      let names, new_binds_setup = List.split (List.map processBind binds) in
      let new_binds, _ = List.split new_binds_setup in
      let body_ans, body_setup = helpC body in
      body_ans, BLetRec (List.combine names new_binds) :: body_setup
    | ELambda (args, body, _) ->
      let processBind bind =
        match bind with
        | BName (name, _, _) -> name
        | _ ->
          raise
            (InternalCompilerError
               (sprintf
                  "Encountered a non-simple binding in ANFing a lambda: %s"
                  (string_of_bind bind)))
      in
      CLambda (List.map processBind args, helpA body, ()), []
    | ELet ((BTuple (_, _), _, _) :: _, _, _) ->
      raise (InternalCompilerError "Tuple bindings should have been desugared away")
    | EApp (func, args, native, _) ->
      let func_ans, func_setup = helpI func in
      let new_args, new_setup = List.split (List.map helpI args) in
      CApp (func_ans, new_args, native, ()), func_setup @ List.concat new_setup
    | ESeq (e1, e2, _) ->
      let e1_ans, e1_setup = helpC e1 in
      let e2_ans, e2_setup = helpC e2 in
      e2_ans, e1_setup @ [ BSeq e1_ans ] @ e2_setup
    | ETuple (args, _) ->
      let new_args, new_setup = List.split (List.map helpI args) in
      CTuple (new_args, ()), List.concat new_setup
    | EGetItem (tup, idx, _) ->
      let tup_imm, tup_setup = helpI tup in
      let idx_imm, idx_setup = helpI idx in
      CGetItem (tup_imm, idx_imm, ()), tup_setup @ idx_setup
    | ESetItem (tup, idx, newval, _) ->
      let tup_imm, tup_setup = helpI tup in
      let idx_imm, idx_setup = helpI idx in
      let new_imm, new_setup = helpI newval in
      CSetItem (tup_imm, idx_imm, new_imm, ()), tup_setup @ idx_setup @ new_setup
    | _ ->
      let imm, setup = helpI e in
      CImmExpr imm, setup
  and helpI (e : tag expr) : unit immexpr * unit anf_bind list =
    match e with
    | ENumber (n, _) -> ImmNum (n, ()), []
    | EBool (b, _) -> ImmBool (b, ()), []
    | EId (name, _) -> ImmId (name, ()), []
    | ENil _ -> ImmNil (), []
    | EString (s, tag) ->
      let tmp = sprintf "string_literal_%d" tag in
      ImmId (tmp, ()), [ BLet (tmp, CStringLiteral (s, ())) ]
    | ESeq (e1, e2, _) ->
      let _, e1_setup = helpI e1 in
      let e2_imm, e2_setup = helpI e2 in
      e2_imm, e1_setup @ e2_setup
    | ETuple (args, tag) ->
      let tmp = sprintf "tup_%d" tag in
      let new_args, new_setup = List.split (List.map helpI args) in
      ImmId (tmp, ()), List.concat new_setup @ [ BLet (tmp, CTuple (new_args, ())) ]
    | EGetItem (tup, idx, tag) ->
      let tmp = sprintf "get_%d" tag in
      let tup_imm, tup_setup = helpI tup in
      let idx_imm, idx_setup = helpI idx in
      ( ImmId (tmp, ())
      , tup_setup @ idx_setup @ [ BLet (tmp, CGetItem (tup_imm, idx_imm, ())) ] )
    | ESetItem (tup, idx, newval, tag) ->
      let tmp = sprintf "set_%d" tag in
      let tup_imm, tup_setup = helpI tup in
      let idx_imm, idx_setup = helpI idx in
      let new_imm, new_setup = helpI newval in
      ( ImmId (tmp, ())
      , tup_setup
        @ idx_setup
        @ new_setup
        @ [ BLet (tmp, CSetItem (tup_imm, idx_imm, new_imm, ())) ] )
    | EPrim1 (op, arg, tag) ->
      let tmp = sprintf "unary_%d" tag in
      let arg_imm, arg_setup = helpI arg in
      ImmId (tmp, ()), arg_setup @ [ BLet (tmp, CPrim1 (op, arg_imm, ())) ]
    | EPrim2 (And, left, right, tag) ->
      let tmp = sprintf "and_%d" tag in
      let left_imm, left_setup = helpI left in
      ( ImmId (tmp, ())
      , left_setup
        @ [ BLet
              ( tmp
              , CIf
                  ( left_imm
                  , helpA right
                  , ACExpr (CImmExpr (ImmBool (false, ())))
                  , OnlyBool
                  , () ) )
          ] )
    | EPrim2 (Or, left, right, tag) ->
      let tmp = sprintf "or_%d" tag in
      let left_imm, left_setup = helpI left in
      ( ImmId (tmp, ())
      , left_setup
        @ [ BLet
              ( tmp
              , CIf
                  ( left_imm
                  , ACExpr (CImmExpr (ImmBool (true, ())))
                  , helpA right
                  , OnlyBool
                  , () ) )
          ] )
    | EPrim2 (op, left, right, tag) ->
      let tmp = sprintf "binop_%d" tag in
      let left_imm, left_setup = helpI left in
      let right_imm, right_setup = helpI right in
      ( ImmId (tmp, ())
      , left_setup @ right_setup @ [ BLet (tmp, CPrim2 (op, left_imm, right_imm, ())) ] )
    | EIf (cond, _then, _else, tag) ->
      let tmp = sprintf "if_%d" tag in
      let cond_imm, cond_setup = helpI cond in
      ( ImmId (tmp, ())
      , cond_setup @ [ BLet (tmp, CIf (cond_imm, helpA _then, helpA _else, Any, ())) ] )
    | EApp (func, args, native, tag) ->
      let tmp = sprintf "app_%d" tag in
      let new_func, func_setup = helpI func in
      let new_args, new_setup = List.split (List.map helpI args) in
      ( ImmId (tmp, ())
      , func_setup
        @ List.concat new_setup
        @ [ BLet (tmp, CApp (new_func, new_args, native, ())) ] )
    | ELet ([], body, _) -> helpI body
    | ELet ((BBlank _, exp, _) :: rest, body, pos) ->
      let exp_ans, exp_setup = helpC exp in
      let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
      body_ans, exp_setup @ [ BSeq exp_ans ] @ body_setup
    | ELetRec (binds, body, tag) ->
      let tmp = sprintf "lam_%d" tag in
      let processBind (bind, rhs, _) =
        match bind with
        | BName (name, _, _) -> name, helpC rhs
        | _ ->
          raise
            (InternalCompilerError
               (sprintf
                  "Encountered a non-simple binding in ANFing a let-rec: %s"
                  (string_of_bind bind)))
      in
      let names, new_binds_setup = List.split (List.map processBind binds) in
      let new_binds, new_setup = List.split new_binds_setup in
      let body_ans, body_setup = helpC body in
      ( ImmId (tmp, ())
      , List.concat new_setup
        @ [ BLetRec (List.combine names new_binds) ]
        @ body_setup
        @ [ BLet (tmp, body_ans) ] )
    | ELambda (args, body, tag) ->
      let tmp = sprintf "lam_%d" tag in
      let processBind bind =
        match bind with
        | BName (name, _, _) -> name
        | _ ->
          raise
            (InternalCompilerError
               (sprintf
                  "Encountered a non-simple binding in ANFing a lambda: %s"
                  (string_of_bind bind)))
      in
      ImmId (tmp, ()), [ BLet (tmp, CLambda (List.map processBind args, helpA body, ())) ]
    | ELet ((BName (bind, _, _), exp, _) :: rest, body, pos) ->
      let exp_ans, exp_setup = helpC exp in
      let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
      body_ans, exp_setup @ [ BLet (bind, exp_ans) ] @ body_setup
    | ELet ((BTuple (_, _), _, _) :: _, _, _) ->
      raise (InternalCompilerError "Tuple bindings should have been desugared away")
  and helpA e : unit aexpr =
    let ans, ans_setup = helpC e in
    List.fold_right
      (fun bind body ->
        match bind with
        | BSeq exp -> ASeq (exp, body, ())
        | BLet (name, exp) -> ALet (name, exp, body, ())
        | BLetRec names -> ALetRec (names, body, ()))
      ans_setup
      (ACExpr ans)
  in
  helpP p
;;

(* IMPLEMENT THIS FROM YOUR PREVIOUS ASSIGNMENT *)
let free_vars_sset (e : 'a aexpr) : StringSet.t =
  let rec helpA (e : 'a aexpr) : StringSet.t =
    match e with
    | ASeq (s1, s2, _) -> StringSet.union (helpC s1) (helpA s2)
    | ALet (s, bind, e, _) ->
      let bind_set = helpC bind in
      let body_set = helpA e in
      StringSet.union bind_set (StringSet.diff body_set (StringSet.add s StringSet.empty))
    | ALetRec (binds, e, _) ->
      let binds_set =
        List.fold_left
          (fun acc_set (name, bind_exp) ->
            let bind_set =
              StringSet.diff (helpC bind_exp) (StringSet.add name StringSet.empty)
            in
            StringSet.union acc_set bind_set)
          StringSet.empty
          binds
      in
      let bind_names, _ = List.split binds in
      let body_set = StringSet.diff (helpA e) (StringSet.of_list bind_names) in
      StringSet.union binds_set body_set
    | ACExpr ce -> helpC ce
  and helpC (e : 'a cexpr) : StringSet.t =
    match e with
    | CStringLiteral _ -> StringSet.empty
    | CLambda (args, body, _) ->
      let argsSet = StringSet.of_list args in
      let bodySet = helpA body in
      StringSet.diff bodySet argsSet
    | CApp (_, _, Native, _) -> StringSet.empty
    | CApp (f_name, args, _, _) ->
      List.fold_left
        (fun acc_set imm_val -> StringSet.union acc_set (helpI imm_val))
        StringSet.empty
        (f_name :: args)
    | CIf (cond, t, f, _, _) ->
      StringSet.union (helpI cond) (StringSet.union (helpA t) (helpA f))
    | CGetItem (to_get, idx, _) -> StringSet.union (helpI to_get) (helpI idx)
    | CSetItem (to_set, idx, set_val, _) ->
      StringSet.union (helpI to_set) (StringSet.union (helpI idx) (helpI set_val))
    | CPrim1 (_, e, _) -> helpI e
    | CPrim2 (_, e1, e2, _) -> StringSet.union (helpI e1) (helpI e2)
    | CImmExpr e -> helpI e
    | CTuple (l, _) ->
      List.fold_left
        (fun acc_set imm_val -> StringSet.union acc_set (helpI imm_val))
        StringSet.empty
        l
  and helpI (e : 'a immexpr) : StringSet.t =
    match e with
    | ImmId (n, _) -> StringSet.add n StringSet.empty
    | _ -> StringSet.empty
  in
  helpA e
;;

let free_vars (e : 'a aexpr) : string list =
  free_vars_sset e |> StringSet.to_seq |> List.of_seq
;;

let free_vars_cache (prog : 'a aprogram) : StringSet.t aprogram =
  let rec helpA (e : 'a aexpr) : StringSet.t aexpr =
    match e with
    | ASeq (s1, s2, _) ->
      let fv_s1 = helpC s1 in
      let fv_s2 = helpA s2 in
      ASeq (fv_s1, fv_s2, StringSet.union (get_tag_cexpr fv_s1) (get_tag_aexpr fv_s2))
    | ALet (s, bind, e, _) ->
      let bind_fv = helpC bind in
      let body_fv = helpA e in
      let bind_set = get_tag_cexpr bind_fv in
      let body_set = get_tag_aexpr body_fv in
      ALet
        ( s
        , bind_fv
        , body_fv
        , StringSet.union
            bind_set
            (StringSet.diff body_set (StringSet.add s StringSet.empty)) )
    | ALetRec (binds, e, _) ->
      let binds_set, new_binds =
        List.fold_left
          (fun (acc_set, acc_binds) (name, bind_exp) ->
            let fv_bind_exp = helpC bind_exp in
            let bind_set =
              StringSet.diff
                (get_tag_cexpr fv_bind_exp)
                (StringSet.add name StringSet.empty)
            in
            StringSet.union acc_set bind_set, (name, fv_bind_exp) :: acc_binds)
          (StringSet.empty, [])
          binds
      in
      let bind_names, _ = List.split binds in
      let fv_body_exp = helpA e in
      let body_set =
        StringSet.diff (get_tag_aexpr fv_body_exp) (StringSet.of_list bind_names)
      in
      ALetRec (new_binds, fv_body_exp, StringSet.union binds_set body_set)
    | ACExpr ce -> ACExpr (helpC ce)
  and helpC (e : 'a cexpr) : StringSet.t cexpr =
    match e with
    | CLambda (args, body, _) ->
      let argsSet = StringSet.of_list args in
      let fv_body_exp = helpA body in
      let bodySet = get_tag_aexpr fv_body_exp in
      CLambda (args, fv_body_exp, StringSet.diff bodySet argsSet)
    | CApp (n, a, Native, _) -> CApp (helpI n, List.map helpI a, Native, StringSet.empty)
    | CApp (f_name, args, ct, _) ->
      let args_set, new_args =
        List.fold_left
          (fun (acc_set, acc_args) imm_val ->
            let fv_imm = helpI imm_val in
            StringSet.union acc_set (get_tag_imm fv_imm), fv_imm :: acc_args)
          (StringSet.empty, [])
          (f_name :: args)
      in
      CApp (helpI f_name, new_args, ct, args_set)
    | CIf (cond, t, f, tr, _) ->
      let fvc = helpI cond in
      let fvt = helpA t in
      let fvf = helpA f in
      CIf
        ( fvc
        , fvt
        , fvf
        , tr
        , StringSet.union
            (get_tag_imm fvc)
            (StringSet.union (get_tag_aexpr fvt) (get_tag_aexpr fvf)) )
    | CGetItem (to_get, idx, _) ->
      let fv_tg = helpI to_get in
      let fv_idx = helpI idx in
      CGetItem (fv_tg, fv_idx, StringSet.union (get_tag_imm fv_tg) (get_tag_imm fv_idx))
    | CSetItem (to_set, idx, set_val, _) ->
      let fv_to_set = helpI to_set in
      let fv_idx = helpI idx in
      let fv_set_val = helpI set_val in
      CSetItem
        ( fv_to_set
        , fv_idx
        , fv_set_val
        , StringSet.union
            (get_tag_imm fv_to_set)
            (StringSet.union (get_tag_imm fv_idx) (get_tag_imm fv_set_val)) )
    | CPrim1 (o, e, _) ->
      let fve = helpI e in
      CPrim1 (o, fve, get_tag_imm fve)
    | CPrim2 (o, e1, e2, _) ->
      let fve1 = helpI e1 in
      let fve2 = helpI e2 in
      CPrim2 (o, fve1, fve2, StringSet.union (get_tag_imm fve1) (get_tag_imm fve2))
    | CImmExpr e -> CImmExpr (helpI e)
    | CStringLiteral (s, _) -> CStringLiteral (s, StringSet.empty)
    | CTuple (l, _) ->
      let fvl = List.map (fun e -> helpI e) l in
      let fvt =
        List.fold_left
          (fun acc_set imm_val -> StringSet.union acc_set (get_tag_imm imm_val))
          StringSet.empty
          fvl
      in
      CTuple (fvl, fvt)
  and helpI (e : 'a immexpr) : StringSet.t immexpr =
    match e with
    | ImmId (n, _) -> ImmId (n, StringSet.add n StringSet.empty)
    | ImmBool (b, _) -> ImmBool (b, StringSet.empty)
    | ImmNil _ -> ImmNil StringSet.empty
    | ImmNum (n, _) -> ImmNum (n, StringSet.empty)
  in
  match prog with
  | AProgram (p, _) ->
    let fv_p = helpA p in
    AProgram (fv_p, get_tag_aexpr fv_p)
;;

(* IMPLEMENT THIS FROM YOUR PREVIOUS ASSIGNMENT *)
let naive_stack_allocation (prog : tag aprogram) : tag aprogram * arg name_envt name_envt =
  let outer_env : arg name_envt name_envt ref = { contents = [] } in
  let rec env_helper (e : tag aexpr) (si : int) (curr_fun_name : string) : arg name_envt =
    match e with
    | ALet (id, exp_to_bind, body, _) ->
      let to_bind_env =
        match exp_to_bind with
        | CLambda _ ->
          let lam_emv = env_helper (ACExpr exp_to_bind) 1 id in
          let _ = outer_env := (id, lam_emv) :: !outer_env in
          []
        | _ -> env_helper (ACExpr exp_to_bind) 1 curr_fun_name
      in
      let body_env =
        env_helper body (si + count_vars (ACExpr exp_to_bind) + 1) curr_fun_name
      in
      [ id, RegOffset (~-si * word_size, RBP) ] @ to_bind_env @ body_env
    | ACExpr c -> env_helper_cexpr c si curr_fun_name
    | ASeq (cexp, aexp, _) ->
      let cexp_env = env_helper_cexpr cexp si curr_fun_name in
      let aexp_env = env_helper aexp (si + count_vars (ACExpr cexp) + 1) curr_fun_name in
      cexp_env @ aexp_env
    | ALetRec (binds, body, _) ->
      let letrec_env =
        List.mapi
          (fun idx (n, exp) ->
            let exp_env_to_add =
              match exp with
              | CLambda _ ->
                let lam_emv = env_helper (ACExpr exp) 1 n in
                let _ = outer_env := (n, lam_emv) :: !outer_env in
                []
              | _ ->
                raise
                  (InternalCompilerError "should not encounter a non lambda in aletrec")
            in
            [ n, RegOffset (~-(si + idx) * word_size, RBP) ] @ exp_env_to_add)
          binds
      in
      let body_env = env_helper body (1 + si + List.length binds) curr_fun_name in
      List.flatten letrec_env @ body_env
  and env_helper_cexpr (e : tag cexpr) (si : int) (curr_fun_name : string) : arg name_envt
    =
    match e with
    | CIf (_, t, f, _, _) -> env_helper t si curr_fun_name @ env_helper f si curr_fun_name
    | CLambda (args, body, _) ->
      let fv =
        free_vars (ACExpr e)
        |> List.sort String.compare
        |> List.filter (fun fv_name -> fv_name != curr_fun_name)
      in
      let fv_env =
        List.mapi (fun i fv -> fv, RegOffset (~-(1 + i) * word_size, RBP)) fv
      in
      (*TODO*)
      let fn_args_env =
        List.mapi (fun i a -> a, RegOffset (word_size * (i + 3), RBP)) args
      in
      env_helper body (1 + List.length fv) curr_fun_name @ fv_env @ fn_args_env
    | _ -> []
  in
  let (AProgram (body, _)) = prog in
  let ocsh_env = env_helper body 1 "" in
  let _ = outer_env := ("ocsh", ocsh_env) :: !outer_env in
  prog, !outer_env
;;

(* IMPLEMENT THE BELOW *)

let set_to_list (sset : StringSet.t) : string list =
  sset |> StringSet.to_seq |> List.of_seq
;;

let graph_merge_fn _ s1 s2 =
  match s1, s2 with
  | Some x, Some y -> Some (NeighborSet.union x y)
  | None, y -> y
  | x, None -> x
;;

(* let live_in_exp (e: StringSet.t aexpr) (live_out: StringSet.t): StringSet.t= 
  let rec helpA (e: StringSet.t aexpr) (live_out: StringSet.t) =
    match e with 
    | ALet (name, bind_exp, body, _) -> 
      let body_live = helpA body live_out in
      let bind_live = helpC bind_exp body_live in
      let fv_e = get_tag_cexpr bind_exp in
      let fv_b = get_tag_aexpr body in
      let name_set = StringSet.add name StringSet.empty in
      (StringSet.union fv_e fv_b) |> StringSet.union live_out |> StringSet.diff name_set
    | _ -> StringSet.empty
  and helpC (e: StringSet.t cexpr) (live_out: StrinSet.t) : StringSet.t = StringSet.empty *)

let interfere (e : StringSet.t aexpr) (live : StringSet.t) : grapht =
  let rec helpA (e : StringSet.t aexpr) (live : StringSet.t) : grapht =
    match e with
    | ALet (name, bind_exp, body, _) ->
      let g = add_node (helpA body live) name in
      let g_bind = helpC bind_exp (live |> StringSet.union (get_tag_aexpr body)) in
      let interfered_body_fv_graph =
        List.fold_right
          (fun fv g_acc -> add_edge g_acc name fv)
          (body |> get_tag_aexpr |> StringSet.union live |> set_to_list)
          g
      in
      Graph.merge graph_merge_fn interfered_body_fv_graph g_bind
    | ALetRec (binds, body, _) ->
      let var_names, var_bodies = List.split binds in
      let new_live = body |> get_tag_aexpr |> StringSet.union live in
      let lam_graph =
        List.fold_right
          (Graph.merge graph_merge_fn)
          (List.map (fun b -> helpC b new_live) var_bodies)
          empty
      in
      let unioned_lam_fvs =
        get_vertices lam_graph |> StringSet.of_list |> StringSet.union live |> set_to_list
      in
      let mut_graph : grapht ref = { contents = lam_graph } in
      (* add interference edges between each of the letrec variables and the free vars of the lambdas*)
      List.iter
        (fun fv_outer ->
          List.iter
            (fun fv_inner -> mut_graph := add_edge !mut_graph fv_outer fv_inner)
            unioned_lam_fvs)
        var_names;
      (* add interference edges between each of the letrec variables *)
      List.iter
        (fun fv_outer ->
          List.iter
            (fun fv_inner -> mut_graph := add_edge !mut_graph fv_outer fv_inner)
            var_names)
        var_names;
      let body_graph = helpA body live in
      let interfered_body_fvs = body |> get_tag_aexpr |> set_to_list in
      (* Merge the body's graph with the one we've generated so far, then add edges between the body fvs and the bind names *)
      mut_graph := Graph.merge graph_merge_fn !mut_graph body_graph;
      List.iter
        (fun fv_outer ->
          List.iter
            (fun fv_inner -> mut_graph := add_edge !mut_graph fv_outer fv_inner)
            interfered_body_fvs)
        var_names;
      !mut_graph
    | ASeq (cexp, axp, _) ->
      let cg = helpC cexp live in
      let ag = helpA axp live in
      Graph.merge graph_merge_fn cg ag
    | ACExpr cexp -> helpC cexp live
  and helpC (e : StringSet.t cexpr) (live : StringSet.t) : grapht =
    match e with
    | CIf (_, t_aexp, f_aexp, _, _) ->
      let g_t = helpA t_aexp live in
      let g_f = helpA f_aexp live in
      Graph.merge graph_merge_fn g_t g_f
    | CLambda (_, _, fv) ->
      let fv_lst = set_to_list fv in
      let g : grapht ref = { contents = empty } in
      (match fv_lst with
      | [] -> ()
      | [ x ] -> g := add_node !g x
      | _ ->
        List.iter
          (fun fv_outer ->
            List.iter (fun fv_inner -> g := add_edge !g fv_outer fv_inner) fv_lst)
          fv_lst);
      !g
    | _ -> empty
  in
  helpA e StringSet.empty
;;

let register_colors_list = [ Reg R10; Reg R12; Reg R13; Reg R14; Reg RBX ]
let register_colors_set = register_colors_list |> ArgSet.of_list
let gen_caller_save_reg_instrs = List.map (fun r -> IPush r) register_colors_list
let gen_caller_restore_reg_instrs = List.rev_map (fun r -> IPop r) register_colors_list

let color_node (node : string) (stack_offset : int) (g : grapht) (env : arg name_envt)
    : arg name_envt * int
  =
  let neighbor_nodes = get_neighbors g node in
  let neighbor_color_list =
    List.fold_right
      (fun n color_lst ->
        match find_opt env n with
        | Some (RegOffset _) -> color_lst
        | Some a -> a :: color_lst
        | None -> color_lst)
      neighbor_nodes
      []
  in
  let neighbor_color_set = ArgSet.of_list neighbor_color_list in
  let remaining_regs =
    ArgSet.diff register_colors_set neighbor_color_set |> ArgSet.to_seq |> List.of_seq
  in
  if remaining_regs = []
  then (node, RegOffset (stack_offset * word_size, RBP)) :: env, stack_offset + 1
  else (node, List.hd remaining_regs) :: env, stack_offset
;;

let color_graph (g : grapht) (init_env : arg name_envt) (stack_offset : int)
    : arg name_envt * int
  =
  let worklist = create_worklist_from_graph g in
  Stack.fold
    (fun (acc_env, stack_offset) node -> color_node node stack_offset g acc_env)
    (init_env, stack_offset)
    worklist
;;

let register_allocation (prog : tag aprogram) : tag aprogram * arg name_envt name_envt =
  let outer_env : arg name_envt name_envt ref = { contents = [] } in
  let rec env_helper (e : StringSet.t aexpr) (si : int) (curr_fun_name : string)
      : arg name_envt
    =
    match e with
    | ALet (id, exp_to_bind, body, _) ->
      let to_bind_env =
        match exp_to_bind with
        | CLambda _ ->
          let lam_emv = env_helper (ACExpr exp_to_bind) 1 id in
          let _ = outer_env := (id, lam_emv) :: !outer_env in
          []
        | _ -> env_helper (ACExpr exp_to_bind) 1 curr_fun_name
      in
      let body_env =
        env_helper body (si + count_vars (ACExpr exp_to_bind) + 1) curr_fun_name
      in
      [ id, RegOffset (~-si * word_size, RBP) ] @ to_bind_env @ body_env
    | ACExpr c -> env_helper_cexpr c si curr_fun_name
    | ASeq (cexp, aexp, _) ->
      let cexp_env = env_helper_cexpr cexp si curr_fun_name in
      let aexp_env = env_helper aexp (si + count_vars (ACExpr cexp) + 1) curr_fun_name in
      cexp_env @ aexp_env
    | ALetRec (binds, body, _) ->
      let letrec_env =
        List.mapi
          (fun idx (n, exp) ->
            let exp_env_to_add =
              match exp with
              | CLambda _ ->
                let lam_emv = env_helper (ACExpr exp) 1 n in
                let _ = outer_env := (n, lam_emv) :: !outer_env in
                []
              | _ ->
                raise
                  (InternalCompilerError "should not encounter a non lambda in aletrec")
            in
            [ n, RegOffset (~-(si + idx) * word_size, RBP) ] @ exp_env_to_add)
          binds
      in
      let body_env = env_helper body (1 + si + List.length binds) curr_fun_name in
      List.flatten letrec_env @ body_env
  and env_helper_cexpr (e : StringSet.t cexpr) (si : int) (curr_fun_name : string)
      : arg name_envt
    =
    match e with
    | CIf (_, t, f, _, _) -> env_helper t si curr_fun_name @ env_helper f si curr_fun_name
    | CLambda (args, body, _) ->
      let g = interfere (ACExpr e) StringSet.empty in
      let allocated_regs, curr_si = color_graph g [] si in
      let fn_args_env =
        List.mapi (fun i a -> a, RegOffset (word_size * (i + 3), RBP)) args
      in
      env_helper body (1 + curr_si) curr_fun_name @ allocated_regs @ fn_args_env
    | _ -> []
  in
  let (AProgram (body, _)) = free_vars_cache prog in
  let g = interfere body StringSet.empty in
  let allocated_regs, curr_si = color_graph g [] 1 in
  let ocsh_env = env_helper body (1 + curr_si) "" in
  let _ = outer_env := ("ocsh", allocated_regs @ ocsh_env) :: !outer_env in
  prog, !outer_env
;;

let rec deepest_stack e env outer_env =
  let rec helpA e =
    match e with
    | ALet (name, bind, body, _) ->
      List.fold_left max 0 [ name_to_offset name; helpC bind name; helpA body ]
    | ALetRec (binds, body, _) ->
      List.fold_left max (helpA body) (List.map (fun (n, bind) -> helpC bind n) binds)
    | ASeq (first, rest, _) -> max (helpC first "") (helpA rest)
    | ACExpr e -> helpC e ""
  and helpC e name =
    match e with
    | CIf (c, t, f, _, _) -> List.fold_left max 0 [ helpI c; helpA t; helpA f ]
    | CPrim1 (_, i, _) -> helpI i
    | CPrim2 (_, i1, i2, _) -> max (helpI i1) (helpI i2)
    | CStringLiteral _ -> 0
    | CApp (_, args, _, _) -> List.fold_left max 0 (List.map helpI args)
    | CTuple (vals, _) -> List.fold_left max 0 (List.map helpI vals)
    | CGetItem (t, _, _) -> helpI t
    | CSetItem (t, _, v, _) -> max (helpI t) (helpI v)
    | CLambda (_, body, _) -> deepest_stack body (find outer_env name) outer_env
    | CImmExpr i -> helpI i
  and helpI i =
    match i with
    | ImmNil _ -> 0
    | ImmNum _ -> 0
    | ImmBool _ -> 0
    | ImmId (name, _) -> name_to_offset name
  and name_to_offset name =
    match find env name with
    | RegOffset (bytes, RBP) ->
      bytes / (-1 * word_size) (* negative because stack direction *)
    | _ -> 0
  in
  max (helpA e) 0 (* if only parameters are used, helpA might return a negative value *)
;;

let count_vars e =
  let rec helpA e =
    match e with
    | ASeq (e1, e2, _) -> max (helpC e1) (helpA e2)
    | ALet (_, bind, body, _) -> 1 + max (helpC bind) (helpA body)
    | ALetRec (binds, body, _) ->
      List.length binds
      + List.fold_left max (helpA body) (List.map (fun (_, rhs) -> helpC rhs) binds)
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf (_, t, f, _, _) -> max (helpA t) (helpA f)
    | _ -> 0
  in
  helpA e
;;

let rec replicate x i = if i = 0 then [] else x :: replicate x (i - 1)

and reserve size tag =
  let ok = sprintf "$memcheck_%d" tag in
  [ IInstrComment
      ( IMov (Reg RAX, LabelContents "?HEAP_END")
      , sprintf "Reserving %d words" (size / word_size) )
  ; ISub (Reg RAX, Const (Int64.of_int size))
  ; ICmp (Reg RAX, Reg heap_reg)
  ; IJge (Label ok)
  ]
  @ native_call
      (Label "?try_gc")
      [ Sized (QWORD_PTR, Reg heap_reg)
      ; (* alloc_ptr in C *)
        Sized (QWORD_PTR, Const (Int64.of_int size))
      ; (* bytes_needed in C *)
        Sized (QWORD_PTR, Reg RBP)
      ; (* first_frame in C *)
        Sized (QWORD_PTR, Reg RSP) (* stack_top in C *)
      ]
  @ [ IInstrComment
        ( IMov (Reg heap_reg, Reg RAX)
        , "assume gc success if returning here, so RAX holds the new heap_reg value" )
    ; ILabel ok
    ]

(* IMPLEMENT THIS FROM YOUR PREVIOUS ASSIGNMENT *)
(* Additionally, you are provided an initial environment of values that you may want to
   assume should take up the first few stack slots.  See the compiliation of Programs
   below for one way to use this ability... *)
and compile_fun
    (fun_name : string)
    (args : string list)
    (closed_over_args : string list)
    (body : tag aexpr)
    (cur_env : arg name_envt)
    (outer_env : arg name_envt name_envt)
    (expr_metadata : compile_metadata)
    : instruction list
  =
  let fn_tag = [ ILabel fun_name ] in
  let self_tagged_arg = RegOffset (2 * word_size, RBP) in
  (* let fn_arg_env =
      List.mapi (fun i s -> (s, RegOffset ((i + 3) * word_size, RBP))) args
    in *)
  let untag_self_and_move_to_r11 =
    [ IMov (Reg R11, self_tagged_arg); ISub (Reg R11, Const closure_tag) ]
  in
  let move_closure_to_stack =
    List.mapi
      (fun i fv ->
        [ IMov (Reg RAX, RegOffset ((3 + i) * word_size, R11))
        ; IMov (find cur_env fv, Reg RAX)
        ])
      closed_over_args
    |> List.flatten
  in
  let stack_count = deepest_stack body cur_env outer_env in
  let stack_offset = if stack_count mod 2 = 0 then stack_count else stack_count + 1 in
  let fn_preamble = [ IPush (Reg RBP); IMov (Reg RBP, Reg RSP) ] in
  let fn_stack_offset = [ ISub (Reg RSP, Const (Int64.of_int (stack_offset * 8))) ] in
  let fn_postscript = [ IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet ] in
  let _, body_env =
    List.find
      (fun e ->
        let a, _ = e in
        a == fun_name)
      outer_env
  in
  let rec_env =
    if expr_metadata.is_recursive then [ fun_name, self_tagged_arg ] else []
  in
  let body_ins = compile_aexpr body (body_env @ rec_env) outer_env expr_metadata 0 in
  fn_tag
  @ fn_preamble
  @ fn_stack_offset
  @ untag_self_and_move_to_r11
  @ move_closure_to_stack
  @ body_ins
  @ fn_postscript

and compile_aexpr
    (e : tag aexpr)
    (env : arg name_envt)
    (outer_env : arg name_envt name_envt)
    (expr_metadata : compile_metadata)
    (num_args : int)
    : instruction list
  =
  match e with
  | ALet (b_name, bind_expr, body_expr, _) ->
    let new_metadata =
      match bind_expr with
      | CLambda _ -> { fun_name = b_name; is_recursive = false }
      | _ -> expr_metadata
    in
    let prelude = compile_cexpr bind_expr env outer_env new_metadata num_args in
    let body = compile_aexpr body_expr env outer_env expr_metadata num_args in
    prelude @ [ IMov (find env b_name, Reg RAX) ] @ body
  | ACExpr cexp -> compile_cexpr cexp env outer_env expr_metadata num_args
  | ASeq (cexp, aexp, _) ->
    compile_cexpr cexp env outer_env expr_metadata 0
    @ compile_aexpr aexp env outer_env expr_metadata 0
  | ALetRec (binds, body, _) ->
    let bind_instrs =
      List.map
        (fun (name, bind_body) ->
          let new_metadata = { fun_name = name; is_recursive = true } in
          (*TODO get rid of self since it shouldnt be free anyways*)
          compile_cexpr bind_body env outer_env new_metadata 0
          @ [ IMov (find env name, Reg RAX) ])
        binds
      |> List.flatten
    in
    let body_instrs = compile_aexpr body env outer_env expr_metadata 0 in
    bind_instrs @ body_instrs

and compile_cexpr
    (e : tag cexpr)
    (env : arg name_envt)
    (outer_env : arg name_envt name_envt)
    (expr_metadata : compile_metadata)
    (num_args : int)
    : instruction list
  =
  (*instrs for checking is tup with a cmp. does not include jmp*)
  let check_is_tup =
    [ IMov (Reg R11, Reg RAX)
    ; IAnd (Reg R11, HexConst 0x0000000000000007L)
    ; ICmp (Reg R11, HexConst 0x0000000000000001L)
    ]
  in
  match e with
  | CStringLiteral (s, tag) -> compile_string_literal s tag reserve
  | CIf (cond, thn, els, restrict, tag) ->
    let cond_reg = compile_imm cond env in
    let thn_ins = compile_aexpr thn env outer_env expr_metadata num_args in
    let els_ins = compile_aexpr els env outer_env expr_metadata num_args in
    let if_false_label = sprintf "if_false_%d" tag in
    let if_true_label = sprintf "if_true_%d" tag in
    let done_label = sprintf "done_%d" tag in
    let check_bool_instrs =
      match restrict with
      | OnlyBool ->
        [ IMov (Reg R11, Reg RAX)
        ; IAnd (Reg R11, Const bool_tag_mask)
        ; ICmp (Reg R11, Const bool_tag)
        ; IJne (Label "err_logic_nab")
        ]
      | Any -> []
    in
    [ IMov (Reg RAX, cond_reg)
    ; IMov (Reg R11, Reg RAX)
    ; IAnd (Reg R11, Const bool_tag_mask)
    ; ICmp (Reg R11, Const bool_tag)
    ; IJne (Label "err_logic_nab")
    ; IMov (Reg R11, const_false)
    ; ICmp (Reg RAX, Reg R11)
    ; IJe (Label if_false_label)
    ; ILabel if_true_label
    ]
    @ thn_ins
    @ check_bool_instrs
    @ [ IJmp (Label done_label); ILabel if_false_label ]
    @ els_ins
    @ check_bool_instrs
    @ [ ILabel done_label ]
  | CPrim1 (IsTuple, e, t) ->
    let not_tup_label = sprintf "not_tup_%d" t in
    [ IMov (Reg RAX, compile_imm e env) ]
    @ check_is_tup
    @ [ (*assume that is not tup*)
        IMov (Reg RAX, const_false)
      ; IJnz (Label not_tup_label)
      ; IMov (Reg RAX, const_true)
      ; ILabel not_tup_label
      ]
  | CPrim1 (Add1, e, _) ->
    [ IMov (Reg RAX, compile_imm e env)
    ; IMov (Reg R11, Const num_tag_mask)
    ; ITest (Reg RAX, Reg R11)
    ; IJnz (Label "err_arith_nan")
    ; (*Adding 2 since a cobra number is 2x a normal number*)
      IAdd (Reg RAX, Const 2L)
    ; IJo (Label "err_overflow")
    ]
  | CPrim1 (Sub1, e, _) ->
    [ IMov (Reg RAX, compile_imm e env)
    ; IMov (Reg R11, Const num_tag_mask)
    ; ITest (Reg RAX, Reg R11)
    ; IJnz (Label "err_arith_nan")
    ; (*Adding 2 since a cobra number is 2x a normal number*)
      ISub (Reg RAX, Const 2L)
    ; IJo (Label "err_overflow")
    ]
  | CPrim1 (IsBool, e, t) ->
    let not_bool_label = sprintf "not_bool_%d" t in
    [ IMov (Reg RAX, compile_imm e env)
    ; IMov (Reg R11, Reg RAX)
    ; IAnd (Reg R11, Const bool_tag_mask)
    ; ICmp (Reg R11, Const bool_tag)
    ; (*assume that is not bool*)
      IMov (Reg RAX, const_false)
    ; IJne (Label not_bool_label)
    ; IMov (Reg RAX, const_true)
    ; ILabel not_bool_label
    ]
  | CPrim1 (Not, e, _) ->
    [ IMov (Reg RAX, compile_imm e env)
    ; IMov (Reg R11, Reg RAX)
    ; IAnd (Reg R11, Const bool_tag_mask)
    ; ICmp (Reg R11, Const bool_tag)
    ; IJne (Label "err_logic_nab")
    ; IMov (Reg R11, bool_mask)
    ; IXor (Reg RAX, Reg R11)
    ]
  | CPrim1 (IsNum, e, t) ->
    let not_num_label = sprintf "not_num_%d" t in
    [ IMov (Reg RAX, compile_imm e env)
    ; IMov (Reg R11, Const num_tag_mask)
    ; ITest (Reg RAX, Reg R11)
    ; (*assume that is not num*)
      IMov (Reg RAX, const_false)
    ; IJnz (Label not_num_label)
    ; IMov (Reg RAX, const_true)
    ; ILabel not_num_label
    ]
  | CPrim1 (Print, e, _) -> [ IMov (Reg RDI, compile_imm e env); ICall (Label "print") ]
  | CPrim1 (PrintStack, _, _) ->
    raise (NotYetImplemented "Don't need for this assignment")
  | CPrim2 (op, left, right, t) ->
    let left_reg = compile_imm left env in
    let right_reg = compile_imm right env in
    (match op with
    | Plus ->
      [ IMov (Reg RAX, left_reg)
      ; IMov (Reg R11, Const num_tag_mask)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_arith_nan")
      ; IMov (Reg RAX, right_reg)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_arith_nan")
      ; IAdd (Reg RAX, left_reg)
      ; IJo (Label "err_overflow")
      ]
    | Minus ->
      [ IMov (Reg RAX, right_reg)
      ; IMov (Reg R11, Const num_tag_mask)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_arith_nan")
      ; IMov (Reg RAX, left_reg)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_arith_nan")
      ; ISub (Reg RAX, right_reg)
      ; IJo (Label "err_overflow")
      ]
    | Times ->
      [ IMov (Reg RAX, left_reg)
      ; IMov (Reg R11, Const num_tag_mask)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_arith_nan")
      ; IMov (Reg RAX, right_reg)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_arith_nan")
      ; IMul (Reg RAX, left_reg)
      ; ISar (Reg RAX, Const 1L)
      ; IJo (Label "err_overflow")
      ]
    | Greater ->
      let less_label = sprintf "less_%d" t in
      let done_label = sprintf "done_%d" t in
      [ IMov (Reg RAX, right_reg)
      ; IMov (Reg R11, Const num_tag_mask)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_comp_nan")
      ; IMov (Reg RAX, left_reg)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_comp_nan")
      ; IMov (Reg R11, right_reg)
      ; ICmp (Reg RAX, Reg R11)
      ; IJle (Label less_label)
      ; IMov (Reg RAX, const_true)
      ; IJmp (Label done_label)
      ; ILabel less_label
      ; IMov (Reg RAX, const_false)
      ; ILabel done_label
      ]
    | Less ->
      let less_label = sprintf "less_%d" t in
      let done_label = sprintf "done_%d" t in
      [ IMov (Reg RAX, right_reg)
      ; IMov (Reg R11, Const num_tag_mask)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_comp_nan")
      ; IMov (Reg RAX, left_reg)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_comp_nan")
      ; IMov (Reg R11, right_reg)
      ; ICmp (Reg RAX, Reg R11)
      ; IJl (Label less_label)
      ; IMov (Reg RAX, const_false)
      ; IJmp (Label done_label)
      ; ILabel less_label
      ; IMov (Reg RAX, const_true)
      ; ILabel done_label
      ]
    | GreaterEq ->
      let less_label = sprintf "less_%d" t in
      let done_label = sprintf "done_%d" t in
      [ IMov (Reg RAX, right_reg)
      ; IMov (Reg R11, Const num_tag_mask)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_comp_nan")
      ; IMov (Reg RAX, left_reg)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_comp_nan")
      ; IMov (Reg R11, right_reg)
      ; ICmp (Reg RAX, Reg R11)
      ; IJl (Label less_label)
      ; IMov (Reg RAX, const_true)
      ; IJmp (Label done_label)
      ; ILabel less_label
      ; IMov (Reg RAX, const_false)
      ; ILabel done_label
      ]
    | LessEq ->
      let less_label = sprintf "less_%d" t in
      let done_label = sprintf "done_%d" t in
      [ IMov (Reg RAX, right_reg)
      ; IMov (Reg R11, Const num_tag_mask)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_comp_nan")
      ; IMov (Reg RAX, left_reg)
      ; ITest (Reg RAX, Reg R11)
      ; IJnz (Label "err_comp_nan")
      ; IMov (Reg R11, right_reg)
      ; ICmp (Reg RAX, Reg R11)
      ; IJle (Label less_label)
      ; IMov (Reg RAX, const_false)
      ; IJmp (Label done_label)
      ; ILabel less_label
      ; IMov (Reg RAX, const_true)
      ; ILabel done_label
      ]
    | Eq ->
      let eq_label = sprintf "eq_%d" t in
      let done_label = sprintf "done_%d" t in
      [ IMov (Reg RAX, left_reg)
      ; IMov (Reg R11, right_reg)
      ; ICmp (Reg RAX, Reg R11)
      ; IJe (Label eq_label)
      ; IMov (Reg RAX, const_false)
      ; IJmp (Label done_label)
      ; ILabel eq_label
      ; IMov (Reg RAX, const_true)
      ; ILabel done_label
      ]
    | _ -> raise (InternalCompilerError "impossible state, should be no ands or ors"))
  | CApp (ImmId (f_name, _), args, Native, _) ->
    let args_len = List.length args in
    let arg_instrs =
      if List.length args <= 6
      then (
        let c_args = List.filteri (fun i _ -> i < args_len) first_six_args_registers in
        List.map2 (fun reg arg -> IMov (Reg reg, compile_imm arg env)) c_args args)
      else
        raise
          (InternalCompilerError "native func calls should be fewer than 6 args (for now)")
    in
    gen_caller_save_reg_instrs
    @ arg_instrs
    @ [ ICall (Label f_name) ]
    @ gen_caller_restore_reg_instrs
  | CApp (_, _, Native, _) ->
    raise (InternalCompilerError "native wrapper calls should only be done with immIDs")
  | CApp (_, _, Prim, _) ->
    raise (InternalCompilerError "prim calltype should not be used")
  | CApp (_, _, Unknown, _) ->
    raise (InternalCompilerError "unknown calltype should not be used")
  | CApp (closure_ptr, args, Snake, _) ->
    let args_len = List.length args in
    let app_arg_to_asm a =
      match a with
      | _ -> [ IMov (Reg R11, a); IPush (Reg R11) ]
    in
    let set_rax_to_closure_ptr_instr = [ IMov (Reg RAX, compile_imm closure_ptr env) ] in
    let check_is_closure_instrs =
      [ IMov (Reg R11, Reg RAX)
      ; IAnd (Reg R11, Const closure_tag_mask)
      ; ICmp (Reg R11, Const closure_tag)
      ; IJne (Label "err_call_not_closure")
        (* ISub (Reg RAX, Const closure_tag) *)
        (*need to get rid of tag so we can access closure tup contents*)
      ]
    in
    let check_arity_instrs =
      [ IMov (Reg R11, RegOffset (-5, RAX))
      ; (*Arity is stored in the 0th position in the closure tup. Moving that into R11*)
        ICmp (Reg R11, Const (Int64.of_int args_len))
      ; IJne (Label "err_call_arity")
      ]
    in
    let arg_instrs =
      List.rev_map (fun a_imm -> app_arg_to_asm (compile_imm a_imm env)) args
      |> List.flatten
    in
    let cleanup_instr =
      [ IAdd (Reg RSP, Const (Int64.of_int (word_size * (args_len + 1)))) ]
    in
    set_rax_to_closure_ptr_instr
    @ check_is_closure_instrs
    @ check_arity_instrs
    @ gen_caller_save_reg_instrs
    @ arg_instrs
    @ [ IPush (Reg RAX) ]
    @ [ ICall (RegOffset (3, RAX)) ]
    @ cleanup_instr
    @ gen_caller_restore_reg_instrs
  | CTuple (imm_lst, tup_tag) ->
    let num_items = List.length imm_lst in
    let reserve_instrs =
      let num_words_to_reserve =
        if num_items mod 2 = 0 then num_items + 2 else num_items + 1
      in
      reserve (num_words_to_reserve * word_size) tup_tag
    in
    let tup_preamble =
      [ IMov (Reg R11, Const (Int64.of_int num_items))
      ; IMov (LabelContents (r_to_asm R15), Reg R11)
      ]
    in
    (*TODO for when i come back: make sure to add padding word at end if num_items is even *)
    let items_instrs =
      List.mapi
        (fun idx imm ->
          [ IMov (Reg R11, compile_imm imm env)
          ; IMov (RegOffset ((idx + 1) * word_size, R15), Reg R11)
          ])
        imm_lst
      |> List.flatten
    in
    let padding_instrs =
      if num_items mod 2 = 0
      then
        [ IMov (Reg R11, padding_word)
        ; IMov (RegOffset ((num_items + 1) * word_size, R15), Reg R11)
        ]
      else []
    in
    let tup_ret_tag_instrs = [ IMov (Reg RAX, Reg R15); IAdd (Reg RAX, HexConst 0x1L) ] in
    let heap_offset_instrs =
      if num_items mod 2 = 0
      then [ IAdd (Reg R15, Const (Int64.of_int ((num_items + 2) * word_size))) ]
      else [ IAdd (Reg R15, Const (Int64.of_int ((num_items + 1) * word_size))) ]
    in
    reserve_instrs
    @ tup_preamble
    @ items_instrs
    @ padding_instrs
    @ tup_ret_tag_instrs
    @ heap_offset_instrs
  | CGetItem (to_access, accessor, _) ->
    [ IMov (Reg RAX, compile_imm to_access env) ]
    @ check_is_tup
    @ [ IJnz (Label "err_not_tup")
      ; IMov (Reg RAX, compile_imm to_access env)
      ; ICmp (Reg RAX, const_nil)
      ; IJz (Label "err_access_nil")
      ; ISub (Reg RAX, Const (Int64.of_int 1))
      ; (*remove tag bit*)
        (*Rax is holding the heap pointer to the start of the tuple and R11 holds the value of the accessor*)
        IMov (Reg R11, compile_imm accessor env)
      ; ITest (Reg R11, Const num_tag_mask)
      ; IJnz (Label "err_arith_nan")
      ; ISar (Reg R11, Const 1L)
      ; ICmp (Reg R11, LabelContents (r_to_asm RAX))
      ; IJge (Label "err_get_high")
      ; ICmp (Reg R11, HexConst 0x0L)
      ; IJl (Label "err_get_low")
      ; IMov (Reg RAX, RegOffsetReg (RAX, R11, word_size, word_size))
      ]
  | CSetItem (to_access, accessor, to_set, _) ->
    [ IMov (Reg RAX, compile_imm to_access env) ]
    @ check_is_tup
    @ [ IJnz (Label "err_not_tup")
      ; IMov (Reg RAX, compile_imm to_access env)
      ; IMov (Reg RDX, compile_imm to_set env)
      ; ICmp (Reg RAX, const_nil)
      ; IJz (Label "err_access_nil")
      ; ISub (Reg RAX, Const (Int64.of_int 1))
      ; (*remove tag bit*)
        (*Rax is holding the heap pointer to the start of the tuple and R11 holds the value of the accessor*)
        (*RDX is holding the value to set*)
        IMov (Reg R11, compile_imm accessor env)
      ; ITest (Reg R11, Const num_tag_mask)
      ; IJnz (Label "err_arith_nan")
      ; ISar (Reg R11, Const 1L)
      ; ICmp (Reg R11, LabelContents (r_to_asm RAX))
      ; IJge (Label "err_get_high")
      ; ICmp (Reg R11, HexConst 0x0L)
      ; IJl (Label "err_get_low")
      ; IMov (RegOffsetReg (RAX, R11, word_size, word_size), Reg RDX)
      ; IMov (Reg RAX, Reg RDX)
      ]
  | CLambda (args, body, lam_tag) ->
    let lambda_label = expr_metadata.fun_name in
    let lambda_end_label = sprintf "%s_end" expr_metadata.fun_name in
    let fv = free_vars (ACExpr e) in
    let free_vars_in_lambda =
      if expr_metadata.is_recursive
      then List.filter (fun v -> v != expr_metadata.fun_name) fv
      else fv
    in
    let compiled_lambda =
      compile_fun
        lambda_label
        args
        free_vars_in_lambda
        body
        (find outer_env lambda_label)
        outer_env
        expr_metadata
    in
    let alloc_func_tuple =
      [ IMov (Reg RAX, Reg R15)
      ; IOr (Reg RAX, HexConst closure_tag)
      ; IMov (Reg R11, Const (Int64.of_int (List.length args)))
      ; IMov (LabelContents (r_to_asm R15), Reg R11)
      ; IMov (Reg R11, Label lambda_label)
      ; IMov (RegOffset (1 * word_size, R15), Reg R11)
      ; IMov (Reg R11, Const (Int64.of_int (List.length free_vars_in_lambda)))
      ; IMov (RegOffset (2 * word_size, R15), Reg R11)
      ]
    in
    let alloc_closed_over_vars =
      List.mapi
        (fun idx var ->
          [ IMov (Reg R11, find env var)
          ; IMov (RegOffset ((3 + idx) * word_size, R15), Reg R11)
          ])
        free_vars_in_lambda
      |> List.flatten
    in
    let closure_len = List.length free_vars_in_lambda + 3 in
    let padded_closure_len =
      if closure_len mod 2 == 0 then closure_len else closure_len + 1
    in
    let padding_instrs =
      if closure_len mod 2 == 0
      then []
      else
        [ IMov (Reg R11, padding_word)
        ; IMov (RegOffset (closure_len * word_size, R15), Reg R11)
        ]
    in
    [ IJmp (Label lambda_end_label) ]
    @ compiled_lambda
    @ [ ILabel lambda_end_label ]
    @ reserve (padded_closure_len * word_size) lam_tag
    @ alloc_func_tuple
    @ alloc_closed_over_vars
    @ padding_instrs
    @ [ IAdd (Reg R15, Const (Int64.of_int (padded_closure_len * word_size))) ]
  | CImmExpr i -> [ IMov (Reg RAX, compile_imm i env) ]

and compile_imm e env =
  match e with
  | ImmNum (n, _) -> Const (Int64.shift_left n 1)
  | ImmBool (true, _) -> const_true
  | ImmBool (false, _) -> const_false
  | ImmId (x, _) -> find env x
  | ImmNil _ -> const_nil

and args_help args regs =
  match args, regs with
  | arg :: args, reg :: regs ->
    IMov (Sized (QWORD_PTR, Reg reg), arg) :: args_help args regs
  | args, [] -> List.rev_map (fun arg -> IPush arg) args
  | [], _ -> []

and native_call label args =
  (* We know that on entry to every function, RSP is 16-byte aligned.
     We know that every frame is a multiple of 16 bytes.
     The call instruction pushes one return pointer onto the stack.
     The first thing we do is push RBP onto the stack
     So, we add 8 bytes of padding IFF the number of spilled args is *ODD*.
  *)
  let num_stack_args = max (List.length args - 6) 0 in
  let padding_needed = num_stack_args mod 2 <> 0 in
  let setup =
    (if padding_needed
    then
      [ IInstrComment (IPush (Sized (QWORD_PTR, Const 0L)), "Padding to 16-byte alignment")
      ]
    else [])
    @ args_help args first_six_args_registers
  in
  let teardown =
    (if num_stack_args = 0
    then []
    else
      [ IInstrComment
          ( IAdd (Reg RSP, Const (Int64.of_int (word_size * num_stack_args)))
          , sprintf "Popping %d arguments" num_stack_args )
      ])
    @
    if padding_needed
    then
      [ IInstrComment
          (IAdd (Reg RSP, Const (Int64.of_int word_size)), "Unpadding one word")
      ]
    else []
  in
  setup @ [ ICall label ] @ teardown

(* UPDATE THIS TO HANDLE FIRST-CLASS FUNCTIONS AS NEEDED -- THIS CODE WILL NOT WORK AS WRITTEN *)
and call (closure : arg) args =
  let setup =
    List.rev_map
      (fun arg ->
        match arg with
        | Sized _ -> IPush arg
        | _ -> IPush (Sized (DWORD_PTR, arg)))
      args
  in
  let teardown =
    let len = List.length args in
    if len = 0
    then []
    else
      [ IInstrComment
          ( IAdd (Reg RSP, Const (Int64.of_int (word_size * len)))
          , sprintf "Popping %d arguments" len )
      ]
  in
  setup @ [ ICall closure ] @ teardown
;;

(* This function can be used to take the native functions and produce DFuns whose bodies
   simply contain an EApp (with a Native call_type) to that native function.  Then,
   your existing compilation can turn these DFuns into ELambdas, which can then be called
   as in the rest of Fer-De-Lance, but the Native EApps will do the work of actually
   native_calling the runtime-provided functions. *)
let add_native_lambdas (p : sourcespan program) =
  let wrap_native name arity =
    let argnames = List.init arity (fun i -> sprintf "%s_arg_%d" name i) in
    [ DFun
        ( name
        , List.map (fun name -> BName (name, false, dummy_srcspan)) argnames
        , EApp
            ( EId (name, dummy_srcspan)
            , List.map (fun name -> EId (name, dummy_srcspan)) argnames
            , Native
            , dummy_srcspan )
        , dummy_srcspan )
    ]
  in
  match p with
  | Program (declss, body, tag) ->
    Program
      ( List.fold_left
          (fun declss (name, (_, arity)) -> wrap_native name arity :: declss)
          declss
          native_fun_bindings
      , body
      , tag )
;;

let compile_prog ((anfed : tag aprogram), (env : arg name_envt name_envt)) : string =
  match anfed with
  | AProgram (body, _) ->
    let ocsh_env = find env "ocsh" in
    let stack_count = deepest_stack body ocsh_env env in
    let stack_offset = if stack_count mod 2 = 0 then stack_count else stack_count + 1 in
    let body_preamble =
      [ (*ILabel("our_code_starts_here:");*)
        IPush (Reg RBP)
      ; IMov (Reg RBP, Reg RSP)
      ; ISub (Reg RSP, Const (Int64.of_int (stack_offset * 8)))
      ]
    in
    let body_postscript = [ IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet ] in
    let builtins =
      [ ILabel "err_arith_nan"
      ; IMov (Reg RDI, Const err_ARITH_NOT_NUM)
      ; ICall (Label "error")
      ; ILabel "err_comp_nan"
      ; IMov (Reg RDI, Const err_COMP_NOT_NUM)
      ; ICall (Label "error")
      ; ILabel "err_if_nab"
      ; IMov (Reg RDI, Const err_IF_NOT_BOOL)
      ; ICall (Label "error")
      ; ILabel "err_logic_nab"
      ; IMov (Reg RDI, Const err_LOGIC_NOT_BOOL)
      ; ICall (Label "error")
      ; ILabel "err_overflow"
      ; IMov (Reg RDI, Const err_OVERFLOW)
      ; ICall (Label "error")
      ; ILabel "err_not_tup"
      ; IMov (Reg RDI, Const err_GET_NOT_TUPLE)
      ; ICall (Label "error")
      ; ILabel "err_get_high"
      ; IMov (Reg RDI, Const err_GET_HIGH_INDEX)
      ; ICall (Label "error")
      ; ILabel "err_get_low"
      ; IMov (Reg RDI, Const err_GET_LOW_INDEX)
      ; ICall (Label "error")
      ; ILabel "err_access_nil"
      ; IMov (Reg RDI, Const err_ACCESS_NIL)
      ; ICall (Label "error")
      ; ILabel "err_call_not_closure"
      ; IMov (Reg RDI, Const err_CALL_NOT_CLOSURE)
      ; ICall (Label "error")
      ; ILabel "err_call_arity"
      ; IMov (Reg RDI, Const err_CALL_ARITY_ERR)
      ; ICall (Label "error")
      ; ILabel "err_val_not_string"
      ; IMov (Reg RDI, Const err_VAL_NOT_STRING)
      ; ICall (Label "error")
      ]
    in
    let our_code_starts_here_label = [ ILabel "our_code_starts_here" ] in
    let prog_body =
      compile_aexpr body ocsh_env env { fun_name = "ocsh"; is_recursive = false } 0
    in
    let heap_start =
      [ ILineComment "heap start"
      ; IInstrComment
          ( IMov (Reg heap_reg, Reg (List.nth first_six_args_registers 0))
          , "Load heap_reg with our argument, the heap pointer" )
      ; IInstrComment
          (IAdd (Reg heap_reg, Const 15L), "Align it to the nearest multiple of 16")
      ; IInstrComment
          ( IAnd (Reg heap_reg, HexConst 0xFFFFFFFFFFFFFFF0L)
          , "by adding no more than 15 to it" )
      ]
    in
    let set_stack_bottom =
      [ IMov (Reg R12, Reg RDI) ]
      @ native_call (Label "?set_stack_bottom") [ Reg RBP ]
      @ [ IMov (Reg RDI, Reg R12) ]
    in
    let all_ins =
      our_code_starts_here_label
      @ body_preamble
      @ set_stack_bottom
      @ heap_start
      @ prog_body
      @ body_postscript
      @ builtins
      @ new_snake_string_of_size_fn native_call
    in
    let ext_setup =
      "section .text\n\
       extern error\n\
       extern print\n\
       extern input\n\
       extern equal\n\
       extern ?try_gc\n\
       extern ?HEAP_END\n\
       extern ?HEAP\n\
       extern snakeStringCmp\n\
       extern snakeStringConcat\n\
       extern snakeStringSubstring\n\
       extern snakeStringToUpper\n\
       extern snakeStringToLower\n\
       extern snakeStringTrim\n\
       extern snakeStringIdxOf\n\
       extern snakeStringContains\n\
       extern snakeStringReplace\n\
       extern ?set_stack_bottom\n\
       global our_code_starts_here\n\
       global create_empty_snake_str\n\
      \ "
    in
    let prog_str = to_asm all_ins in
    sprintf "%s%s\n" ext_setup prog_str
;;

let run_if should_run f = if should_run then f else no_op_phase

let pick_alloc_strategy (strat : alloc_strategy) =
  match strat with
  | Naive -> naive_stack_allocation
  | Register -> register_allocation
;;

let compile_to_string
    ?(no_builtins = false)
    (alloc_strat : alloc_strategy)
    (prog : sourcespan program pipeline)
    : string pipeline
  =
  prog
  |> add_err_phase well_formed is_well_formed
  |> run_if (not no_builtins) (add_phase add_natives add_native_lambdas)
  |> add_phase desugared desugar
  |> add_phase tagged tag
  |> add_phase renamed rename_and_tag
  |> add_phase anfed (fun p -> atag (anf p))
  |> add_phase locate_bindings (pick_alloc_strategy alloc_strat)
  |> add_phase result compile_prog
;;
