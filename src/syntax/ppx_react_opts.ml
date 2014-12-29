open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident


let const_string st =
  Exp.constant (Const_string (st, None))

let args exprs =
  List.map (fun exp -> ("", exp)) exprs

let ident ?(loc = !default_loc) ?left name =
  let n = match left with
    | Some(l) -> Ldot (Lident l, name)
    | None -> Lident name
  in
  Exp.ident { txt = n; loc }

let apply id exprs = Exp.apply id (args exprs)

let open_fresh ?(loc = !default_loc) name =
  Exp.open_ Fresh { txt = Lident name; loc }


let types = [
  "int"; "int32"; "int64"; "nativeint"; "float"; "str"; "bool";
  "list"; "array"; "opts"; "char"
]

let strip_attrs value_expr =
  {value_expr with pexp_attributes=[]}

let conv_func_from_value_type value_expr =
  let desc_attrs = (value_expr.pexp_desc, value_expr.pexp_attributes) in
  match desc_attrs with
  | (Pexp_constant const, _) ->
     (match const with
      | Const_string(_, _) -> "str"
      | Const_int(_) -> "int"
      | Const_float(_) -> "float"
      | Const_int32(_) -> "int32"
      | Const_int64(_) -> "int64"
      | Const_nativeint(_) -> "nativeint"
      | Const_char(_) -> "char"
     )
  | (Pexp_construct({txt = Lident("true"); loc}, None), _) -> "bool"
  | (Pexp_construct({txt = Lident("false"); loc}, None), _) -> "bool"
  | (Pexp_construct({txt = Lident("::"); loc}, _), _) -> "list"
  | (Pexp_array(_), _) -> "array"
  | (Pexp_fun(_, _, _, _), _) -> "clb"
  | (Pexp_function(_), _) -> "clb"
  | (Pexp_extension({txt = "opts"; _}, _), _) -> "opts"
  | (Pexp_ident({txt; loc}), attrs) ->
     (match attrs with
      | [] -> "str"
      | [({ txt; _ }, _)] -> (
        if List.mem txt types
        then txt
        else
          Location.raise_errorf ~loc "Unknown type:%s in type conversion attribute" txt
      )
      | _ -> Location.raise_errorf ~loc "Option values can only have one type attribute"
     )
  | _ -> "str"

let rec opts_expr mapper expr = match expr.pexp_desc with
  | Pexp_extension({txt = "opts"; loc }, PStr(items)) ->
     (match items with
      | [] -> ident ~loc ~left:"Options" "empty"
      | [{ pstr_desc=Pstr_eval(options, _)}] ->
         let rev_exprs = option_expr mapper options in
         let application = List.fold_left
                             (fun apl exp ->
                              apply (ident ~loc "<|") [apl; exp])
                             (ident ~loc "empty")
                             rev_exprs in
         open_fresh "Options" @@ application
      | _ -> Location.raise_errorf ~loc "[%%opts] should be empty or contain sequence k/v pairs: k1=v1; k2=v2"
     )
  | _ -> default_mapper.expr mapper expr
and option_expr mapper expr =
  let item_expr mapper expr = match expr.pexp_desc with
    | Pexp_apply({pexp_desc = Pexp_ident {txt = Lident "="}},
                 [("", {pexp_desc = Pexp_ident({txt = Lident key})});
                  ("", value_expr)
                 ]
                ) ->
       let conv_func = conv_func_from_value_type value_expr in
       apply (ident conv_func)
                       [const_string key;
                        opts_expr mapper @@ strip_attrs value_expr]
    | _ -> Location.raise_errorf "[%%opts items should be key-value pairs: k=v;]"
  in
  let rec wrk mapper expr acc = match expr.pexp_desc with
    | Pexp_apply(_, _) ->
       (item_expr mapper expr)::acc
    | Pexp_sequence(exp1, exp2) ->
       (item_expr mapper exp1)::(wrk mapper exp2 acc)
    | _ -> Location.raise_errorf "[%%opts] should be empty or contain sequence k/v pairs: k1=v1; k2=v2"
  in wrk mapper expr []

let opts_mapper argv =
  {default_mapper with expr = opts_expr }

let () = register "opts" opts_mapper
