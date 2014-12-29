
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


let endswith s char =
  String.get s (String.length s - 1) = char

let func_from_tag ?(loc = !default_loc) t =
  let tag_name = if endswith t '_'
                 then String.sub t 0 (String.length t - 1)
                 else t
  in
  let func_name = Pat.var {txt = t; loc} in
  let func_body = apply (ident ~loc ~left:"React" "tag")
                        [const_string tag_name] in
  let bindings = Vb.mk func_name func_body in
  Str.value Nonrecursive [bindings]


let generate_tags_mapper argv =
  let tags_expr mapper expr =
    let tag_expr mapper expr = match expr.pexp_desc with
      | Pexp_ident({txt = Lident t}) ->
         func_from_tag t
      | _ -> Location.raise_errorf "[%%generate_tags items should be tag names: a; br]"
    in
    let rec wrk mapper expr acc = match expr.pexp_desc with
      | Pexp_ident(_) ->
         (tag_expr mapper expr)::acc
      | Pexp_sequence(exp1, exp2) ->
         (tag_expr mapper exp1)::(wrk mapper exp2 acc)
      | _ -> Location.raise_errorf "[%%generate_tags] should contain sequence of tag names: a;br"
    in List.rev @@ wrk mapper expr []
  in
  let generate_tags_expr mapper expr = match expr.pexp_desc with
    | Pexp_extension({txt = "generate_tags"; loc }, PStr(tags)) ->
       (match tags with
        | [{ pstr_desc=Pstr_eval(tags, _)}] ->
           tags_expr mapper tags
        | _ -> Location.raise_errorf ~loc "[%%opts] should contain sequence of tags to generate"
       )
    | _ -> let () = print_string "dwdwf\n"in
           Location.raise_errorf "[%%opts] should contain sequence of tags to generate"
  in
  let structure mapper items =
    match items with
    | { pstr_desc =
          Pstr_eval({pexp_desc =
                       Pexp_extension({txt = "generate_tags"; loc },
                                      _)} as expr,
                    _);
        pstr_loc } as item :: rest ->
       let tags_defs = generate_tags_expr mapper expr in
       default_mapper.structure mapper (List.append tags_defs rest)
    | _ -> default_mapper.structure mapper items
  in
  {default_mapper with structure = structure }


let () = register "opts" generate_tags_mapper
