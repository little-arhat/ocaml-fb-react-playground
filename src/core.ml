
module Html = Dom_html


let to_obj l = Js.Unsafe.obj @@ Array.of_list l
let jss s = Js.string s
let inj o = Js.Unsafe.inject o


module ReactTypes = struct
  type component = Js.Unsafe.any
  type child = String of string | Component of component
  type children = child list
end

module type COMPONENT  = sig
    type arg
    type jsval
    val to_js : arg -> jsval
    val from_js : jsval -> arg
    val render : arg -> ReactTypes.component
  end

module type REACT = sig
    include module type of ReactTypes
    val element_of_tag : string -> (* tag *)
                         (string * Js.Unsafe.any) list ->
                         children ->
                         component
    val render : component -> Dom_html.element Js.t -> unit

    val defcomponent : (module COMPONENT with type arg = 'a) -> ('a -> component)
  end

module React:REACT = struct
  include ReactTypes

  let react = (Js.Unsafe.variable "React")

  let create_class renderer =
    let comp_opts = to_obj [("render",
                             inj @@ Js.wrap_meth_callback renderer)] in
    Js.Unsafe.meth_call react "createClass" [| comp_opts |]

  let element_of_component comp opts =
    Js.Unsafe.meth_call react "createElement" [| comp; to_obj opts |]

  let element_of_tag tag opts children =
    let children_ar = Array.of_list @@ List.map
                                         (fun child ->
                                          match child with
                                          | String(st) -> inj @@ jss st
                                          | Component(comp) -> inj comp)
                                         children in
    let opts = Array.append [| inj @@ jss tag; to_obj opts |] children_ar in
    Js.Unsafe.meth_call react "createElement" opts

  let defcomponent (type a) (module Comp:COMPONENT with type arg = a) =
    let render_callback this _ =
      let props = Js.Unsafe.get this "props" in
      let value = Js.Unsafe.get props "value" in
      let value' = Comp.from_js value in
      Comp.render value'
    in
    let comp = create_class render_callback in
    let r value =
      element_of_component comp [("value", inj @@ Comp.to_js value)]
    in r

  let render comp node =
    Js.Unsafe.meth_call react "render" [| inj comp; inj node |]
end

module StringComponent = struct
  type arg = string
  type jsval = Js.js_string Js.t
  let from_js jsv = Js.to_string jsv
  let to_js ov = jss ov
end

module CommentList = struct
  include StringComponent
  let render st = React.element_of_tag "div"
                                       [("className", inj @@ jss "commentList")]
                                       [React.String(st)]
end
let comment_list = React.defcomponent (module CommentList)

module CommentForm = struct
  include StringComponent
  let render st = React.element_of_tag "div"
                                       [("className", inj @@ jss "commentForm")]
                                       [React.String(st)]
end
let comment_form = React.defcomponent (module CommentForm)

module CommentBox = struct
  include StringComponent
  let render st =
    let header = React.element_of_tag "h1" [] [React.String("Comment: ")] in
    React.element_of_tag "div"
                         [("className", inj @@ jss "commentBox")]
                         [React.Component(header);
                          React.Component((comment_list "This is comment list"));
                          React.Component((comment_form "This is comment form"))]
end

let comment_box = React.defcomponent (module CommentBox)

let start _:(bool Js.t) =
  let div = Dom_html.getElementById "main-area" in
  let () = React.render (comment_box "This is a comment box") div in
  Js._false

let () =
  Html.window##onload <- Dom.handler start
