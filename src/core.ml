
module Html = Dom_html


let to_obj l = Js.Unsafe.obj @@ Array.of_list l
let jss s = Js.string s
let inj o = Js.Unsafe.inject o

module type COMPONENT = sig
    type arg
    type jsval
    val to_js : arg -> jsval
    val from_js : jsval -> arg
    val render : arg -> 'a (* REACT.component *)
  end

module type REACT = sig
    type component
    val element_of_tag : string -> (string * Js.Unsafe.any) list -> string -> 'a (* component *)
    val render : component -> Dom_html.element Js.t -> unit

    val component : (module COMPONENT with type arg = 'a) -> ('a -> component)
  end

module React:REACT = struct
  let react = (Js.Unsafe.variable "React")
  type component = Js.Unsafe.any

  let create_class renderer =
    let comp_opts = to_obj [("render",
                             inj @@ Js.wrap_meth_callback renderer)] in
    Js.Unsafe.meth_call react "createClass" [| comp_opts |]

  let element_of_component comp opts =
    Js.Unsafe.meth_call react "createElement" [| comp; to_obj opts |]

  let element_of_tag tag opts children =
    Js.Unsafe.meth_call react "createElement"
                        [| inj @@ jss tag;
                           inj @@ to_obj opts;
                           inj @@ jss children |]

  let component (type a) (module Comp:COMPONENT with type arg = a) =
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

module CommentBox = struct
  type arg = string
  type jsval = Js.js_string Js.t

  let from_js jsv = Js.to_string jsv

  let to_js ov = jss ov

  let render st = React.element_of_tag "div" [("className",
                                               inj @@ jss "commentBox")] st
end

let comment_box = React.component (module CommentBox)

let start _:(bool Js.t) =
  let div = Dom_html.getElementById "main-area" in
  let () = React.render (comment_box "This is a comment box") div in
  Js._false

let () =
  Html.window##onload <- Dom.handler start
