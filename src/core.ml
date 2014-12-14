
module Html = Dom_html


let to_obj l = Js.Unsafe.obj @@ Array.of_list l
let jss s = Js.string s
let inj o = Js.Unsafe.inject o

module type REACT = sig
    type component
    val create_class : ('a -> 'b -> 'c ) -> component
    val element_of_component : component -> (string * Js.Unsafe.any) list -> component
    val element_of_tag : string -> (string * Js.Unsafe.any) list -> string -> component
    val render : component -> Dom_html.element Js.t -> unit
  end


module React:REACT = struct
  let react = (Js.Unsafe.variable "React")
  type component = Js.Unsafe.any

  let create_class renderer =
    let comp_opts = to_obj [("render",
                             inj @@ Js.wrap_meth_callback renderer)] in
    Js.Unsafe.meth_call react "createClass" [| comp_opts |]

  let element_of_component comp opts =
    Js.Unsafe.meth_call react "createElement" [| comp; inj @@ to_obj opts |]

  let element_of_tag tag opts children =
    Js.Unsafe.meth_call react "createElement"
                        [| inj @@ jss tag;
                           inj @@ to_obj opts;
                           inj @@ jss children |]

  let render comp node =
    Js.Unsafe.meth_call react "render" [| inj comp; inj node |]
end

module type COMPONENT = sig
    type arg
    type jsval
    val to_js : arg -> jsval
    val from_js : jsval -> arg
    val render : arg -> React.component
  end

module MakeComponent (Comp:COMPONENT) : sig
  val init : Comp.arg -> React.component
end
  = struct
  let react = (Js.Unsafe.variable "React")

  let render_callback this _ =
    let props = Js.Unsafe.get this "props" in
    let value = Js.Unsafe.get props "value" in
    let value' = Comp.from_js value in
    Comp.render value'

  let init value =
    let comp = React.create_class render_callback in
    let value' = Comp.to_js value in
    React.element_of_component comp [("value", inj value')]

end


module CommentBoxSpec = struct
  type arg = string
  type jsval = Js.js_string Js.t

  let from_js jsv = Js.to_string jsv

  let to_js ov = jss ov

  let render st = React.element_of_tag "div" [("className",
                                               inj @@ jss "commentBox")] st
end

module CommentBox = MakeComponent(CommentBoxSpec)

let start _:(bool Js.t) =
  let div = Dom_html.getElementById "main-area" in
  let () = React.render (CommentBox.init "This is a comment box") div in
  Js._false

let () =
  Html.window##onload <- Dom.handler start
