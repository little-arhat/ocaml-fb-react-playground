
module Html = Dom_html


let to_obj l = Js.Unsafe.obj @@ Array.of_list l
let jss s = Js.string s
let inj o = Js.Unsafe.inject o

module type OPTIONS = sig
    type t
    type el

    val empty : t
    val (<|) : t -> el -> t
    val to_js : t -> 'a

    val el_of_int : string -> int -> el
    val el_of_float : string -> float -> el
    val el_of_string : string -> string -> el
    val el_of_bool : string -> bool -> el
    val el_of_list : string -> el list -> el
    val el_of_array : string -> el array -> el
    val el_of_options : string -> t -> el

  end

module Options:OPTIONS = struct
  type el = (string * Js.Unsafe.any)
  type t = el list

  let empty = []
  let (<|) o p = p :: o
  let to_js o = Js.Unsafe.obj @@ Array.of_list o

  let el_of_int k v = (k, inj @@ Js.number_of_float @@ float_of_int v)
  let el_of_float k v = (k, inj @@ Js.number_of_float v)
  let el_of_string k v = (k, inj @@ Js.string v)
  let el_of_bool k v = (k, inj @@ Js.bool v)
  let el_of_list k v = (k, inj @@ Js.array @@ Array.of_list v)
  let el_of_array k v = (k, inj @@ Js.array v)
  let el_of_array k v = (k, inj @@ Js.array v)
  let el_of_options k v = (k, inj @@ to_obj v)
end
module O = Options

module ReactTypes = struct
  (* Want to hide impl details *)
  type component = Js.Unsafe.any
end

module type REACT_TYPES = sig
    type component = ReactTypes.component
end

module type COMPONENT  = sig
    type arg
    type jsval
    val to_js : arg -> jsval
    val from_js : jsval -> arg
    val render : arg -> ReactTypes.component
  end

module type REACT = sig
    include REACT_TYPES
    type child
    val tag : string -> (* tag *)
              Options.t ->
              child list ->
              component
    val text : string -> child
    val component : component -> child
    val render : component -> Dom_html.element Js.t -> unit

    val defcomponent : (module COMPONENT with type arg = 'a) -> ('a -> component)
  end

module React:REACT = struct
  include ReactTypes
  type child = TextContent of string | Component of component

  let react = (Js.Unsafe.variable "React")

  let create_class renderer =
    let comp_opts = to_obj [("render",
                             inj @@ Js.wrap_meth_callback renderer)] in
    Js.Unsafe.meth_call react "createClass" [| comp_opts |]

  let make_component comp opts =
    Js.Unsafe.meth_call react "createElement" [| comp; to_obj opts |]

  (* public: *)
  let text st = TextContent(st)
  let component c = Component(c)

  let tag tag opts children =
    let children_ar = Array.of_list @@ List.map
                                         (fun child ->
                                          match child with
                                          | TextContent(st) -> inj @@ jss st
                                          | Component(comp) -> inj comp)
                                         children in
    let opts = Array.append [| inj @@ jss tag; Options.to_js opts |] children_ar in
    Js.Unsafe.meth_call react "createElement" opts

  let defcomponent (type a) (module Comp:COMPONENT with type arg = a) =
    let render_callback this _ =
      let props = Js.Unsafe.get this "props" in
      let value = Js.Unsafe.get props "value" in
      let value' = Comp.from_js value in
      Comp.render value'
    in
    let comp_class = create_class render_callback in
    let r value =
      make_component comp_class [("value", inj @@ Comp.to_js value)]
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
  let render st =
    React.tag "div" O.(empty <| el_of_string "className" "commentList")
              [React.text st]
end
let comment_list = React.defcomponent (module CommentList)

module CommentForm = struct
  include StringComponent
  let render st =
    React.tag "div" O.(empty <| el_of_string "className" "commentForm")
              [React.text st]
end
let comment_form = React.defcomponent (module CommentForm)

module CommentBox = struct
  include StringComponent
  let render st =
    let header = React.tag "h1" O.empty [React.text "Comment: "] in
    React.tag "div" O.(empty <| el_of_string "className" "commentBox")
              [React.component header;
               React.component @@ comment_list "This is comment list";
               React.component @@ comment_form "This is comment form"]
end

let comment_box = React.defcomponent (module CommentBox)

let start _:(bool Js.t) =
  let div = Dom_html.getElementById "main-area" in
  let () = React.render (comment_box "This is a comment box") div in
  Js._false

let () =
  Html.window##onload <- Dom.handler start
