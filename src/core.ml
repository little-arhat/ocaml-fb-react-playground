
module Html = Dom_html

(* var CommentBox = React.createClass({displayName: 'CommentBox', *)
(*   render: function() { *)
(*     return ( *)
(*       React.createElement('div', {className: "commentBox"}, *)
(*         "Hello, world! I am a CommentBox." *)
(*       ) *)
(*     ); *)
(*   } *)
(* }); *)
(* React.render( *)
(*   React.createElement(CommentBox, null), *)
(*   document.getElementById('content') *)
(* ); *)

let to_obj l = Js.Unsafe.obj @@ Array.of_list l
let jss s = Js.string s
let inj o = Js.Unsafe.inject o



module type REACT = sig
    type component
    val component : ('a -> component) -> ('a -> 'b) -> ('c -> 'a) -> ('a -> component)
    val element_of_tag : string -> (string * Js.Unsafe.any) list -> string -> component
    val render : component -> Dom_html.element Js.t -> unit
  end

module React:REACT = struct
  let react = (Js.Unsafe.variable "React")
  type component = Js.Unsafe.any

  let component renderer to_js from_js =
    let rfunc this _ =
      let props = Js.Unsafe.get this "props" in
      let value = Js.Unsafe.get props "value" in
      let value' = from_js value in
      (* let statics = Js.Unsafe.get props "statics" in *)
      renderer value'
    in
    let opts = to_obj [("render", inj @@ Js.wrap_meth_callback rfunc)] in
    let comp = Js.Unsafe.meth_call react "createClass" [| opts |] in
    let r value =
      let value' = to_js value in
      let opts = to_obj [("value", inj value')] in
      Js.Unsafe.meth_call react "createElement" [| comp; opts |]
    in r

  let element_of_tag tag opts children =
    Js.Unsafe.meth_call react "createElement"
                        [| inj @@ jss tag;
                           inj @@ to_obj opts;
                           inj @@ jss children |]

  let render comp node =
    Js.Unsafe.meth_call react "render" [| inj comp; inj node |]
end

let box = React.component
            (fun v ->
             React.element_of_tag "div" [("className",
                                          inj @@ jss "commentBox")]
                                  v)
            (fun ov -> jss ov)
            (fun jsv -> Js.to_string jsv)

let start _:(bool Js.t) =
  let div = Dom_html.getElementById "main-area" in
  let () = React.render (box "I am comment box") div in
  Js._false

let () =
  Html.window##onload <- Dom.handler start
