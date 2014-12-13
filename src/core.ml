
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

let jss s = Js.string s
let inj o = Js.Unsafe.inject o

let react = (Js.Unsafe.variable "React")
let react_create_element = Js.Unsafe.meth_call react "createElement"
let react_create_class = Js.Unsafe.meth_call react "createClass"
let react_render = Js.Unsafe.meth_call react "render"

let div_opts = Js.Unsafe.obj [|
                   ("className", inj @@ jss "commentBox");
                  |]

let clb () = react_create_element [|
                    inj @@ jss "div";
                    inj @@ div_opts;
                    inj @@ jss "I am commentbox"
               |]

let box_opts = Js.Unsafe.obj [|
        ("render", inj @@ Js.wrap_callback clb
        )|]

let box_class =
  react_create_class [| box_opts |]

let box_elem = react_create_element [| inj box_class; inj Js.null|]

let start _:(bool Js.t) =
  let div = Dom_html.getElementById "main-area" in
  let () = react_render [| box_elem; inj div |] in
  Js._false

let () =
  Html.window##onload <- Dom.handler start
