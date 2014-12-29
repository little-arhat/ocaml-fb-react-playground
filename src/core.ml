
module Html = Dom_html


let to_obj l = Js.Unsafe.obj @@ Array.of_list l
let jss s = Js.string s
let inj o = Js.Unsafe.inject o

(* Future plans: *)
(*  - Use experimental ppx ext for js_of_ocaml for typed access *)
(*  - Implement react tutorial *)
(*  - Implement snake game *)
(*  - Define ppx-deriving plugin for marshalling records to js objects *)
(*  - Remove Options and use objects instead *)
(*  - Generate object from [%opts ] extension with props and method _to_js *)

module type OPTIONS = sig
    type t
    type el

    val empty : t
    val (<|) : t -> el -> t
    val to_js : t -> 'a

    val int : string -> int -> el
    val int32 : string -> int32 -> el
    val int64 : string -> int64 -> el
    val nativeint : string -> nativeint -> el
    val float : string -> float -> el
    val str : string -> string -> el
    val bool : string -> bool -> el
    val char : string -> char -> el
    val list : string -> el list -> el
    val array : string -> el array -> el
    val opts : string -> t -> el
    val clb : string -> (('b #Dom.event as 'a) Js.t -> bool Js.t) -> el
    val func : string -> ('a Js.t -> 'b Js.t) -> el

  end

module Options:OPTIONS = struct
  type el = (string * Js.Unsafe.any)
  type t = el list

  let empty = []
  let (<|) o p = p :: o
  let to_js o = Js.Unsafe.obj @@ Array.of_list o

  let int k v = (k, inj @@ Js.number_of_float @@ float_of_int v)
  let int32 k v = (k, inj @@ Js.number_of_float @@ Int32.to_float v)
  let int64 k v = (k, inj @@ Js.number_of_float @@ Int64.to_float v)
  let nativeint k v = (k, inj @@ Js.number_of_float @@ Nativeint.to_float v)
  let float k v = (k, inj @@ Js.number_of_float v)
  let str k v = (k, inj @@ Js.string v)
  let bool k v = (k, inj @@ Js.bool v)
  let char k v = (k, inj @@ Js.string @@ Char.escaped v)
  let list k v = (k, inj @@ Js.array @@ Array.of_list v)
  let array k v = (k, inj @@ Js.array v)
  let opts k v = (k, inj @@ to_obj v)
  let clb k v = (k, inj @@ Dom.handler v)
  let func k v = (k, inj v)
end

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
              child
    val text : string -> child
    val component : component -> child
    val dom : child -> component
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
    let js_opts = Options.to_js opts in
    let args = Array.append [| inj @@ jss tag; js_opts |] children_ar in
    Component(Js.Unsafe.meth_call react "createElement" args)

  let rec dom ch = match ch with
    | TextContent(st) -> dom @@ tag "span" [%opts] [ch]
    | Component(c) -> c

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

module React_DOM = struct
  [%generate_tags
   a; abbr; address; area; article; aside; audio; b; base;
   bdi; bdo; big; blockquote; body; br; button; canvas;
   caption; cite; code; col; colgroup; data; datalist; dd;
   del; details; dfn; div; dl; dt; em; embed; fieldset;
   figcaption; figure; footer; form; h1; h2; h3; h4; h5; h6;
   head; header; hr; i; iframe; img; input; ins; kbd;
   keygen; label; legend; li; link; main; map; mark; menu;
   menuitem; meta; meter; nav; noscript; object_; ol; optgroup;
   option; output; p; param; pre; progress; q; rp; rt; ruby;
   s; samp; script; section; select; small; source; span;
   strong; style; sub; summary; sup; table; tbody; td;
   textarea; tfoot; th; thead; time; title; tr; track;
   u; ul; var; video; wbr; circle; g; line; path; polygon;
   polyline; rect; svg; text; end_]
  end

module StringComponent = struct
  type arg = string
  type jsval = Js.js_string Js.t
  let from_js jsv = Js.to_string jsv
  let to_js ov = jss ov
end

module CommentList = struct
  include StringComponent
  let render prop =
    React_DOM.(React.dom @@
      (div [%opts className="commentList"]
           [
             React.text prop
           ]))
end
let comment_list = React.defcomponent (module CommentList)

module CommentForm = struct
  include StringComponent
  let render prop =
    React_DOM.(React.dom @@
      (div [%opts className="commentForm"]
           [
             React.text prop
           ]))
end
let comment_form = React.defcomponent (module CommentForm)

let log s =
  Js.Unsafe.meth_call Firebug.console "log" [| Js.Unsafe.inject (Js.string s) |]

module CommentBox = struct
  include StringComponent
  let render prop =
    React_DOM.(React.dom @@
      (div [%opts className="commentBox"]
           [
             (h1 [%opts id="mmmm";
                        onClick=(fun _ -> let () = log "lol" in Js._false)]
                 [React.text "Comment:"]);
             React.component @@ comment_list "This is comment list";
             React.component @@ comment_form "This is comment form"
           ]))
end
let comment_box = React.defcomponent (module CommentBox)


let start t =
  let div = Dom_html.getElementById "main-area" in
  let () = React.render (comment_box "This is a comment box") div in
  Js._false


(* let () = *)
(*   Html.window##onload <- Dom.handler start *)

(* XXX: Use hand-written expansion of above expr in the absence of camlp4 *)
let () =
  let _ =
    let module M =
      struct
        let res =
          let _ = (Html.window : 'B Js.t) in
          let _ =
            fun (x : 'B) -> (x#onload : < set : 'A -> unit; .. > Js.gen_prop)
          in (Dom.handler start : 'A)

      end
    in M.res
  in
  Js.Unsafe.set Html.window "onload" (Dom.handler start)
