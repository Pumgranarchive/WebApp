(**
   {b SideBar HTML Module}
*)

(** The two possible mode to build AddContent *)
type mode =
[ `Contents of (Html5_types.div Eliom_content.Html5.D.elt Lwt.t * string option * string)
| `Detail of Rdf_store.uri
| `Link ]

(** [make type add_content]
    Generate the appropriate SideBar in function of the given type *)
val make :
  mode ->
  AddContent.mode AddContent.t ->
  Html5_types.div Eliom_content.Html5.F.elt Lwt.t
