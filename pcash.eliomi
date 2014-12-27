(**
  {b Pcash -
   This module manage persistant cash and refreshing}
*)

(** The Pcash type *)
type 'a t

(** Type of listenner used for refreshing  *)
type 'a listenner = (Rdf_store.uri -> 'a Lwt.t)

(** [make name listenner]
    [name] is used to make data persistant
    [listenner] will be used for refreshing data *)
val make : string -> 'a listenner -> 'a t Lwt.t

(** [add cash key data]
    Add the given [data] with the given [key] in the cash *)
val add : 'a t -> Rdf_store.uri -> 'a -> unit Lwt.t

(** [exists cash key]
    Check if the given [key] exists in the [cash] *)
val exists : 'a t -> Rdf_store.uri -> bool Lwt.t

(** [not_exists cash key]
    Check if the given [key] is not existing in the [cash] *)
val not_exists : 'a t -> Rdf_store.uri -> bool Lwt.t

(** [get cash key]
    Return the data assossiated with the given [key]
    @raise Not_found if no such binding exists *)
val get : 'a t -> Rdf_store.uri -> 'a Lwt.t
