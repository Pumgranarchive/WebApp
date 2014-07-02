{shared{

(**
   {b rdf_store -
   This Module do request to the rdf store}
*)

exception Invalid_uri of string
exception Invalid_link_id of string

type uri = Ptype.uri

type link_id = Ptype.link_id

(** link_id * target_uri * title * summary  *)
type linked_content = link_id * uri * string * string

type content = Nosql_store.id * string * string

(** uri * subject  *)
type tag = uri * string
type tag_type = TagLink | TagContent

(** {6 Utils}  *)

(** Create a URI from a string.
    @raise Invalid_uri in case of the string does not represent an URI. *)
val uri_of_string : string -> uri

(** Create a string from a URI  *)
val string_of_uri : uri -> string

(** If the given string URI is a pumgrana one, return true *)
val is_pumgrana_uri: uri -> bool

(** Create a link_id from a string
    @raise Invalid_link_id *)
val link_id_of_string : string -> link_id

(** Create a string from a link_id  *)
val string_of_link_id : link_id -> string

val target_uri_from_link_id : link_id -> uri
val origin_uri_from_link_id : link_id -> uri

val uri_of_content_id : Nosql_store.id -> uri

(** Create an id from an uri
    @raise Invalid_uri in case of a full invalid or a not Pumgrana uri.
    may @raise Nosql_store.Invalid_id *)
val content_id_of_uri : uri -> Nosql_store.id

(** Encode all slash of the given string url  *)
val slash_encode : string -> string

(** Decode all slash of the given string url  *)
val slash_decode : string -> string

}}

(** {6 Contents}  *)

(** [get_triple_contents tags_uri]
    if [tags_uri] is an empty list, return all contents' triple. *)
val get_triple_contents : uri list -> content list Lwt.t

(** [insert_content content_id title summary tags_uri]
    [tags_uri] can be an empty list
    @raise Invalid_argument if the content is already registred. *)
val insert_content : Nosql_store.id -> string -> string -> uri list -> unit Lwt.t

(** [delete_contents contents_id *)
val delete_contents : Nosql_store.id list -> unit Lwt.t

(** [update_content content_id ?title ?summary ?tags_uri ()]
    @param at least one have to be gave, otherwised @raise Invalid_argument,
    @raise Not_found if the content is not registred. *)
val update_content : Nosql_store.id -> ?title: string -> ?summary: string ->
  ?tags_uri: uri list -> unit -> unit Lwt.t

(** [update_content_tags content_uri tags_uri] *)
val update_content_tags : uri -> uri list -> unit Lwt.t


(** {6 Links}  *)

(** [get_link_detail link_id] *)
val get_link_detail : link_id -> (link_id * uri * uri * tag list) Lwt.t

(** [get_links_from_content content_uri]  *)
val links_from_content : uri -> linked_content list Lwt.t

(** [get_links_from_content_tags content_id tags_uri]  *)
val links_from_content_tags : uri -> uri list -> linked_content list Lwt.t

(** [insert_links (origin_uri, targets_uri, tag_uri list) list]
    @raise Invalid_argument if at least one tags list is empty. *)
val insert_links : (uri * uri * uri list) list -> link_id list Lwt.t

(** [update_link (link_id, new_tag_uri list) list]
    @raise Invalid_argument if at least one tags list is empty. *)
val update_links : (link_id * uri list) list -> unit Lwt.t

(** [delete_links links_id] *)
val delete_links : link_id list -> unit Lwt.t


(** {6 Tags} *)

(** [get_tags tag_type tags_uri]
    if [tags_uri] list is null, all tags of the given tag_type are returned *)
val get_tags : tag_type -> uri list -> tag list Lwt.t

(** [get_tags_from_link link_id] *)
val get_tags_from_link : link_id -> tag list Lwt.t

(** [get_tags_from_content content_uri] *)
val get_tags_from_content: uri -> tag list Lwt.t

(** [get_tags_from_content_link content_uri]  *)
val get_tags_from_content_link : uri -> tag list Lwt.t

(** [insert_tags tag_type ?link_id ?content_uri subjects]
    @param [link_id] and [content_uri] have to correspond with the tag_type,
    @param [link_id] or [content_uri] can be setted to associate all new tags on,
    return a list of existing + created tags_uri *)
val insert_tags : tag_type -> ?link_id:link_id -> ?content_uri:uri ->
  string list -> uri list Lwt.t

(** [delete_tags tags_uri]  *)
val delete_tags : uri list -> unit Lwt.t
