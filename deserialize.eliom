{shared{

(*
** GUI deserliaze
** Deserializer for API to GUI
 *)

include  Pdeserialize

open Yojson.Util
open Pjson

type id =
| Id of Nosql_store.id
| Uri of Ptype.uri

type content =
| Internal of (id * string * string * string)
| External of (id * string * string * string)

let uri_of_id = function
  | Id id   -> Ptype.string_of_uri (Rdf_store.uri_of_content_id id)
  | Uri uri -> Ptype.string_of_uri uri

let string_of_id = function
  | Id id   -> Nosql_store.string_of_id id
  | Uri uri -> Ptype.string_of_uri uri

let id_of_uri uri =
  try
    if Rdf_store.is_pumgrana_uri uri
    then Id (Rdf_store.content_id_of_uri uri)
    else Uri uri
  with Nosql_store.Invalid_id s -> Uri uri

(** {6 Pumgrana format deserializer}  *)

(** deserialize content from yojson to ocaml format *)
let get_content json_content =
  let (uri, title, summary, body, v_external) = get_content json_content in
  if v_external
  then External (Uri uri, title, summary, body)
  else Internal (id_of_uri uri, title, summary, body)

(** deserialize short content from yojson to ocaml format *)
let get_short_content json_content =
  let (uri, title, summary) = get_short_content json_content in
  id_of_uri uri, title, summary

let get_link json_link =
  let (link_id, uri, title, summary) = get_link json_link in
  link_id, id_of_uri uri, title, summary

(** deserialize content list from yojson to ocaml *)
let get_content_list tl =
  List.map get_content (to_list tl)

(** deserialize short content list from yojson to ocaml *)
let get_short_content_list tl =
  List.map get_short_content (to_list tl)

(** deserialize link list from yojson to ocaml *)
let get_link_list tl =
  List.map get_link (to_list tl)

}}
