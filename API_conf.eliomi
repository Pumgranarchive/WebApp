{server{

(**
   {b API Configuration file}
*)


(** {6 Basic data} *)

(** The database url *)
val db_url: string

(** The name of the database *)
val db_name: string

(** The using port to connect to the database *)
val db_port: int

(** The name of the contents collection *)
val contents_coll_name: string

(** The name of the tags collection *)
val tags_coll_name: string

(** The name of the links collection *)
val links_coll_name: string


(** {6 API exception} *)

(** Pum_exc (error_value, error_message)
    The exception which could be raised to manage error in the API *)
exception Pum_exc of int * string


(** {6 Return values} *)

(** Succesful return value *)
val return_ok: int

(** Status return if data was created *)
val return_created: int

(** Status return if data there is no data to return *)
val return_no_content: int

(** Status return if one of the parameters corresponds to no founded data *)
val return_not_found: int

(** Status return if an unexecpted internal server error occure *)
val return_internal_error: int


(** {6 Error string functions / values} *)

(** Build not found error message with specify which data is about *)
val errstr_not_found: string -> string

(** Build not obejct id error message with specify which string is about *)
val errstr_not_objectid: string -> string

(** Build not expected error message with specify which data is about *)
val errstr_not_expected: string -> string

(** Build not existing error message with specify which data is about *)
val errstr_exist: string -> string

(** Generic internal error message *)
val errstr_internal_error: string


(** {6  Tag types} *)

(** The value of parameter to select tags on link *)
val link_tag: string

(** The value of parameter to select tags on content *)
val content_tag: string

(** The value to set in database tags on link *)
val link_tag_str: string

(** The value to set in database tags on content *)
val content_tag_str: string

}}

{client{

(**
   {b API Configuration file}
*)

(** {6  Tag types} *)

(** The value of parameter to select tags on link *)
val link_tag: string

(** The value of parameter to select tags on content *)
val content_tag: string

(** The value to set in database tags on link *)
val link_tag_str: string

(** The value to set in database tags on content *)
val content_tag_str: string

}}
