(** Representation of a todo list. This module represents the structure of the
    todo list as a list of todo items, the maximum id of any todo item, and a
    list of notes. In aggregate all of this data is stored in a data table. It
    handles exporting a datatable into a JSON and importing a JSON into datable.
    Additionally, it handles adding todo items and notes, changing the priority
    of a todo item, and deleting todo items. *)

(**************************)

(** [priority] represents priority levels*)
type priority =
  | Red
  | Yellow
  | Green

type todo = {
  id : int;
  name : string;
  priority : priority;
}
(**[todo] represents a todo item*)

type datatable = {
  max_id : int;
  todo_items : todo list;
  note_items : string list;
}
(**[datatable] represents a datatable of todos and notes*)

val priority_to_string : priority -> string
(** [priority_to_string priority] turns the priority into the corresponding
    string*)

val clear : string -> unit
(** [clear] makes an initial state json *)

val datatable_to_json : string -> datatable -> unit
(** [datatable_to_json n d ] creates a json file of name "n.json" containing the
    data in d*)

val json_to_datatable : string -> datatable
(** [json_to_datatable n] reads a json file of name "n.json" and returns a
    corresponding datatable.*)

val delete : string -> int -> datatable -> datatable
(** [delete name id] removes the todo item with the given [id] from the
    datatable with the given [name]. *)

val add : string -> todo -> datatable
(** [add name td] adds a todo [td] to the json file of name /data/[name].json.
    If the file does not exist, it is then created*)

val add_note : string -> string -> datatable
(** [add_note name n] adds a note [n] to the json file of name
    /data/[name].json. If the file does not exist, it is then created*)

val change_priority : string -> int -> priority -> datatable
(** [change_priority name id new_priority] changes the priority of the todo with
    the given [id] in the datatable with the given /data/[name].json to the
    [new_priority]. *)

val hours_worked : int
(** [hours_worked] is the approximate number of hours worked*)
