(************************************************************
   Copyright (C) 2023 Cornell University.
   Created by Justin Hsu (jah659@cornell.edu), Dexter Kozen (dck10@cornell.edu)
   and the CS 3110 course staff.
   You may not redistribute this assignment, distribute any derivatives,
   or use it for commercial purposes.
 ************************************************************)

(** CS 3110 Spring 2023 Final Project

    @author Krupa Sekhar, Isuru Herath, Mith Patel *)

(* TODO: complete the academic integrity statement below, then delete this TODO
   comment. *)

(************************************************************

  Academic Integrity Statement

  I, the person named in the author comment above, have fully reviewed the
  course academic integrity policies. I have adhered to those policies in
  solving the assignment.

  The policies do permit some limited collaboration among students currently
  enrolled in the course. If I did engage in such collaboration, here is the
  list of other students with whom I collaborated, and a brief summary of that
  collaboration:

  - Krupa Sekhar, Isuru Herath, Mith Patel

  ************************************************************)
open Graphics
open Yojson.Basic
open Yojson.Basic.Util
open Unix

type priority =
  | Red
  | Yellow
  | Green

type todo = {
  id : int;
  name : string;
  priority : priority;
}

type datatable = {
  max_id : int;
  todo_items : todo list;
  note_items : string list;
}

(** [priority_to_string priority] turns the priority into the corresponding
    string*)
let priority_to_string priority =
  match priority with
  | Red -> "Red"
  | Yellow -> "Yellow"
  | Green -> "Green"

(** [string_to_priority string] turns the string into the corresponding priority *)

let string_to_priority string =
  if string = "Red" then Red
  else if string = "Yellow" then Yellow
  else if string = "Green" then Green
  else failwith "invalid priority string"

(** [datatable_to_json n d ] creates a json file of name "n.json" containing the
    data in d*)
let datatable_to_json (name : string) (dtl : datatable) =
  let name_priority_list =
    List.map
      (fun x ->
        `Assoc
          [
            ("id", `Int x.id);
            ("todo", `String x.name);
            ("priority", `String (priority_to_string x.priority));
          ])
      dtl.todo_items
  in
  let note_list = List.map (fun x -> `String x) dtl.note_items in
  let data =
    `Assoc
      [
        ("max_id", `Int dtl.max_id);
        ("todo_items", `List name_priority_list);
        ("note_items", `List note_list);
      ]
  in
  to_file ("./data/" ^ name ^ ".json") data

(** [clear] makes an initial state json *)
let clear (name : string) =
  let u = { max_id = 0; todo_items = []; note_items = [] } in
  datatable_to_json name u

(** [todo_from_json json] turns the input json into a todo item*)
let todo_from_json json =
  let new_todo =
    {
      id = json |> member "id" |> to_int;
      name = json |> member "todo" |> to_string;
      priority = json |> member "priority" |> to_string |> string_to_priority;
    }
  in
  new_todo

(** [note_from_json json] turns the input json into a note item*)
let note_from_json json =
  let new_note = json |> to_string in
  new_note

(** [json_to_datatable n] reads a json file of name "n.json" and returns a
    corresponding datatable.*)
let json_to_datatable (name : string) : datatable =
  let json = Yojson.Basic.from_file ("./data/" ^ name ^ ".json") in
  let dt_max_id = json |> member "max_id" |> to_int in
  let new_todo_list =
    json |> member "todo_items" |> to_list |> List.map todo_from_json
  in
  let new_note_list =
    json |> member "note_items" |> to_list |> List.map note_from_json
  in
  { max_id = dt_max_id; todo_items = new_todo_list; note_items = new_note_list }

(*match json with | `Assoc [ ("todo_items", `List items) ] -> print_endline "in
  assoc"; let todos = List.map (fun item -> { name = item |>
  Yojson.Basic.to_string }) items in { todo_items = todos } | _ -> failwith
  "Invalid JSON format"*)

(** gui_maker launches a blank gui window*)
let gui_maker = open_graph ""

(** [todo_name_to_string h] is the name of the todo item*)
let todo_name_to_string (h : todo) =
  match h with
  | { name } -> name

let todo_id_to_string (h : todo) =
  match h with
  | { id } -> string_of_int id

let priority_to_color prior =
  match prior with
  | Red -> red
  | Green -> green
  | Yellow -> yellow

(* [gui_updater_todos] creates a gui window with the todo item names in todos*)
let rec gui_updater_todos (todos : todo list) (position_x : int)
    (position_y : int) =
  match todos with
  | ({ name; priority } as h) :: t ->
      moveto position_x position_y;
      set_color (priority_to_color priority);
      fill_circle position_x (position_y + 7) 3;
      set_color black;
      moveto (current_x () + 7) position_y;
      draw_string (todo_id_to_string h);
      moveto (current_x () + 7) position_y;
      draw_string (todo_name_to_string h);
      moveto (current_x () + 7) position_y;
      gui_updater_todos t position_x (position_y - 10)
  | [] -> ()

(* [gui_updater_notes] creates a gui window with the note item names in notes*)

let rec gui_updater_notes (notes : string list) (position_x : int)
    (position_y : int) =
  match notes with
  | h :: t ->
      moveto position_x position_y;
      fill_circle position_x (position_y + 7) 3;
      moveto (current_x () + 7) position_y;
      draw_string h;
      gui_updater_notes t position_x (position_y - 10)
  | [] -> ()

(** for testing: let rec gui_updater_todos (todos : string list) (position_x :
    int) (position_y : int) = match todos with | h :: t -> moveto position_x
    position_y; draw_string h; gui_updater_todos t position_x (position_y - 10)
    | [] -> ()*)

(** gui_updater dtl makes a gui window with the text items from dtl*)
let gui_updater (dtl : datatable) =
  gui_maker;
  resize_window 600 450;
  moveto 20 420;
  set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
  draw_string "Welcome to your To-Do List!";
  set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  moveto 20 395;
  draw_string "To Do:";
  set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1";

  match dtl with
  | { todo_items } -> (
      gui_updater_todos todo_items 20 377;
      match dtl with
      | { note_items } ->
          draw_rect 20 (current_y () - 10) 75 1;
          set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
          moveto 20 (current_y () - 40);
          draw_string "My Notes:";
          set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1";
          gui_updater_notes note_items 20 (current_y () - 25))

(** [green_todos todo_items td] inserts a green priority td into todo_items in
    the correct order *)

let rec green_todos (todo_items : todo list) (td : todo) =
  match todo_items with
  | { id; name; priority } :: t ->
      if priority = Red || priority = Yellow then
        { id; name; priority } :: green_todos t td
      else td :: todo_items
  | [] -> [ td ]

let rec green_helper (dtl : datatable) (td : todo) : datatable =
  match dtl with
  | { max_id; todo_items; note_items } ->
      let u = green_todos todo_items td in
      { max_id; todo_items = u; note_items }

(** [yellow_todos todo_items td] inserts a yellow priority td into todo_items in
    the correct order *)

let rec yellow_todos (todo_items : todo list) (td : todo) =
  match todo_items with
  | { id; name; priority } :: t ->
      if priority = Red then { id; name; priority } :: yellow_todos t td
      else td :: todo_items
  | [] -> [ td ]

let rec yellow_helper (dtl : datatable) (td : todo) : datatable =
  match dtl with
  | { max_id; todo_items; note_items } ->
      let u = yellow_todos todo_items td in
      { max_id; todo_items = u; note_items }

(** [priority_add dtl td] inserts a todo item into the datatable in the correct
    order based on its priority *)
let priority_add (dtl : datatable) (td : todo) : datatable =
  match td with
  | { name; priority } ->
      if priority = Red then
        {
          max_id = dtl.max_id;
          todo_items = td :: dtl.todo_items;
          note_items = dtl.note_items;
        }
      else if priority = Yellow then yellow_helper dtl td
      else green_helper dtl td

(** add_helper [c] is the datatable [dtl] with the todo [td] added to it; no
    priority. Requires: [td] is a todo item and [dtl] is a datatable*)
let add_helper (dtl : datatable) (td : todo) : datatable =
  {
    max_id = dtl.max_id;
    todo_items = td :: dtl.todo_items;
    note_items = dtl.note_items;
  }

(** [add name td] adds a todo [td] to the json file of name [name]. If the file
    does not exist, it is then created*)
let add (name : string) (td : todo) : datatable =
  let dtl =
    if Sys.file_exists ("./data/" ^ name ^ ".json") then json_to_datatable name
    else { max_id = 0; todo_items = []; note_items = [] }
  in
  let new_id = dtl.max_id + 1 in
  let new_todo = { td with id = new_id } in
  let new_dtl = priority_add dtl new_todo in
  let updated_dtl = { new_dtl with max_id = new_id } in
  datatable_to_json "todo" updated_dtl;
  gui_updater updated_dtl;
  updated_dtl

(** [add name td] adds a todo [td] to the json file of name [name]. If the file
    does not exist, it is then created*)
let add_note (name : string) (note : string) =
  let dtl =
    if Sys.file_exists ("./data/" ^ name ^ ".json") then json_to_datatable name
    else { max_id = 0; todo_items = []; note_items = [] }
  in
  let new_notes =
    match dtl with
    | { note_items } -> note_items @ [ note ]
  in
  let updated_dtl =
    match dtl with
    | { max_id; todo_items } -> { max_id; todo_items; note_items = new_notes }
  in
  datatable_to_json "todo" updated_dtl;
  gui_updater updated_dtl;
  updated_dtl

(* [delete_by_id_helper id todos] returns the list of todo items after removing
   the todo item with the given [id] from the list [todos]. *)
let rec delete_by_id_helper (id : int) (todos : todo list) =
  match todos with
  | h :: t -> if h.id = id then t else h :: delete_by_id_helper id t
  | [] -> []

(* [delete name id] removes the todo item with the given [id] from the datatable
   with the given [name]. *)
let delete (name : string) (id : int) (dt : datatable) : datatable =
  let dtl = json_to_datatable name in
  let new_todos = delete_by_id_helper id dtl.todo_items in
  (* Find the maximum id among the remaining todo items *)
  let new_max_id =
    match new_todos with
    | [] -> 0
    | _ -> List.fold_left (fun acc todo -> max acc todo.id) 0 new_todos
  in
  let (new_dtl : datatable) =
    { max_id = new_max_id; todo_items = new_todos; note_items = dt.note_items }
  in
  datatable_to_json name new_dtl;
  gui_updater new_dtl;
  new_dtl

(* [delete_helper td todos] returns the list of todo items after removing the
   todo item [td] from the list [todos]. *)
let rec delete_helper (td : todo) (todos : todo list) =
  match todos with
  | h :: t -> if h.id = td.id then t else h :: delete_helper td t
  | [] -> []

(* [find_todo_by_id id todos] returns the todo item with the given [id] in the
   todo list [todos]. *)
let find_todo_by_id (id : int) (todos : todo list) : todo option =
  List.find_opt (fun todo -> todo.id = id) todos

(* [change_priority name id new_priority] changes the priority of the todo with
   the given [id] in the datatable with the given [name] to the
   [new_priority]. *)
let change_priority (name : string) (id : int) (new_priority : priority) :
    datatable =
  let dtl = json_to_datatable name in
  match find_todo_by_id id dtl.todo_items with
  | Some todo ->
      let updated_todo = { todo with priority = new_priority } in
      let todos_without_updated = delete_helper updated_todo dtl.todo_items in
      let new_dtl =
        priority_add
          { dtl with todo_items = todos_without_updated }
          updated_todo
      in
      datatable_to_json name new_dtl;
      gui_updater new_dtl;
      new_dtl
  | None ->
      (* No todo with the given id found *)
      dtl

let hours_worked = 100
