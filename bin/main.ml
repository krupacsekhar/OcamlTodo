open Todolist
open Command
open Todo

(** [todo_modifier t] starts modifying the todo list with todo item [t] as a
    string. *)

let generate_id () =
  let current_time = Unix.gettimeofday () in
  Int64.to_int (Int64.of_float (current_time *. 1000.))

(* let tag_helper (op : object_phrase) = match List.rev op with | [] -> raise
   (Malformed "empty") | "\NONE" :: tl -> let tag = [ "" ] in let todo_item =
   List.rev tl in print_endline (String.concat " " todo_item); (tag, todo_item)
   | hd :: tl -> let tag = [ hd ] in let todo_item = List.rev tl in
   print_endline (String.concat " " todo_item); (tag, todo_item) *)

let rec todo_modifier (dtl : datatable) (t : string) =
  match parse t with
  | Quit ->
      print_endline "Farewell. \n";
      exit 1
  | Add op ->
      let new_dtl =
        match op with
        | [] -> raise (Malformed "add is empty")
        | h :: t ->
            let id = generate_id () in
            if String.lowercase_ascii h = "red" then
              add "todo" { id; name = String.concat " " t; priority = Red }
            else if String.lowercase_ascii h = "yellow" then
              add "todo" { id; name = String.concat " " t; priority = Yellow }
            else if String.lowercase_ascii h = "green" then
              add "todo" { id; name = String.concat " " t; priority = Green }
            else if String.lowercase_ascii h = "note" then
              add_note "todo" (String.concat " " t)
            else
              add "todo"
                { id; name = String.concat " " (h :: t); priority = Green }
      in
      print_string "> ";
      let new_todo = read_line () in
      todo_modifier new_dtl new_todo
  | Delete op ->
      let new_dtl =
        match op with
        | [] -> raise (Malformed "delete is empty")
        | id_str :: _ ->
            let id = int_of_string id_str in
            delete "todo" id dtl
      in
      print_string "> ";
      let new_todo = read_line () in
      todo_modifier new_dtl new_todo
  | Change op ->
      let new_dtl =
        match op with
        | [] -> raise (Malformed "change is empty")
        | id_str :: tl -> (
            let id = int_of_string id_str in
            match tl with
            | [] -> raise (Malformed "Did not provide new priority")
            | x :: _ ->
                if String.lowercase_ascii x = "green" then
                  change_priority "todo" id Green
                else if String.lowercase_ascii x = "yellow" then
                  change_priority "todo" id Yellow
                else if String.lowercase_ascii x = "red" then
                  change_priority "todo" id Red
                else
                  raise
                    (Malformed
                       "Did not provide an acceptable priority. Try \
                        [red/yellow/green]"))
      in
      print_string "> ";
      let new_todo = read_line () in
      todo_modifier new_dtl new_todo

(** [main ()] prompts for the first todo item. *)
let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to your To-Do List.\n";
  print_endline "Please enter a to-do item.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | todo -> (
      try todo_modifier { max_id = 0; todo_items = []; note_items = [] } todo
      with e ->
        print_endline
          "That is not an acceptable todo command. Please try again. Format \
           command as [add/delete] [optional: red/yellow/green] [task], or \
           [change] [id] [red/yellow/green] or [add note] [note] \n";
        main ())

(* Execute the game engine. *)
let () = main ()
