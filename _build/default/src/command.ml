open Str

type object_phrase = string list

type command =
  | Add of object_phrase
  | Delete of object_phrase
  | Change of object_phrase
  | Quit

exception Empty
exception Malformed of string

let check_priority strls =
  match strls with
  | [] -> raise (Malformed "check_priority is empty")
  | h :: t ->
      if
        String.lowercase_ascii h = "red"
        || String.lowercase_ascii h = "green"
        || String.lowercase_ascii h = "yellow"
      then strls
      else raise (Malformed "priority is not one of the options")

let parse str =
  let l = str |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") in
  match l with
  | [] -> raise Empty
  | h :: t ->
      if String.lowercase_ascii h = "quit" then
        if List.length t = 0 then Quit
        else raise (Malformed "quit is not empty")
      else if String.lowercase_ascii h = "add" then (
        if List.length t = 0 then raise (Malformed "add is empty");
        Add t)
      else if String.lowercase_ascii h = "delete" then
        if List.length t = 0 then raise (Malformed "delete is empty")
        else Delete t
      else if String.lowercase_ascii h = "change" then
        if List.length t <> 2 then
          raise
            (Malformed
               "Change doesn't have the right number of arguments. Try change \
                [id] [red/yellow/green]")
        else
          match t with
          | [] ->
              raise
                (Malformed
                   "change doesn't have the right number of arguments. Try \
                    change [id] [red/yellow/green]")
          | th :: tt ->
              if Str.string_match (Str.regexp "[0-9]+$") th 0 then
                if
                  String.lowercase_ascii (List.hd tt) = "green"
                  || String.lowercase_ascii (List.hd tt) = "yellow"
                  || String.lowercase_ascii (List.hd tt) = "red"
                then Change t
                else
                  raise
                    (Malformed
                       "Second argument must be a priority. Try change [id] \
                        [red/yellow/green]")
              else
                raise
                  (Malformed
                     "First argument must be an id. Try change\n\
                     \        [id] [red/yellow/green] ")
      else raise (Malformed "first word not right")
