(** Parsing of user commands. *)

type object_phrase = string list
(** The type [object_phrase] represents the object phrase that is either the
    todo item (with or without priority) or note in the case of an Add command.
    It can be the id in the case of a Delete Command and it could be the id and
    priority in the case of a Change command. Type [object_phrase] is a string
    list with each element representing a "word" in the command, which is a
    consecutive sequence of non-space characters. Thus, no element of the list
    should contain any leading, internal, or trailing spaces. The list is in the
    same order as the words in the original player command. For example:

    - If the user command is ["add green task1"], then the object phrase is
      [\["green"; "task1"\]].

    - If the player command is ["add green     task1"], then the object phrase
      is also [\["green"; "task1"\]]. *)

(** The type [command] represents a user command that is categorized by the type
    of command along with its accompanying object phrase. Invariant: the
    [object_phrase] carried by [Add], [Delete], or [Change] must not be empty. *)
type command =
  | Add of object_phrase
  | Delete of object_phrase
  | Change of object_phrase
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed of string
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses the user's input into a [command]. The first word of
    [str] represents the verb (add/delete/change). The rest of the words, if
    any, become the object phrase. Examples:

    - [parse "    add   green   task1   "] is [Add \["green"; "task1"\]]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the verb is neither "quit", "add","delete", nor "change" or if the verb is
    "quit" and the accompanying object phrase is not empty, or if the verb is
    one of the other verbs and there is an empty object phrase, or if the
    "change" verb is not followed by a string representing an id and then a
    priority*)