open OUnit2
open Todolist
open Todo

(*Test Plan*)
(* We chose to OUnit test the functions that most affected our todo list
   functionality. This included add, delete, datatable_to_json,
   json_to_datatable, and add_note. For each of these functions, we checked that
   priority was being added, modified, and ordered properly, and that the max id
   in each datatable was being modified correctly. We also tested the clear
   function as part of the add test (the json was cleared before every add test
   was run).

   We manually tested all interface/gui functionality (the command module in
   conjunction with the todo gui functions), namely that the text rendered
   properly, priority colors were displayed appropriately, and titles were the
   correct font/size on the screen. We also tested that the priority was
   changing properly (change_priority, which is a part of the todo module).

   We used a combination of black box and glass box testing, and we tested the
   todo module using this methodology. First, we tested that each function
   followed the specification provided in the mli. After we had exhausted many
   paths through the spec, we then read through our code in order to see if any
   paths through the code were untested, and went about creating tests for those
   paths.

   Our testing approach demonstrates correctness because it utilizes a
   combination of glass and black box testing and attempts to exhaust all paths
   through the specification and code. Additionally, we tested the interface
   over 50 times in order to ensure that the interface and gui worked together
   properly for a varying amount of todo items and note items. *)

let rec td_list_eq td ans =
  match td with
  | { id; name; priority } :: t -> (
      let i = id in
      let n = name in
      let p = priority in
      match ans with
      | { id; name; priority } :: s ->
          if
            i = id && n = name
            && priority_to_string p = priority_to_string priority
          then td_list_eq t s
          else failwith "td lists not equal"
      | [] -> failwith "not equal")
  | [] -> if ans = [] then true else failwith "todolist empty too soon"

let rec dt_equality dt ans =
  match dt with
  | { max_id; todo_items } -> (
      let u = max_id in
      let td = todo_items in
      match ans with
      | { max_id; todo_items } ->
          if u = max_id then td_list_eq td todo_items
          else failwith "max id not equal")

let rec string_todos (tdl : todo list) : string =
  match tdl with
  | [] -> ""
  | h :: t -> h.name ^ " " ^ string_todos t

let dt_json_todoitems_test (name : string) (json_name : string)
    (data_table : datatable) (expected_output : todo list) (clearyn : bool) :
    test =
  name >:: fun _ ->
  if clearyn then clear json_name else ();
  datatable_to_json json_name data_table;
  let d = json_to_datatable json_name in
  assert_equal (td_list_eq expected_output d.todo_items) true

let dt_json_maxid_test (name : string) (json_name : string)
    (data_table : datatable) (expected_output : int) (clearyn : bool) : test =
  name >:: fun _ ->
  if clearyn then clear json_name else ();
  datatable_to_json json_name data_table;
  let d = json_to_datatable json_name in
  assert_equal expected_output d.max_id

let dt1 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task2"; priority = Red };
      ];
    note_items = [];
  }

let dt2 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 2; name = "task2"; priority = Red };
        { id = 1; name = "task1"; priority = Red };
      ];
    note_items = [];
  }

let dt3 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 1; name = "task1"; priority = Red };
      ];
    note_items = [];
  }

let dt4 =
  {
    max_id = 53;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 53; name = "task1"; priority = Red };
      ];
    note_items = [];
  }

let dt5 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task5"; priority = Green };
      ];
    note_items = [];
  }

let dt6 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
      ];
    note_items = [];
  }

let dt7 =
  {
    max_id = 3;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
        { id = 2; name = "task5"; priority = Red };
      ];
    note_items = [];
  }

let dt8 =
  {
    max_id = 3;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
        { id = 3; name = "task5"; priority = Green };
      ];
    note_items = [];
  }

let dt9 =
  {
    max_id = 5;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
        { id = 2; name = "task5"; priority = Green };
        { id = 3; name = "task5"; priority = Yellow };
      ];
    note_items = [];
  }

let dt_to_json_todoitems_tests =
  [
    dt_json_todoitems_test "2 item todo list" "jsontest1" dt1
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task2"; priority = Red };
      ]
      true;
    dt_json_todoitems_test "reverse 2 item todo list" "jsontest2" dt2
      [
        { id = 2; name = "task2"; priority = Red };
        { id = 1; name = "task1"; priority = Red };
      ]
      true;
    dt_json_todoitems_test "repeat 2 item todo list" "jsontest3" dt3
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 1; name = "task1"; priority = Red };
      ]
      true;
    dt_json_todoitems_test "non traditional max id" "jsontest4" dt4
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 53; name = "task1"; priority = Red };
      ]
      true;
    dt_json_todoitems_test "2 diff priorities" "jsontest5" dt5
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task5"; priority = Green };
      ]
      true;
    dt_json_todoitems_test "priorities in non traditional order" "jsontest6" dt6
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
      ]
      true;
    dt_json_todoitems_test "3 items, 2 priorities" "jsontest7" dt7
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
        { id = 2; name = "task5"; priority = Red };
      ]
      true;
    dt_json_todoitems_test "3 items, 2 priorities in nontraditional order"
      "jsontest8" dt8
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
        { id = 3; name = "task5"; priority = Green };
      ]
      true;
    dt_json_todoitems_test "Testing yellow" "jsontest9" dt9
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
        { id = 2; name = "task5"; priority = Green };
        { id = 3; name = "task5"; priority = Yellow };
      ]
      true;
  ]

let dt_to_json_maxid_tests =
  [
    dt_json_maxid_test "id different from todo_items length" "mjsontest1" dt4 53
      true;
    dt_json_maxid_test "id matches todo_items length" "mjsontest2" dt5 2 true;
    dt_json_maxid_test "id for repeated items" "mjsontest3" dt7 3 true;
  ]

(*********************************** JS_TO_DT
  **************************************)
let json_to_dt_todoitems_test (name : string) (json_name : string)
    (data_table : datatable) (expected_output : todo list) (clearyn : bool) :
    test =
  name >:: fun _ ->
  if clearyn then clear json_name else ();
  datatable_to_json json_name data_table;
  let d = json_to_datatable json_name in
  assert_equal (td_list_eq expected_output d.todo_items) true

let json_to_dt_maxid_test (name : string) (json_name : string)
    (data_table : datatable) (expected_output : int) (clearyn : bool) : test =
  name >:: fun _ ->
  if clearyn then clear json_name else ();
  datatable_to_json json_name data_table;
  let d = json_to_datatable json_name in
  assert_equal expected_output d.max_id

let dt1 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task2"; priority = Red };
      ];
    note_items = [];
  }

let dt2 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 2; name = "task2"; priority = Red };
        { id = 1; name = "task1"; priority = Red };
      ];
    note_items = [];
  }

let dt3 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 1; name = "task1"; priority = Red };
      ];
    note_items = [];
  }

let dt4 =
  {
    max_id = 53;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 53; name = "task1"; priority = Red };
      ];
    note_items = [];
  }

let dt5 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task5"; priority = Green };
      ];
    note_items = [];
  }

let dt6 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
      ];
    note_items = [];
  }

let dt7 =
  {
    max_id = 3;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
        { id = 2; name = "task5"; priority = Red };
      ];
    note_items = [];
  }

let dt8 =
  {
    max_id = 3;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
        { id = 3; name = "task5"; priority = Green };
      ];
    note_items = [];
  }

let dt9 =
  {
    max_id = 5;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task5"; priority = Red };
        { id = 2; name = "task5"; priority = Green };
        { id = 3; name = "task5"; priority = Yellow };
      ];
    note_items = [];
  }

let dt10 = { max_id = 0; todo_items = []; note_items = [] }

let dt11 =
  {
    max_id = 1;
    todo_items = [ { id = 1; name = "task1"; priority = Yellow } ];
    note_items = [];
  }

let dt12 =
  {
    max_id = 3;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Yellow };
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Yellow };
      ];
    note_items = [];
  }

let dt13 =
  {
    max_id = 3;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Yellow };
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Red };
      ];
    note_items = [];
  }

let dt14 =
  {
    max_id = 4;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Yellow };
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Red };
        { id = 4; name = "task4"; priority = Green };
      ];
    note_items = [];
  }

let dt15 =
  {
    max_id = 2;
    todo_items =
      [
        { id = 2; name = "task2"; priority = Green };
        { id = 1; name = "task1"; priority = Yellow };
      ];
    note_items = [];
  }

let dt16 =
  {
    max_id = 3;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 3; name = "task3"; priority = Red };
        { id = 2; name = "task2"; priority = Yellow };
      ];
    note_items = [];
  }

let dt17 =
  {
    max_id = 1;
    todo_items = [ { id = 1; name = "task1"; priority = Red } ];
    note_items = [];
  }

let dt18 =
  {
    max_id = 4;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task2"; priority = Red };
        { id = 3; name = "task3"; priority = Yellow };
        { id = 4; name = "task4"; priority = Red };
      ];
    note_items = [];
  }

let json_to_dt_todoitems_tests =
  [
    json_to_dt_todoitems_test "2 item todo list" "jsontest1" dt1
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task2"; priority = Red };
      ]
      true;
    json_to_dt_todoitems_test "reverse 2 item todo list" "jsontest2" dt2
      [
        { id = 2; name = "task2"; priority = Red };
        { id = 1; name = "task1"; priority = Red };
      ]
      true;
    json_to_dt_todoitems_test "empty todo list" "jsontest10" dt10 [] true;
    json_to_dt_todoitems_test "single item todo list" "jsontest11" dt11
      [ { id = 1; name = "task1"; priority = Yellow } ]
      true;
    json_to_dt_todoitems_test "3 item todo list all yellow" "jsontest12" dt12
      [
        { id = 1; name = "task1"; priority = Yellow };
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Yellow };
      ]
      true;
    json_to_dt_todoitems_test "3 item todo list with red" "jsontest13" dt13
      [
        { id = 1; name = "task1"; priority = Yellow };
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Red };
      ]
      true;
    json_to_dt_todoitems_test "4 item todo list with green" "jsontest14" dt14
      [
        { id = 1; name = "task1"; priority = Yellow };
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Red };
        { id = 4; name = "task4"; priority = Green };
      ]
      true;
    json_to_dt_todoitems_test "2 items with Green and Yellow priorities"
      "jsontest10" dt15
      [
        { id = 2; name = "task2"; priority = Green };
        { id = 1; name = "task1"; priority = Yellow };
      ]
      true;
    json_to_dt_todoitems_test "3 items with Green, Red, and Yellow priorities"
      "jsontest11" dt16
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 3; name = "task3"; priority = Red };
        { id = 2; name = "task2"; priority = Yellow };
      ]
      true;
    json_to_dt_todoitems_test "1 item with Red priority" "jsontest12" dt17
      [ { id = 1; name = "task1"; priority = Red } ]
      true;
    json_to_dt_todoitems_test "4 items with all priorities" "jsontest13" dt18
      [
        { id = 1; name = "task1"; priority = Green };
        { id = 2; name = "task2"; priority = Red };
        { id = 3; name = "task3"; priority = Yellow };
        { id = 4; name = "task4"; priority = Red };
      ]
      true;
  ]

let json_to_dt_maxid_tests =
  [
    json_to_dt_maxid_test "id different from todo_items length" "mjsontest1" dt4
      53 true;
    json_to_dt_maxid_test "id matches todo_items length" "mjsontest2" dt5 2 true;
    json_to_dt_maxid_test "id for repeated items" "mjsontest3" dt7 3 true;
    json_to_dt_maxid_test "no items" "mjsontest10" dt10 0 true;
    json_to_dt_maxid_test "single item" "mjsontest11" dt11 1 true;
    json_to_dt_maxid_test "3 items all yellow" "mjsontest12" dt12 3 true;
    json_to_dt_maxid_test "3 items with red" "mjsontest13" dt13 3 true;
    json_to_dt_maxid_test "4 items with green" "mjsontest14" dt14 4 true;
    json_to_dt_maxid_test "2 items with Green and Yellow priorities"
      "mjsontest15" dt15 2 true;
    json_to_dt_maxid_test "3 items with Green, Red, and Yellow priorities"
      "mjsontest16" dt16 3 true;
    json_to_dt_maxid_test "1 item with Red priority" "mjsontest17" dt17 1 true;
    json_to_dt_maxid_test "4 items with all priorities" "mjsontest18" dt18 4
      true;
  ]

(**********************)

let rec add_list (name : string) (td : todo list) =
  match td with
  | h :: t ->
      if t <> [] then (
        let u = add name h in
        ignore u;
        add_list name t)
      else
        let p = add name h in
        p
  | [] -> failwith "reached empty"

let add_test (name : string) (td : todo list) (answer_dt : datatable)
    (file_name : string) (clearyn : bool) : test =
  if clearyn then clear "todo" else ();
  let data_table = add_list file_name td in
  name >:: fun _ ->
  assert_equal (dt_equality data_table answer_dt) true;
  if clearyn then clear "todo"

let dt_1_add =
  {
    max_id = 1;
    todo_items = [ { id = 1; name = "task1"; priority = Red } ];
    note_items = [];
  }

let dt_2_add =
  {
    max_id = 1;
    todo_items = [ { id = 1; name = "task2"; priority = Red } ];
    note_items = [];
  }

let dt_3_add =
  {
    max_id = 2;
    todo_items =
      [
        { id = 2; name = "task2"; priority = Red };
        { id = 1; name = "task1"; priority = Red };
      ];
    note_items = [];
  }

let dt_4_add =
  {
    max_id = 2;
    todo_items =
      [
        { id = 2; name = "task2"; priority = Red };
        { id = 1; name = "task3"; priority = Green };
      ];
    note_items = [];
  }

let dt_5_add =
  {
    max_id = 2;
    todo_items =
      [
        { id = 1; name = "task2"; priority = Red };
        { id = 2; name = "task3"; priority = Green };
      ];
    note_items = [];
  }

let dt_6_add =
  {
    max_id = 3;
    todo_items =
      [
        { id = 1; name = "task2"; priority = Red };
        { id = 2; name = "task4"; priority = Yellow };
        { id = 3; name = "task3"; priority = Green };
      ];
    note_items = [];
  }

let dt_7_add =
  {
    max_id = 3;
    todo_items =
      [
        { id = 1; name = "task2"; priority = Red };
        { id = 3; name = "task4"; priority = Yellow };
        { id = 2; name = "task3"; priority = Green };
      ];
    note_items = [];
  }

let todo_1_add = { id = 1; name = "task1"; priority = Red }
let todo_2_add = { id = 2; name = "task2"; priority = Red }
let todo_3_add = { id = 2; name = "task3"; priority = Green }
let todo_4_add = { id = 2; name = "task4"; priority = Yellow }
let todo_11_add = { id = 1; name = "todo1"; priority = Red }
let todo_22_add = { id = 2; name = "todo2"; priority = Yellow }
let todo_33_add = { id = 3; name = "todo3"; priority = Green }
let todo_44_add = { id = 4; name = "todo4"; priority = Yellow }

let dt_8_add =
  {
    max_id = 4;
    todo_items =
      [
        { id = 1; name = "todo1"; priority = Red };
        { id = 4; name = "todo4"; priority = Yellow };
        { id = 2; name = "todo2"; priority = Yellow };
        { id = 3; name = "todo3"; priority = Green };
      ];
    note_items = [];
  }

let add_tests =
  [
    add_test "add one item" [ todo_1_add ] dt_1_add "todo" true;
    add_test "max\n      id is 2" [ todo_2_add ] dt_2_add "todo" true;
    add_test "max id is 2" [ todo_1_add; todo_2_add ] dt_3_add "todo" true;
    add_test "order with\n      priority" [ todo_3_add; todo_2_add ] dt_4_add
      "todo" true;
    add_test "changed input order with priority" [ todo_2_add; todo_3_add ]
      dt_5_add "todo" true;
    add_test "three diff priorities"
      [ todo_2_add; todo_4_add; todo_3_add ]
      dt_6_add "todo" true;
    add_test "three priorities, switched order"
      [ todo_2_add; todo_3_add; todo_4_add ]
      dt_7_add "todo" true;
    add_test "duplicate priorities order"
      [ todo_11_add; todo_22_add; todo_33_add; todo_44_add ]
      dt_8_add "todo" true;
  ]

let rec add_note_list (name : string) (note : string list) =
  match note with
  | h :: t ->
      if t <> [] then (
        let u = add_note name h in
        ignore u;
        add_note_list name t)
      else
        let p = add_note name h in
        p
  | [] -> failwith "reached empty"

let dt_1_nadd = { max_id = 0; todo_items = []; note_items = [ "note1" ] }

let dt_2_nadd =
  { max_id = 0; todo_items = []; note_items = [ "note1"; "note2" ] }

let dt_3_nadd =
  {
    max_id = 0;
    todo_items = [];
    note_items = [ "note1"; "my third note"; "note2" ];
  }

let dt_4_nadd =
  {
    max_id = 0;
    todo_items = [];
    note_items = [ "note1"; "My Third Note"; "note2" ];
  }

let dt_5_nadd =
  {
    max_id = 0;
    todo_items = [];
    note_items = [ "note1"; "add My Third Note"; "note2" ];
  }

let dt_6_nadd =
  {
    max_id = 0;
    todo_items = [];
    note_items = [ "note1"; "add green My Third Note"; "note2" ];
  }

let dt_7_nadd =
  {
    max_id = 0;
    todo_items = [];
    note_items = [ "note1"; "note add green My Third Note"; "note2" ];
  }

let dt_8_nadd = { max_id = 0; todo_items = []; note_items = [] }

let add_note_test (name : string) (notes : string list) (answer_dt : datatable)
    (file_name : string) (clearyn : bool) : test =
  if clearyn then clear "todo" else ();
  let data_table = add_note_list file_name notes in
  name >:: fun _ ->
  assert_equal (dt_equality data_table answer_dt) true;
  if clearyn then clear "todo"

let add_note_tests =
  [
    add_note_test "one note" [ "note1" ] dt_1_nadd "todo" true;
    add_note_test "two note" [ "note1"; "note2" ] dt_2_nadd "todo" true;
    add_note_test "note with spaces, diff order"
      [ "note1"; "my third note"; "note2" ]
      dt_3_nadd "todo" true;
    add_note_test "note with spaces, diff order, capital letters"
      [ "note1"; "My Third Note"; "note2" ]
      dt_4_nadd "todo" true;
    add_note_test "note with spaces, diff order, keyword add"
      [ "note1"; "add My Third Note"; "note2" ]
      dt_5_nadd "todo" true;
    add_note_test "note with spaces, diff order, keyword green"
      [ "note1"; "add green My Third Note"; "note2" ]
      dt_6_nadd "todo" true;
    add_note_test "note with spaces, diff order, keyword note"
      [ "note1"; "note add green My Third Note"; "note2" ]
      dt_7_nadd "todo" true;
    add_note_test "no notes" [ " " ] dt_8_nadd "todo" true;
  ]

(************************* DELETE *********************************)

let delete_test (name : string) (id : int) (input_dt : datatable)
    (answer_dt : datatable) (file_name : string) (clearyn : bool) : test =
  if clearyn then clear "todo" else ();
  let _ = delete file_name id input_dt in
  let data_table = json_to_datatable file_name in
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool (dt_equality data_table answer_dt) true;
  if clearyn then clear "todo"

let dt_empty = { max_id = 0; todo_items = []; note_items = [] }

let dt_1 =
  {
    max_id = 1;
    todo_items = [ { id = 1; name = "task1"; priority = Red } ];
    note_items = [];
  }

let dt_2 =
  {
    max_id = 2;
    todo_items = [ { id = 2; name = "task2"; priority = Yellow } ];
    note_items = [];
  }

let dt_3 =
  {
    max_id = 3;
    todo_items =
      [
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Green };
      ];
    note_items = [];
  }

let dt_4 =
  {
    max_id = 4;
    todo_items =
      [
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Green };
        { id = 4; name = "task4"; priority = Red };
      ];
    note_items = [];
  }

let dt_6 =
  {
    max_id = 6;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Green };
        { id = 4; name = "task4"; priority = Red };
        { id = 5; name = "task5"; priority = Green };
        { id = 6; name = "task6"; priority = Yellow };
      ];
    note_items = [];
  }

let dt_7 =
  {
    max_id = 7;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Green };
        { id = 4; name = "task4"; priority = Red };
        { id = 5; name = "task5"; priority = Green };
        { id = 6; name = "task6"; priority = Yellow };
        { id = 7; name = "task7"; priority = Green };
      ];
    note_items = [];
  }

let dt_8 =
  {
    max_id = 8;
    todo_items =
      [
        { id = 1; name = "task1"; priority = Red };
        { id = 2; name = "task2"; priority = Yellow };
        { id = 3; name = "task3"; priority = Green };
        { id = 4; name = "task4"; priority = Red };
        { id = 5; name = "task5"; priority = Green };
        { id = 6; name = "task6"; priority = Yellow };
        { id = 7; name = "task7"; priority = Green };
        { id = 8; name = "task8"; priority = Red };
      ];
    note_items = [];
  }

let delete_tests =
  [
    delete_test "delete from empty list" 1 dt_empty dt_empty "todo" true;
    delete_test "delete only item" 1 dt_1 dt_empty "todo" true;
    delete_test "delete first item" 2 dt_2 dt_empty "todo" true;
    delete_test "delete first item from three items" 3 dt_3 dt_empty "todo" true;
    delete_test "delete first item from four items" 4 dt_4 dt_empty "todo" true;
    delete_test "delete first item from six items" 6 dt_6 dt_empty "todo" true;
    delete_test "delete first item from seven items" 7 dt_7 dt_empty "todo" true;
    delete_test "delete first item from eight items" 8 dt_8 dt_empty "todo" true;
  ]

(******************************************************************************************************)

let tests =
  "test suite for final project"
  >::: List.flatten
         [
           dt_to_json_todoitems_tests;
           dt_to_json_maxid_tests;
           add_tests;
           json_to_dt_maxid_tests;
           json_to_dt_todoitems_tests;
           add_note_tests (* delete_tests; *);
           delete_tests;
         ]
(*let json_to_datatable_tests = [ ( "empty datatable from json" >:: fun _ ->
  assert_equal { todo_items = [] } (Todo.json_to_datatable "./test/test1") ); (
  "non-empty datatable from json" >:: fun _ -> assert_equal { todo_items = [ {
  name = "string1" }; { name = "string2" } ] } (Todo.json_to_datatable
  "./test/test2") ); ] *)

let _ = run_test_tt_main tests
