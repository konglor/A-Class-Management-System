(* A Class Management System - cmd.ml
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.10.2017
 *
 * functions in this module will operate on the list of commands [cmdlist]
 * where exceptions are acted merely (but not fully) to break out of
 * loops/recursion and perform hot fixes/rollbacks.
 *)

open College
open Oper
open Cmdfunc

(** [Bad_Arg] is thrown when an user tries to execute unavailable commands 
  * or when an user provides too many or too few arguments
  * or when an user provides a string in place of an expected int conversion *)
exception Bad_Arg of string

(** [argtype] a variant of the possible arguments for commands *)
type argtype = I of (string -> int) | S of (string -> string)

type 'a datacmd = {
  (* [rw] read/write. Some - Administrator, Student, Instructor, or None *)
  rw        : Role.role_v option list; 
  param     : argtype list;
  func      : ('a -> param_t list -> data -> data);
  hint      : string
}

(** [tostr s] returns same string [s] *)
let tostr s : string = s

(** [toint] returns int_of_string from stdlib *)
let toint = int_of_string

(* before, cmdlist was an immutable regular list.
 * because the <help> command needs to access the <cmdlist> list [to loop and display the commands],
 * I'd imagine the <help>
 * command must be coded under the <cmdlist> list to access it.
 * But the <cmdlist> list must also access the <help> command as well [to put it in the <func> field
 * of the key <help>] resulting a collision and linker issues as to which comes first.
 * after some long hours, i thought about mutable objects for the first time -> pointers *)
let cmdlist = ref [];;

let b = "\n\t\t" in
(** [cmdlist] a dictionary of commands and their datacmd record. 
  * functions can be found in Cmdfunc module *)
cmdlist := [
  ("register",  { rw      = [None];
                  param   = [S(tostr); S(tostr); I(toint); S(tostr)];
                  func    = cmd_createrole;
                  hint    = "<name> <lastname> <id> <password>"});

  ("login",     { rw      = [None];
                  param   = [S(tostr); S(tostr); I(toint); S(tostr)];
                  func    = cmd_loginrole;
                  hint    = "<name> <lastname> <id> <password>"});

  ("help",      { rw      = [Some(Role.Register); Some(Role.Student); Some(Role.Administrator); Some(Role.Instructor)];
                  param   = [];
                  func    = (fun t l c ->
                              List.iter (fun (k, v) ->
                              if List.mem t v.rw then
                                Printf.printf "   %s \t%s\n\n" k v.hint
                            ) !cmdlist; c);
                  hint    = "--display available commands"});

  ("save",      { rw      = [Some(Role.Register); Some(Role.Student); Some(Role.Administrator); Some(Role.Instructor)];
                  param   = [];
                  func    = cmd_savecollege;
                  hint    = "--saves the state of the college"});

  ("upgrade",   { rw      = [Some(Role.Register); Some(Role.Student); Some(Role.Administrator); Some(Role.Instructor)];
                  param   = [I(toint); I(toint); S(tostr)];
                  func    = cmd_upgrade;
                  hint    = "<id> <permission> <s-password>" ^b^
                            "--permission: 1..3 where 1=Student 2=Instructor 3=Administrator"});

  ("account",   { rw      = [Some(Role.Register); Some(Role.Student); Some(Role.Administrator); Some(Role.Instructor)];
                  param   = [I(toint); S(tostr)];
                  func    = cmd_account;
                  hint    = "<id> <password>" ^b^
                            "--Displays the account details."});

  ("changepsw", { rw      = [Some(Role.Register); Some(Role.Student); Some(Role.Administrator); Some(Role.Instructor)];
                  param   = [I(toint); S(tostr); S(tostr)];
                  func    = cmd_changepsw;
                  hint    = "<id> <password> <new_password>"});

  ("addcourse", { rw      = [Some(Role.Administrator)];
                  param   = [I(toint); S(tostr); I(toint); I(toint)];
                  func    = cmd_addcourse;
                  hint    = "<year> <department> <course_id> <section>" ^b^
                            "--ex: addcourse 2017 CSCI 1410 10"});

  ("listcourse", { rw     = [Some(Role.Register); Some(Role.Student); Some(Role.Administrator); Some(Role.Instructor)];
                  param   = [];
                  func    = cmd_listcourse;
                  hint    = "--displays all courses."});

  ("namecourse", { rw     = [Some(Role.Administrator)];
                  param   = [I(toint); S(tostr)];
                  func    = cmd_namecourse;
                  hint    = "<course_id> <name>" ^b^
                            "--ex: namecourse 3 \"Computer Science\""});

  ("credcourse", { rw     = [Some(Role.Administrator)];
                  param   = [I(toint); I(toint)];
                  func    = cmd_creditcourse;
                  hint    = "<course_id> <credits>" ^b^
                            "--ex: credcourse 3 4" ^b^
                            "--assigns the courseid 3 to 4 credits"});

  ("semcourse", { rw      = [Some(Role.Administrator)];
                  param   = [I(toint); I(toint)];
                  func    = cmd_semcourse;
                  hint    = "<course_id> <semester>" ^b^
                            "--semester: 0..2 where 0=Spring 1=Summer 2=Fall"});

  ("assign",    { rw      = [Some(Role.Administrator)];
                  param   = [I(toint); I(toint)];
                  func    = cmd_assign;
                  hint    = "<course_id> <instructor_id>" ^b^
                            "--assigns an instructor to a course"});

  ("schedule",  { rw      = [Some(Role.Instructor); Some(Role.Administrator)];
                  param   = [I(toint)];
                  func    = cmd_schedule;
                  hint    = "<instuctor_id>"});

  ("regcourse",  { rw     = [Some(Role.Student)];
                  param   = [I(toint); I(toint); S(tostr)];
                  func    = cmd_regcourse;
                  hint    = "<course_id> <account_id> <account_password>" ^b^
                            "--register for a course"});

  ("mycourse",  { rw      = [Some(Role.Student)];
                  param   = [I(toint); S(tostr)];
                  func    = cmd_mycourse;
                  hint    = "<account_id> <account_password>" ^b^
                            "--displays all courses registered to the provided account"});

  ("assigngrade", { rw    = [Some(Role.Instructor)];
                  param   = [I(toint); S(tostr); I(toint); I(toint); S(tostr)];
                  func    = cmd_assigngrade;
                  hint    = "<account_id> <account_password> <course_id> <student_id> <grade>" ^b^
                            "--where grade is A..F or W" ^b^
                            "--ex: assigngrade 1004 Vs1004 1 10001 F"});
]

(** [find_cmd x l] returns the value of the given key [x] from a list [l]. *)
let find_cmd x l =
  let rec r_tail acc =
    match acc with
    | [] -> raise (Bad_Arg (x^" command cannot be found!"))
    | (k,v)::t -> (if k = x then v else r_tail t)
  in r_tail !l

(* be partial *)
let find_cmdlist x = find_cmd x cmdlist

(** [get_rw] returns the rw field [r] of a command provided by its key *)
let get_rw = (fun x -> match x with { rw = r; _} -> r) >> find_cmdlist

(** [get_arglist] returns the param field [s] of a command provided by its key *)
let get_arglist = (fun x -> match x with { rw; param = s } -> s) >> find_cmdlist

(** [get_func] returns the function of a given command *)
let get_func = (fun x -> match x with { rw; param; func = f } -> f) >> find_cmdlist

(** [get_hint] returns the hint field [h] of a command provided by its key *)
let get_hint = (fun x -> match x with { rw; param; func; hint = h } -> h) >> find_cmdlist

(** [get_cmd l] returns the first element in the list. System will raise exception
  * [Failure] if list is insufficient *)
let get_cmd l = (List.nth l 0)

(** [search_arglist] retreives the param of a command given by the list the user inputted *)
let search_arglist = get_arglist >> get_cmd

(** [search_func] returns the function that is mapped to the command given by the user *)
let search_func = get_func >> get_cmd

(** [search_rw] retreives the rw of a command given by the list the user inputted *)
let search_rw = get_rw >> get_cmd

(** [check_rw r, l] checks if the command [l] given by the user can be executed by checking
  * his/her permission [r] *)
let check_rw (r, l) = 
  let cmdrw = search_rw l in
  let cmd = get_cmd l in
  (if List.mem r cmdrw then (r, l) else raise (Bad_Arg (cmd^" command cannot be found!")))

(** [chec_argcount r, l] checks if the user inputted enough arguments to run the command *)
let check_argcount (r, l) =
  let userarg = (List.length l)-1 in
  let cmdarg = List.length (search_arglist l) in
  (if userarg = cmdarg then (r, l) else
  raise (Bad_Arg ("Expected "^string_of_int cmdarg^" argument but received "^string_of_int userarg^".")))

(** [check_argtype r,l] checks if the arguments of the command [l] agrees with system's cmdlist
  * abit difficult to do due to the strong type checking. *)
let check_argtype (r, l) =
  let param = search_arglist l in
  let func = search_func l in
  (* because commands with no argument requires no argument checking
   * skep the process if the user input list has only 1 element - that is, the command *)
  if List.length l = 1 then func r [] else
  let arg = List.tl l in
  (** [two_list acc l1 l2] the initialization of checking if user inputs [l2] can be typecaste according to
    * the rules of the system [l1], then returns a parsed list [acc] *)
  let rec two_list acc l1 l2 =
    try match List.hd l1 with
      | S s ->
        let str = s (List.hd l2) in
        let acc = acc @ [Word(str)] in
        two_list acc (List.tl l1) (List.tl l2)
      | I i ->
      try begin
        let num = i (List.hd l2) in
        let acc = acc @ [Number(num)] in
        two_list acc (List.tl l1) (List.tl l2)
      end with
      | int_of_string ->
        let n = (List.length acc)+1 in
        (raise (Bad_Arg ("Expected a number for argument "^string_of_int n^" but received string.")))
    with
    (* Failure exception to "break" from recursion given by List.hd function *)
    | Failure e -> func r acc
  in two_list [] param arg

(** [sanitize_cmd] a composition of all the former functions who's main
  * functionality is to sanitize the user's input *)
let sanitize_cmd = check_argtype >> check_argcount >> check_rw


