(* A Class Management System - state.ml
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.11.2017
 *)

open Util
(** [open Oper] dependency for some custom operator functions *)
open Oper

(** [step] the three stages of the program *)
type step = Pick | Login | Operate

(** the state of the program [state] consists of who is logged in [user] 
  * which college [college] and what stage the state is in [stage] *)
type state = { user : int option; college : College.empty; stage : step }

(** [s_init s] intializes the college field of the state [s] *)
let s_init s = let c = College.init in { s with college = Some c }

(** [get_college] takes in a state and returns the college data [c] if found, otherwise None *)
let get_college = function { user; college = c } -> c

(** [get_stage] takes in a state and returns the stage of the state [c] *)
let get_stage = function { user; college; stage = c } -> c

(** [set_stage s] takes in a state [s] and changes the stage field to a new one [g] *)
let set_stage g = (fun s -> { s with stage = g })

(** [get_user] returns Some user [a] or None if unfound *)
let get_user = function { user = a; _ } -> a

(** [set_user i] sets the user field [i] of the state [s] *)
let set_user i = (fun s -> { s with user = Some(i) })

(** [retreive_college collegefunc] applies a college function [collegefunc]
  * to the college field of the state [s] *)
let retreive_college collegefunc = (fun s ->
  let a = (@$) (get_college s) in collegefunc a)

(** [#$> f] is a lambda expression [type _ = 'a option -> 'a] that returns the raw data [a]
  * given by it's composite function [retreive_college] whom returns [Some a]
  * todo: function was created before I made the [@$] operator from the Oper module.
  * clean code if have time - done 05.11.17 *)
let (#$>) f = (@$) >> retreive_college f

(** [save_state s] save the state [s] of the program via serialize *)
let save_state s =
  let a = (@$) (get_college s) in let n = (#$>) College.get_name s in a |> College.save_role (n ^ f_role)

(** [operate_college f] takes in a polymorphic argument [f], a function from the college module [collegefunc]
  * and the state of the program [s] and operates on the college field and returns a transformed state
  * clean code - done 05.11.17 *)
let operate_college (f : 'a) = (fun collegefunc s ->
  let c = (@$) (get_college s) in let a = collegefunc c f in { s with college = Some a } )

(** [fmtstr_0] the format print expression used when there are no college *)
let fmtstr_0 = (Printf.printf "%s")

(** [fmtstr_0 n] the format print expression used where user has picked a college and the name of the college is given [n] *)
let fmtstr_1 n = (Printf.printf "[%s]%s" n)

(** [fmtstr_2 n u] the format print expression used after the user has logged into the college *)
let fmtstr_2 n u = (Printf.printf "[%s][%s]%s" n u)

(** [fmtstr_get c] retreives a fmtstr_* function by checking the stage of the state [c] *)
let fmtstr_get c = 
  match get_stage c with
  | Pick -> fmtstr_0
  | Login -> let n = (#$>) College.get_name c in (fmtstr_1 n)
  | Operate -> 
    let n = (#$>) College.get_name c in
    let id = (@$) (get_user c) in
    let a = (@$) (get_college c) in
    let u = College.get_rolename id a in
    (fmtstr_2 n u)

(** [fmtprt_get f] applies a fmtstr_* function to some strings according to the stage of the state *)
let fmtprt_get f = function
  | Pick ->
    (f "Enter a college: \n")
  | Login -> (
    (* todo: automate this *)
    f "List of commands: \n";
    Printf.printf "   login \t<name> <lastname> <id> <password>\n";
    Printf.printf "   register \t<name> <lastname> <id> <password>\n")
  | Operate ->
    f "Enter help for user commands\n"

(** [fixsplit l] fixes the splitting of a string list produced by the regular expression " +" *)
let fixsplit l =
  (** [loop acc acc2 acc3 b] where acc holds the given list, acc2 holds the next string to be concated,
    * acc3 is the returning list, and b is a flag to let the function know which character to find next *)
  let rec loop acc acc2 acc3 b =
    match acc with
    | [] -> acc3
    | h::t ->
    if not b then
      match h.[0] with
      | '"' -> loop t (String.sub h 1 (String.length h - 1)) acc3 true
      | _ -> loop t acc2 (acc3@[h]) b
    else
      match h.[String.length h - 1] with
      | '"' -> loop t "" (acc3@[acc2^" "^(String.sub h 0 (String.length h - 1))]) false
      | _ -> loop t (acc2^" "^h) acc3 b
  in loop l "" [] false

(** [cmd_get f] calls the format print function [f] and begins the bargain of the outside world for a list *)
let cmd_get f = f;
 (* atm, strings are splited by spaces, need something else just in case user inputs a string with the intent
  * to space. e.g: "Computer Science"
  * todo: reg expression. - the engine doesnt support the usuals r/w/s/etc.
  *
  * wrote quick routine to do this: [fixsplit l]
  * before:
  * when user enters: namecourse "Computer Science"
  * the result would be: ["namecourse"; "\"Computer"; "Science\""]
  * after:
  * results: ["namecouse"; "Computer Science"]
  * it is still without proper validation, but will suffice for now *)
  let split = Str.split (Str.regexp " +") in
  let l = split (read_line()) in
  fixsplit l

(** [cmd_handle] handler to process user input [l] after some simple sanitizing routine from Util module *)
let cmd_handle l = (fun c ->
  let cmd = List.nth l 0 in
  match get_stage c with
  (* Picking a college *)
  | Pick -> let r' = cmd ^ f_role and c' = cmd ^ f_college in
    if sanitize_college cmd then (* run Util.sanitize_college on the command *)
      c |> operate_college cmd College.set_name
        |> operate_college r' College.load_role
        |> operate_college c' College.load_course
        |> set_stage Login
    else raise (NameInadequate "College directory does not exist!")
  (* Logging into the college *)
  | Login -> 
    let a = (@$) (get_college c) in
    (** [execute_cmd] a partial application of Cmd.sanitize_cmd *)
    let execute_cmd = Cmd.sanitize_cmd (None, l) in
    let n = execute_cmd a in
    { c with college = Some(n) }
      (* int_of_string hardcoded... *)
      |> set_user (int_of_string (List.nth l 3))
      |> set_stage Operate
  | Operate -> 
    (* todo: clean repetive codes for both Login stage and Operate stage. *)
    let a = (@$) (get_college c) in
    (* this time, get the user's permission *)
    let id = (@$) (get_user c) in
    let rw = College.get_rolerw id a in
    let execute_cmd = Cmd.sanitize_cmd (rw, l) in
    let n = execute_cmd a in
    { c with college = Some(n) }
)

(** [cmd_process] a function composition of cmd_handle and cmd_get. cmd_handle(cmd_get _) *)
let cmd_process = cmd_handle >> cmd_get

(** [s_loop c] an infinite loop for user I/O and the portal to the outside world *)
let rec s_loop c = 
  (* print a helping message for the user *)
  let fmtfct = fmtstr_get c in 
  let s = get_stage c in
  fmtprt_get fmtfct s;
  
  (** [begin_get acc] is an infinite loop to handle user I/O and exception handle *)
  let rec begin_get acc = 
    try begin
      acc |> cmd_process (fmtfct "> ")
          |> (if get_stage c = Operate then begin_get else s_loop)
    end
    with
    (* exceptions are not being handle where they are needed to be, rollback
     * takes place... but i guess that is fine for now.
     * todo: clean this mess / place back exception handlings to where it belongs *)
    | Invalid_argument e
    | Failure e (* nth *)
    | Oper.Bad_Match e
    | NameInadequate e
    | Cmdfunc.Role_Exists e
    | Cmdfunc.MisMatch_Info e
    | Cmd.Bad_Arg e -> Printf.printf "** %s\n" e;
    acc |> begin_get
  in begin_get c

(** [start] begins with a default state of No user and No college and a default stage [Pick] *)
let start _ =
  let s = { user = None; college = None; stage = Pick } in
  try
    s |> s_init |> s_loop
  with End_of_file ->
    Printf.printf "Saving all data... (todo) "