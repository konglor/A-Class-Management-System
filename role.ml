(* A Class Management System - role.ml
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.04.2017
 *)

open Oper

(** type t [int] is the key that will map to a role's data *)
type t = int

(** [role_v] possible roles within a college
  * S - Student, I - Instructor, A - Administrator
  * where each role is given one of the three to
  * identify their read/write permissions *)
type role_v = Register | Student | Instructor | Administrator

(* fields each role has *)
type data = {
  id    : t;
  psw   : string;
  fname : string;
  lname : string;
  role  : role_v option;
  (* dictionary to hold (course-key, grade) *)
}

(** [get_key] returns the role's id [id] *)
let get_key = function { id; _ } -> id

(* [empty] perhaps a college without anyone in it *)
type empty = data option

(** [role_vfromint] returns the raw contructor supplied by a number *)
let role_vfromint = function
  | 0 -> Register
  | 1 -> Student
  | 2 -> Instructor
  | 3 -> Administrator
  | n -> raise (Bad_Match ("Expected value of 0..3 but received "^string_of_int n^"."))

(** [role_vtostring] takes in a role_v variant and returns a string representation *)
let role_vtostring = function
  | Register -> "Register" (* 05.12.2017 added this for newly created accounts *)
  | Student -> "Student"
  | Instructor -> "Instructor"
  | Administrator -> "Administrator"

(** [create f l i p] creates a Role.data record *)
let create f l i p = { id = i; psw = p; fname = f; lname = l; role = Some(Register) }

(** [get_name] returns the name [n] of the supplied role *)
let get_name = function { id; psw; fname = n; _ } -> n

(** [get_lname] returns the last name [n] of the supplied role *)
let get_lname = function { id; psw; fname; lname = l; _ } -> l

(** [get_fullname r] retursn the first and last name *)
let get_fullname r = Printf.sprintf "%s %s" (get_name r) (get_lname r)

(** [get_rw] returns the permission [r] *)
let get_rw = function { id; psw; fname; lname; role = r } -> r

let getraw_rw r = (@$) (get_rw r)

(** [get_rolestr] returns the read/write permission in terms of a string [a]
  * todo: composition *)
let get_rolestr = function { id; psw; fname; lname; role = r } -> 
  match r with
  | None -> raise (Invalid_argument "Found nothing")
  | Some a -> role_vtostring a

(** [set_rw p r] sets the read/write permission [p] of an user [r] *)
let set_rw p r = { r with role = Some p }

(** [get_psw] returns the password [p] *)
let get_psw = function { id; psw = p } -> p

(** [set_psw p r] sets the password [p] of an user [r] *)
let set_psw p r = { r with psw = p }