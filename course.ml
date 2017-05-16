(* A Class Management System - course.ml
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.04.2017
 *)

open Oper

(** type t [int] is the key that will map to a course's data *)
type t = int

(* available semester *)
type semester_v = Spring | Summer | Fall

type data = {
  id          : t;
  year        : int;
  department  : string;
  courseid    : int;
  section     : int;
  semester    : semester_v option;
  coursename  : string option;        (* name of course ex: Computer Science & Information Systems *)
  instructor  : int option;           (* instructors id *)
  maxenroll   : int;                  (* max number of chairs *)
  credit      : int option;           (* ex: 4 credits. sometimes courses dont give credits *)
  students    : (int * char) list;    (* list of students enrolled by their student id and a grade 'A-F' *)
}

(** [get_key] returns the role's id [id] *)
let get_key = function { id; _ } -> id

(** [s_vfromint] returns the raw contructor supplied by a number *)
let s_vfromint = function
  | 0 -> Spring
  | 1 -> Summer
  | 2 -> Fall
  | n -> raise (Bad_Match ("Expected value of 0-2 but received "^string_of_int n^"."))

(** [s_vtostring] takes in a semester_v variant and returns a string representation *)
let s_vtostring = function
  | Spring -> "Spring"
  | Summer -> "Summer"
  | Fall -> "Fall"

(** [get_year] returns the year of given course *)
let get_year = function { id; year; _ } -> year

(** [get_department] returns the department of given course *)
let get_department = function { id; year; department; _ } -> department

(** [get_courseid] returns the id of given course *)
let get_courseid = function { id; year; department; courseid; _ } -> courseid

(** [get_section] returns the section of given course *)
let get_section = function { id; year; department; courseid; section; _ } -> section

(** [get_instructor] returns Some instructor id of given course *)
let get_instructor = function { id; year; department; courseid; section; 
    semester; coursename; instructor; _ } -> instructor

(** [getraw_instructor r] returns the raw id of given course [r] *)
let getraw_instructor r = (@$) (get_instructor r)

(** [get_semester] returns the semester of given course *)
let get_semester = function { id; year; department; courseid; section; semester; _ } -> semester

(** [getraw_semester r] returns the raw semester of given course [r] *)
let getraw_semester r = (@$) (get_semester r)

let get_maxenrolled = function
  { id; year; department; courseid; section; semester; coursename;
    instructor; maxenroll; _ } -> maxenroll

(** [get_studentlist] returns the student list of a given course *)
let get_studentlist = function
  { id; year; department; courseid; section; semester; coursename;
    instructor; maxenroll; credit; students } -> students

(** [get_student s c] returns the student tuple that contains the ID and grade *)
let get_student s c =
  let rec loop = function
    | [] -> raise (Bad_Match ("Could not find student id "^string_of_int s^" in this course."))
    | (k,v)::t -> if k = s then (k,v) else loop t
  in loop (get_studentlist c)

(** [get_v] returns the value of the given tuple *)
let get_v = function (k,v) -> v

(** [get_k] returns the key of the given tuple *)
let get_k = function (k,v) -> k

(** [get_grade] returns the of a student *)
let get_grade = get_v >>> get_student

(** [set_grade s g c] assigns a student [s] in a course [c] with a letter grade [g] *)
let set_grade s g c =
  let l = List.mapi (fun i (k,v) -> if k = s then (k,g) else (k,v)) (get_studentlist c) in
  { c with students = l }

(** [student_exist s c] returns true if the student [s] exists in the course [c] *)
let student_exist s c =
  let rec loop = function
    | [] -> false
    | (k,v)::t -> if k = s then true else loop t
  in loop (get_studentlist c)

(** [add_student s c] adds a student [s] into the course [c] with an initial grade of 'A' *)
let add_student s c =
  let b = student_exist s c in
  if b then raise (Bad_Match "You are already registered!")
  else
    let l = get_studentlist c in
    let n = List.length l in
    if n >= (get_maxenrolled c) then raise (Bad_Match "This course is already full!")
    else { c with students = l@[(s, 'A')] }

(** [create k y d id sc sm] creates a course data with the supplied values, other fields are given default value *)
let create k y d id sc =
  { id = k; year = y; department = d; courseid = id; section = sc; semester = None;
    coursename = None; instructor = None; maxenroll = 30; credit = None; students = [] }

(** [set_name n c] sets the coursename field of a course [c] to Some name [n] *)
let set_name n c = { c with coursename = Some n }

(** [set_instructor n c] sets the instructor field of a course [c] to Some id [n] *)
let set_instructor n c = { c with instructor = Some n }

(** [set_credit n c] sets the credit field of a course [c] to some number [n] *)
let set_credit n c = { c with credit = Some n }

(** [set_semester n c] sets the semester field of a course [c] to some number [s] *)
let set_semester s c = { c with semester = Some s }

(** [get_defaultname c] takes in a course [c] and returns a formated string of the course
  * e.g: 2017 CSCI 1410-10 *)
let get_defaultname c =
  let y = get_year c in
  let d = get_department c in
  let id = get_courseid c in
  let sc = get_section c in
  Printf.sprintf "%d %s %d-%d" y d id sc