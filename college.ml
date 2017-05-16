(* A Class Management System - college.ml
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.10.2017
 *)

(** [open Oper] dependency for some custom operator functions *)
open Oper

(** [RoleMap] is a Serializable map for the module Role *)
module RoleMap = Serialize.Make(Role)

(** [CourseMap] is a Serializable map for the module Course *)
module CourseMap = Serialize.Make(Course)

type role_t = Role.data RoleMap.t
type course_t = Course.data CourseMap.t

(** the college [data] consists of the name of the college [name]
  * a map of all the courses [courses] and a map of the roles [roles] *)
type data = {
  name      : string option;
  courses   : course_t option;
  roles     : role_t option;
}

(* the state of the program may have a college, or may have none *)
type empty = data option

(** [get_name] takes in a college data and returns the name of the college [n] if exists *)
let get_name = function { name = n; _ } -> n

(** [getraw_coursemap] takes in a college data and returns the raw map of courses *)
let getraw_name c = (@$) (get_name c)

(** [set_name c] takes in a college data [c] and transform the college with some name [n] *)
let set_name c = (fun n -> { c with name = Some n })

(** [get_rolemap] takes in a college data and returns the Some map of roles [r] if exists, else None *)
let get_rolemap = function { name; courses; roles = r } -> r

(** [getraw_rolemap] takes in a college data and returns the map of roles [r] *)
let getraw_rolemap c = (@$) (get_rolemap c)

(** [load_role c] takes in a college data [c] and transform the college with some role [rd]
  * provided by the Serialize.deserialize function *)
let load_role c = (fun f ->
  let rd = RoleMap.deserialize f RoleMap.empty in { c with roles = Some rd })

(** [load_course c] takes in a college data [c] and transform the college with some courses [cd]
  * provided by the Serialize.deserialize function *)
let load_course c = (fun f ->
  let cd = CourseMap.deserialize f CourseMap.empty in { c with courses = Some cd })

(** [init] returns a default college with no data *)
let init = { name = None; courses = None; roles = None }

(** [find_role k] checks if the role id [k] exists in the college *)
let find_role k = (fun c -> let r_map = getraw_rolemap c in RoleMap.mem k r_map)

(** [add_role k d c] adds a role [d] into the college [c] mapped by his/her id [k] *)
let add_role k d c = let r_map = getraw_rolemap c in RoleMap.add k d r_map

(** [get_role k c] gets the role's data given by his/her key [k] in the college [c] *)
let get_role k c = let r_map = getraw_rolemap c in RoleMap.find k r_map

(** [get_rolename] retrieves the role's name after getting the role with [get_role] *)
let get_rolename = Role.get_name >>> get_role

(** [get_rolerw] retrieves the role's read/write permission after getting the role with [get_role] *)
let get_rolerw = Role.get_rw >>> get_role

(** [save_role f c] saves the map of roles in the college [c] into a file [f] *)
let save_role f c = let r_map = getraw_rolemap c in RoleMap.serialize f r_map

(** [get_coursemap] takes in a college data and returns the Some map of courses [c] if exists, else None *)
let get_coursemap = function { name; courses = c; _ } -> c

(** [getraw_coursemap] takes in a college data and returns the raw map of courses *)
let getraw_coursemap c = (@$) (get_coursemap c)

(** [find_course k] checks if the course key [k] exists in the college *)
let find_course k = (fun c -> let c_map = getraw_coursemap c in CourseMap.mem k c_map)

(** [add_course k d c] adds a course [d] into the college [c] mapped by their key [k] *)
let add_course k d c = let c_map = getraw_coursemap c in CourseMap.add k d c_map

(** [get_course k c] gets the course's data given by the key [k] in the college [c] *)
let get_course k c = let c_map = getraw_coursemap c in CourseMap.find k c_map

(** [save_course f c] saves the map of courses in the college [c] into a file [f] *)
let save_course f c = let c_map = getraw_coursemap c in CourseMap.serialize f c_map

(** [iter_coursemap f c] iterates through the course of the given college [c] with a function [f] *)
let iter_coursemap f c = let m = getraw_coursemap c in CourseMap.iter f m

(** [get_courselen c] returns the length/size of the course map of a given college [c] *)
let get_courselen c = let m = getraw_coursemap c in CourseMap.length m