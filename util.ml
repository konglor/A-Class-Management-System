(* A Class Management System - util.ml
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.09.2017
 *)

open Oper

(** [FileInadequate] the OS cannot handle a directory that exceeds x character long or no character. *)
exception NameInadequate of string

(** [f_role] and [f_college] the two files associated to per college *)
let f_role = "/roles.dat" and f_college = "/course.dat"

(** [create_file f] opens channel [o] to the file [f] and closes *)
let create_file f = let o = open_out f in close_out o

(** [file_len f] checks the length of the college name inputted by the user.
  * will be composite later *)
let file_len f =
  if String.length f > 0 && String.length f < 25 then f
  (* good thing about exception is that they are polymorphic?/doesnt abide to function type.
   * this function is type string, if unfound, better throw an exception than returning
   * a false/dummy string *)
  else raise (NameInadequate "must be 1-25 characters long!")

(** [dir_exists f] checks if the college directory [f] is present, if not, handle the exception and create *)
let rec dir_exists f =
  try if Sys.is_directory f then f else let () = Unix.mkdir f 0o666 in dir_exists f
  with Sys_error e -> Unix.mkdir f 0o666; dir_exists f

(** [file_exist f] checks if the college directory [f] have the two files
  * roles.dat and course.dat. If dne, create the file [create_file] *)
let rec file_exist f =
  let r' = f ^ f_role and c' = f ^ f_college in
  Sys.file_exists r' && Sys.file_exists c'
  || (create_file r'; create_file c'; file_exist f)

(** [sanitize_college] = file_exists(dir_exists(file_len(x))) 
  * where file_len checks the length of the command
  * dir_exists creates the directory if missing 
  * and file_exists creates the required file for every state of the program if need be *)
let sanitize_college = file_exist >> dir_exists >> file_len