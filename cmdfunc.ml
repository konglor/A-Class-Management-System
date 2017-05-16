(* A Class Management System - cmdfunc.ml
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.11.2017
 *
 * due to ocaml's strong type checking, all functions must
 * agree to the type (Role.role_v option -> param_t list -> College.data -> College.data)
 * found in the Cmd module - that is, all function must have the same
 * number of parameters and the same type of parameters and the same return type.
 * a return of c means that the command was executed, anything else is an exception.
 * a scripting engine would be nice for this task.
 *
 * todo: clean this mess/pyramid of doom
 * todo: un-supress warning [-10] and fix hidden side-effect for cmd_upgrade - done
 * todo: atm, every function takes a [t l c] parameter
 * where [t] is the permission
 * [l] is the arguments
 * [c] is the college.
 * instead of the permission [t],
 * i believe passing in the role's id would have been better
 * and useful choice.
 * todo: validations/security on commands.
 * 
 *)
open Oper

exception Role_Exists of string
exception MisMatch_Info of string

(** [param_t] user inputs can be numbers or strings *)
type param_t = Number of int | Word of string

(** [unpack_int] returns the raw data [a] of a Number constructor *)
let [@warning "-8"] unpack_int = function Number a -> a

(** [unpack_str] returns the raw data [a] of a String constructor *)
let [@warning "-8"] unpack_str = function Word a -> a

(** [paramlist] user input is a list of [param_t] *)
type paramlist = param_t list

(** [cmd_createrole t l c] creates a role given the user input list [l] and adds it
 * to the college [c] if the role hasn't exist, otherwise throws an exception [Role_Exists] *)
let cmd_createrole t l c =
  let arg = List.nth l in 
  let cmd = Role.create in
  let cmd = cmd (unpack_str (arg 0)) in
  let cmd = cmd (unpack_str (arg 1)) in
  let cmd = cmd (unpack_int (arg 2)) in
  (* ... *)
  let r : Role.data = cmd (unpack_str (arg 3)) in
  let k = Role.get_key r in
  let b = College.find_role k c in
  if not b then 
    let new_rmap = College.add_role k r c in
    let () = Printf.printf "   Sucessfully registered. Welcome to %s college!\n" (College.getraw_name c) in
    {c with College.roles = Some(new_rmap)}
  else raise (Role_Exists("Someone with ID "^string_of_int k^" already exists."))

(** [cmd_loginrole t l c] logs into the college if the name, last name password, and id 
  * matches the database (a serialized .dat file) *)
let cmd_loginrole t l c = 
  let arg = List.nth l in 
  let k = unpack_int (arg 2) in
  let b = College.find_role k c in
  if not b then raise (Role_Exists("The identification "^string_of_int k^" cannot be found."))
  else
    let r = College.get_role k c in
    let name = Role.get_name r in
    let lastname = Role.get_lname r in
    let psw = Role.get_psw r in 
    let u_name = unpack_str (arg 0) in
    let u_lname = unpack_str (arg 1) in
    let u_psw = unpack_str (arg 3) in
    (* ... *)
    if name <> u_name || lastname <> u_lname || psw <> u_psw then raise (MisMatch_Info("The provided information does not match the id: "^string_of_int k^"."))
    else
      let () = Printf.printf "   Logging in...\n" in c

(** [cmd_savecollege t l c] saves the data of the college [c] *)
let cmd_savecollege t l c = 
  let n = (@$) (College.get_name c) in
  let r_file = n^Util.f_role and c_file = n^Util.f_college in
  c |> College.save_role r_file;
  c |> College.save_course c_file;
  let () = Printf.printf "   Saving state...\n" in c

(** [cmd_upgrade t l c] a backdoor to upgrade the read/write permission of any given account *)
let cmd_upgrade t l c =
  let arg = List.nth l in 
  let k = unpack_int (arg 0) in
  let rw = unpack_int (arg 1) in
  let p = unpack_str (arg 2) in
  let b = College.find_role k c in
  (* ... *)
  if not b then raise (Role_Exists("The identification "^string_of_int k^" cannot be found."))
  else
    let r = College.get_role k c in
    let np = Role.role_vfromint rw in
    if p <> "csci2469" then raise (MisMatch_Info("Incorrect password..."))
    else (* todo: check if account is already the provided arg *)
      let r = Role.set_rw np r in
      let r_map = College.add_role k r c in
      let n_np = Role.role_vtostring np in
      let () = Printf.printf "   User %d now has a(n) %s account.\n" k n_np in
        {c with College.roles = Some(r_map)}

(** [cmd_account t l c] displays the information about an account given by the user. *)
let cmd_account t l c =
  let arg = List.nth l in
  let arg_k = unpack_int (arg 0) in
  let arg_pw = unpack_str (arg 1) in
  let b = College.find_role arg_k c in
  (* ... *)
  if not b then raise (Role_Exists("The identification "^string_of_int arg_k^" cannot be found."))
  else
    let role = College.get_role arg_k c in
    let role_pw = Role.get_psw role in
    if arg_pw <> role_pw then raise (MisMatch_Info("Incorrect password..."))
    else
      let role_name = Role.get_name role in
      let role_lname = Role.get_lname role in
      let role_key = Role.get_key role in
      let role_rw = Role.get_rolestr role in
      let () = Printf.printf "   User ID: %d\n" role_key in
      let () = Printf.printf "   Password: %s\n" role_pw in
      let () = Printf.printf "   Name: %s %s\n" role_name role_lname in
      let () = Printf.printf "   Account type: %s\n" role_rw in c

(** [cmd_changepsw t l c] changes the password of an account where user have provided a key
  * to that account, the old password for verification, and a new password *)
let cmd_changepsw t l c =
  let arg = List.nth l in
  let arg_k = unpack_int (arg 0) in
  let arg_pw = unpack_str (arg 1) in
  let arg_npw = unpack_str (arg 2) in
  let b = College.find_role arg_k c in
  (* ... *)
  if not b then raise (Role_Exists("The identification "^string_of_int arg_k^" cannot be found."))
  else (* todo: check if password is already the provided arg *)
    let role = College.get_role arg_k c in
    let role_pw = Role.get_psw role in
    if arg_pw <> role_pw then raise (MisMatch_Info "Incorrect password...")
    else
      let r = Role.set_psw arg_npw role in
      let r_map = College.add_role arg_k r c in
      let () = Printf.printf "   Password successfully changed.\n" in
        {c with College.roles = Some(r_map)}

(** [cmd_addcourse t l c] adds a new course to the college [c] *)
let cmd_addcourse t l c =
  let arg = List.nth l in
  let arg_year = unpack_int (arg 0) in
  let arg_dep = unpack_str (arg 1) in
  let arg_id = unpack_int (arg 2) in
  let arg_sc = unpack_int (arg 3) in
  (* ... *)
  let k = College.get_courselen c + 1 in
  let course = Course.create k arg_year arg_dep arg_id arg_sc in
  let new_cmap = College.add_course k course c in
  let n = Course.get_defaultname course in
  let () = Printf.printf "   Successfully added the course: %s\n" n in
    {c with College.courses = Some(new_cmap)}

(** [cmd_allcourse t l c] iterates through the course catalog and prints each course *)
let cmd_listcourse t l c =
  let () = College.iter_coursemap (fun k v ->
    let () = Printf.printf "   Course ID: %d\n" k in
    let () = Printf.printf "   Course: %s\n" (Course.get_defaultname v) in
    let () = Printf.printf "   Course Name: %s\n" (if v.Course.coursename = None then "Unassigned" else (@$) (v.Course.coursename)) in
    let () = Printf.printf "   Instructor: %s\n"
      (if v.Course.instructor = None then "Unassigned"
      else let r = College.get_role ((@$) (v.Course.instructor)) c in Role.get_fullname r) in
    let () = Printf.printf "   Semester: %s\n" (if v.Course.semester = None then "Unassigned" else Course.s_vtostring ((@$) (v.Course.semester))) in
    let () = Printf.printf "   Credits: %d\n" (if v.Course.credit = None then 0 else (@$) (v.Course.credit)) in
             Printf.printf "   Enrolled: %d/%d\n\n" (List.length v.Course.students) v.Course.maxenroll) c
  in c

(** [cmd_namecourse t l c] gives provided course a name *)
let cmd_namecourse t l c =
  let arg = List.nth l in
  let arg_id = unpack_int (arg 0) in
  let arg_name = unpack_str (arg 1) in
  (* ... *)
  let b = College.find_course arg_id c in
  if not b then raise (Role_Exists "The provided course id could not be found.")
  else
    let course = College.get_course arg_id c in
    let cs = Course.set_name arg_name course in
    let c_map = College.add_course arg_id cs c in
    let () = Printf.printf "   Successfully assigned course %d with the name: %s.\n" arg_id arg_name in
      {c with College.courses = Some(c_map)}

(** [cmd_semcourse t l c] assigns the given course a semester *)
let cmd_semcourse t l c =
  let arg = List.nth l in 
  let arg_id = unpack_int (arg 0) in
  let arg_sem = unpack_int (arg 1) in
  let b = College.find_course arg_id c in
  (* ... *)
  if not b then raise (Role_Exists "The provided course id could not be found.")
  else
    let course = College.get_course arg_id c in
    let sem = Course.s_vfromint arg_sem in
    let new_course = Course.set_semester sem course in
    let s_sm = Course.s_vtostring sem in
    let c_map = College.add_course arg_id new_course c in
    let () = Printf.printf "   Successfully assigned course %s to %s semester.\n" (Course.get_defaultname new_course) s_sm in
      {c with College.courses = Some(c_map)}

(** [cmd_creditcourse t l c] assigns the given course a credit *)
let cmd_creditcourse t l c =
  let arg = List.nth l in
  let arg_id = unpack_int (arg 0) in
  let arg_cr = unpack_int (arg 1) in
  (* ... *)
  let b = College.find_course arg_id c in
  if not b then raise (Role_Exists "The provided course id could not be found.")
  else
    let course = College.get_course arg_id c in
    let cs = Course.set_credit arg_cr course in
    let c_map = College.add_course arg_id cs c in
    let () = Printf.printf "   Successfully assigned course %s to %d credits.\n" (Course.get_defaultname cs) arg_cr in
      {c with College.courses = Some(c_map)}

(** [cmd_assign t l c] assigns a course to an instructor *)
let cmd_assign t l c =
  let arg = List.nth l in
  let arg_cid = unpack_int (arg 0) in
  let arg_tid = unpack_int (arg 1) in
  (* ... *)
  match College.find_course arg_cid c with
  | false -> raise (Role_Exists "The provided course id could not be found.")
  | true ->
    match College.find_role arg_tid c with 
    | false -> raise (Role_Exists "The provided instructor id could not be found.")
    | true ->
      let r = College.get_role arg_tid c in
      let rw = Role.getraw_rw r in
      if rw = Role.Instructor then
        let course = College.get_course arg_cid c in
        let cs = Course.set_instructor arg_tid course in
        let c_map = College.add_course arg_cid cs c in
        let () = Printf.printf "   Successfully assigned %s to instructor: %s.\n" (Course.get_defaultname cs) (Role.get_fullname r) in
          {c with College.courses = Some(c_map)}
      else raise (MisMatch_Info "Provided Instructor ID does not have an Instructor Account.")

(** [cmd_schedule t l c] displays the schedule of a given instructor *)
let cmd_schedule t l c =
  let arg = List.nth l in
  let id = unpack_int (arg 0) in
  match College.find_role id c with
  | false -> raise (Role_Exists "The provided instructor id could not be found.")
  | true ->
      (* ... *)
      let r = College.get_role id c in
      let rw = Role.getraw_rw r in
      if rw = Role.Instructor then
        begin
          let () = Printf.printf "   Schedule for Instructor %s:\n" (Role.get_fullname r) in
          let () = College.iter_coursemap (fun k v ->
          if v.Course.instructor <> None && Course.getraw_instructor v = id then
            Printf.printf "      courseid: %d - %s\n" k (if v.Course.coursename = None then "Unassigned" else (@$) (v.Course.coursename))) c
            in c
        end
      else raise (MisMatch_Info "Provided ID does not have an Instructor Account.")

(** [cmd_regcourse t l c] registers a course *)
let cmd_regcourse t l c =
  let arg = List.nth l in
  let arg_cid = unpack_int (arg 0) in
  let arg_sid = unpack_int (arg 1) in
  let arg_spw = unpack_str (arg 2) in
  let b = College.find_role arg_sid c in
  (* ... *)
  if not b then raise (Role_Exists("The identification "^string_of_int arg_sid^" cannot be found."))
  else
    let role = College.get_role arg_sid c in
    let role_pw = Role.get_psw role in
    if arg_spw <> role_pw then raise (MisMatch_Info("Incorrect password..."))
    else
      let b2 = College.find_course arg_cid c in
      if not b2 then raise (Role_Exists "The provided course id could not be found.")
      else
        let course = College.get_course arg_cid c in
        let cs = Course.add_student arg_sid course in
        let c_map = College.add_course arg_cid cs c in
        let () = Printf.printf "   Successfully registered for %s.\n" (Course.get_defaultname cs) in
          {c with College.courses = Some(c_map)}

(** [cmd_mycourse t l c] displays the courses and grades of a student role *)
let cmd_mycourse t l c =
  let arg = List.nth l in
  let arg_sid = unpack_int (arg 0) in
  let arg_spw = unpack_str (arg 1) in
  let b = College.find_role arg_sid c in
  (* ... *)
  if not b then raise (Role_Exists("The identification "^string_of_int arg_sid^" cannot be found."))
  else
    let role = College.get_role arg_sid c in
    let role_pw = Role.get_psw role in
    if arg_spw <> role_pw then raise (MisMatch_Info("Incorrect password..."))
    else
      let r = College.get_role arg_sid c in
      let rw = Role.getraw_rw r in
      if rw = Role.Student then begin
        let () = Printf.printf "   %s's Courses:\n" (Role.get_fullname r) in
        let () = College.iter_coursemap (fun k v ->
          let b = Course.student_exist arg_sid v in
          if b then begin
            let g = Course.get_grade arg_sid v in
            Printf.printf "      Course ID: %d - %s %s\n" k (Course.get_defaultname v) (if v.Course.coursename = None then "Unassigned" else (@$) (v.Course.coursename));
            Printf.printf "      Grade: %c\n\n" g
          end) c in c
        end
      else raise (MisMatch_Info "Provided ID does not have a(n) Student Account.")

(** [cmd_assigngrade t l c] assigns a grade to a student in a particular course *)
let cmd_assigngrade t l c =
  let arg = List.nth l in
  let arg_tid = unpack_int (arg 0) in
  let arg_tpw = unpack_str (arg 1) in
  let arg_cid = unpack_int (arg 2) in
  let arg_sid = unpack_int (arg 3) in
  let arg_g = unpack_str (arg 4) in
  let b = College.find_role arg_sid c in
  match b with true -> begin
  let t_role = College.get_role arg_tid c in
  let t_psw = Role.get_psw t_role in
  let b = (arg_tpw = t_psw) in
  match b with true -> begin
  let r = College.get_role arg_sid c in
  let rw = Role.getraw_rw r in
  let b = (rw = Role.Student) in
  match b with true -> begin
  let b = College.find_course arg_cid c in
  match b with true -> begin
  let course = College.get_course arg_cid c in
  let b = (Course.getraw_instructor course) = arg_tid in
  match b with true -> begin
  let g = String.get arg_g 0 in
  let cs = Course.set_grade arg_sid g course in
  let c_map = College.add_course arg_cid cs c in
    let () = Printf.printf "   Sucessfully assigned Grade %c to %s\n" g (Role.get_fullname r) in
    { c with College.courses = Some(c_map) }
  end | false -> raise (MisMatch_Info "You are not authorized to perform read/write for this course")
  end | false -> raise (Role_Exists "The provided course id could not be found.")
  end | false -> raise (MisMatch_Info "Provided ID does not have a(n) Student Account.")
  end | false -> raise (MisMatch_Info("Incorrect password..."))
  end | false -> raise (Role_Exists("The identification "^string_of_int arg_sid^" cannot be found."))