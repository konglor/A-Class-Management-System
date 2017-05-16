(* A Class Management System
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.02.2017 - initial date of project
 *
 * ABOUT:
 * A Class Management System is a simple program to
 * emulate the real life operations of the roles within a college.
 * Any given role within a college can be categorized into one of three
 * following: Administrator, Instructor, and Students - 
 * where each role is given a different set of permission and
 * operations. Overall, the goal is to learn the syntax of ocaml and
 * learn the functional paradigm without the use of third party libs.
 *
 * USAGE:
 * Once the program executes, the user will be asked to input
 * a college. If the college directory (e.g: ./saintpaul/) exists then 
 * all roles and courses from that college will be loaded (deserialize).
 * If the directory isn't present, the directory will be created.
 * college [saintpaul] is provided by default
 * Following the choosing of college, the user will be prompt
 * to whether login or register.
 *
 * There is a backdoor command called: "upgrade" where any user can become a
 * Student, Instructor, or Administator but the command is password protected.
 * the password: "csci2469"
 * example usage: [saintpaul][kong]> upgrade 10003 3 csci2469
 * where upgrade is the command
 * 10003 is the id of the chosen role
 * 3 is a number [1..3] where 1 = Student. 2 = Instructor. 3 = Administrator.
 * csci2469 is the password
 *
 * TODO:
 * incorporate .mli for encapsulation and move all documentation to .mli
 * compile to native code, see asm output + performance
 * 
 * TO COMPILE:
 * ocaml >= 4.04.0
 * make utility - run "make" or 
 * ocamlc -o classmgr unix.cma str.cma oper.ml util.ml role.ml course.ml serialize.ml college.ml cmdfunc.ml cmd.ml state.ml main.ml
 * run program: ./classmgr
 *)

(** Entry Point - calls the start function from the state module *)
let _ = State.start ()
