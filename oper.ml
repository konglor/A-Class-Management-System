(* A Class Management System - oper.ml
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.05.2017
 *)

(** [Bad_Match] exception is raised when a inputted value isnt the expected one *)
exception Bad_Match of string

(** [>> f g] a composition of function [f] and function [g] *)
let (>>) f g = (fun x -> f (g x))

(** [>>> f g] a composition of function [f] and function [g] where g receives two parameter *)
let (>>>) f g = (fun x y -> f (g x y))

(** [@$] gets the raw data [b] from an option data [Some b] *)
let (@$) = function
  | Some b -> b 
  | None -> raise (Invalid_argument "Found nothing")
