(* A Class Management System - serialize.ml
 * Kongmeng Lor
 * Instructor: Ahmed Naumaan
 * College: Saint Paul College
 * CSCI 2469-01 Advance Programming Principles
 * 05.11.2017
 *
 * The serialize module is dedicated to creating
 * a map that is serializable and suitable for
 * the two fields of the college: course map and role map
 *
 * todo: Check performance
 *)

(** [Serialize.Make T] is a functor
  * that takes another module [T] with a signature of polymorphic <type t>, <type data>
  * and a <val get_key>
  * to produce a Map of T.t
  * and inherit the serialize and deserialize functions
  *)
module Make = functor(T : sig
    type t and data
    val get_key : data -> t
  end) -> struct

  (* constructs a map of type t [type T.t] provided
   * by the Serialize.Make functor with the Map.Make functor
   * to the current namespace
   *)
  include Map.Make(struct
    type t = T.t
    let compare = compare
  end)

  (** [serialize f m] saves the data of a map [m] into a file [f] *)
  let serialize f m =
    let outchannel = open_out_bin f in
    try iter (fun x y -> output_value outchannel y) m with
    | _ -> close_out outchannel

  (** [deserialize f m] returns a map [m] of the data within a file [f]
    * concerns: static data records & performance? to check. *)
  let deserialize f m =
    let channel = open_in_bin f in
    let rec routine acc = 
      try
        let r = input_value channel in
        let acc = add (T.get_key r) r acc in
        routine acc
      with
      (* exception as "break" *)
      | End_of_file -> close_in channel; (acc)
    in routine m

  (** [length m] returns the size of the map [m] *)
  let length m = let n = ref 0 in let () = iter (fun _ _ -> n := !n + 1) m in !n

end