(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)


(* Primitives for handling rule rates when detecting symmetries*)

(* divide a rule rate by an int constant *)
(* to account for rate conventions and the cardinal of the stabiliser *)
val divide_expr_by_int:
  ('a,'b) Alg_expr.e Locality.annot ->
  int -> ('a,'b) Alg_expr.e Locality.annot

(* muiltiplu a rule rate by an int constant *)
val mult_expr_by_int:
    ('a,'b) Alg_expr.e Locality.annot ->
    int -> ('a,'b) Alg_expr.e Locality.annot

(* add to rule rate *)
(* to gather rule by isomorphic-class*)

val add:
  ('a,'b) Alg_expr.e Locality.annot ->
  ('a,'b) Alg_expr.e Locality.annot ->
  ('a,'b) Alg_expr.e Locality.annot

(* Partial normal form for expressions *)
(* We only deal with constant, single alg_var multiplied/divided by a constant,
   sum of two expr either both constant or dealing with the same alg_var *)
(* I think this is enough to deal with symmetries *)
(* We may be more complete later *)

type 'a corrected_rate_const

(* printer *)
val print :
  (Format.formatter -> 'a option -> unit) ->
  Format.formatter -> 'a corrected_rate_const option -> unit

(* conversion *)
val get_corrected_rate:
  ('a,'b) Alg_expr.e Locality.annot -> 'b corrected_rate_const option

(* partial equality test *)
(* true means "yes they are equal" *)
(* false means "either equal, or not"*)

val necessarily_equal:
  'a corrected_rate_const option -> 'a corrected_rate_const option -> bool
