(**
  * export_to_Trace_Checker.mli
  * openkappa
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * Creation: Apr 10 2017
  * Last modification: Time-stamp: <Apr 11 2017>
  * *
  *
  * Copyright 2010,2011,2012,2013,2015,2016,2017
  * Institut National de Recherche en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


type state

val init:
  ?compil:Ast.parsing_compil -> unit -> state

val query_inhibition_map:
  ?accuracy_level:Remanent_state.accuracy_level ->
  state ->
  Remanent_state.rule_id ->
  Remanent_state.rule_id ->
  state * (Remanent_state.location * Remanent_state.location) list

val dump_errors_light: state -> unit


val flush_errors: state -> state
