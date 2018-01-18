(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let on_message ~none ~weak ~strong ~send_message text =
  try
    let parameter = ref
        (Compression_main.build_parameter
           ~called_from:Remanent_parameters_sig.Server
           ~send_message ~none ~weak ~strong) in
    match Yojson.Basic.from_string text with
    | `List [ `String "CONFIG"; conf ] ->
      let none = match Yojson.Basic.Util.to_bool_option
                         (Yojson.Basic.Util.member "none" conf)
      with None -> none | Some b -> b in
    let weak = match Yojson.Basic.Util.to_bool_option
                       (Yojson.Basic.Util.member "weak" conf)
      with None -> weak | Some b -> b in
    let strong = match Yojson.Basic.Util.to_bool_option
                         (Yojson.Basic.Util.member "strong" conf)
      with None -> strong | Some b -> b in
      let () = parameter :=
          Compression_main.build_parameter
            ~called_from:Remanent_parameters_sig.Server
            ~send_message
            ~none ~weak ~strong in
      Lwt.return_unit
    | `List [ `String "RUN"; json ] ->
      let env = Model.of_yojson (Yojson.Basic.Util.member "model" json) in
      let steps = Trace.of_yojson (Yojson.Basic.Util.member "trace" json) in
      let () = Compression_main.compress_and_print
          !parameter ~dotFormat:Causal.Html
          env (Compression_main.init_secret_log_info ())
          steps in
      Lwt.return_unit
    | x ->
      raise (Yojson.Basic.Util.Type_error ("Invalid KaStor message",x))
  with Yojson.Basic.Util.Type_error (e,x) ->
    let () = Format.eprintf "%s:@ %s@." e (Yojson.Basic.pretty_to_string x) in
    Lwt.return_unit (*TODO*)
     | e ->
       let () = Format.eprintf "%s@." (Printexc.to_string e) in
       Lwt.return_unit
