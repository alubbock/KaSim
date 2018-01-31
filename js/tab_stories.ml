(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false


let navli () = ReactiveData.RList.empty

let none_checkbox = Html.input ~a:[Html.a_input_type `Checkbox] ()
let weak_checkbox = Html.input ~a:[Html.a_input_type `Checkbox; Html.a_checked ()] ()
let strong_checkbox = Html.input ~a:[Html.a_input_type `Checkbox] ()

let none_box = Tyxml_js.To_dom.of_input none_checkbox
let weak_box = Tyxml_js.To_dom.of_input weak_checkbox
let strong_box = Tyxml_js.To_dom.of_input strong_checkbox

let launch_button = Html.button
    ~a:[Html.a_class ["btn"; "btn-default"]; Html.a_button_type `Submit]
    [Html.pcdata "Launch"]

let%html setup_form =
  {|<form class="form-inline">
    <div class="form-group">
    <label>Compression level</label>
    <div class="checkbox"><label>|}[none_checkbox]{| None</label></div>
    <div class="checkbox"><label>|}[weak_checkbox]{| Weakly</label></div>
    <div class="checkbox"><label>|}[strong_checkbox]{| Strongly</label></div>
    </div>
    <div class="form-group">|}[launch_button]{|</div>
    </form>|}

let story_list, list_control = ReactiveData.RList.create []
let story_log, log_control = ReactiveData.RList.create []
let current_info, set_info = React.S.create ""

let rec inspect_stories () =
  State_project.with_project ~label:"Stories list"
    (fun manager ->
       let () =
         ReactiveData.RList.set list_control
           (Mods.IntMap.fold
              (fun id _ acc ->
                 Html.option
                   ~a:[Html.a_value (string_of_int id)]
                   (Html.pcdata (string_of_int id)
                      (*Printf.sprintf "%i: after %i events at t=%f"
                         d.Trace.Simulation_info.story_id
                         d.Trace.Simulation_info.story_event
                         d.Trace.Simulation_info.story_time*))
                 :: acc
              ) manager#story_list []) in
       let () =
         ReactiveData.RList.set log_control
           (List.rev_map
              (fun line -> Html.pcdata (line^"\n"))
              manager#story_log) in

       Lwt.return (Api_common.result_ok ())) >>= fun _ ->
  State_project.with_project ~label:"Stories computing"
    (fun manager ->
       Lwt.return (Api_common.result_ok manager#story_is_computing)) >>=
  Api_common.result_map
    ~ok:(fun _ b -> if b && React.S.value tab_is_active
          then Lwt_js.sleep 3. >>= inspect_stories
          else Lwt.return_unit)
    ~error:(fun _ _ -> Lwt.return_unit)

let select_stories = Tyxml_js.R.Html5.select story_list
let log_div = Tyxml_js.R.Html5.div
    ~a:[Html.a_class ["panel-pre";"panel-scroll"]]
    story_log

let graph_display_id = "story_graph_display"
let story_graph =
  Js_graphlogger.create_graph_logger
    graph_display_id (fun _ -> ())

let story_content =
    [Html.p ~a:[Html.a_class ["panel-pre"]]
       [Tyxml_js.R.Html5.pcdata current_info];
     Html.div ~a:[Html.a_id graph_display_id; Html.a_class ["flex-content"]] []]

let content () =
  [Html.div ~a:[Html.a_class ["col-md-3";"flex-content"]] [setup_form; log_div];
   Html.div ~a:[Html.a_class ["col-md-9";"flex-content"]]
     (select_stories::story_content)
  ]

let do_update_compression_level () =
  State_project.with_project ~label:"Config compression"
    (fun manager ->
       let none = Js.to_bool none_box##.checked in
       let weak = Js.to_bool weak_box##.checked in
       let strong = Js.to_bool strong_box##.checked in
       manager#config_story_computation ~none ~weak ~strong >|=
       Api_common.result_lift)

let update_compression_level =
  Dom_html.handler (fun _ ->
      let _ = do_update_compression_level () in
      Js._false)

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = Common.jquery_on
      "#navstories" "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navstories" "shown.bs.tab"
      (fun _ ->
         let () = tab_was_active := true in
         let () = set_tab_is_active true in
         let _ = do_update_compression_level () in
         Lwt.async inspect_stories
      ) in
  let () = none_box##.onchange := update_compression_level in
  let () = weak_box##.onchange := update_compression_level in
  let () = strong_box##.onchange := update_compression_level in
  let () =
      (Tyxml_js.To_dom.of_select select_stories)##.onchange :=
        Dom_html.full_handler (fun select _ ->
            let id = int_of_string (Js.to_string select##.value) in
            let _ =
              State_project.with_project ~label:"Launch stories"
                (fun manager ->
                   match Mods.IntMap.find_option id manager#story_list with
                   | None -> Lwt.return (Api_common.result_ok ())
                   | Some (d,v) ->
                     let () = set_info
                         (Format.asprintf "@[<v>%a@]"
                            (Pp.list
                               Pp.space
                               (Pp.list Pp.space (fun f d ->
                                    Format.fprintf f "%i: at t=%f after %i events"
                                      d.Trace.Simulation_info.story_id
                                      d.Trace.Simulation_info.story_time
                                      d.Trace.Simulation_info.story_event)))
                            d) in
                     let () =
                       story_graph##setData
                         (Js.string
                            (Yojson.Basic.to_string (Graph_json.to_json v))) in
                     Lwt.return (Api_common.result_ok ())
                ) in
            Js._false) in
  let () =
      (Tyxml_js.To_dom.of_button launch_button)##.onclick :=
        Dom_html.handler (fun _ ->
            let _ =
              State_project.with_project ~label:"Launch stories"
                (fun manager ->
                   if manager#story_is_computing then
                     Lwt.return (Api_common.result_ok ())
                   else
                     manager#simulation_raw_trace >>=
                     Api_common.result_bind_lwt
                       ~ok:(fun trace ->
                           manager#raw_launch_story_computation trace >|=
                           Api_common.result_lift)) in
            let () = Lwt.async inspect_stories in
            Js._false) in
  ()
let onresize () : unit = ()
