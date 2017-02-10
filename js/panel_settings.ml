(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module UIState = Ui_state
module Html = Tyxml_js.Html5
module R = Tyxml_js.R

let simulation_pause_id = "simulation_limit"
let simulation_pause_input =
  Html.input
    ~a:[Html.a_id simulation_pause_id ;
        Html.a_input_type `Text;
        Html.a_class ["form-control"];
        Html.a_placeholder "[T] > 100" ;
        Tyxml_js.R.Html.a_value UIState.model_pause_condition
       ]
    ()

let format_float_string value =
  let n = string_of_float value in
  let length = String.length n in
  if length > 0 && String.get n (length - 1) = '.' then
    n^"0"
  else
    n

let plot_period_input =
  Html.input
    ~a:[Html.a_input_type `Number;
        Html.a_class [ "form-control"];
        Html.a_placeholder "time units";
        Tyxml_js.R.Html.a_value
          (React.S.l1 format_float_string UIState.model_plot_period)]
    ()

let perturbation_code_id = "perturbation_code"
let perturbation_code_input =
  Html.input
    ~a:[Html.a_id perturbation_code_id;
        Html.a_input_type `Text;
        Html.a_class ["form-control"];
        Html.a_placeholder "Simulation Perturbation";]
    ()

let signal_change input_dom signal_handler =
  input_dom##.onchange :=
    Dom_html.handler
      (fun _ ->
         let () =
           signal_handler
             (Js.to_string
                (input_dom##.value))
         in Js._true)

let error_messages signal =
  Html.div
    ~a:[Html.a_class ["panel-heading" ;
                      "panel-pre" ;
                      "panel-message" ;] ]
    [Tyxml_js.R.Html.pcdata
       (React.S.bind
          signal
          (fun error ->
             React.S.const
               (match error with
                | [] -> ""
                | h::_ -> h.Api_types_j.message_text
               )
          )
       )
    ]

let start_button_id = "start_button"
let start_button =
  Html.button
    ~a:([ Html.a_id start_button_id ;
          Html.Unsafe.string_attrib "type" "button" ;
          Html.a_class [ "btn" ;
                         "btn-default" ; ] ; ])
    [ Html.cdata "start" ]

let clear_button_id = "clear_button"
let clear_button =
  Html.button
    ~a:[ Html.a_id clear_button_id
       ; Html.Unsafe.string_attrib "type" "button"
       ; Html.a_class ["btn" ;
                       "btn-default" ; ] ]
    [ Html.cdata "clear" ]

let pause_button_id = "pause_button"
let pause_button =
  Html.button
    ~a:[ Html.a_id pause_button_id
       ; Html.Unsafe.string_attrib "type" "button"
       ; Html.a_class ["btn" ;
                       "btn-default" ; ] ]
    [ Html.cdata "pause" ]

let continue_button_id = "continue_button"
let continue_button =
  Html.button
    ~a:[ Html.a_id continue_button_id
       ; Html.Unsafe.string_attrib "type" "button"
       ; Html.a_class ["btn" ;
                       "btn-default" ; ] ]
    [ Html.cdata "continue" ]

let perturbation_button_id = "perturbation_button"
let perturbation_button =
  Html.button
    ~a:[ Html.a_id perturbation_button_id
       ; Html.Unsafe.string_attrib "type" "button"
       ; Html.a_class ["btn" ;
                       "btn-default" ; ] ]
    [ Html.cdata "perturbation" ]

let select_default_runtime = [ UIState.WebWorker ;
                               UIState.Embedded ; ]
let select_runtime_options, select_runtime_options_handle =
  ReactiveData.RList.create select_default_runtime
let select_runtime =
  Tyxml_js.R.Html.select
    (ReactiveData.RList.map
       (fun runtime -> Html.option
           ~a:[Html.a_value
                 (UIState.runtime_value runtime)]
           (Html.pcdata (UIState.runtime_label runtime)))
       select_runtime_options)

let hidden_class = ["hidden"]
let visible_class = ["visible"]
let visible_on_states
    ?(a_class=[])
    (state : Ui_simulation.ui_status list) : string list React.signal =
  (React.S.bind
     (Ui_simulation.simulation_status ())
     (fun run_state ->
        React.S.const
          (if List.mem run_state state then
             a_class@visible_class
           else
             a_class@hidden_class)
     )
  )

let progress_bar
    (percent_signal : int Tyxml_js.R.Html.wrap)
    (value_signal : string React.signal) =
  Html.div
    ~a:[ Html.Unsafe.string_attrib "role" "progressbar" ;
         Tyxml_js.R.Html.Unsafe.int_attrib "aria-valuenow" percent_signal ;
         Html.Unsafe.int_attrib "aria-valuemin" 0 ;
         Html.Unsafe.int_attrib "aria-valuemax" 100 ;
         Tyxml_js.R.Html.Unsafe.string_attrib
           "style"
           (React.S.map
              (fun s -> Format.sprintf "width: %d%%;" s)
              percent_signal) ;
         Html.a_class ["progress-bar"] ]
    [ Tyxml_js.R.Html.pcdata
        (React.S.bind
           value_signal
           (fun value -> React.S.const value)
        )
    ]

let lift f x = match x with | None -> None | Some x -> f x
let time_progress_bar  () =
  let simulation_output = (Ui_simulation.simulation_output ()) in
  progress_bar
    (React.S.map (fun state ->
         let time_percent : int option =
           lift
             (fun (status : Api_types_j.simulation_info) ->
                status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_time_percentage )
             state
         in
         let time_percent : int = Tools.unsome 100 time_percent in
         time_percent
       )
        simulation_output)
    (React.S.map (fun state ->
         let time : float option =
           lift (fun (status : Api_types_j.simulation_info) ->
             Some status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_time) state in
         let time : float = Tools.unsome 0.0 time in
         Printf.sprintf "%.4g" time
       )
       simulation_output)

let event_progress_bar () =
  let simulation_output = (Ui_simulation.simulation_output ()) in
  progress_bar
    (React.S.map (fun state ->
         let event_percentage : int option =
           lift (fun (status : Api_types_j.simulation_info) ->
               status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_event_percentage) state in
         let event_percentage : int = Tools.unsome 100 event_percentage in
         event_percentage
       )
       simulation_output)
    (React.S.map (fun status ->
         let event : int option =
           lift (fun (status : Api_types_j.simulation_info) ->
               Some status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_event)
             status
         in
         let event : int = Tools.unsome 0 event in
         string_of_int event
       )
        simulation_output)

let tracked_events state =
  let tracked_events : int option =
    lift (fun (status : Api_types_j.simulation_info) ->
        status.Api_types_j.simulation_info_progress.Api_types_j.simulation_progress_tracked_events)
      state
  in
  match tracked_events with
    None -> None
  | Some tracked_events ->
    if tracked_events > 0 then
      Some tracked_events
    else
      None

let tracked_events_count () =
  let simulation_output = (Ui_simulation.simulation_output ()) in
  Tyxml_js.R.Html.pcdata
    (React.S.map (fun state -> match tracked_events state with
           Some tracked_events -> string_of_int tracked_events
         | None -> " "
       )
        simulation_output)

let tracked_events_label () =
  let simulation_output = (Ui_simulation.simulation_output ()) in
  Tyxml_js.R.Html.pcdata
    (React.S.map (fun state -> match tracked_events state with
           Some _ -> "tracked events"
         | None -> " "
       )
        simulation_output)

let status_indicator () =
  Html.div
    ~a:[ Html.a_class [ "col-md-2" ] ]
    (Ui_common.level
    ~debug:(Tyxml_js.R.Html.pcdata
              (React.S.bind
                 (Ui_simulation.simulation_status ())
                 (fun status ->
                    React.S.const
                      (match status with
                       | Ui_simulation.STOPPED -> "stopped"
                       | Ui_simulation.INITALIZING -> "initalizing"
                       | Ui_simulation.RUNNING -> "running"
                       | Ui_simulation.PAUSED -> "paused"
                      )
                 )
              )) ())

let perturbation_control () =
  Html.div
    ~a:[ Tyxml_js.R.Html.a_class
           (visible_on_states ~a_class:["row"] [Ui_simulation.PAUSED]) ]
    [Html.div ~a:[Html.a_class [ "col-md-10" ]] [ perturbation_code_input ] ;
     Html.div ~a:[Html.a_class [ "col-md-2"  ]] [ perturbation_button ] ; ]
let initializing_xml () =
  Html.div
    ~a:[ Tyxml_js.R.Html.a_class
           (visible_on_states [ Ui_simulation.INITALIZING ; ])
       ]
  [[%html {|
  <div class="panel-body panel-controls">
    |}[ Html.entity "nbsp" ]{|
  </div>|}]]

let alert_messages =
  Html.div
    ~a:[Tyxml_js.R.Html.a_class
          (React.S.bind
             UIState.model_error
             (fun error ->
                React.S.const
                  (match error with
                   | None -> [ "alert-sm" ; "alert" ; ]
                   | Some _ -> [ "alert-sm" ; "alert" ; "alert-danger" ; ]
                  )
             )
          );
       ]
    [Tyxml_js.R.Html.pcdata
       (React.S.bind
          UIState.model_error
          (fun error ->
             React.S.const
               (match error with
                | None -> ""
                | Some localized_errors ->
                  (match localized_errors.Ui_state.model_error_messages with
                   | [] -> ""
                   | h::_ -> h.Api_types_j.message_text)
               )
          )
       )
    ]

let stopped_xml () =
  Html.div
    ~a:[ Tyxml_js.R.Html.a_class
           (visible_on_states
              ~a_class:[ "panel-body" ;
                         "panel-controls" ; ]
              [Ui_simulation.STOPPED ;
               Ui_simulation.PAUSED ; ])
       ]
  [%html {|
     <div class="row">
        <div class="col-md-3">
        <form class="form-horizontal">
          <div class="form-group">
           <label class="col-sm-5">Pause if</label>
           <div class="col-sm-7">|}[ simulation_pause_input ]{|</div>
          </div>
      |}[ Html.div
            ~a:[ Tyxml_js.R.Html.a_class
                   (visible_on_states
                      ~a_class:[ "form-group" ]
                      [ Ui_simulation.STOPPED ; ]) ]
            [ Html.label ~a:[Html.a_class ["col-sm-5"] ]
                [ Html.pcdata  "Plot period" ] ;
              Html.div ~a:[Html.a_class ["col-sm-7"] ] [plot_period_input] ]
        ]{|
        </form></div>
        <div class="col-md-9">|}[ alert_messages ]{|</div>
     </div>

     |}[ perturbation_control () ]{|
   |}]

let running_xml () =
  Html.div
    ~a:[ Tyxml_js.R.Html.a_class
           (visible_on_states
              ~a_class:[ "panel-body" ;
                         "panel-controls" ; ]
              [ Ui_simulation.RUNNING ; ]) ]
  [%html {|
     <div class="row">
        <div class="col-md-4">
            <div class="progress">
            |}[ event_progress_bar () ]{|
            </div>
        </div>
        <div class="col-md-2">events</div>
     </div>

     <div class="row">
        <div class="col-md-4">
            <div class="progress">
            |}[ time_progress_bar () ]{|
            </div>
        </div>
        <div class="col-md-2">time</div>
     </div>

     <div class="row">
        <div class="col-md-4">
           |}[ tracked_events_count () ]{|
        </div>
        <div class="col-md-2">
           |}[ tracked_events_label ()]{|
        </div>
     </div>
   |}]
let controls_xml () =
  [Html.div
     ~a:[ Tyxml_js.R.Html.a_class
            (visible_on_states
               ~a_class:[ "col-md-2" ]
               [ Ui_simulation.STOPPED ; ]) ]
     [ start_button ] ;
   Html.div
     ~a:[ Tyxml_js.R.Html.a_class
            (visible_on_states
               ~a_class:[ "col-md-2" ]
               [ Ui_simulation.PAUSED ; ]) ]
     [ continue_button ] ;
   Html.div
            ~a:[ Tyxml_js.R.Html.a_class
                   (visible_on_states
                     ~a_class:[ "col-md-2" ]
                     [ Ui_simulation.RUNNING ; ]) ]
            [ pause_button ] ;
   status_indicator () ;
   Html.div
     ~a:[ Tyxml_js.R.Html.a_class
            (visible_on_states
               ~a_class:[ "col-md-2" ]
               [ Ui_simulation.PAUSED ;
                 Ui_simulation.RUNNING ; ]) ]
     [ clear_button ] ;
   Html.div
     ~a:[ Tyxml_js.R.Html.a_class
            (visible_on_states
               ~a_class:[ "col-md-2" ]
               [ Ui_simulation.STOPPED ; ]) ]
     [ select_runtime ] ;
   Html.entity "nbsp" ;

  ]
let footer_xml () =
  [%html {|
  <div class="panel-footer">
      |}[ Html.div
            ~a:[ Html.a_class [ "row"; ] ]
            (controls_xml ()) ]{|
  </div>|}]

let configuration_id = "configuration-id"
let content  () =
  [Html.div
    ~a:[ Html.a_id configuration_id;
         R.Html.a_class
           (React.S.bind
              UIState.model_error
              (fun e -> React.S.const
                  ("panel" ::
                   (match e with
                    | None -> ["panel-default"]
                    | Some localized_errors ->
                      (match localized_errors.Ui_state.model_error_messages with
                       | [] -> ["panel-default"]
                       | { Api_types_t.message_severity = `Error ; _ }::_ -> ["panel-danger"]
                       | { Api_types_t.message_severity = `Warning ; _ }:: _ -> ["panel-warning"]
                       | { Api_types_t.message_severity = `Info ; _ }::_ -> ["panel-info"]
                      )
                   )
                  )
              )
           )]
    [ initializing_xml () ;
      stopped_xml () ;
      running_xml () ;
      footer_xml () ; ] ]

let onload () : unit =
  let perturbation_button_dom =
    Tyxml_js.To_dom.of_button perturbation_button
  in
  let select_runtime_dom =
    Tyxml_js.To_dom.of_select select_runtime
  in
  let start_button_dom =
    Tyxml_js.To_dom.of_button start_button
  in
  let pause_button_dom =
    Tyxml_js.To_dom.of_button pause_button
  in
  let clear_button_dom =
    Tyxml_js.To_dom.of_button clear_button
  in
  let continue_button_dom =
    Tyxml_js.To_dom.of_button continue_button
  in
  let perturbation_code_input_dom =
    Tyxml_js.To_dom.of_input perturbation_code_input
  in

  let args = Url.Current.arguments in
  let set_runtime
      (runtime : Ui_state.runtime)
      (continuation : unit -> unit) =
    let r_val = Ui_state.runtime_value runtime in
    Ui_state.set_runtime_url
      r_val
      (fun success ->
         if success then
           select_runtime_dom##.value := Js.string r_val
         else
           continuation ())
  in
  let default_runtime () =
    set_runtime
      UIState.default_runtime
      (fun _ -> ())
  in
  let () =
    try
      let hosts = List.filter (fun (key,_) -> key = "host") args in
      let hosts : Ui_state.remote option list =
        List.map (fun (_,url) -> Ui_state.parse_remote url) hosts in
      let () = List.iter
          (fun value ->
             match value
             with
             | Some remote ->
               ReactiveData.RList.cons
                 (Ui_state.Remote remote)
                 select_runtime_options_handle
             | None -> ())
          hosts
      in
      match ReactiveData.RList.value select_runtime_options
      with
      | head::_ -> set_runtime head (default_runtime)
      | _ -> default_runtime ()
    with _ -> default_runtime () in
  let run_perturbation () : unit =
    Common.async
      (fun _ ->
         let code : string =
           Js.to_string perturbation_code_input_dom##.value
         in
         Ui_simulation.perturb_simulation ~code:code)
  in
  let () = continue_button_dom##.onclick :=
      Dom.handler
        (fun _ ->
           let () =
             Common.async
               (fun _ -> Ui_simulation.continue_simulation ()) in
           Js._true)
  in
  let () = pause_button_dom##.onclick :=
      Dom.handler
        (fun _ ->
           let () =
             Common.async
               (fun _ -> Ui_simulation.pause_simulation ()) in
           Js._true)
  in
  let () = clear_button_dom##.onclick :=
      Dom.handler
        (fun _ ->
           let () =
             Common.async
               (fun _ -> Ui_simulation.stop_simulation ())
           in
           Js._true)
  in
  let () = start_button_dom##.onclick :=
      Dom.handler
        (fun _ ->
           let () = Common.async
               (fun _ -> Ui_simulation.start_simulation ()) in
           Js._true)
  in
  let () = select_runtime_dom##.onchange :=
      Dom.handler
        (fun _ ->
           let () = UIState.set_runtime_url
               (Js.to_string select_runtime_dom##.value)
               (fun success ->
                  if success then
                    ()
                  else
                    select_runtime_dom##.value :=
                      Js.string
                        (UIState.runtime_value UIState.default_runtime)
               ) in
           Js._true
        )
  in
  let () = signal_change (Tyxml_js.To_dom.of_input simulation_pause_input)
      (fun value ->
         let v' = if value = "" then "[false]" else value in
           UIState.set_model_pause_condition v') in
  let () = signal_change (Tyxml_js.To_dom.of_input plot_period_input)
      (fun value ->
         try UIState.set_model_plot_period (float_of_string value)
         with | Not_found | Failure _ -> ()) in
  let () = perturbation_button_dom##.onclick :=
      Dom.handler (fun _ -> let () = run_perturbation () in Js._true) in

  ()
let onresize () : unit = ()