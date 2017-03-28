(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let divide_expr_by_int e i =
  Locality.dummy_annot
      (Alg_expr.BIN_ALG_OP
              (Operator.DIV, e, Locality.dummy_annot (Alg_expr.CONST (Nbr.I i))))

type ('a,'b) corrected_rate_const =
  {
    num: Nbr.t ;
    den: Nbr.t ;
    var: ('a,'b) Alg_expr.e Locality.annot option
  }

let rec simplify expr =
  match expr
  with
  | Alg_expr.BIN_ALG_OP (op,a,b),loc ->
    begin
      let a,b = simplify a, simplify b in
      match op with
      | Operator.SUM ->
        begin
          match a,b with
          | (Alg_expr.CONST a,_), (Alg_expr.CONST b,_) ->
            Alg_expr.CONST (Nbr.add a b),loc
          | (Alg_expr.CONST a,_),_ when Nbr.is_zero a -> b
          | _,(Alg_expr.CONST b,_) when Nbr.is_zero b -> a
          | ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _
             | Alg_expr.TOKEN_ID _
             | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
             | Alg_expr.DIFF_TOKEN _ ),_),
            ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
             | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
             | Alg_expr.DIFF_TOKEN _),_)
            -> Alg_expr.BIN_ALG_OP(op,a,b),loc
        end
      | Operator.MINUS ->
        begin
          match a,b with
          | (Alg_expr.CONST a,_), (Alg_expr.CONST b,_) ->
            Alg_expr.CONST (Nbr.sub a b),loc
          | _,(Alg_expr.CONST b,_) when Nbr.is_zero b -> a
          | ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
             | Alg_expr.IF _ | Alg_expr.DIFF_TOKEN  _
             | Alg_expr.DIFF_KAPPA_INSTANCE _),_),
            ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
             | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
             | Alg_expr.DIFF_TOKEN _),_) ->
            Alg_expr.BIN_ALG_OP(op,a,b),loc
        end
      | Operator.MULT ->
        begin
          match a,b with
          | (Alg_expr.CONST a,_), (Alg_expr.CONST b,_) ->
            Alg_expr.CONST (Nbr.mult a b),loc
          | (Alg_expr.CONST a',_),_ when Nbr.is_equal a' Nbr.zero -> a
          | _,(Alg_expr.CONST b',_) when Nbr.is_equal b' Nbr.zero -> b
          | (Alg_expr.CONST a,_),_ when Nbr.is_equal a Nbr.one -> b
          | _,(Alg_expr.CONST b,_) when Nbr.is_equal b Nbr.one -> a
          | ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
             | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
             | Alg_expr.DIFF_TOKEN _),_),
            ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
             | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
             | Alg_expr.DIFF_TOKEN _),_)
            -> Alg_expr.BIN_ALG_OP(op,a,b),loc
        end
      | Operator.DIV ->
        begin
          match a,b with
          | _,(Alg_expr.CONST b,_) when Nbr.is_equal b Nbr.one -> a
          | ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
             | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
             | Alg_expr.DIFF_TOKEN _),_),
            ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _ | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _ | Alg_expr.DIFF_TOKEN _),_) -> Alg_expr.BIN_ALG_OP(op,a,b),loc
        end
      | Operator.POW | Operator.MODULO ->
        begin
          match a,b with
          | _,(Alg_expr.CONST b,_) when Nbr.is_equal b Nbr.one -> a
          | ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
             | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
             | Alg_expr.DIFF_TOKEN _),_),
            ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
             | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
             | Alg_expr.KAPPA_INSTANCE _
             | Alg_expr.TOKEN_ID _
             | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
             | Alg_expr.DIFF_TOKEN _),_) -> Alg_expr.BIN_ALG_OP(op,a,b),loc
        end
      | Operator.MIN | Operator.MAX ->
        begin
          Alg_expr.BIN_ALG_OP(op,a,b),loc
        end
    end
    | Alg_expr.UN_ALG_OP (op,a),loc ->
      let a = simplify a in
      begin
        match op with
        | Operator.UMINUS ->
          begin
            match a with
              Alg_expr.CONST a,_ -> Alg_expr.CONST (Nbr.neg a),loc
            | (Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
              | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
              | Alg_expr.KAPPA_INSTANCE _
              | Alg_expr.TOKEN_ID _
              | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
              | Alg_expr.DIFF_TOKEN _ ),_ -> Alg_expr.UN_ALG_OP(op,a),loc
          end
        | Operator.COSINUS | Operator.EXP ->
        begin
          match a with
          | Alg_expr.CONST a,_ when Nbr.is_zero a ->
            Alg_expr.CONST Nbr.one,loc
          | (Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
            | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
            | Alg_expr.KAPPA_INSTANCE _
            | Alg_expr.TOKEN_ID _
            | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
            | Alg_expr.DIFF_TOKEN _ ),_
            ->
            Alg_expr.UN_ALG_OP(op,a),loc
        end
        | Operator.SINUS | Operator.TAN ->
        begin
          match a with
          | Alg_expr.CONST a,_ when Nbr.is_equal a Nbr.one ->
            Alg_expr.CONST Nbr.zero,loc
          | (Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
            | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
            | Alg_expr.KAPPA_INSTANCE _
            | Alg_expr.TOKEN_ID _
            | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
            | Alg_expr.DIFF_TOKEN _ ),_
            ->
            Alg_expr.UN_ALG_OP(op,a),loc
        end
        | Operator.SQRT
        | Operator.LOG  | Operator.INT
          -> Alg_expr.UN_ALG_OP(op,a),loc
      end
    | Alg_expr.DIFF_KAPPA_INSTANCE (expr,mix),loc ->
      begin
        let expr = simplify expr in
        match expr with
        |  Alg_expr.CONST _,_ -> Alg_expr.CONST (Nbr.zero),loc
        | (Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
          | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
          | Alg_expr.KAPPA_INSTANCE _
          | Alg_expr.TOKEN_ID _
          | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
          | Alg_expr.DIFF_TOKEN _ ),_->
          Alg_expr.DIFF_KAPPA_INSTANCE (expr,mix),loc
      end
    | Alg_expr.DIFF_TOKEN (expr,token),loc ->
      begin
        let expr = simplify expr in
        match expr with
        |  Alg_expr.CONST _,_ -> Alg_expr.CONST (Nbr.zero),loc
        | (Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
          | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
          | Alg_expr.KAPPA_INSTANCE _
          | Alg_expr.TOKEN_ID _
          | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
          | Alg_expr.DIFF_TOKEN _ ),_->
          Alg_expr.DIFF_TOKEN (expr,token),loc
      end
    | Alg_expr.STATE_ALG_OP _,_
    | Alg_expr.ALG_VAR _,_
    | Alg_expr.KAPPA_INSTANCE _,_
    | Alg_expr.TOKEN_ID _,_
    | Alg_expr.CONST _,_ -> expr
    | Alg_expr.IF (cond,yes,no),loc ->
      let cond,yes,no =
        simplify_bool cond, simplify yes, simplify no
      in
      begin
        match cond with
        | Alg_expr.TRUE,_ -> yes
        | Alg_expr.FALSE, _ -> no
        | Alg_expr.BOOL_OP (_,_,_),_
        | Alg_expr.COMPARE_OP (_,_,_),_ -> Alg_expr.IF (cond,yes,no),loc
      end
and simplify_bool expr_bool =
  match expr_bool with
  | Alg_expr.TRUE, _ | Alg_expr.FALSE, _ -> expr_bool
  | Alg_expr.BOOL_OP (op, a ,b),loc ->
    begin
      let a,b = simplify_bool a, simplify_bool b in
      match
        op
      with
      | Operator.AND ->
        begin
          match a,b with
          | (Alg_expr.TRUE,_),_ -> b
          | (Alg_expr.FALSE,_),_ -> a
          | _,(Alg_expr.TRUE,_) -> a
          | _,(Alg_expr.FALSE,_) -> b
          | ((Alg_expr.BOOL_OP (_,_,_)
             | Alg_expr.COMPARE_OP (_,_,_)),_),
            ((Alg_expr.BOOL_OP (_,_,_)
             | Alg_expr.COMPARE_OP (_,_,_)),_)
            -> Alg_expr.BOOL_OP(op,a,b),loc
        end
      | Operator.OR ->
        begin
          match a,b with
          | (Alg_expr.TRUE,_),_ -> a
          | (Alg_expr.FALSE,_),_ -> b
          | _,(Alg_expr.TRUE,_) -> b
          | _,(Alg_expr.FALSE,_) -> a
          | ((Alg_expr.BOOL_OP (_,_,_)
             | Alg_expr.COMPARE_OP (_,_,_)),_),
            ((Alg_expr.BOOL_OP (_,_,_)
             | Alg_expr.COMPARE_OP (_,_,_)),_)
            -> Alg_expr.BOOL_OP(op,a,b),loc
        end
    end
  | Alg_expr.COMPARE_OP (op,a,b),loc ->
    let a,b = simplify a, simplify b in
    match a,b with
    | (Alg_expr.CONST a,_), (Alg_expr.CONST b,_) ->
      begin
        match op
        with
          Operator.GREATER ->
          if Nbr.is_greater a b
          then
            Alg_expr.TRUE, loc
          else
            Alg_expr.FALSE, loc
        | Operator.SMALLER ->
        if Nbr.is_smaller a b
        then
          Alg_expr.TRUE, loc
        else
          Alg_expr.FALSE, loc
        | Operator.EQUAL ->
          if Nbr.is_equal a b
          then
            Alg_expr.TRUE, loc
          else
            Alg_expr.FALSE, loc
        | Operator.DIFF ->
        if Nbr.is_equal a b
        then
          Alg_expr.FALSE, loc
        else
          Alg_expr.TRUE, loc
      end
    | ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
       | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
       | Alg_expr.KAPPA_INSTANCE _
       | Alg_expr.TOKEN_ID _
       | Alg_expr.IF _
       | Alg_expr.DIFF_TOKEN _ | Alg_expr.DIFF_KAPPA_INSTANCE _) ,_),
      ((Alg_expr.CONST _ | Alg_expr.ALG_VAR _ | Alg_expr.BIN_ALG_OP _
       | Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
       | Alg_expr.KAPPA_INSTANCE _
       | Alg_expr.TOKEN_ID _
       | Alg_expr.IF _
       | Alg_expr.DIFF_TOKEN _ | Alg_expr.DIFF_KAPPA_INSTANCE _),_) -> Alg_expr.COMPARE_OP(op,a,b),loc


let rec clean expr =
  let expr = fst expr in
  match expr
  with
  | Alg_expr.BIN_ALG_OP (op,a,b) ->
    Locality.dummy_annot
      (Alg_expr.BIN_ALG_OP
         (op, clean a, clean b))
  | Alg_expr.UN_ALG_OP (op,a) ->
    Locality.dummy_annot
      (Alg_expr.UN_ALG_OP (op, clean a))
  | Alg_expr.DIFF_TOKEN (expr,dt) ->
    Locality.dummy_annot
      (Alg_expr.DIFF_TOKEN (clean expr,dt))
  | Alg_expr.DIFF_KAPPA_INSTANCE(expr,dt) ->
    Locality.dummy_annot
      (Alg_expr.DIFF_KAPPA_INSTANCE (clean expr,dt))
  | Alg_expr.STATE_ALG_OP _
  | Alg_expr.ALG_VAR _
  | Alg_expr.KAPPA_INSTANCE _
  | Alg_expr.TOKEN_ID _
  | Alg_expr.CONST _ ->
    Locality.dummy_annot expr
  | Alg_expr.IF (cond,yes,no) ->
    Locality.dummy_annot
      (Alg_expr.IF (clean_bool cond, clean yes, clean no))
and clean_bool expr_bool=
  let expr = fst expr_bool in
  match expr with
  | Alg_expr.TRUE
  | Alg_expr.FALSE ->
    Locality.dummy_annot expr
  | Alg_expr.BOOL_OP (op,a,b) ->
    Locality.dummy_annot (Alg_expr.BOOL_OP (op,clean_bool a,clean_bool b))
  | Alg_expr.COMPARE_OP (op,a,b) ->
    Locality.dummy_annot (Alg_expr.COMPARE_OP (op,clean a,clean b))

let rec get_corrected_rate e =
  match e with
    Alg_expr.BIN_ALG_OP
      (Operator.MULT,(Alg_expr.CONST cst,_),e),_
  | Alg_expr.BIN_ALG_OP
      (Operator.MULT,e,(Alg_expr.CONST cst,_)),_
    ->
    begin
      match get_corrected_rate e with
      | None -> None
      | Some corrected_rate ->
        Some {corrected_rate with num = Nbr.mult cst corrected_rate.num}
    end
  | Alg_expr.BIN_ALG_OP
      (Operator.DIV,e,(Alg_expr.CONST cst,_)),_ ->
    begin
      match get_corrected_rate e with
      | None -> None
      | Some corrected_rate ->
        Some {corrected_rate with den = Nbr.mult cst corrected_rate.den}
    end
  | Alg_expr.BIN_ALG_OP
      (Operator.SUM,e1,e2),_ ->
    begin
      match get_corrected_rate e1 with
      | None -> None
      | Some corrected_rate1 ->
        begin
          match get_corrected_rate e2 with
          | Some corrected_rate2
            when compare corrected_rate1.var corrected_rate2.var = 0
              && Nbr.is_equal corrected_rate1.den corrected_rate2.den
            ->
            Some
              {
                corrected_rate1 with
                num = Nbr.add corrected_rate1.num corrected_rate2.num;
              }
          | Some corrected_rate2 when compare corrected_rate1.var corrected_rate2.var = 0 ->
            Some
              {
                corrected_rate1 with
                num =
                  Nbr.add
                    (Nbr.mult corrected_rate2.den corrected_rate1.num)
                    (Nbr.mult corrected_rate1.den corrected_rate2.num);
                den = Nbr.mult corrected_rate1.den corrected_rate2.den;
          }
          | None | Some _ -> None
        end
    end
  | Alg_expr.BIN_ALG_OP
      ((Operator.MULT | Operator.DIV | Operator.MINUS |
        Operator.POW | Operator.MODULO | Operator.MAX | Operator.MIN),_,_),_
  | (Alg_expr.UN_ALG_OP _ | Alg_expr.STATE_ALG_OP _
    | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
    | Alg_expr.DIFF_TOKEN _ | Alg_expr.DIFF_KAPPA_INSTANCE _
    | Alg_expr.IF _) ,_ -> None
  | Alg_expr.ALG_VAR _,_ ->
    Some
      {
        var = Some e ;
        num = Nbr.one ;
        den = Nbr.one }
  | Alg_expr.CONST cst,_ ->
    Some
      { var = None ;
        num = cst ;
        den = Nbr.one
      }

let get_corrected_rate e =
  get_corrected_rate (clean e)

let print pr_var f corrected_rate_const =
  match corrected_rate_const with
  | None -> Format.fprintf f "None"
  | Some a ->
    begin
      match a.var with
      | Some _ ->
        (Format.fprintf f "(%a/%a).%a"
           Nbr.print a.num Nbr.print a.den pr_var a.var)
      | None ->
        Format.fprintf f "(%a/%a)" Nbr.print a.num Nbr.print a.den
    end

let necessarily_equal a_opt b_opt =
  match a_opt,b_opt
  with
  | None,_ | _,None -> false
  | Some a, Some b ->
    a.var = b.var
    && Nbr.is_equal (Nbr.mult a.num b.den) (Nbr.mult a.den b.num)

let dep empty add_mixture add_token union dep_env ?time_var expr =
  let rec aux add_mixture add_token union dep_env expr accu =
    match fst expr with
    | Alg_expr.BIN_ALG_OP (_,e1,e2) | Alg_expr.IF(_,e1,e2) ->
    aux add_mixture add_token union dep_env e1
      (aux add_mixture add_token union dep_env e2 accu)
    | Alg_expr.UN_ALG_OP (_,e)
    | Alg_expr.DIFF_TOKEN (e,_)
    | Alg_expr.DIFF_KAPPA_INSTANCE (e,_)
      -> aux add_mixture add_token union dep_env e accu
    | Alg_expr.STATE_ALG_OP Operator.TIME_VAR ->
      begin
        match time_var with
        | Some id -> add_mixture id accu
        | None ->
          raise
            (ExceptionDefn.Internal_Error
               ("A variable for time shall be provided to analyse the dependences in a time-dependent expression",snd expr))
      end
    | Alg_expr.STATE_ALG_OP
        (Operator.CPUTIME | Operator.EVENT_VAR | Operator.NULL_EVENT_VAR | Operator.TMAX_VAR | Operator.EMAX_VAR) -> accu
    | Alg_expr.ALG_VAR id ->
      union (dep_env id) accu
    | Alg_expr.KAPPA_INSTANCE mix ->
      add_mixture mix accu
    | Alg_expr.TOKEN_ID id ->
      add_token id accu
    | Alg_expr.CONST _ -> accu
  in
  aux add_mixture add_token union dep_env expr empty

let rec diff_gen f_mix f_token f_symb f_time expr =
  match fst expr with
  | Alg_expr.IF(b,e1,e2) ->
    Locality.dummy_annot
      (Alg_expr.IF
         (b,
          diff_gen f_mix f_token f_symb f_time e1,
          diff_gen f_mix f_token f_symb f_time e2))
  | Alg_expr.BIN_ALG_OP (op,e1,e2) ->
    begin
      match op with
      | Operator.SUM ->
        Alg_expr.add
          (diff_gen f_mix f_token f_symb f_time e1)
          (diff_gen f_mix f_token f_symb f_time e2)
      | Operator.MULT ->
        Alg_expr.add
          (Alg_expr.mult
             e1
             (diff_gen f_mix f_token f_symb f_time e2))
          (Alg_expr.mult
             e2
             (diff_gen f_mix f_token f_symb f_time e1))
      | Operator.MINUS ->
        Alg_expr.minus
          (diff_gen f_mix f_token f_symb f_time e1)
          (diff_gen f_mix f_token f_symb f_time e2)
      | Operator.MIN | Operator.MAX ->
        Alg_expr.int 0
      | Operator.MODULO ->
        diff_gen f_mix f_token f_symb f_time e1
      | Operator.DIV ->
        Alg_expr.div
          (Alg_expr.minus
             (Alg_expr.mult
                (diff_gen f_mix f_token f_symb f_time e1)
                e2)
             (Alg_expr.mult
                (diff_gen f_mix f_token f_symb f_time e2)
                e1)
          )
          (Alg_expr.pow
               e2
               (Alg_expr.int 2)
          )
      | Operator.POW ->  (* (u^v)*(v'*ln(u)+v*u'/u) *)
        Alg_expr.mult
          (Alg_expr.pow e1 e2)
          (Alg_expr.add
             (Alg_expr.mult
                (diff_gen f_mix f_token f_symb f_time e2)
                (Alg_expr.ln e1))
             (Alg_expr.div
                (Alg_expr.mult
                   e2
                   (diff_gen f_mix f_token f_symb f_time e1))
                e1))
    end
  | Alg_expr.UN_ALG_OP (op,e) ->
    begin
      match op with
      | Operator.UMINUS ->
        Alg_expr.uminus
          (diff_gen f_mix f_token f_symb f_time e)
      | Operator.COSINUS ->
        Alg_expr.mult
          (diff_gen f_mix f_token f_symb f_time e)
          (Alg_expr.uminus (Alg_expr.sin e))
      | Operator.SINUS ->
        Alg_expr.mult
          (diff_gen f_mix f_token f_symb f_time e)
          (Alg_expr.cos e)
      | Operator.LOG ->
        Alg_expr.mult
          (diff_gen f_mix f_token f_symb f_time e)
          (Alg_expr.div
             (Alg_expr.int 1)
             (e))
      | Operator.SQRT ->
        Alg_expr.mult
          (diff_gen f_mix f_token f_symb f_time e)
          (Alg_expr.div
             (Alg_expr.int (-1))
             (Alg_expr.sqrt e))
      | Operator.EXP ->
        Alg_expr.mult
          (diff_gen f_mix f_token f_symb f_time e)
          e
      | Operator.TAN ->
      Alg_expr.mult
        (diff_gen f_mix f_token f_symb f_time e)
        (Alg_expr.add
           (Alg_expr.int 1)
           (Alg_expr.pow e (Alg_expr.int 2)))
      | Operator.INT -> Alg_expr.int 0
    end
  | Alg_expr.STATE_ALG_OP Operator.TIME_VAR -> f_time ()
  | Alg_expr.STATE_ALG_OP
        (Operator.CPUTIME | Operator.EVENT_VAR | Operator.NULL_EVENT_VAR | Operator.TMAX_VAR | Operator.EMAX_VAR) -> Alg_expr.int 0
    | Alg_expr.KAPPA_INSTANCE mix ->
      f_mix mix
    | Alg_expr.TOKEN_ID id ->
      f_token id
    | Alg_expr.CONST _ -> Alg_expr.int 0
    | Alg_expr.ALG_VAR _
    | Alg_expr.DIFF_TOKEN _
    | Alg_expr.DIFF_KAPPA_INSTANCE _ -> f_symb expr

let diff_token expr token =
  let f_mix _ = Alg_expr.int 0 in
  let f_token a =
    if a = token then
      Alg_expr.int 1
    else
      Alg_expr.int 0
  in
  let f_symb expr =
    Alg_expr.DIFF_TOKEN
      (expr,token),
    Locality.dummy
  in
  let f_time _ = Alg_expr.int 0 in
  diff_gen
    f_mix
    f_token
    f_symb
    f_time
    expr

let diff_mixture ?time_var expr mixture =
  let f_mix a =
    if a = mixture then
      Alg_expr.int 1
    else
      Alg_expr.int 0
  in
  let f_token _ =
    Alg_expr.int 0
  in
  let f_symb expr  =
    Alg_expr.DIFF_KAPPA_INSTANCE
      (expr,mixture),
    Locality.dummy
  in
  let f_time () =
    match time_var with
    | Some b when mixture=b -> Alg_expr.int 1
    | Some _ -> Alg_expr.int 0
    | None ->
    raise
      (ExceptionDefn.Internal_Error
         ("A time-dependent expression cannot be differentiated without specifying a variable for time progress",Locality.dummy))
  in
  diff_gen f_mix f_token f_symb f_time expr

let fold_over_mix_in_list f mix accu =
  List.fold_left (fun accu array_id ->
      Array.fold_left (fun accu pid ->
          f pid accu
        ) accu array_id
    ) accu mix

let fold_over_mix_in_alg_expr f expr accu =
  let l = Alg_expr.extract_connected_components expr in
  List.fold_left
    (fun accu mix -> fold_over_mix_in_list f mix accu)
    accu
    l

let fold_over_mixtures_in_alg_exprs f model accu_algs accu_obs accu_rate
    accu_unary_rate accu_delta_tokens =
  let algs_expr = Model.get_algs model in
  let observables = Model.get_obs model in
  (*algs*)
  let accu_algs =
    Array.fold_left
      (fun accu (_, mix) ->
         fold_over_mix_in_alg_expr
           f
           mix
           accu
      ) accu_algs algs_expr
  in
  (*observations*)
  let accu_obs =
    Array.fold_left
      (fun accu mix ->
         fold_over_mix_in_alg_expr
           f
           mix
           accu
      ) accu_obs observables
  in
  (*rules*)
  let rules = Model.get_rules model in
  (*rate*)
  let accu_rate, accu_unary_rate, accu_delta_tokens =
    Array.fold_left (fun (accu_rate, accu_unary_rate, accu_delta_tokens)
                      elementary_rule ->
        let rate = Primitives.get_rate elementary_rule in
        let accu_rate =
          fold_over_mix_in_alg_expr f rate accu_rate
        in
        (*unary_rate*)
        let unary_rate = Primitives.get_unary_rate elementary_rule in
        let accu_unary_rate =
          match unary_rate with
          | None -> accu_unary_rate
          | Some (expr, _) ->
            fold_over_mix_in_alg_expr f expr accu_unary_rate
        in
        let delta_tokens = Primitives.get_delta_tokens elementary_rule in
        let accu_delta_tokens =
          List.fold_left (fun accu_delta_tokens (expr, _) ->
              fold_over_mix_in_alg_expr f expr accu_delta_tokens
            ) accu_delta_tokens delta_tokens
        in
        accu_rate, accu_unary_rate, accu_delta_tokens
      ) (accu_rate, accu_unary_rate, accu_delta_tokens) rules
  in
  accu_algs, accu_obs, accu_rate, accu_unary_rate, accu_delta_tokens
