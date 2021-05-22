module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Exceptions
  open Tds
  open Type
  open Ast
  open AstType

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme


let rec analyse_type_expression e =
  match e with
  | AstTds.True ->
    (True, Bool)
  | AstTds.False ->
    (False, Bool)
  | AstTds.Entier e ->
    (Entier e, Int) 
  | AstTds.Binaire (bin, e1, e2) ->
    let (ne1, te1) = analyse_type_expression e1
    in let (ne2, te2) = analyse_type_expression e2
    in if est_compatible te1 te2 then
      match bin, te1, te2 with
      | Plus, Int, Int -> (Binaire (PlusInt,ne1,ne2), Int)
      | Mult, Int, Int -> (Binaire (MultInt,ne1,ne2), Int)
      | Equ, Int, Int -> (Binaire (EquInt,ne1,ne2), Bool)
      | Inf, Int, Int -> (Binaire (Inf,ne1,ne2), Bool)
      | Plus, Rat, Rat -> (Binaire (PlusRat,ne1,ne2), Rat)
      | Mult, Rat, Rat -> (Binaire (MultRat,ne1,ne2), Rat)
      | Equ, Bool, Bool -> (Binaire (EquBool,ne1,ne2), Bool)
      | _ -> raise (TypeBinaireInattendu (bin,te1,te2))
    else
      raise (TypeInattendu (te2, te1))
  | AstTds.Numerateur num ->
    let (nnum, tnum) = analyse_type_expression num
    in
      if tnum = Rat then
        (Numerateur nnum, Int)
      else
        raise (TypeInattendu (tnum, Rat))
  | AstTds.Denominateur denom ->
    let (ndenom, tdenom) = analyse_type_expression denom
    in
      if tdenom = Rat then
        (Denominateur ndenom, Int)
      else
        raise (TypeInattendu (tdenom, Rat))
  | AstTds.Rationnel (e1, e2) ->
    let (ne1, te1) = analyse_type_expression e1
    in let (ne2, te2) = analyse_type_expression e2
    in if te1 != Int then
      raise(TypeInattendu(te1,Int))
    else if te2 != Int then
      raise (TypeInattendu(te2,Int))
    else
      (AstType.Rationnel(ne1, ne2), Rat)
  | AstTds.Ident ia ->
    (
      match info_ast_to_info ia with
      | InfoVar(_,t,_,_) -> (Ident ia, t)
      | InfoConst(_,_) -> (Ident ia, Int)
      | _ -> failwith "internal error"
    )
  | AstTds.AppelFonction (ia, el) ->
    let tel = List.map analyse_type_expression el
    in
      (
        match info_ast_to_info ia with
        | InfoFun(_,t,tl) -> 
          (
            let rec aux_appel tel tl =
              match tel, tl with
              | [], [] -> []
              | [], _ -> raise (TypesParametresInattendus(List.map snd tel, tl))
              | _, [] -> raise (TypesParametresInattendus(List.map snd tel, tl))
              | (v, t1)::q1, t2::q2 ->
                if (est_compatible t1 t2) then
                  v::(aux_appel q1 q2)
                else
                  raise (TypesParametresInattendus(List.map snd tel, tl))
            in let lv = aux_appel tel tl
            (*in let () = modifier_type_info t ia*)
            in let () = modifier_type_fonction_info t tl ia
            in (AppelFonction(ia, lv), t)
          ) 
        | _ -> failwith "internal error"
      )


let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, e, ia) ->
    let (ne, te) = analyse_type_expression e in
    if est_compatible te t then
      begin
        modifier_type_info t ia;
        Declaration(ne, ia)
      end
    else
      raise (TypeInattendu (te,t))
  | AstTds.Affectation (e, ia) ->
    let (ne, te) = analyse_type_expression e in
      (
        match info_ast_to_info ia with
        | InfoVar(_,t,_,_) ->
          (
            if est_compatible t te then
              Affectation (ne, ia)
            else
              raise (TypeInattendu (te,t))
          )
        | _ -> failwith "internal error"
      )
  | AstTds.Affichage e ->
    let (ne, te) = analyse_type_expression e in
      (
        match te with
        | Int -> AffichageInt(ne)
        | Bool -> AffichageBool(ne)
        | Rat -> AffichageRat(ne)
        | _ -> raise (TypeInattendu (te,Undefined))
      )
  | AstTds.Conditionnelle (e,b1,b2) ->
    let (ne,te) = analyse_type_expression e
    in
        if (te=Bool) then
        let tb1 = analyse_type_bloc b1
        in let tb2 = analyse_type_bloc b2
        in (Conditionnelle(ne, tb1, tb2))
      else
        raise(TypeInattendu(te,Bool))
  | AstTds.TantQue (e, b) ->
    let (ne,te) = analyse_type_expression e
    in
      if (te=Bool) then
        let tb = analyse_type_bloc b
        in (TantQue(ne, tb))
      else
        raise(TypeInattendu(te,Bool))
  | AstTds.Empty ->
    Empty

      
and analyse_type_bloc b =
  List.map analyse_type_instruction b


let analyse_type_fonction (AstTds.Fonction(t,n,lp,li,e)) =
  (* On affecte aux paramètres leur type *)
  let rec params_type l =
    match l with
    | [] -> ();
    | (t, ia)::q -> modifier_type_info t ia; params_type q
  in params_type lp;
  let tlp = List.map fst lp
  (* On affecte à la fonction son type de retour *)
  in let () = modifier_type_fonction_info t tlp n
  (* Analyse du bloc *)
  in let tli = analyse_type_bloc li
  (* Analyse de l'expression de retour *)
  in let ee, te = analyse_type_expression e in
    if (est_compatible t te) then
      let ilp = List.map snd lp
      in Fonction(n, ilp, tli, ee)
    else
      raise(TypeInattendu(te,t))


let analyser (AstTds.Programme (fonctions,prog)) =
  let tf = List.map analyse_type_fonction fonctions in 
  let tb = analyse_type_bloc prog in
  Programme (tf,tb)


end
