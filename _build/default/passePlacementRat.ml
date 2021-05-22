module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Type
  open Ast
  open AstPlacement

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme

  (* SB = base de la pile *)
  (* LB = base de l'enregistrement d'activation *)

let rec analyse_placement_instruction i dep reg =
  match i with
  | AstType.Declaration(e,info) -> 
    (
      match info_ast_to_info info with
      | InfoVar(_,typ,_,_) -> modifier_adresse_info dep reg info;
        (AstType.Declaration(e,info), getTaille typ)
      | _ -> failwith "internal error"
    )
  | AstType.Affectation (a,e) -> (Affectation(a,e),0)
  | AstType.Conditionnelle(c,b1,b2) -> 
    let pb1 = analyse_placement_bloc b1 dep reg
    in let pb2 = analyse_placement_bloc b2 dep reg
    in (Conditionnelle(c,pb1,pb2),0)
  | AstType.TantQue(c,b) -> 
    let pb = analyse_placement_bloc b dep reg
    in (TantQue(c,pb),0)
  | AstType.AffichageBool(e) -> (AffichageBool(e),0)
  | AstType.AffichageInt(e) -> (AffichageInt(e),0)
  | AstType.AffichageRat(e) -> (AffichageRat(e),0)
  | AstType.Empty -> (Empty,0)
      
and analyse_placement_bloc b dep reg =
  match b with
  | [] -> []
  | t::q ->
    let i,len = analyse_placement_instruction t dep reg 
    in i::(analyse_placement_bloc q (dep+len) reg)


let rec analyse_parametres lp = 
  match lp with
  | [] -> 0
  | info::q -> 
    (
      match info_ast_to_info info with
      | InfoVar(_,typ,_,_) -> 
        let lent = getTaille typ
        in let lenq = analyse_parametres q
        in modifier_adresse_info (-lent - lenq) "LB" info;
        lent + lenq
      | _ -> failwith "internal error"
    )


let analyse_placement_fonction (AstType.Fonction(n,lp,li,e)) =
  let _ = analyse_parametres lp in
  let pli = analyse_placement_bloc li 3 "LB" in
  Fonction(n,lp,pli,e)


let analyser (AstType.Programme (fonctions,prog)) =
  let pf = List.map analyse_placement_fonction fonctions in 
  let pb = analyse_placement_bloc prog 0 "SB" in
  Programme (pf,pb)


end
