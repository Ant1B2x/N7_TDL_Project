module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

  open Tds
  open Ast
  open Type
  open Code

  type t1 = Ast.AstPlacement.programme
  type t2 = string


let rec analyse_expression e = 
  match e with
  | AstType.Ident(info) ->
    (
      match info_ast_to_info info with
      | InfoVar(_,t,dep,reg) -> "LOAD ("^(string_of_int (getTaille t))^") "^(string_of_int dep)^"["^reg^"]\n"
      (*| InfoConst(_,v) -> "LOAD ("^(string_of_int (getTaille Int))^") "^(string_of_int dep)^"["^reg^"]\n"*)
      | _ -> failwith("internal error 1")
    )
  | AstType.Rationnel(e1, e2) ->
    let c1 = analyse_expression e1 in
    let c2 = analyse_expression e2 in
    c1^c2
  | AstType.True -> "LOADL 1\n"
  | AstType.False -> "LOADL 0\n"
  | AstType.Entier(i) -> "LOADL "^(string_of_int i)^"\n"
  | AstType.Numerateur(e1) ->
    analyse_expression e1^"POP (0) 1\n"
  | AstType.Denominateur(e1) ->
    analyse_expression e1^"POP (1) 1\n"
  | AstType.Binaire(b,e1,e2) ->
    let c1 = analyse_expression e1 
    in let c2 = analyse_expression e2 in
    (
      match b with
      | PlusInt -> c1^c2^"SUBR IAdd\n"
      | PlusRat -> c1^c2^"CALL (ST) RAdd\n"
      | MultInt -> c1^c2^"SUBR Imul\n"
      | MultRat -> c1^c2^"CALL (ST) RMul\n"
      | EquInt -> c1^c2^"SUBR IEq\n"
      | EquBool -> c1^c2^"SUBR IEq\n"
      | Inf -> c1^c2^"SUBR ILss\n"
    )
  | AstType.AppelFonction(info,le) ->
    let cle = List.fold_left (fun t e -> t^(analyse_expression e)) "" le in
    (
      match info_ast_to_info info with
      | InfoFun(n,_,_) -> cle^"CALL (SB) "^n^"\n"
      | _ -> failwith "internal error 2"
    )


let analyse_affectable a =
  match info_ast_to_info a with
  | InfoVar(_,t,dep,reg) -> "STORE ("^(string_of_int(getTaille t))^") "^(string_of_int dep)^"["^reg^"]\n"
  | _ -> failwith "internal error 3"


let rec analyse_taille_bloc b =
  match b with
  | [] -> 0
  | t::q ->
    (
      match t with
      | AstType.Declaration (_,i) ->
        (
          match info_ast_to_info i with
          | InfoVar(_,t,_,_) -> getTaille t
          | InfoConst(_,_) -> getTaille Int
          | _ -> 0
        )
      | _ -> 0
    ) + analyse_taille_bloc q


let rec analyse_instruction i =
  match i with
  | AstType.Declaration(e,info) ->
    let ce = analyse_expression e
    in
    (
      match info_ast_to_info info with
      | InfoVar(_,t,dep,reg) ->
        "PUSH "^(string_of_int (getTaille t))^
        "\n"^ce^
        "STORE ("^(string_of_int (getTaille t))^") "^(string_of_int dep)^"["^reg^"]\n"
      | _ -> failwith "internal error 4"
    )
  | AstType.Affectation(e,i) -> (analyse_expression e)^(analyse_affectable i)
  | AstType.AffichageInt(e) ->
    let ce = analyse_expression e in
    ce^"SUBR IOut\n"
  | AstType.AffichageRat(e) -> 
    let ce = analyse_expression e in
    ce^"CALL (ST) ROut\n"
  | AstType.AffichageBool(e) -> 
    let ce = analyse_expression e in
    ce^"SUBR BOut\n"
  | AstType.Conditionnelle(_,b1,b2) -> 
    let cb1 = analyse_bloc b1
    in let ptb1 = analyse_taille_bloc b1
    in let cb2 = analyse_bloc b2
    in let ptb2 = analyse_taille_bloc b2
    in let etiqe = getEtiquette () (* étiquette du else *)
    in let etiqf = getEtiquette () (* étiquette de fin du if *)
    in cb1^
      "JUMPIF (0) "^etiqe^
      "\n"^cb1^
      "POP (0)"^(string_of_int ptb1)^"\n"^
      "JUMP "^etiqf^
      "\nLABEL "^etiqe^
      "\n"^cb2^
      "POP (0)"^(string_of_int ptb2)^"\n"^
      "LABEL "^etiqf^"\n"
  | AstType.TantQue(_,b) -> 
    let cb = analyse_bloc b
    in let ptb = analyse_taille_bloc b
    in let etiq = getEtiquette ()
    in let etiq2 = getEtiquette ()
    in "LABEL "^etiq^
      "\n"^cb^
      "JUMPIF (0) "^etiq2^
      "\n"^cb^
      "POP (0)"^(string_of_int ptb)^"\n"^
      "JUMP "^etiq^
      "\nLABEL "^etiq2^"\n"
  | AstType.Empty -> ""


and analyse_bloc li =
  match li with
  | [] -> ""
  | t::q -> (analyse_instruction t) ^ (analyse_bloc q)


let analyse_fonction (Ast.AstPlacement.Fonction(info,_,li,e)) = 
  let info = info_ast_to_info info in
  match info with
  | InfoFun(n,t,tl) ->
      let cb = analyse_bloc li
      in let ptb = analyse_taille_bloc li
      in let ce = analyse_expression e in 
      "LABEL "^n^
      "\n"^cb^
      ce^
      "POP ("^(string_of_int (getTaille t))^")"^(string_of_int ptb)^
      "\nRETURN ("^(string_of_int (getTaille t))^")"^(string_of_int(List.fold_left (fun nb t -> (getTaille t) + nb) 0 tl))^"\n"
  | _ -> failwith "internal error 5"


let analyser (Ast.AstPlacement.Programme (fonctions, prog)) = 
  let eprog = getEntete ()
  in let cf = List.map (analyse_fonction) fonctions
  in let rec aux l =
    match l with
    | [] -> ""
    | t::q -> t^"\n"^(aux q)
  in let fprog = aux cf
  in let cb = analyse_bloc prog
  in let ptb = analyse_taille_bloc prog
  in eprog^
    fprog^
    "LABEL main\n"^
    cb^
    "POP (0)"^
    (string_of_int ptb)^
    "\n\nHALT"


end
