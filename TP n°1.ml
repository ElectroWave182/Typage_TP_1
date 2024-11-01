(*
Exercice 1 :
------------
*)


type base_tp =
    | BoolT
    | IntT
;;

type tp =
    | ConstT of base_tp
    | FunT of tp * tp
;;


(* 1 - *)

let a1 =
    FunT
    (
        ConstT (IntT),
        FunT
        (
            ConstT (BoolT),
            ConstT (IntT)
        )
    )
;;


(* 2 - *)

let a2 =
    FunT
    (
        FunT
        (
            ConstT (IntT),
            ConstT (BoolT)
        ),
        ConstT (IntT)
    )
;;


(* 3 - *)

let a3 =
    FunT
    (
        FunT
        (
            ConstT (IntT),
            ConstT (IntT)
        ),
        FunT
        (
            ConstT (IntT),
            ConstT (IntT)
        )
    )
;;



(*
Exercice 2 :
------------
*)


type const_expr =
    | BoolE of bool
    | IntE of int
;;

type expr =
    | Const of const_expr
    | Var of string
    | Abs of string * tp * expr
    | App of expr * expr
;;


(* 1 - *)

let b1 =
    App
    (
        App
        (
            Abs
            (
                "x",
                ConstT (IntT),
                Var ("x")
            ),
            Const (IntE 2)
        ),
        Const (IntE 3)
    )
;;


(* 2 - *)

let b2 =
    App
    (
        App
        (
            Abs
            (
                "x",
                ConstT (IntT),
                Abs
                (
                    "y",
                    ConstT (BoolT),
                    Var ("y")
                )
            ),
            Const (IntE 2)
        ),
        Const (IntE 3)
    )
;;


(* 3 - *)

let b3 =
    App
    (
        Abs
        (
            "f",
            FunT
            (
                ConstT (IntT),
                ConstT (BoolT)
            ),
            App
            (
                Var ("f"),
                Const (IntE 2)
            )
        ),
        Abs
        (
            "x",
            ConstT (IntT),
            Const (BoolE true)
        )
    )
;;



(*
Exercice 3 :
------------
*)

print_endline ("Exercice 3 :\n------------\n\n") ;;


(* 1 - *)

let rec string_of_typeRec =
function

    (* Cas récursif *)
    | (FunT (entree, sortie)) ->
        "("
        ^ string_of_typeRec (entree)
        ^ " -> "
        ^ string_of_typeRec (sortie)
        ^ ")"
    
    (* Cas de base : types primitifs *)
    | (ConstT (BoolT)) -> "bool"
    | (ConstT (IntT)) -> "int"
;;


(* 2 - *)
print_endline ("2 -\n") ;;

let string_of_type =
function

    (* Cas récursif *)
    | (FunT (entree, sortie)) ->
        string_of_typeRec (entree)
        ^ " -> "
        ^ string_of_typeRec (sortie)
    
    (* Cas élémentaires : types primitifs *)
    | (ConstT (BoolT)) -> "bool"
    | (ConstT (IntT)) -> "int"
;;


print_endline (string_of_type (a1)) ;;
(* Sortie - int -> (bool -> int) *)

print_endline (string_of_type (a2)) ;;
(* Sortie - (int -> bool) -> int *)

print_endline (string_of_type (a3)) ;;
(* Sortie - (int -> int) -> (int -> int) *)



(*
Exercice 4 :
------------
*)

print_endline ("\n\n\nExercice 4 :\n------------\n\n") ;;


(* 1 - *)

let rec string_of_exprRec =
function

    (* Cas récursif : abstraction *)
    | (Abs (nom, entree, sortie)) ->
        "(fun ("
        ^ nom
        ^ " : "
        ^ string_of_type (entree)
        ^ ") -> "
        ^ string_of_exprRec (sortie)
        ^ ")"
        
    (* Cas récursif : application *)
    | (App (fonction, parametre)) ->
        "("
        ^ string_of_exprRec (fonction)
        ^ " "
        ^ string_of_exprRec (parametre)
        ^ ")"
    
    (* Cas élémentaires : constante *)
    | (Const (BoolE (valeur))) ->
        string_of_bool (valeur)
    | (Const (IntE (valeur))) ->
        string_of_int (valeur)
    
    (* Cas élémentaire : variable *)
    | (Var (nom)) -> nom
;;


(* 2 - *)
print_endline ("2 -\n") ;;

let string_of_expr =
function

    (* Cas récursif : abstraction *)
    | (Abs (nom, entree, sortie)) ->
        "fun ("
        ^ nom
        ^ " : "
        ^ string_of_type (entree)
        ^ ") -> "
        ^ string_of_exprRec (sortie)
        
    (* Cas récursif : application *)
    | (App (fonction, parametre)) ->
        string_of_exprRec (fonction)
        ^ " "
        ^ string_of_exprRec (parametre)
    
    (* Cas élémentaires : constante *)
    | (Const (BoolE (valeur))) ->
        string_of_bool (valeur)
    | (Const (IntE (valeur))) ->
        string_of_int (valeur)
    
    (* Cas élémentaire : variable *)
    | (Var (nom)) -> nom
;;


print_endline (string_of_expr (b1)) ;;
(* Sortie - ((fun (x : int) -> x) 2) 3 *)

print_endline (string_of_expr (b2)) ;;
(* Sortie - ((fun (x : int) -> (fun (y : bool) -> y)) 2) 3 *)

print_endline (string_of_expr (b3)) ;;
(* Sortie - (fun (f : int -> bool) -> (f 2)) (fun (x : int) -> true) *)



(*
Exercice 5 :
------------
*)

print_endline ("\n\n\nExercice 5 :\n------------\n\n") ;;


type 'a option =
    | None
    | Some of 'a
;;

let exemple_assoc = [("Max", 10); ("Nicolas", 4); ("Nicole", 9)] ;;


(* 1 - *)
print_endline ("1 -\n") ;;

(* 1a - *)

let affiche_option =
function
    | (None) ->
        print_endline ("None")
        
    | (Some (nb)) ->
        print_endline
        (
            "Some "
            ^ string_of_int (nb)
        )
;;


(* 1b - *)

let lookup_assoc =
function
    cible ->
        let rec parcours =
        function
            
            (* Cas de base : personne trouvée *)
            | ((nom, valeur) :: _)
            when nom = cible
            ->
                Some (valeur)
        
            (* Cas récursif *)
            | (_ :: queue) ->
                parcours (queue)
                
            (* Cas de base : personne introuvable *)
            | _ -> None
            
        in
            parcours
;;


affiche_option (lookup_assoc "Nicolas" exemple_assoc) ;;
(* Sortie - Some 4 *)

affiche_option (lookup_assoc "Maurice" exemple_assoc) ;;
(* Sortie - None *)


(* 2 - *)
print_endline ("\n\n2 -\n") ;;

(* 2a - *)

let rec string_of_assoc =
function

    (* Cas récursif *)
    | ((clef, valeur) :: queue) ->
        "("
        ^ clef
        ^ ", "
        ^ string_of_int (valeur)
        ^ ") ;\n"
        ^ string_of_assoc (queue)
    
    (* Cas de base : [] *)
    | _ -> ""
;;

let affiche_assoc =
function
    assoc ->
        print_endline (string_of_assoc (assoc))
;;


(* 2b - *)

let add_assoc =
function
    | (assoc, liste) ->
        assoc :: liste
;;


affiche_assoc (add_assoc (("JeanMi", 42), exemple_assoc)) ;;


(* 3 - *)
print_endline ("\n3 -\n") ;;

let remove_assoc =
function
    cible ->
        let rec parcours =
        function
            
            (* Cas de base : personne trouvée *)
            | ((nom, valeur) :: queue)
            when nom = cible
            ->
                queue
        
            (* Cas récursif *)
            | (assoc :: queue) ->
                assoc :: parcours (queue)
                
            (* Cas de base : personne introuvable *)
            | _ -> []
            
        in
            parcours
;;


affiche_assoc (remove_assoc "Nicolas" exemple_assoc) ;;
(* Sortie - 
(Max, 10) ;
(Nicole, 9) ;
*)

affiche_assoc (remove_assoc "Maurice" exemple_assoc) ;;
(* Sortie - 
(Max, 10) ;
(Nicolas, 4) ;
(Nicole, 9) ;
*)



(*
Exercice 6 :
------------
*)

print_endline ("\n\nExercice 6 :\n------------\n\n") ;;


(* 1 - *)

let exemple_map =
function
    | "Max" -> (Some 10)
    | "Nicolas" -> (Some 4)
    | "Nicole" -> (Some 9)
    | _ -> None
;;


(* 2 - *)
print_endline ("2 -\n") ;;

(* 2a - *)

let lookup_map =
function
    cible ->
        fun
            map -> map (cible)
;;


affiche_option (lookup_map "Nicolas" exemple_map) ;;
(* Sortie - Some 4 *)

affiche_option (lookup_map "Maurice" exemple_map) ;;
(* Sortie - None *)


(* 2b - *)

let add_map =
function
    ((cible, valeur), map) ->
        function
        
            | clef
            when clef = cible
            -> Some (valeur)
            
            | autreClef ->
                map (autreClef)
;;


(* 2c - *)

let remove_map =
function
    ((cible, valeur), map) ->
        function
        
            | clef
            when clef = cible
            -> None
            
            | autreClef ->
                map (autreClef)
;;


(* 3 - *)

let empty_map =
function
    _ -> None
;;


(* 4 - *)
print_endline ("\n\n4 -\n") ;;

let rec assoc2map =
function

    (* Cas récursif *)
    | (tete :: queue) ->
        let appel = assoc2map (queue)
        in
            add_map (tete, appel)
    
    (* Cas de base : [] *)
    | _ -> empty_map
;;


affiche_option (lookup_map "Nicole" (assoc2map exemple_assoc)) ;;
(* Sortie - Some 9 *)



(*
Exercice 7 :
------------
*)

print_endline ("\n\n\nExercice 7 :\n------------\n\n") ;;


(* 1 - *)
print_endline ("1 -\n") ;;

(* 1a - *)

let affiche_type =
function
    type_ ->
        print_endline (string_of_type (type_))
;;


(* 1b - *) 

let rec tp_of_expr =
function
    env ->
    
        let rec typage =
        function
             
            
            (* Cas récursif : application *)   
            | (App (appele, appelant)) ->
                let typeA = typage (appele)
                in
                    if
                        typeA = (ConstT BoolT)
                        || typeA = (ConstT IntT)
                    then failwith "Une des applications appelle un objet."
                    
                    (* L'expression appelée est bien une fonction. *)
                    else
                        let FunT (depart, arrivee) = typeA
                        in
                            if depart <> typage (appelant)
                            then failwith "Une des fonctions est appelée avec le mauvais type."
                            
                            (* Cette fonction prend bien des arguments du type de l'appelant. *)
                            else arrivee
                
            
            (* Cas récursif : abstraction *)
            | (Abs (nom, argument, image)) ->
                let opt = lookup_assoc nom env
                in
                    if opt = None
                    then
                        (* La variable n'est pas utilisée *)
                        FunT
                        (
                            argument,
                            tp_of_expr
                            (
                                add_assoc
                                (
                                    (nom, argument),
                                    env
                                )
                            )
                            image
                        )
                        
                    else
                        failwith
                        (
                            "La variable "
                            ^ nom
                            ^ " est déjà utilisée, et ne peut pas servir d'argument."
                        )
            
            
            (* Cas de base : constante *)
            | (Const (BoolE _)) -> (ConstT BoolT)
            | (Const (IntE _)) -> (ConstT IntT)
            
            
            (* Cas de base : variable *)
            | (Var nom) ->
                let opt = lookup_assoc nom env
                in
                
                    if opt = None
                    then
                        failwith
                        (
                            "La variable "
                            ^ nom
                            ^ " n'est pas définie."
                        )
                    
                    (* La variable est bien présente dans l'environnement. *)
                    else
                        let Some (resultat) = opt
                        in resultat
            
            
        in typage
;;


affiche_type (tp_of_expr [] (App (Abs ("x", ConstT IntT, Var "x"), Const (IntE 2)))) ;;
(* Sortie - int *)

affiche_type (tp_of_expr [] (App (Abs ("x", ConstT IntT, Var "x"), Const (BoolE true)))) ;;
(* Sortie - erreur : Une des fonctions est appelée avec le mauvais type. *)
