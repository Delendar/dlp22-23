
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyString
  | TyArr of ty * ty
  | TyRecord of (string * ty) * ty
  | TyTuple of ty * ty
  | TyNil
  | TyList of ty
  | TyUnit
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmRecord of (string * term) * term
  | TmString of string
  | TmConcat of term * term
  | TmTuple of term * term
  | TmCons of term * term
  | TmHead of term
  | TmTail of term
  | TmIsNil of term
  | TmProj of term * term
  | TmNil
  | TmUnit
  (*| TmIsNil of term*)
;;

type command =
    Eval of term
  | Bind of string * term
;;

type binding =
    TyBind of ty
  | TyTmBind of (ty * term)
;;

type context =
  (string * binding) list
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addtbinding ctx s ty =
  (s, TyBind ty) :: ctx
;;

let addbinding ctx s ty tm =
  (s, TyTmBind (ty,tm)) :: ctx
;;

let gettbinding ctx s =
  match List.assoc s ctx with
      TyBind ty -> ty
    | TyTmBind (ty, _) -> ty
;;

let getvbinding ctx s =
  match List.assoc s ctx with
      TyTmBind (_, tm) -> tm
    | _ -> raise Not_found
;;

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyString ->
      "String"      
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyRecord ((tag, ty1), ty2) ->
    let rec recordTyping record = match record with
      | TyRecord ((tag, ty), TyNil) -> 
          tag ^ ":" ^ string_of_ty ty
      | TyRecord ((tag, ty), record) -> 
          tag ^ ":" ^ string_of_ty ty ^ ", " ^ (recordTyping record)
      | _ -> "Invalid record constructor."
    in ("{" ^ recordTyping (TyRecord((tag, ty1), ty2)) ^ "}")
  | TyTuple(ty1, ty2) ->
    let rec aux t = match t with
      TyTuple (x, TyNil) -> string_of_ty x
      | TyTuple (x, y) -> string_of_ty x ^ ", " ^ (aux y)
      | _ -> "Invalid"
    in ("{" ^ (aux (TyTuple(ty1,ty2))) ^ "}")
  | TyNil -> 
      "Null"
  | TyList t1 ->
    string_of_ty t1 ^ " list"
      
  | TyUnit ->
      "Unit"
;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addtbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in(
        match tyT1 with
        TyArr (tyT11, tyT12) ->
          if tyT11 = tyT12 then tyT12
          else raise (Type_error "result of body not compatible with domain")
      | _ -> raise (Type_error "arrow type expected")
      )

  | TmRecord ((tag, t1), t2) ->
      let rec recordTypeof tags = function
        | TmRecord ((tag, element), TmNil) -> 
            if (List.mem tag tags) then raise (Invalid_argument "Record - Unique key duplicated in record")
            else TyRecord ((tag, typeof ctx element), TyNil)
        | TmRecord ((tag, element), record) ->
            if (List.mem tag tags) then raise (Invalid_argument "Record - Unique key duplicated in record")
            else TyRecord ((tag, typeof ctx element), (recordTypeof (tag::tags) record))
        | _ -> raise (Type_error "invalid record syntax")
      in recordTypeof [] (TmRecord ((tag, t1), t2))

  | TmProj (t1, TmVar var) ->
      let projTy = typeof ctx t1 in
      (match projTy with
        TyRecord (term1,term2) ->
          let rec recProjTypeOf = function
            | TyRecord ((tag, elem), TyNil) -> 
                if tag = var then elem else TyNil
            | TyRecord ((tag, elem), record) ->
                if tag = var then elem else recProjTypeOf record
            | _ -> 
                TyNil
          in recProjTypeOf (TyRecord (term1,term2))
        | _ -> raise (Type_error "head argument of projection not of type record"))

  | TmProj (t1, t2) ->
        let ty1 = typeof ctx t1 in
        let ty2 = typeof ctx t2 in
        (match ty1, ty2 with
          | TyTuple (term1, term2), TyNat ->
              let rec tupProjTypeOf ty term = 
                match ty, term with
                  | TyTuple (elem, _), TmZero -> 
                      elem
                  | TyTuple (elem1, elem2), TmSucc(num) -> 
                      tupProjTypeOf elem2 num
                  | _ -> TyNil
              in tupProjTypeOf (TyTuple(term1, term2)) t2
          | TyRecord (term1, term2), _ -> 
              raise (Type_error "second argument of projection not a tag")
          | _, TyNat -> 
              raise (Type_error "head argument of projection not of type tuple")
          | _ -> 
              raise (Type_error "second argument of projection not of type Nat")
        )
    (* T-String *)
  | TmString s ->
      TyString

    (* T-Concat *)
  | TmConcat (t1, t2) ->
      let tyt1 = typeof ctx t1 in
      let tyt2 = typeof ctx t2 in
        if tyt1 = TyString then
          if tyt2 = TyString then TyString
          else raise (Type_error "second argument is not an string")
        else raise (Type_error "first argument is not an string")

    (* T-Tuple*)
  | TmTuple (t1, t2) ->
      TyTuple (typeof ctx t1, typeof ctx t2)
  
    (*T-Nil*)
  | TmNil ->
      TyNil
  
  | TmUnit ->
      TyUnit

    (*T-Cons*)
  | TmCons (t1, t2) ->
    let tyt1 = typeof ctx t1 in(
    let tyt2 = typeof ctx t2 in
    match tyt2 with
      TyList(aux) ->
        if tyt1 = aux then TyList(tyt1)
        else raise (Type_error "not same type (Cons)")
      | TyNil ->
        TyList(tyt1)
      | _ -> raise (Type_error "not same type (Cons)")
    )

    (*T-IsNil*)
  | TmIsNil t1 ->
    let ty1 = typeof ctx t1 in (
      match ty1 with
        TyList _ -> TyBool
        | _ -> raise (Type_error "argument not a list (IsNil)")
    )

    (*T-Head*)
  | TmHead t1 ->
    let ty1 = typeof ctx t1 in (
      match ty1 with
        TyList ty2 -> ty2
        | _ -> raise (Type_error "argument not a list (Head)")
    )

    (*T-Tail*)
  | TmTail t1 ->
    let ty1 = typeof ctx t1 in (
      match ty1 with
        TyList ty2 -> TyList ty2
        | _ -> raise (Type_error "argument not a list (Tail)")
    )
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ " )"
  | TmRecord ((tag, t1), t2) ->
      let rec recordSoF record = match record with
        | TmTuple (TmNil, TmNil) -> "{}"
        | TmRecord ((tag, element), TmNil) -> 
            tag ^ "=" ^ string_of_term element
        | TmRecord ((tag, element), record) -> 
            tag ^ "=" ^ string_of_term element ^ ", " ^ (recordSoF record)
        | element ->
            string_of_term element
      in ("{" ^ (recordSoF (TmRecord((tag, t1), t2))) ^ "}")
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
      "(++) " ^ string_of_term t1 ^ " " ^ string_of_term t2
  | TmTuple (t1,t2) ->
      let rec aux t = match t with
          TmTuple (TmNil, TmNil) -> "{}"
        | TmTuple (x, TmNil) -> string_of_term x
        | TmTuple (x, y) -> string_of_term x ^ ", " ^ (aux y)
        | x -> string_of_term x
      in ("{" ^ (aux(TmTuple(t1,t2))) ^ "}") 
  | TmProj (t1, t2) ->
    string_of_term t1 ^ "." ^ string_of_term t2
  | TmNil ->
      ""
  | TmCons (l1, l2)->
    let rec aux l1 = match l1 with
    TmCons(TmNil, TmNil) -> ""
    |TmCons(x, TmNil) -> string_of_term x
    |TmCons(x, y) -> string_of_term x ^ "; " ^ (aux y)
    | _ -> "Invalid"
    in ("[" ^ (aux (TmCons(l1, l2))) ^ "] list")   
  | TmIsNil l->
      "isnil (" ^string_of_term l ^ " )"      
  | TmHead h->
      "head (" ^string_of_term h ^ " )"      
  | TmTail t->
      "tail (" ^string_of_term t ^ " )"      
  | TmUnit ->
      "unit"

;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t -> 
      free_vars t
  | TmRecord ((tag, t1), t2) ->
      ldif (lunion (free_vars t1) (free_vars t2)) [tag]
  | TmString s -> 
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmTuple (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmProj (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmNil -> 
      []
  | TmIsNil l-> 
      free_vars l
  | TmCons (l1, l2)-> 
      lunion (free_vars l1) (free_vars l2)
  | TmHead h-> 
      free_vars h      
  | TmTail t-> 
      free_vars t
  | TmUnit ->
      []
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmRecord ((tag, t1), t2) ->
      TmRecord ((tag, subst x s t1), subst x s t2)
  | TmString s ->
      TmString s    
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmTuple (t1, t2) ->
      TmTuple (subst x s t1, subst x s t2)
  | TmProj (t1, t2) ->
      TmProj (subst x s t1, subst x s t2)
  | TmNil ->
      TmNil
  | TmIsNil l-> 
      TmIsNil (subst x s l)
  | TmCons (l1, l2)-> 
      TmCons (subst x s l1, subst x s l2)
  | TmHead h-> 
      TmHead (subst x s h)    
  | TmTail t-> 
      TmTail (subst x s t)
  | TmUnit ->
      TmUnit
;;

let apply_ctx ctx tm =
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmRecord _ -> true
  | TmString s -> true  
  | TmTuple _ -> true
  | TmCons _ -> true
  | t when isnumericval t -> true
  | TmUnit -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2) 

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'

  | TmRecord ((tag, t1), t2) when isval t1 ->
      let t2' = eval1 ctx t2 in TmRecord ((tag, t1), t2')

  | TmRecord((tag,t1), t2) ->
      let t1' = eval1 ctx t1 in
      TmRecord((tag, t1'), t2)
    (* E-ConcatV *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

    (* E-Concat2 *)
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 ctx t2 in
      TmConcat (TmString s1, t2')

    (* E-Concat1 *)
  | TmConcat (t1, TmString s2) ->
      let t1' = eval1 ctx t1 in
      TmConcat (t1', TmString s2)

   (* E-Tuple2*)
   | TmTuple (t1, t2) when isval t1 ->
    let t2' = eval1 ctx t2 in
      TmTuple(t1, t2')

    (* E-Tuple1*)
  | TmTuple (t1, t2) ->
    let t1' = eval1 ctx t1 in
      TmTuple(t1', t2)

    (* E-Cons2*)
  | TmCons (t1, t2) when isval t1->
      let t2' = eval1 ctx t2 in
        TmCons (t1, t2')

   (* E-Cons1*)
  | TmCons (t1, t2) ->
      let t1' = eval1 ctx t1 in
        TmCons (t1', t2)

    (* E-Proj2*)
  | TmProj (TmTuple (t11, t12), TmZero) ->
      t11

    (* E-ProjErr*)
  | TmProj (TmTuple (t11, TmNil), t2) ->
      raise (Invalid_argument "index out of bounds")

    (* E-Proj1*)
  | TmProj (TmTuple (t11, t12), t2) ->
      let t2' = eval1 ctx (TmPred t2) in
      TmProj(t12, t2')

    (* E-RecProj2*)
  | TmProj (TmRecord ((tag1,t1), TmNil), (TmVar projTag)) ->
      if tag1 = projTag then t1
      else raise (Invalid_argument "key not found")

    (* E-RecProj1*)
  | TmProj (TmRecord ((tag1,t1),t2), (TmVar projTag)) ->
      if tag1 = projTag then t1
      else TmProj (t2,(TmVar projTag))
    
    (* E-Proj*)
  | TmProj (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmProj (t1', t2)

    (* E-IsNilNil*)
  | TmIsNil (TmCons(TmNil,TmNil)) ->
        TmTrue

   (* E-IsNilCons*)
  | TmIsNil t1 when isval t1 ->
      TmFalse
      
    (* E-IsNil*)
  | TmIsNil t1 ->
      let t1' = eval1 ctx t1 in
        TmIsNil t1'
        
    (* E-HeadCons*)
  | TmHead (TmCons(v1, v2)) ->
        v1        
        
   (* E-Head*)
  | TmHead h1 ->
      let h1' = eval1 ctx h1 in
        TmHead h1'

   (* E-TailCons*)
  | TmTail (TmCons(v1, v2)) ->
      v2
      
    (* E-Tail*)
  | TmTail t1 ->
      let t1' = eval1 ctx t1 in
        TmTail t1'
               

  | TmVar s ->
      getvbinding ctx s
    
  | _ ->
      raise NoRuleApplies
;;

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;

let execute ctx = function
    Eval tm ->
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      ctx
  | Bind (s, tm) ->
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      addbinding ctx s tyTm tm'
  ;;