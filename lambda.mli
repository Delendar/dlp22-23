
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

val emptyctx : context;;
val addtbinding : context -> string -> ty -> context;;
val addbinding : context -> string -> ty -> term -> context;;
val gettbinding : context -> string -> ty;;
val getvbinding : context -> string -> term;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : context -> term -> term;;

val execute : context -> command -> context;;

