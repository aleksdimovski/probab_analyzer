(***************************************************)
(*                                                 *)
(*                 BDDDomain.ml                    *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2018 - 2019                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open Bddapron
open Cudd 

type error = unit

(** Signature for a single partition of the domain of a ranking function. *)
module type BDDDomain = sig
  
  type domain (* = ('a, Param.lib) Bddapron.Mtbdddomain1.t *)
  type env (* = 'a Bddapron.Env.t (ou string ?)*)
  
  type apron_expr (* string Bddapron.Expr1.Apron.t*)
  type bool_expr (* string bddapon.Expr1.Bool.t*)
  type bint_expr
  type expr
  type label (* Bddapron.Expr1.Bint.t || int ?*)
  
  type t
  val dom : t -> domain
  val env : t -> string Bddapron.Env.t
  val vars : t -> var list  
  val feats : t -> var list
  val configs : t -> string list list  

(*  val apron : Param.lib Apron.Manager.t*)
(*  val man : ('a, Param.lib) Bddapron.Mtbdddomain0.man*)
(*  val init : unit -> Cudd.Man.v Cudd.Man.t*)
  (* val cudd : Cudd.Man.v Cudd.Man.t *)
  (* val cond : string Bddapron.Cond.t *)
  val init : unit -> unit 
  val mkenv : unit -> string Bddapron.Env.t 
  val empty : unit -> domain

  
  val bot : string Bddapron.Env.t -> var list -> var list -> string list list -> t
  val top : string Bddapron.Env.t -> var list -> var list -> string list list -> t
  val inner : string Bddapron.Env.t -> var list -> var list -> string list list -> domain -> t
  
  val isBot : t -> bool
  val eq : t -> t -> bool
  val isLeq : t -> t -> bool
	
  val print : Format.formatter -> t -> unit

  val fwdAssign : t -> aExp * aExp -> t
  val filter : t -> bExp -> t
  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t
  val widen_threshold : t -> t -> Apron.Lincons1.earray -> t
  
  
  val getenv : domain -> string Bddapron.Env.t 
  val getcudd : unit -> Cudd.Man.v Cudd.Man.t
  

  val to_lincons_array : domain -> Apron.Lincons1.earray
  val get_apron_env : domain -> Apron.Environment.t

  val bint_cst : env -> int -> int -> bint_expr
  val bint_expr : bint_expr -> expr

  val apron_cst : env -> Apron.Coeff.t -> apron_expr
  val apron_int : env -> int -> int -> apron_expr
  val apron_add : apron_expr -> apron_expr -> apron_expr
  val apron_sub : apron_expr -> apron_expr -> apron_expr
  val apron_mul : apron_expr -> apron_expr -> apron_expr
  val apron_div : apron_expr -> apron_expr -> apron_expr
  val apron_gmod : apron_expr -> apron_expr -> apron_expr
  val apron_neg : apron_expr -> apron_expr
  val apron_var : env -> string -> apron_expr
  val apron_eq : apron_expr -> bool_expr
  val apron_supeq : apron_expr -> bool_expr
  val apron_sup : apron_expr -> bool_expr
  val apron_print : Format.formatter -> apron_expr -> unit
  val apron_expr : apron_expr -> expr

  val bool_var : env -> string -> bool_expr
  val bool_and : bool_expr -> bool_expr -> bool_expr
  val bool_or : bool_expr -> bool_expr -> bool_expr
  val bool_not : bool_expr -> bool_expr
  val bool_eq : bool_expr -> bool_expr -> bool_expr
  val bool_true : env -> bool_expr
  val bool_false : env -> bool_expr
  val bool_print : Format.formatter -> bool_expr -> unit 
  val bool_expr : bool_expr -> expr

  val expr_print : Format.formatter -> expr -> unit
  val expr_var : env -> string -> expr
  val env_print : Format.formatter -> env -> unit


  (* val add_vars : ?init:bool -> domain -> (string * string Bddapron.Env.typ) list -> domain *)
  val del_vars : domain -> (string * 'a) list -> domain
  val ren_vars : domain -> (string * 'b) list -> (string * 'c) list -> domain

end