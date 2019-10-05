(***************************************************)
(*                                                 *)
(*                 MakeTuple.ml                    *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2018 - 2019                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open Apron
open Partition
open Tuple
open Constraints


(** Creates a tuple abstract domain parameterized by a Partition. *)
module Maketuple(P: PARTITION): TUPLE  = struct

  (** The partition represents a value of one component of a tuple. *)
  module P = P 


  (** An element of the tuple. *)
  type t = { 
    elems : P.t list; (* representation as list of Partition elements *)
    env : Environment.t; (* APRON environment *)
    vars : var list; (* list of variables in the APRON environment *)	
    configs : string list list (* list of configurations *)
  }


  (** The current representation as list of Partition components. *)
  let elems u: P.t list = u.elems

  (** The current APRON environment. *)
  let env u = u.env

  (** The current list of variables in the APRON environment. *)
  let vars u = u.vars
  
  (** The current configurations. *)
  let configs u = u.configs
  

  (**)

  let bot e vs c = {
    elems = (let l = ref [] in List.iter (fun key -> l := (P.bot e vs) :: !l ) c; !l);
    env = e;
    vars = vs;	
    configs = c
  }

  let inner e vs c el = {
    elems = el;
    env = e;
    vars = vs; 
	configs = c
  }

  let top e vs c = {
    elems = (let l = ref [] in List.iter (fun key -> l := (P.top e vs) :: !l ) c; !l);
    env = e;
    vars = vs;
	configs = c
  }

  (*  *)
  

  let isBot u =
  	let b = ref true in List.iter (fun elem -> if (not (P.isBot elem)) then b := false ) u.elems; !b


  let isLeq u1 u2 = 
    let b = ref true in List.iter (fun key -> if (not key) then b:=false) (List.map2 (fun e1 e2 -> (P.isLeq e1 e2) ) u1.elems u2.elems); !b

  (**)


  (* let to_apron_t (u:t) : apron_t list = 
    let l = ref [] in List.iter (fun elem -> l := (P.to_apron_t elem) :: !l ) u; !l

  let of_apron_t env vars configs (a:apron_t list) : t = 
    let elems = ref [] in 
		List.iter (fun elem -> elems := (P.of_apron_t elem) :: !elems ) a; 
		{ elems = !elems; env = env; vars = vars; configs = configs }  *)

  let join u1 u2 = 
    let elems = List.map2 (fun e1 e2 -> (P.join e1 e2)) u1.elems u2.elems in 
	let env = u1.env in
    let vars = u1.vars in 
	let configs = u1.configs in 
	{ elems = elems; env = env; vars = vars; configs = configs}	


  let widen u1 u2 = 
    let elems = List.map2 (fun e1 e2 -> (P.widen e1 e2)) u1.elems u2.elems in
	let env = u1.env in
    let vars = u1.vars in 
	let configs = u1.configs in 
	{ elems = elems; env = env; vars = vars; configs = configs}		

  let meet u1 u2 = 
    let elems = List.map2 (fun e1 e2 -> (P.meet e1 e2)) u1.elems u2.elems in
	let env = u1.env in
    let vars = u1.vars in 
	let configs = u1.configs in 
	{ elems = elems; env = env; vars = vars; configs = configs}		

  (*  *)



  let fwdAssign u (x,e) = match x with
    | A_var x ->
      let elems = List.map (fun b -> (P.fwdAssign b (A_var x,e))) u.elems in 
	  let env = u.env in
      let vars = u.vars in 
	  let configs = u.configs in 
	  { elems = elems; env = env; vars = vars; configs = configs}		  
    | _ -> raise (Invalid_argument "fwdAssign: unexpected lvalue")


  let bwdAssign_underapprox (u:t) ((x,e): aExp * aExp) : t = match x with
    | A_var x ->
      	if not P.supports_underapproximation then
        	raise (Invalid_argument "Underapproximation not supported by this abstract domain, use polyhedra instead");	
		let elems = List.map (fun b -> P.bwdAssign_underapprox b (A_var x,e)) u.elems in 
		let env = u.env in
        let vars = u.vars in 
		let configs = u.configs in 
		{ elems = elems; env = env; vars = vars; configs = configs}
    | _ -> raise (Invalid_argument "bwdAssign_underapprox: unexpected lvalue")

  let bwdAssign u (x,e) = match x with
    | A_var x ->
      let elems = List.map (fun b -> P.bwdAssign b (A_var x,e)) u.elems in 
	  let env = u.env in
      let vars = u.vars in 
	  let configs = u.configs in 
	  { elems = elems; env = env; vars = vars; configs = configs}		  
    | _ -> raise (Invalid_argument "bwdAssign: unexpected lvalue")


  (* we are here *)
  
  let filter_underapprox (u:t) (e:bExp) : t = 
    if not P.supports_underapproximation then
        raise (Invalid_argument "Underapproximation not supported by this abstract domain, use polyhedra instead");  
	let elems = List.map (fun b -> P.bwdfilter_underapprox b e) u.elems in 
	let env = u.env in
    let vars = u.vars in 
	let configs = u.configs in 
	{ elems = elems; env = env; vars = vars; configs = configs}


  let rec filter u e =
    let elems = List.map (fun b -> P.filter b e) u.elems in 
	let env = u.env in
    let vars = u.vars in 
	let configs = u.configs in 
	{ elems = elems; env = env; vars = vars; configs = configs}	

	let rec mem x = function 
		| [] -> false
		| hd :: tl -> if (hd = x) then true else (mem x tl)
	
  let rec config_filter u e =
    match e with
    | A_TRUE -> u
    | A_MAYBE -> u
    | A_FALSE -> bot u.env u.vars u.configs
	| A_bvar v -> 
		let elems = List.map2 (fun el1 el2 -> if (el1) then el2 else (P.bot u.env u.vars)) (List.map (fun el -> mem v.varName el) u.configs) u.elems in 
		let env = u.env in
    	let vars = u.vars in 
		let configs = u.configs in 
		{ elems = elems; env = env; vars = vars; configs = configs}
    | A_bunary (o,e) ->
      (match o with
       | A_NOT -> let (e,_) = e in let b = config_filter u e in {elems = List.map2 (fun el1 el2 -> if (P.isBot el1) then el2 else (P.bot u.env u.vars)) b.elems u.elems; env = u.env; vars = u.vars; configs = u.configs})
    | A_bbinary (o,(e1,_),(e2,_)) ->
      let b1 = config_filter u e1 and b2 = config_filter u e2 in
      (match o with
       | A_AND -> meet b1 b2
       | A_OR -> join b1 b2)
    | _ -> raise (Invalid_argument "config_filter: unexpected boolean expression")
	
  let rec config_filter_not u u' =	
		{elems = List.map2 (fun el1 el2 -> if (P.isBot el1) then el2 else (P.bot u.env u.vars)) u'.elems u.elems; env = u.env; vars = u.vars; configs = u.configs}
(**)

  let print fmt u =
    Format.fprintf fmt "{ ";
	List.iter (fun el -> P.print fmt el; Format.fprintf fmt "; ") u.elems;
	Format.fprintf fmt " }"

  let print_assert fmt u =
    Format.fprintf fmt "{ ";
	List.iter (fun el -> (if (P.isBot el) then Format.fprintf fmt "CORRECT: " else Format.fprintf fmt "ERROR: "); P.print fmt el; Format.fprintf fmt "; ") u.elems;
	Format.fprintf fmt " }"

end

(** Single Tuple domain parameterized by the boxes numerical abstract domain. *)
 module TB = Maketuple(Numerical.B)

(** Single Tuple domain parameterized by the octagons abstract domain. *)
module TO = Maketuple(Numerical.O)

(** Single Tuple domain parameterized by the polyhedra abstract domain. *)
module TP = Maketuple(Numerical.P)
