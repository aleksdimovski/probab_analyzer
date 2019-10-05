(***************************************************)
(*                                                 *)
(*      Forward/Backward BDD Analysis Iterator     *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2018 - 2019                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open InvMap
open Apron
open Bddapron
open Iterator
open Partition
open BDDDomain

module BDDAnalysisIterator (B: BDDDomain) =
struct

  module B = B

  let list_configs = ref []
  let fwdInvMap = ref InvMap.empty

  let addFwdInv l (a:B.t) = fwdInvMap := InvMap.add l a !fwdInvMap

  let fwdMap_print fmt m = InvMap.iter (fun l a -> 
      Format.fprintf fmt "%a: %a\n" label_print l B.print a) m


  (* Forward Iterator *)

  let rec fwdStm funcs (env: string Bddapron.Env.t) vars feats configs p s =
    match s with
    | A_label _ -> p
    | A_return -> B.bot env vars feats configs
    | A_assign ((l,_),(e,pos)) -> B.fwdAssign p (l,e)
    | A_assert (b,ba) -> (*B.filter p b*)
	      let p2' = (B.filter p b) in
      	  let p2 = B.filter p (fst (negBExp (b,ba))) in
		  if (not (B.isBot p2)) then (Format.fprintf !fmt "\nassert (%a) ERROR:\n %a\n" bExp_print_aux b B.print p2; p2') 
		  else (Format.fprintf !fmt "\nassert (%a) CORRECT\n" bExp_print_aux b; p2')	
    | A_if ((b,ba),s1,s2) ->
      let p1' = fwdBlk funcs env vars feats configs (B.filter p b) s1 in
      let p2 = B.filter p (fst (negBExp (b,ba))) in
      let p2' = fwdBlk funcs env vars feats configs p2 s2 in
      B.join p1' p2'
    | A_ifdef ((b,ba),s1,s2) ->
	  let p1 = B.filter p b in 
	  let p2 = B.filter p (fst (negBExp (b,ba))) in
      let p1' = fwdBlk funcs env vars feats configs p1 s1 in  
      let p2' = fwdBlk funcs env vars feats configs p2 s2 in
      (* B.print !fmt p; B.print !fmt p1; B.print !fmt p2; print_endline ""; B.print !fmt (B.join p1' p2'); print_endline "here";  *)
	  B.join p1' p2'
    | A_while (l,(b,ba),s) ->
      let rec aux i p2 n =
        let i' = B.join p p2 in
        if !tracefwd && not !minimal then begin
          Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
          Format.fprintf !fmt "p1: %a\n" B.print p;
          Format.fprintf !fmt "i: %a\n" B.print i;
          Format.fprintf !fmt "p2: %a\n" B.print p2;
          Format.fprintf !fmt "i': %a\n" B.print i'
        end;
        if B.isLeq i' i then i else
          let i'' = if n <= !joinfwd then i' else 
              B.widen i (B.join i i') in
          if !tracefwd && not !minimal then
            Format.fprintf !fmt "i'': %a\n" B.print i'';
          aux i'' (fwdBlk funcs env vars feats configs (B.filter i'' b) s) (n+1)
      in
      let i = B.bot env vars feats configs in
      let p2 = fwdBlk funcs env vars feats configs (B.filter i b) s in
      let p = aux i p2 1 in
      addFwdInv l p; B.filter p (fst (negBExp (b,ba)))
    | A_call (f,ss) ->
      let f = StringMap.find f funcs in
      let p = List.fold_left (fun ap (s,_) -> 
          fwdStm funcs env vars feats configs p s) p ss in
      fwdBlk funcs env vars feats configs p f.funcBody
    | A_recall (f,ss) -> B.top env vars feats configs (* TODO *)

  and fwdBlk funcs (env: string Bddapron.Env.t) vars feats configs (p:B.t) (b:block) : B.t =
    let result_print l p =
      Format.fprintf !fmt "### %a ###: %a\n" label_print l B.print p
    in
    match b with
    | A_empty l ->
      if !tracefwd && not !minimal then result_print l p;
      addFwdInv l p; p
    | A_block (l,(s,_),b) ->
      if !tracefwd && not !minimal then result_print l p;
      addFwdInv l p; 
      fwdBlk funcs env vars feats configs (fwdStm funcs env vars feats configs p s) b


  (* Analyzer *)

let rec process list = 
	if List.length list = 0 then [[]]
	else match list with
		| [] -> []
		| hd :: tl -> 
			let with_hd = List.map (fun l -> hd :: l) (process tl) in 
			let without_hd = (process tl) in 
			with_hd @ without_hd;;
			
let print_listlist l =
  List.iter (fun el -> print_string "config: "; List.iter (fun elem -> Printf.printf "%s " elem) el; print_endline "") l;;  			

let rec aux_int xs env = match xs with
      | [] -> env
      | x::xs -> 
        aux_int xs (Env.add_vars env [(x.varId, `Int)]);;

let rec aux_bool xs env = match xs with
      | [] -> env
      | x::xs -> 
        aux_bool xs (Env.add_vars env [(x.varName, `Bool)] );;
		
(* IMPORTANT FUNCTION THAT DOES THE ANALYSIS*)
  let analyze (vars,stmts,funcs) main =
    let f = StringMap.find main funcs in
    let v1 = snd (List.split (StringMap.bindings vars)) in
    let v2 = snd (List.split (StringMap.bindings f.funcVars)) in
    let vars = List.append v1 v2 in
    B.init(); (*List.iter (var_print !fmt) vars;  *)
	(* let cudd = Cudd.Man.make_v () in  *)
	let envemp = Env.make ~symbol:Env.string_symbol (B.getcudd ()) in 
	(*let env1 = envemp in *)
	let env1 = aux_int vars envemp in
    let s = f.funcBody in
    (*initBlk env vars stmts; initBlk env vars s; *)
    (* TODO: handle functions calls *)
    (* Forward Analysis *)
    if !tracefwd && not !minimal then
      Format.fprintf !fmt "\nForward Analysis Trace:\n";
    let startfwd = Sys.time () in
	let list_feat = ref [] in
	let list_featV = ref [] in
	StringMap.iter (fun key value -> list_feat := key :: !list_feat; list_featV := value :: !list_featV ) !ItoA.features; list_configs := process !list_feat; 
	(*List.iter (var_print !fmt) !list_featV; print_listlist !list_configs; *)
	let env = aux_bool !list_featV (env1) in
    let state = fwdBlk funcs env vars !list_featV !list_configs (fwdBlk funcs env vars !list_featV !list_configs (B.top env vars !list_featV !list_configs) stmts) s in
    let stopfwd = Sys.time () in
    if not !minimal then
      begin
        if !timefwd then
          Format.fprintf !fmt "\nForward Analysis (Time: %f s):\n" (stopfwd-.startfwd)
        else
          Format.fprintf !fmt "\nForward Analysis:\n";
        fwdMap_print !fmt !fwdInvMap;
      end;
	  true

end
