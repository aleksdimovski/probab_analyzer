(***************************************************)
(*                                                 *)
(*    Forward/Backward Tuple Analysis Iterator     *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2018 - 2019                   *)
(*                                                 *)
(***************************************************)

open AbstractSyntax
open InvMap
open Apron
open Iterator
open Partition
open Tuple

module TupleAnalysisIterator (B: TUPLE) =
struct

  module B = B

  let list_configs = ref []
  let fwdInvMap = ref InvMap.empty

  let addFwdInv l (a:B.t) = fwdInvMap := InvMap.add l a !fwdInvMap

  let fwdMap_print fmt m = InvMap.iter (fun l a -> 
      Format.fprintf fmt "%a: %a\n" label_print l B.print a) m


  (* Forward Iterator *)

  let rec fwdStm funcs env vars configs p s =
    match s with
    | A_label _ -> p
    | A_return -> B.bot env vars configs
    | A_assign ((l,_),(e,_)) -> B.fwdAssign p (l,e)
    | A_assert (b,ba) -> (*B.filter p b*)
	      let p2' = (B.filter p b) in
      	  let p2 = B.filter p (fst (negBExp (b,ba))) in
		  if (not (B.isBot p2)) then (Format.fprintf !fmt "\nassert (%a) \n %a\n" bExp_print_aux b B.print_assert p2; p2') 
		  else (Format.fprintf !fmt "\nassert (%a) CORRECT\n" bExp_print_aux b; p2')	
    | A_if ((b,ba),s1,s2) ->
      let p1' = fwdBlk funcs env vars configs (B.filter p b) s1 in
      let p2 = B.filter p (fst (negBExp (b,ba))) in
      let p2' = fwdBlk funcs env vars configs p2 s2 in
      B.join p1' p2'
    | A_ifdef ((b,ba),s1,s2) ->
	  let p1 = B.config_filter p b in 
      let p1' = fwdBlk funcs env vars configs p1 s1 in
      let p2 = B.config_filter_not p p1 in
      let p2' = fwdBlk funcs env vars configs p2 s2 in
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
          aux i'' (fwdBlk funcs env vars configs (B.filter i'' b) s) (n+1)
      in
      let i = B.bot env vars configs in
      let p2 = fwdBlk funcs env vars configs (B.filter i b) s in
      let p = aux i p2 1 in
      addFwdInv l p; B.filter p (fst (negBExp (b,ba)))
    | A_call (f,ss) ->
      let f = StringMap.find f funcs in
      let p = List.fold_left (fun ap (s,_) -> 
          fwdStm funcs env vars configs p s) p ss in
      fwdBlk funcs env vars configs p f.funcBody
    | A_recall (f,ss) -> B.top env vars configs (* TODO *)

  and fwdBlk funcs env vars configs (p:B.t) (b:block) : B.t =
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
      fwdBlk funcs env vars configs (fwdStm funcs env vars configs p s) b


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

(* IMPORTANT FUNCTION THAT DOES THE ANALYSIS*)
  let analyze (vars,stmts,funcs) main =
    let rec aux xs env = match xs with
      | [] -> env
      | x::xs -> 
        aux xs (Environment.add env [|(Var.of_string x.varId)|] [||])
    in
    let f = StringMap.find main funcs in
    let v1 = snd (List.split (StringMap.bindings vars)) in
    let v2 = snd (List.split (StringMap.bindings f.funcVars)) in
    let vars = List.append v1 v2 in
    let env = aux vars (Environment.make [||] [||]) in
    let s = f.funcBody in
    (*initBlk env vars stmts; initBlk env vars s; *)
    (* TODO: handle functions calls *)
    (* Forward Analysis *)
    if !tracefwd && not !minimal then
      Format.fprintf !fmt "\nForward Analysis Trace:\n";
    let startfwd = Sys.time () in
	let list_feat = ref [] in
	StringMap.iter (fun key value -> list_feat := key :: !list_feat ) !ItoA.features; list_configs := process !list_feat; (*print_listlist !list_configs;*)
    let state = fwdBlk funcs env vars !list_configs (fwdBlk funcs env vars !list_configs (B.top env vars !list_configs) stmts) s in
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
