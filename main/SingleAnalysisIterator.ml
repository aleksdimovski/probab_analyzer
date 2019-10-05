(***************************************************)
(*                                                 *)
(*    Forward/Backward Single Analysis Iterator    *)
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
(*open Yices*)

module SingleAnalysisIterator (B: PARTITION) =
struct

  	module B = B
	module InvMapTuple = Map.Make(struct type t=label*int let compare=compare end)

	(* exceptions *)
	exception Bottom_error

	let fwdInvMapTuple = ref InvMapTuple.empty
	let addFwdInvTuple (l,n) (a:B.t) = fwdInvMapTuple := InvMapTuple.add (l,n) a !fwdInvMapTuple
	let bwdInvMapTuple = ref InvMapTuple.empty
	let addBwdInvTuple (l,n) (a:B.t) = bwdInvMapTuple := InvMapTuple.add (l,n) a !bwdInvMapTuple	
	let bwdOverInvMapTuple1 = ref InvMapTuple.empty
	let addBwdOverInvTuple1 (l,n) (a:B.t) = bwdOverInvMapTuple1 := InvMapTuple.add (l,n) a !bwdOverInvMapTuple1	
	let bwdOverInvMapTuple2 = ref InvMapTuple.empty
	let addBwdOverInvTuple2 (l,n) (a:B.t) = bwdOverInvMapTuple2 := InvMapTuple.add (l,n) a !bwdOverInvMapTuple2	
	
  	let fwdInvMap = ref InvMap.empty
  	let addFwdInv l (a:B.t) = fwdInvMap := InvMap.add l a !fwdInvMap

  	let fwdMap_print fmt m1 m2 = InvMap.iter (fun l a -> if l>0 then (
		if InvMapTuple.mem (l,1) m2 then ( for i = 1 to !joinbwd do 
												Format.fprintf fmt "%a unwind %d: %a\n" label_print l (i-1) B.print (InvMapTuple.find (l,i) !fwdInvMapTuple) 
											done; 
											Format.fprintf fmt "%a unwind >%d: %a\n" label_print l (!joinbwd-1) B.print a )
      	else Format.fprintf fmt "%a: %a\n" label_print l B.print a)
		) m1
	
	let findFwdInv l m : B.t = InvMap.find l m

  	let bwdInvMap = ref InvMap.empty
  	let addBwdInv l (a:B.t) = bwdInvMap := InvMap.add l a !bwdInvMap

  	let bwdMap_print fmt m1 m2 = let sr = Stack.create () in
								 InvMap.iter (fun l a -> Stack.push l sr) m1;
								 Stack.iter (fun l -> 
									if InvMapTuple.mem (l,!joinbwd) m2 then (  Format.fprintf fmt "%a unwind >%d: %a\n" label_print l (!joinbwd-1) B.print (InvMapTuple.find (l,!joinbwd) m2);
									for i = !joinbwd-1 downto 1 do 
												Format.fprintf fmt "%a unwind %d: %a\n" label_print l i B.print (InvMapTuple.find (l,i) m2) 
									done;
									Format.fprintf fmt "%a unwind 0: %a\n" label_print l B.print (InvMap.find l m1)
									)	
      								else Format.fprintf fmt "%a: %a\n" label_print l B.print (InvMap.find l m1)								 
								 ) sr

  	let bwdOver2Map_print fmt m1 = let sr = Stack.create () in
								 InvMap.iter (fun l a -> Stack.push l sr) m1;
								 Stack.iter (fun l -> Format.fprintf fmt "%a: %a\n" label_print l B.print (InvMap.find l m1) ) sr
		
  	let bwdOverInvMap1 = ref InvMap.empty
  	let addBwdOverInv1 l (a:B.t) = bwdOverInvMap1 := InvMap.add l a !bwdOverInvMap1

  	let bwdOverInvMap2 = ref InvMap.empty
  	let addBwdOverInv2 l (a:B.t) = bwdOverInvMap2 := InvMap.add l a !bwdOverInvMap2
  (* Forward Iterator *)

	(*let ls = Unix.open_process_in "ls"
	try
  		while true do
    		Printf.printf "%s\n" (input_line ls)
  		done
	with End_of_file -> ()
		Unix.close_process_in ls*)

(*let string_of_command =
 let lines = ref [] in
 let file = "out.txt" in 
 let _ = Sys.command @@ "count latte &> out.txt" in
 let chan = open_in file in
  	begin
    try
    	while true do
       		lines := input_line chan :: !lines
     	done; 
    with End_of_file -> close_in chan
  	end;
  	List.rev !lines *)
 
  	let read_process_lines command =
  		let lines = ref [] in
  		let in_channel = Unix.open_process_in command in
  		begin
    	try
      		while true do
        		lines := input_line in_channel :: !lines
      		done;
    	with End_of_file ->
      	ignore (Unix.close_process_in in_channel)
  		end;
  		List.rev !lines

(*	let process_output_to_list2 = fun command -> 
  		let chan = Unix.open_process_in command in
  		let res = ref ([] : string list) in
  		let rec process_otl_aux () =  
    		let e = input_line chan in
    		res := e::!res; print_string e ;
    		process_otl_aux() in
  		try process_otl_aux ()
  		with End_of_file ->
    		let stat = Unix.close_process_in chan in (List.rev !res,stat) *)


	let rec print_list n  = function 
		[] -> ()
		| e::l -> print_int n; print_string e ; print_string " \n" ; print_list (n+1) l 


	let rec print_result n  = function 
		[] -> ()
		| e::l -> if (n=1) then (if (e="sat") then (print_string "There is a solution \n"; print_result (n+1) l) else (print_string "There is NO solution \n"))
					else (print_string e ; print_string " \n" ; print_result (n+1) l)

	let rec print_Latteresult  = function 
		[] -> 1
		| e :: [] -> int_of_string e
		| e::l -> print_Latteresult l


	let partition_toYices (p:B.t) =
		let lines = ref [] in
		List.iter (fun v -> lines := !lines @ [("(define " ^ v.varName ^ "::int)")]) (B.vars p); 
		List.iter (fun s -> lines := !lines @ [(s)]) (B.to_stringYices p);
		lines := !lines @ [("(check)")] @ [("(show-model)")];
		String.concat "\n" !lines;;	

	let write_to_Yicesfile file (p:B.t) =
		let oc = open_out file in    						    (* create or truncate file, return channel *)
  		Printf.fprintf oc "%s\n" (partition_toYices p); 		(*(process_message message); *)   (* write something *)   
  		close_out oc;;


	let partition_toLatte (p:B.t) =
		let lines = ref [] in
		let countVars = ref 0 in 
		let liveVars = B.varsLive p in 
		List.iter (fun v -> print_string v.varName; print_string " ") liveVars;
		(*countLines := List.length (B.constraints p); (*print_list 1 (List.map (fun v -> v.varName) (B.varsLive p));*)
		print_string "len0 = "; print_int (!countLines); *)
		countVars := (List.length liveVars) + 1;		
		let (lin,eq,countLines) = B.to_stringLatte p liveVars in
		lines := !lines @ [((string_of_int countLines) ^ " " ^ (string_of_int !countVars))]; 
		List.iter (fun s -> lines := !lines @ [(s)]) lin;
		if ((List.length eq)>0) then 
			( 	let lin = ref "linearity " in 
				lin := !lin ^ string_of_int (List.length eq);
				List.iter (fun e -> lin := !lin ^ " " ^ string_of_int e) eq; 
				lines := !lines @ [(!lin)]; );
		(*List.iter (fun s -> lines := !lines @ [(s)]) (B.to_stringYices p);
		lines := !lines @ [("(check)")] @ [("(show-model)")]; *)
		String.concat "\n" !lines;;	

	let write_to_Lattefile file (p:B.t) =
		let oc = open_out file in    						    (* create or truncate file, return channel *)
  		Printf.fprintf oc "%s\n" (partition_toLatte p); 		(*(process_message message); *)   (* write something *)   
  		close_out oc;;




  let rec fwdStm funcs env vars p s =
    match s with
    | A_label (s,sa) -> p
    | A_return -> B.bot env vars
    | A_assign ((l,_),(e,_)) -> B.fwdAssign p (l,e)
    | A_assert (b,ba) ->
		  let b' = negBExp (b,ba) in
      	  let p2 = B.filter p (fst (b')) in	
	      let p2' = (B.filter p b) in 
		  addFwdInv (-1) (p2'); addFwdInv (-2) (p2);  p2'
		  (* let b' = negBExp (b,ba) in
      	  let p2 = B.filter p (fst (b')) in
		  let file = "latte" in 
		  write_to_Lattefile file p2; 
		  (*let l2 = read_process_lines ("count "^(file)) in 
		  let points = print_Latteresult l2 in print_string "Points are "; print_int points;*) let points = 10 in 
		  (*let s2 = string_of_command in print_Latteresult s2;*)
		  (*let file = "example.ys" in 
		  write_to_Yicesfile file p2; 
		  let l2 = read_process_lines ("yices "^(file)) in *) (*print_result 1 l2;*)
		  (* 	CALL EXTERNAL SOLVER HERE - TO RESOLVE ASSERTION 		*)
		  (*Format.fprintf !fmt "\n neg b = (%a) \n" bExp_print b';*)
		  let totalPoints = ref 1 in 
		  StringMap.iter (fun key value -> totalPoints := !totalPoints * value; Format.fprintf !fmt " init: %s %d \n" key value) !ItoA.spaceVars;
		  if (not (B.isBot p2)) then (Format.fprintf !fmt "\nassert (%a) ERROR:\n %a\n" bExp_print_aux b B.print p2; 
		  							  Format.fprintf !fmt "PROBABLITY OF ERROR IS: %d / %d = %.4f percent \n" points !totalPoints ((float points) /. (float !totalPoints)); (*print_Latteresult l2;*) p2') 
		  else (Format.fprintf !fmt "\nassert (%a) CORRECT\n" bExp_print_aux b; p2')  *)
    | A_if ((b,ba),s1,s2) ->
      let p1' = fwdBlk funcs env vars (B.filter p b) s1 in
      let p2 = B.filter p (fst (negBExp (b,ba))) in
      let p2' = fwdBlk funcs env vars p2 s2 in
      B.join p1' p2'
    | A_ifdef ((b,ba),s1,s2) -> p  
    | A_while (l,(b,ba),s) ->
      let rec aux i p2 n =
        let i' = B.join p p2 in
		(*Format.fprintf !fmt "%d inv: %a\n" n B.print i';*)
		addFwdInvTuple (l,n) i';
        if !tracefwd && not !minimal then begin
          Format.fprintf !fmt "### %a:%i ###:\n" label_print l n;
          Format.fprintf !fmt "p1: %a\n" B.print p;
          Format.fprintf !fmt "i: %a\n" B.print i;
          Format.fprintf !fmt "p2: %a\n" B.print p2;
          Format.fprintf !fmt "i': %a\n" B.print i'
        end;
        if B.isLeq i' i then (if n < !joinfwd then ((*Format.fprintf !fmt "joinbwd: %d\n" n;*) Iterator.joinbwd := n); i) else
          let i'' = if n <= !joinfwd then i' else 
              B.widen i (B.join i i') in
          if !tracefwd && not !minimal then
            Format.fprintf !fmt "i'': %a\n" B.print i'';
          aux i'' (fwdBlk funcs env vars (B.filter i'' b) s) (n+1)
      in
	  (*Format.fprintf !fmt "before inv: %a\n" B.print p;*)
      let i = B.bot env vars in
      let p2 = fwdBlk funcs env vars (B.filter i b) s in
      let pp = aux i p2 1 in
	  (*Format.fprintf !fmt "inv: %a\n" B.print p;*)
	  let p_down = fwdBlk funcs env vars (B.filter pp b) s in   (* this line is added additionally: performs narrowing  *)
	  let p_final = B.join p_down (B.filter p (fst (negBExp (b,ba)))) in (* this line is added to handle example1.c, when the loop variable is non-deterministic *)
	  (*Format.fprintf !fmt "inv narrow down: %a\n" B.print p_down;
	  Format.fprintf !fmt "inv narrow+join final: %a\n" B.print p_final;*)
	  let p_final_neg = B.filter p_final (fst (negBExp (b,ba))) in 
	  let p_down_neg = B.filter p_down (fst (negBExp (b,ba))) in 
	  (*Format.fprintf !fmt "inv narrow final neg: %a\n" B.print p_final_neg;
	  Format.fprintf !fmt "inv narrow down neg: %a\n" B.print p_down_neg;*)
      addFwdInv l p_down; p_final_neg
    | A_call (f,ss) ->
      let f = StringMap.find f funcs in
      let p = List.fold_left (fun ap (s,_) -> 
          fwdStm funcs env vars p s) p ss in
      fwdBlk funcs env vars p f.funcBody
    | A_recall (f,ss) -> B.top env vars (* TODO *)

  and fwdBlk funcs env vars (p:B.t) (b:block) : B.t =
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
      fwdBlk funcs env vars (fwdStm funcs env vars p s) b



  (* Backward Iterator + Recursion *)

  let rec bwdStm funcs env vars (p,r,flag) s =
    let b_filter = B.bwdfilter_underapprox in
	let b_bwdAssign = if !Iterator.nondet then B.bwdAssign_underapprox else B.bwdAssign in
    match s with
    | A_label (s,sa) -> (if s="input" then (
		  let start = Sys.time () in 
		  if B.isBot p then ( if !Iterator.nondet then ( 
		  							if !Iterator.approx=1 then (Iterator.probab_sat_lower := 0.0; Iterator.probab_viol_upper := 1.0 )
		  							else (Iterator.probab_viol_lower := 0.0; Iterator.probab_sat_upper :=1.0) ) 
							  else ( 
		  							if !Iterator.approx=1 then (Iterator.probab_sat_upper := 0.0; Iterator.probab_viol_lower := 1.0 )
		  							else (Iterator.probab_viol_upper := 0.0; Iterator.probab_sat_lower :=1.0) ) ) 							  
		  else ( let file = "latte" in 
		  (*if !Iterator.underapprox then ( *)
		  write_to_Lattefile file p; 
		  let l2 = read_process_lines ("count "^(file)) in 
		  let points = print_Latteresult l2 in (*print_string "Points are "; print_int points; print_int !Iterator.approx; *)
		  let totalPoints = ref 1 in 
		  StringMap.iter (fun key value -> totalPoints := !totalPoints * value; (*Format.fprintf !fmt " init: %s %d \n" key value*)) !ItoA.spaceVars;
		  Iterator.probability_time := Sys.time() -. start; 
		  if !Iterator.nondet then (
		  		(* Format.fprintf !fmt " nondet: %d %d %d \n" !Iterator.approx points !totalPoints; *)
		  		if !Iterator.approx=1 then Iterator.probab_sat_lower := ((float points) /. (float !totalPoints)) else Iterator.probab_viol_lower := ((float points) /. (float !totalPoints));
		  		if !Iterator.approx=1 then Iterator.probab_viol_upper := ((float (!totalPoints - points)) /. (float !totalPoints)) else Iterator.probab_sat_upper := ((float (!totalPoints - points)) /. (float !totalPoints)) ) 
		  else (
		  		if !Iterator.approx=1 then Iterator.probab_sat_upper := ((float points) /. (float !totalPoints)) else Iterator.probab_viol_upper := ((float points) /. (float !totalPoints));
		  		if !Iterator.approx=1 then Iterator.probab_viol_lower := ((float (!totalPoints - points)) /. (float !totalPoints)) else Iterator.probab_sat_lower := ((float (!totalPoints - points)) /. (float !totalPoints))		  
		  )
		  (* Format.fprintf !fmt "\nUNDERAPPROX. PROBABLITY OF CORRECTNESS: %.4f percent \n" ((float points) /. (float !totalPoints)) *)
		  ))
		  else Format.fprintf !fmt "Enter Label: input \n");
		  (p,r,flag)
    | A_return -> B.bot env vars, r, flag
    | A_assign ((l,_),(e,_)) ->
      	b_bwdAssign p (l, e), r, flag
    | A_assert (b,_) ->
		let p = if !Iterator.approx=1 then InvMap.find (-1) !fwdInvMap else (if !Iterator.approx=2 then InvMap.find (-2) !fwdInvMap else InvMap.find (-1) !fwdInvMap) in
		(* Format.fprintf !fmt "bwd assert: %d : %a\n" !Iterator.approx B.print p; *) 
		(*if B.isBot p then raise Bottom_error; *)
      	p, r, flag
    | A_if ((b,ba),s1,s2) ->
      let (p1, _, flag1) = bwdBlk funcs env vars (p, r, flag) s1 in
	  (*Format.fprintf !fmt "before then p1: %a\n" B.print p1;*)
      let p1 = b_filter p1 b in					
	  (*Format.fprintf !fmt "before if p1: %a\n" B.print p1; *)
      let (p2, _, flag2) = bwdBlk funcs env vars (p, r, flag) s2 in
	  (*Format.fprintf !fmt "before else p2: %a\n" B.print p2;*)
      let p2 = b_filter p2 (fst (negBExp (b, ba))) in      
      (* Format.fprintf !fmt "before if p2: %a\n" B.print p2; *)
      (B.join p1 p2, r, flag1 || flag2)
    | A_while (l, (b, ba), s) ->
	  (* if not !Iterator.overapprox then ( *)
	  let rec iter_down post pre nb = 
	  		  (*Format.fprintf !fmt "post in while %d: %a\n" nb B.print post;
			  Format.fprintf !fmt "exit in while %d: %a\n" nb B.print p;
			  Format.fprintf !fmt "pre in while %d: %a\n" nb B.print pre;*)
			  let (p2, _, flag2) = bwdBlk funcs env vars (post,r,flag) s in
			  let p2' = if !Iterator.approx>=0 then B.bwdfilter_underapproxwhile p2 p b pre 
			  			else B.bwdfilter p2 b (*B.bwdfilter_underapproxwhile p2 p b pre*) 		  
			  in 	 	
			  (*if !Iterator.underapprox then Format.fprintf !fmt "result in while %B %d: %a\n" !Iterator.underapprox nb B.print p2'
			  else Format.fprintf !fmt "result in while %B %d: %a\n" !Iterator.underapprox nb B.print p2'; *)
			  
			  (*if !Iterator.underapprox then*) (
			  	if (B.isLeq post p2') then
           			( unroll p2' !joinbwd ) (* Format.fprintf !fmt "post in while %d: %a\n" nb B.print post; let pinv = InvMapTuple.find (l,!joinbwd) !fwdInvMapTuple in Format.fprintf !fmt "p-inv while: %a\n" B.print pinv; (post, r, flag2) *)
              	else let c = if nb <= !joinbwd then B.meet post p2'
                		   else B.lowerwiden post p2' in 
					   (*Format.fprintf !fmt "inv in while %B %d: %a\n" !Iterator.underapprox nb B.print c;*)
		               iter_down c pre (nb + 1) )
			  (* else (
			  	if (B.isLeq p2' post) then
           			( unroll p2' !joinbwd ) (* Format.fprintf !fmt "post in while %d: %a\n" nb B.print post; let pinv = InvMapTuple.find (l,!joinbwd) !fwdInvMapTuple in Format.fprintf !fmt "p-inv while: %a\n" B.print pinv; (post, r, flag2) *)
              	else let c = if nb <= !joinbwd then B.meet post p2'
                		   else B.widen post p2' in 
					   Format.fprintf !fmt "inv in while %B %d: %a\n" !Iterator.underapprox nb B.print c;
		               iter_down c pre (nb + 1)			  
			  ) *)
	  and unroll post nb = 
	  		if nb=0 then (post,r,flag) else 
	  		let pre = InvMapTuple.find (l,!joinbwd) !fwdInvMapTuple in
			(* Format.fprintf !fmt "p-inv while %d: %a\n" nb B.print pre; *)
			let (p2, _, flag2) = bwdBlk funcs env vars (post,r,flag) s in
			let p2' = if !Iterator.approx>=0 then B.bwdfilter_underapproxwhile p2 p b pre else p2 in 
			if !Iterator.approx=1 then addBwdOverInvTuple1 (l,nb) p2' else addBwdOverInvTuple2 (l,nb) p2';
			unroll p2' (nb-1)
	  in
	  let a = InvMap.find l !fwdInvMap in 
      let (p, r, flag) = iter_down a a 1 in
	  (* Format.fprintf !fmt "Result inv while: %a\n" B.print p; *)
	  (*let p = B.meet a p in*)
      if !Iterator.approx=1 then addBwdOverInv1 l p else addBwdOverInv2 l p;
        (p, r, flag)
	(*  )
	else (
	  let widen = ref false in 	
	  let rec iter_down post nb = 
	  		  Format.fprintf !fmt "enter iter_down %d: post = %a\n" nb B.print post;
			  let (p2, _, flag2) = bwdBlk funcs env vars (post,r,flag) s in
			  (*let p2' = B.filter p2 b in 	*) 
			  Format.fprintf !fmt "before isLeq iter_down widen=%B: p2 = %a : %B\n" !widen B.print p2 (B.isLeq post p2);
			  if (B.isLeq post p2) then
           			(post,r,flag2)
				else if !widen then (p2,r,flag2)
              	else let c = if nb <= !joinbwd then B.meet post p2
                		   else (widen:=true; B.lowerwiden post p2) in 
				Format.fprintf !fmt "inv in while %d: %a\n" nb B.print c;
		        iter_down c (nb + 1)
	  in
	  let a = InvMap.find l !fwdInvMap in 
	  let (a2, _, flag2) = bwdBlk funcs env vars (a,r,flag) s in
	  (*Format.fprintf !fmt "Invariant BEFORE :  %a\n%a" B.print a2 B.print a;*)
	  a = B.join a a2; (* this join does not work correctly, it is put here because we do narrowing when Forward Analysis *)
	  (*Format.fprintf !fmt "Invariant AFTER:  %a\n" B.print a;
	  Format.fprintf !fmt "p join b :  %a\n" B.print (B.bwdfilter p b); *)
      let (p, r, flag) = (B.meet (B.bwdfilter p b) a2, r, flag) (*iter_down (B.join a p) 1*) in
	  addBwdOverInv2 l p;
        (p, r, flag)	
(*	  let a = InvMap.find l !fwdInvMap in 
	  let p1 = b_filter p (fst (negBExp (b, ba))) in 
      let rec aux (i, _, _) (p2, _, flag2) n =
          let i' = B.join p1 p2 in
		  Format.fprintf !fmt "i' in while '': %d %a\n" !joinbwd B.print i';
          if (B.isLeq i' i) then
              (i, r, flag2)
            else
              let i'' = if n <= !joinbwd then i'
                		else B.widen i i'
              in
              Format.fprintf !fmt "i in while '': %d %a\n" n B.print i'';
			  let (p2, _, flag2) = bwdBlk funcs env vars (i'',r,flag2) s in
			  let p2' = b_filter p2 b in 
              aux (i'', r, flag2) (p2', r, flag2) (n + 1)
      in
      let i = (B.bot env vars, r, flag) in
      let (p2, _, flag2) = bwdBlk funcs env vars i s in
      let p2' = b_filter p2 b in
      let (p, r, flag) = aux i (p2', r, flag2) 1 in
	  let p = B.meet a p in
      addBwdOver2Inv l p;
        (p, r, flag)  *) )	*)
    | A_call (f, ss) ->  (* do ovde stignav *)
      let f = StringMap.find f funcs in
      let p = bwdRec funcs env vars p f.funcBody in
      List.fold_left (fun (ap, ar, aflag) (s, _) ->
          bwdStm funcs env vars (ap, ar, aflag) s
        ) (p, r, flag) ss
    | A_recall (f, ss) ->
          List.fold_left (fun (ap, ar, aflag) (s, _) ->
             bwdStm funcs env vars (ap, ar, aflag) s
           ) (B.join p r, r, true) ss

  and bwdBlk funcs env vars (p,r,flag) (b:block) : B.t * B.t * bool =
    let result_print l p =
      Format.fprintf !fmt "### %a ###:\n%a@." label_print l B.print p
    in
    match b with
    | A_empty l ->
      if !tracebwd && not !minimal then result_print l p;
      if !Iterator.approx=1 then addBwdOverInv1 l p else addBwdOverInv2 l p; (p,r,flag) (*else addBwdOverInv l p; (p,r,flag)*)
    | A_block (l,(s,_),b) ->
      stop := Sys.time ();
      if ((!stop -. !start) > !timeout)
      then raise Timeout
      else
        let (b,rb,flagb) = bwdBlk funcs env vars (p,r,flag) b in
        let (p,r,flag) = bwdStm funcs env vars (b,rb,flagb) s in
        if !tracebwd && not !minimal then result_print l p;
		let finv = findFwdInv l !fwdInvMap in
		(*if (l=10) then Format.fprintf !fmt "in ### %a ###:\n%a\n%a@." label_print l B.print p B.print finv;*)
		let p = B.meet p finv in
		(*if (l=10) then Format.fprintf !fmt "res ### %a ###:\n%a@." label_print l B.print p;*)
        if !Iterator.approx=1 then addBwdOverInv1 l p else addBwdOverInv2 l p; (p,rb,flagb)  (* else addBwdOverInv l p; (p,r,flag) *)

  and bwdRec funcs env vars (p:B.t) (b:block) : B.t = 
    let (res, _, _) = bwdBlk funcs env vars (p,B.bot env vars,false) b  in
    res


  (* Analyzer *)
  
  
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
    let state = fwdBlk funcs env vars (fwdBlk funcs env vars (B.top env vars) stmts) s in
    let stopfwd = Sys.time () in
    if not !minimal then
      begin
        if !timefwd then
          Format.fprintf !fmt "\nForward Analysis (Time: %f s):\n" (stopfwd-.startfwd)
        else
          Format.fprintf !fmt "\nForward Analysis:\n";
        fwdMap_print !fmt !fwdInvMap !fwdInvMapTuple;
		(*Format.fprintf !fmt "\n pfinal_sat = %a; pfinal_viol = %a\n" B.print (InvMap.find (-1) !fwdInvMap) B.print (InvMap.find (-2) !fwdInvMap);*)
      end;
	  
    (* Backward UnderApproximating Analysis *)
(*    if !tracebwd && not !minimal then
      Format.fprintf !fmt "\nBackward Analysis Trace:\n";
    start := Sys.time ();
    let startbwd = Sys.time () in
	Iterator.underapprox := true;
    let i = bwdRec funcs env vars (bwdRec funcs env vars (B.top env vars) s) stmts in
    let stopbwd = Sys.time () in
    if not !minimal then begin
      if !timebwd then
        Format.fprintf !fmt "\nBackward UnderApproximating Analysis (Time: %f s):\n" (stopbwd-.startbwd)
      else
        Format.fprintf !fmt "\nBackward UnderApproximating Analysis:\n";
      bwdMap_print !fmt !bwdInvMap !bwdInvMapTuple;
    end;
*)
    (* Backward OverApproximating Analysis 1 *)
    if !tracebwd && not !minimal then
      Format.fprintf !fmt "\nBackward Analysis Trace:\n";
    start := Sys.time ();
    let startbwd = Sys.time () in
	Iterator.approx := 1;
    let i = bwdRec funcs env vars (bwdRec funcs env vars (B.top env vars) s) stmts in
    let stopbwd = Sys.time () in
    if not !minimal then begin
      if !timebwd then
        Format.fprintf !fmt "\nBackward OverApproximating Analysis Sat (Time: %f s):\n" (stopbwd-.startbwd)
      else
        Format.fprintf !fmt "\nBackward OverApproximating Analysis Sat :\n";
      bwdMap_print !fmt !bwdOverInvMap1 !bwdOverInvMapTuple1;
    end;
	
    (* Backward OverApproximating Analysis 2 *)
    if !tracebwd && not !minimal then
      Format.fprintf !fmt "\nBackward Analysis Trace:\n";
    start := Sys.time ();
    let startbwd = Sys.time () in
	Iterator.approx := 2;
	(*(try *)
    let i = bwdRec funcs env vars (bwdRec funcs env vars (B.top env vars) s) stmts in
    let stopbwd = Sys.time () in
    if not !minimal then begin
      if !timebwd then
        Format.fprintf !fmt "\nBackward OverApproximating Analysis Viol (Time: %f s):\n" (stopbwd-.startbwd)
      else
        Format.fprintf !fmt "\nBackward OverApproximating Analysis Viol :\n";
      bwdMap_print !fmt !bwdOverInvMap2 !bwdOverInvMapTuple2;
    end;
	(*with Bottom_error -> Iterator.probab_viol := 0.0;)*)
    true
	

end
