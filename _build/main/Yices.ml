(***************************************************)
(*                                                 *)
(*    Creates and calls external Yices solver      *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2018 - 2019                   *)
(*                                                 *)
(***************************************************)


module StringSet = Set.Make(String)


let process_message mess =
	let lines = ref [] in
	let varSet = ref StringSet.empty in
	begin
	let n1 = ref 0 in 
	let n2 = ref 0 in 	
	try
		while (String.contains_from mess !n1 '{') do 
			n1 := String.index_from mess !n2 '{';
			n2 := String.index_from mess !n1 '}';
			(*print_int !n1; print_string " "; print_int !n2; print_string " ";*)
			let vv = String.sub mess (!n1+1) (!n2-(!n1)-1) in (*print_string vv; *)
			let mm = "(define " ^ vv ^ "::int)" in
			if (not (StringSet.mem vv !varSet)) then ( varSet := StringSet.add vv !varSet; lines := !lines @ [("(define " ^ vv ^ "::int)")] )
		done;
		(*print_list 1 !lines;*)
	with 
	| Not_found ->
	   try
		n1 := 0; n2 := 0; 
		let vv = ref " " in
		while (String.contains_from mess !n1 '&') do 
			n2 := String.index_from mess !n1 '&';
			(*print_int !n1; print_string " "; print_int !n2; print_string " ";*)
			vv := String.sub mess (!n1) (!n2-(!n1)-1); 
			let r0 = Str.regexp "\\(.*\\)-\\$[0-9]{\\([a-zA-Z]+\\)}\\(.*\\)" in
			let r1 = Str.regexp "\\$[0-9]{\\([a-zA-Z]+\\)}" in
			vv := Str.global_replace r0 "\\1(- 0 \\2)\\3" !vv;
			vv := String.trim (Str.global_replace r1 "\\1" !vv);
			let r2 = Str.regexp "\\((- 0 [a-zA-Z0-9]+)\\|[a-zA-Z0-9]+\\)\\( >= \\| > \\| <= \\| < \\| == \\)\\(-?[a-zA-Z0-9]+\\)" in
			let r22 = Str.regexp "\\(==\\)" in
			let r3 = Str.regexp "\\(.*\\)\\( >= \\| > \\| <= \\| < \\)\\(-?[a-zA-Z0-9]+\\)" in
			let r4 = Str.regexp "\\((- *[a-zA-Z0-9]+ [a-zA-Z0-9]+)\\|(\+ *[a-zA-Z0-9]+ [a-zA-Z0-9]+)\\|[a-zA-Z0-9]+\\) *\\(+\\|-\\|\*\\) *\\(-?[a-zA-Z0-9]+\\)" in
      		if (Str.string_match r2 !vv 0) then (vv := String.trim (Str.global_replace r2 "\\2 \\1 \\3" !vv); vv := Str.global_replace r22 "=" !vv) 
					else (  (*print_string " vv = "; print_string !vv;*)
							while (Str.string_match r4 !vv 0) do 
								print_string " ";
								vv := String.trim (Str.replace_first r4 "(\\2 \\1 \\3)" !vv)
							done; 
							vv := String.trim (Str.replace_first r3 "\\2 \\1 \\3" !vv)
						 );
			lines := !lines @ [("(assert (" ^ !vv ^ "))")];		
			(*let mm = "(define " ^ vv ^ "::int)" in print_string mm;
			if (not (StringSet.mem vv !varSet)) then ( varSet := StringSet.add vv !varSet; lines := ("(define " ^ vv ^ "::int)") :: !lines );*)
			n1 := !n2+2;
		done;	
		vv := String.sub mess (!n1) ((String.length mess) - (!n1));
		let r3 = Str.regexp "\\(.*\\)-\\$[0-9]{\\([a-zA-Z]+\\)}\\(.*\\)" in
		let r4 = Str.regexp "\\$[0-9]{\\([a-zA-Z]+\\)}" in
		vv := String.trim (Str.global_replace r3 "\\1(- 0 \\2)\\3" !vv);
      	vv := String.trim (Str.global_replace r4 "\\1" !vv);
		(*print_string " vv5 = "; print_string vv5;*)
		let r5 = Str.regexp "\\((- 0 [a-zA-Z0-9]+)\\|[a-zA-Z0-9]+\\)\\( >= \\| > \\| <= \\| < \\| == \\)\\(-?[a-zA-Z0-9]+\\)" in
		let r52 = Str.regexp "\\(==\\)" in
		let r6 = Str.regexp "\\(.*\\)\\( >= \\| > \\| <= \\| < \\)\\(-?[a-zA-Z0-9]+\\)" in
		let r7 = Str.regexp "\\((- *[a-zA-Z0-9]+ [a-zA-Z0-9]+)\\|(\+ *[a-zA-Z0-9]+ [a-zA-Z0-9]+)\\|[a-zA-Z0-9]+\\) *\\(+\\|-\\|\*\\) *\\(-?[a-zA-Z0-9]+\\)" in
      	if (Str.string_match r5 !vv 0) then (vv := String.trim (Str.global_replace r5 "\\2 \\1 \\3" !vv); vv := Str.global_replace r52 "=" !vv) 
					else (  while (Str.string_match r7 !vv 0) do 
								print_string " ";
								vv := String.trim (Str.replace_first r7 "(\\2 \\1 \\3)" !vv)
							done; 
							vv := String.trim (Str.replace_first r6 "\\2 \\1 \\3" !vv)
						 );
		lines := !lines @ [("(assert (" ^ !vv ^ "))")];		
		lines := !lines @ [("(check)")] @ [("(show-model)")];
	   with
	   | Not_found -> lines := !lines @ [("(check)")]; lines := !lines @ [("(show-model)")];
	end;
	String.concat "\n" !lines;;

let process_partition t =
	let lines = ref [] in
	
	

let write_to_file file b =
	let oc = open_out file in    						    (* create or truncate file, return channel *)
  	Printf.fprintf oc "%s\n" (process_partition b); (*(process_message message); *)   (* write something *)   
  	close_out oc;;