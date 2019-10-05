(***************************************************)
(*                                                 *)
(*                        Main                     *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2018 - 2019                   *)
(*                                                 *)
(***************************************************)

(* parsing *)

let analysis = ref "single"
let domain = ref "boxes"
let filename = ref ""
let fmt = ref Format.std_formatter
let main = ref "main"
let minimal = ref false
let precondition = ref "true"
let time = ref true
let noinline = ref false


let parseFile filename =
  let f = open_in filename in
  let lex = Lexing.from_channel f in
  try
    lex.Lexing.lex_curr_p <- { lex.Lexing.lex_curr_p
                               with Lexing.pos_fname = filename; };
    let r = Parser.file Lexer.start lex in
    close_in f; r
  with
  | Parser.Error ->
    Printf.eprintf "Parse Error (Invalid Syntax) near %s\n"
      (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
    failwith "Parse Error"
  | Failure e ->
    if e == "lexing: empty token" then 
      begin
        Printf.eprintf "Parse Error (Invalid Token) near %s\n" (IntermediateSyntax.position_tostring lex.Lexing.lex_start_p);
        failwith "Parse Error"
      end 
    else
      failwith e

let parse_args () =
  let rec doit args =
    match args with
    (* General arguments -------------------------------------------*)
    | "-domain"::x::r -> (* abstract domain: boxes|octagons|polyhedra *)
      domain := x; doit r
    | "-timeout"::x::r -> (* analysis timeout *)
      Iterator.timeout := float_of_string x; doit r
    | "-joinfwd"::x::r -> (* widening delay in forward analysis *)
      Iterator.joinfwd := int_of_string x; doit r
    | "-joinbwd"::x::r -> (* widening delay in backward analysis *)
      Iterator.joinbwd := int_of_string x; doit r
    | "-main"::x::r -> (* analyzer entry point *) main := x; doit r
    | "-meetbwd"::x::r -> (* dual widening delay in backward analysis *)
      Iterator.meetbwd := int_of_string x; doit r
    | "-minimal"::r -> (* analysis result only *)
      minimal := true; Iterator.minimal := true; doit r
    | "-nondet"::r -> (* refine in backward analysis *)
      Iterator.nondet := true; doit r	  
    | "-refine"::r -> (* refine in backward analysis *)
      Iterator.refine := true; doit r
    | "-retrybwd"::x::r ->
      Iterator.retrybwd := int_of_string x;
      (*DecisionTree.retrybwd := int_of_string x;*)
      doit r
    | "-tracefwd"::r -> (* forward analysis trace *)
      Iterator.tracefwd := true; doit r
    (* Single analysis -------------------------------*)
    | "-single"::r -> (* single analysis *)
      analysis := "single"; doit r
    (* Tuple analysis -------------------------------*)
    | "-tuple"::r -> (* tuple analysis *)
      analysis := "tuple"; doit r
    | "-bdd"::r -> (* bdt analysis *)
      analysis := "bdd"; doit r
    | "-time"::r -> (* track analysis time *)
      time := true; doit r
    | "-timebwd"::r -> (* track backward analysis time *)
      Iterator.timebwd := true; doit r
    | "-timefwd"::r -> (* track forward analysis time *)
      Iterator.timefwd := true; doit r  
    | "-precondition"::c::r -> (* optional precondition that holds 
                                  at the start of the program, default = true *)
      precondition := c; doit r 
    | "-noinline"::r -> (* don't inline function calls, only for CFG based analysis *)
      noinline := true; doit r
    | x::r -> filename := x; doit r
    | [] -> ()
  in
  doit (List.tl (Array.to_list Sys.argv))

(* do all *)

module SingleBoxes =
  SingleAnalysisIterator.SingleAnalysisIterator(Numerical.B)
module SingleOctagons =
  SingleAnalysisIterator.SingleAnalysisIterator(Numerical.O)
module SinglePolyhedra =
  SingleAnalysisIterator.SingleAnalysisIterator(Numerical.P)  

module TupleBoxes =
  TupleAnalysisIterator.TupleAnalysisIterator(Maketuple.TB)
module TupleOctagons =
  TupleAnalysisIterator.TupleAnalysisIterator(Maketuple.TO)
module TuplePolyhedra =
  TupleAnalysisIterator.TupleAnalysisIterator(Maketuple.TP)  

module BDDBoxes =
  BDDAnalysisIterator.BDDAnalysisIterator(MakeBDDDomain.BB)
module BDDOctagons =
  BDDAnalysisIterator.BDDAnalysisIterator(MakeBDDDomain.BO)
module BDDPolyhedra =
  BDDAnalysisIterator.BDDAnalysisIterator(MakeBDDDomain.BP) 


let result = ref false

let run_analysis analysis_function program () =
  try 
    let start = Sys.time () in
    let terminating = analysis_function program !main in
    let stop = Sys.time () in
	(*Format.fprintf !fmt "\nsat upper: %f, sat lower: %f, viol upper: %f, viol lower: %f" !Iterator.probab_sat_upper !Iterator.probab_sat_lower !Iterator.probab_viol_upper !Iterator.probab_viol_lower;*)
    Format.fprintf !fmt "Analysis Result: ";
	if (((!Iterator.probab_sat_upper) -. (!Iterator.probab_sat_lower) < 0.01)) && (((!Iterator.probab_sat_upper) -. (!Iterator.probab_sat_lower) > -0.01)) then Format.fprintf !fmt "\nPROBABLITY OF SATISFACTION: %.4f percent " !Iterator.probab_sat_upper 
	else Format.fprintf !fmt "\nAPPROX. PROBABLITY OF SATISFACTION BETWEEN: %.4f percent AND %.4f percent " !Iterator.probab_sat_lower !Iterator.probab_sat_upper;
	if (((!Iterator.probab_viol_upper) -. (!Iterator.probab_viol_lower) < 0.01) && (((!Iterator.probab_viol_upper) -. (!Iterator.probab_viol_lower) > -0.01))) then Format.fprintf !fmt "\nPROBABLITY OF VIOLATION: %.4f percent " !Iterator.probab_viol_upper 
	else Format.fprintf !fmt "\nAPPROX. PROBABLITY OF VIOLATION BETWEEN: %.4f percent AND %.4f percent " !Iterator.probab_viol_lower !Iterator.probab_viol_upper;
    if !time then
      Format.fprintf !fmt "\nTotal Time: %f s" (stop-.start);
	  Format.fprintf !fmt "\nAnalysis Time: %f s" (stop -. start -. (!Iterator.probability_time));
	  Format.fprintf !fmt "\nProbability Time: %f s" !Iterator.probability_time;  
    Format.fprintf !fmt "\nDone.\n"
  with
  | Iterator.Timeout ->
    Format.fprintf !fmt "\nThe Analysis Timed Out!\n";
    Format.fprintf !fmt "\nDone.\n"


let single () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let sources = parseFile !filename in
  let (program,_) = ItoA.prog_itoa sources in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> SingleBoxes.analyze
    | "octagons" -> SingleOctagons.analyze 
    | "polyhedra" -> SinglePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in Format.fprintf !fmt "%s\n" !domain; run_analysis analysis_function program ()
  (* Format.fprintf !fmt "End ... \n"; AbstractSyntax.StringMap.iter (fun key value -> Format.fprintf !fmt "%s " key ) !ItoA.features; Format.fprintf !fmt "%s\n" !domain CONTINUE FROM HERE ...  *)


let liftedtuple () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let sources = parseFile !filename in
  let (program,_) = ItoA.prog_itoa sources in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> TupleBoxes.analyze 
    | "octagons" -> TupleOctagons.analyze 
    | "polyhedra" -> TuplePolyhedra.analyze
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in Format.fprintf !fmt "%s\n" !domain; run_analysis analysis_function program ()
  (* Format.fprintf !fmt "End ... \n"; AbstractSyntax.StringMap.iter (fun key value -> Format.fprintf !fmt "%s " key ) !ItoA.features; Format.fprintf !fmt "%s\n" !domain CONTINUE FROM HERE ...  *)


let liftedbdd () =
  if !filename = "" then raise (Invalid_argument "No Source File Specified");
  let sources = parseFile !filename in
  let (program,_) = ItoA.prog_itoa sources in
  if not !minimal then
    begin
      Format.fprintf !fmt "\nAbstract Syntax:\n";
      AbstractSyntax.prog_print !fmt program;
    end;
  let analysis_function =
    match !domain with
    | "boxes" -> BDDBoxes.analyze (* if !ordinals then TerminationBoxesOrdinals.analyze else TerminationBoxes.analyze *)
    | "octagons" -> BDDOctagons.analyze (* if !ordinals then TerminationOctagonsOrdinals.analyze else TerminationOctagons.analyze *)
    | "polyhedra" -> BDDPolyhedra.analyze (* if !ordinals then TerminationPolyhedraOrdinals.analyze else TerminationPolyhedra.analyze *)
    | _ -> raise (Invalid_argument "Unknown Abstract Domain")
  in Format.fprintf !fmt "%s\n" !domain; run_analysis analysis_function program ()
  (* Format.fprintf !fmt "End ... \n"; AbstractSyntax.StringMap.iter (fun key value -> Format.fprintf !fmt "%s " key ) !ItoA.features; Format.fprintf !fmt "%s\n" !domain CONTINUE FROM HERE ...  *)





(*Main entry point for application*)
let doit () =
  parse_args ();
  match !analysis with
  | "single" -> single ()
  | "tuple" -> liftedtuple ()
  | "bdd" -> liftedbdd ()
  | _ -> raise (Invalid_argument "Unknown Analysis")

let _ = doit () 
