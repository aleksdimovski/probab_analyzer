(***************************************************)
(*       ********* Iterator ************           *)
(*                                                 *)
(*                                                 *)
(*             Aleksandar Dimovski                 *)
(*          Mother Teresa Uni, Skopje              *)
(*                   2018 - 2019                   *)
(*                                                 *)
(***************************************************)

let abort = ref false
let compress = ref true (* false *)
let fmt = ref Format.std_formatter
let joinbwd = ref 3
let joinfwd = ref 3
let meetbwd = ref 2
let minimal = ref false
let refine = ref false
let retrybwd = ref 5
let start = ref 0.0
let stop = ref 0.0
let timebwd = ref false
let timefwd = ref false
let timeout = ref 300.0
let tracefwd = ref false
let tracebwd = ref false
let approx = ref 0 
let probab_sat_upper = ref 0.0
let probab_viol_upper = ref 0.0
let probab_sat_lower = ref 0.0
let probab_viol_lower = ref 0.0
let probability_time = ref 0.0
let nondet = ref false

exception Abort
exception Timeout
