#require "ppx_deriving.show";;


type l = string list [@@deriving show];;

let l1 = ["aaaaaa";"bbbbbb";"ccc";"ddddddddddd";"eeeeeeeeeeeeeeeeeeeee";"ffff";"gggg"]
let l2 = l1 @ l1 @ l1 @ l1 @ l1 @ l1;;



let log_prefix = "@[<b 1>[time level scannedf]@ %a@]@.";;

let log_formatter_0 = Scanf.format_from_string log_prefix "@[<b 5> @ %a@]@.";;
let log_formatter = Scanf.format_from_string log_prefix "%a";;

(* Format.printf "is this ok ? @ %a"  pp_l l2;; *)

(* Printf.printf  "is this ok ? \n %s"  (show_l l2);; *)

Format.printf log_formatter Format.pp_print_string "ok";; 
Format.printf log_formatter Format.pp_print_string (show_l l2);; 
(* Format.printf log_formatter pp_l l2;; *)

(* Format.printf  "@[<hv 5>[time level]@ %a@]@." pp_l l2;; *)
(* Format.printf log_formatter pp_l l2;;*)

let print_log_str  = Format.printf  "@[<b 1>[time level]@ %a@]@.";;
(* print_log pp_l l2;; *)
print_log_str Format.pp_print_string "ok";;
print_log_str Format.pp_print_string (show_l l2);;

(* let fprint_log ppf = Format.fprintf ppf  "@[<hv 5>[time level]@ %a@]@.";;
fprint_log Format.std_formatter ("is this message ? %a" pp_l l2);;
 *)

Buffer.clear Format.stdbuf;;
Format.fprintf Format.str_formatter "is this test %a ok" pp_l l2;;

Buffer.output_buffer stdout Format.stdbuf;;



let pp_print =
  Format.fprintf Format.std_formatter "[prefix] %a";;

pp_print Format.pp_print_string "mystr";;
(* pp_print pp_l l2;;*)

let print_log_fmt fmt a = 
  Format.printf "@[<b 1>[time level]@ %s@]@."
    (Printf.sprintf fmt a);;

Format.sprintf "@[is it here @ %s ?@]" (show_l l2);;

print_log_fmt "is it here %s ?" (show_l l2);;
print_log_fmt "%s" (Format.sprintf "@[is it here @ %s ?@]"  (show_l l2));;


let print_log_fmt_2 pp fmt s x= 
  Format.fprintf pp "@[<b 1>[time level]@ %a@]@."
    (Format.fprintf fmt s);;



print_log_fmt_2 Format.std_formatter "@[is it here @ %a ?@]" pp_l l2;; 
  
