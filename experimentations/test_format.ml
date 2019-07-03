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
  Format.printf "@[<b 5>[time level]@ %s@]@."
    (Printf.sprintf fmt a);;

Format.sprintf "@[is it here @ %s ?@]" (show_l l2);;

print_log_fmt "is it here %s ?" (show_l l2);;
print_log_fmt "%s" (Format.sprintf "@[is it here @ %s ?@]"  (show_l l2));;


let print_log_fmt_2 pp fmt s= 
  Format.fprintf pp "@[<b 1>[time level]@ %a@]@."
    (Format.fprintf fmt s);;

print_log_fmt_2 Format.std_formatter "@[is it here @ %a ?@]" pp_l l2;; 

print_log_fmt Format.std_formatter (Printf.sprintf "@[is it here @ %a ?@]" pp_l l2);; 

let print_log_fmt_3 ppf pp_arg  arg =
  Format.fprintf ppf "@[<b 6>[time level]@ %a@]@." pp_arg arg;;

let pp_temp pp_arg ppf = Format.fprintf ppf "@[is it here @ %a ?@]" pp_arg;;

print_log_fmt_3 Format.std_formatter (pp_temp pp_l)  l2;;


let print_log_fmt_4 ppf pp_arg format_arg arg =
  Format.fprintf ppf "@[<b 6>[time level]@ %a@]@."
  (Format.fprintf ppf format_arg pp_arg) arg

print_log_fmt_4 Format.std_formatter  "@[is it here @ %a ?@]" l2;;

  
(* examples from format unraveled *)
let pp_int ppf = Format.fprintf ppf "%d";;
let pp_pair pp_x pp_y ppf (x, y) =
  Format.fprintf ppf "@[(%a,@ %a)@]" pp_x x pp_y y;;

pp_pair pp_int pp_int Format.std_formatter (1,2);;
