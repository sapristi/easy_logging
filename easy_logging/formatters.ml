(*
    This file is part of easy_logging.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
*)


open Logging_types

(** {1 Formatting functions} *)

let reduce (f: 'a -> 'a -> 'a) (l: 'a list) (d: 'a) =
  let rec aux l res =
    match l with
    | [] -> res
    | h::t ->
      let res' = f res h in
      aux t res'
  in
  match l with
  | [] -> d
  | h::t -> aux t h

let rec join (sep: string) (l : string list) =
  match l with
  | [] -> ""
  | h :: [] -> h
  | h :: t -> h ^ sep ^ (join sep t)

let format_tags (tags : string list) =
  match tags with
  | [] -> ""
  | _ ->
    let elems_str = join  " | " tags
    in "[" ^ elems_str ^ "] "

(** Auxiliary functions. *)

let format_default (item : log_item) =
  Printf.sprintf "%-6.3f %-10s %-20s %s%s" (Sys.time ())
    (show_level item.level)
    item.logger_name
    (format_tags item.tags)
    item.msg
(** Human readable log messages. *)

let format_color (item : log_item) =

  let level_to_color lvl =
    match lvl with
    | Flash -> Colorize.LMagenta
    | Error -> Colorize.LRed
    | Warning -> Colorize.LYellow
    | Info -> Colorize.LBlue
    | Trace -> Colorize.Cyan
    | Debug -> Colorize.Green
    | NoLevel -> Colorize.Default
  in

  let item_level_fmt = Colorize.format [ Fg (level_to_color item.level)]  (show_level item.level)
  and logger_name_fmt = Colorize.format [ Underline] item.logger_name
  and item_msg_fmt =
    match item.level with
    | Flash -> Colorize.format [ Fg Black; Bg LMagenta] item.msg
    | _ -> item.msg in

  Format.pp_set_max_indent Format.str_formatter 200;
  Format.sprintf "@[<hov 2>[%-6.3f %-20s %-30s] %s%s@]"
    (Sys.time ())
    item_level_fmt
    logger_name_fmt
    (format_tags item.tags)
    item_msg_fmt
(** Human readable log messages, with level depending colors.*)

let format_json (item: log_item) =
  let format_tags_json tags =
    let elems_str =
      join ", "
      (List.map (fun e ->  "\"" ^ e ^ "\"") tags)
    in "[" ^ elems_str ^ "] "

  in

  Printf.sprintf
    "{\"timestamp\": %f, \"level\": \"%s\", \"logger_name\": \"%s\", \"message\": \"%s\", \"tags\": %s}"
    (item.timestamp)
    (show_level item.level)
    (String.escaped item.logger_name)
    (String.escaped item.msg)
    (format_tags_json item.tags)
(** JSON object. *)
