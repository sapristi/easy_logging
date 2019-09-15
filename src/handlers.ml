(**
   This is the [Handlers] module. It provides simple yet adaptable handlers implementation.

*)


open Easy_logging_types
open Formatters
(** {1 Type definitions } *)
type log_formatter = log_item -> string
type filter= log_item -> bool

(** type of a handler *)
type t =
  {
    mutable fmt : log_formatter;
    mutable level : Easy_logging_types.level;
    mutable filters: filter list;
    output : string -> unit;
  }

(**
   A handler is made of:
   - a formatter that transforms a log item into a string.
   - a level used to filter out items.
   - an array of possible additional custom filters.
   - an [out_channel], where log strings are outputed by the function [Pervasives.output_string].

*)


(** {1 Handlers creation helpers } *)

let make_cli_handler level =
  {fmt = format_color;
   level = level;
   output = (fun s -> output_string stdout s; flush stdout);
   filters = []}

type file_handlers_config = {
  logs_folder: string;
  truncate: bool;
  file_perms: int;
  date_prefix : string option;
  versioning: int option;
  suffix: string;
}

let file_handlers_defaults = {
  logs_folder = "logs/";
  truncate = true;
  file_perms = 0o660;
  date_prefix = Some "%Y%m%d_";
  versioning = Some 3;
  suffix : string = ".log"
}


type config =
  {mutable file_handlers: file_handlers_config}
let config = {file_handlers = file_handlers_defaults}

let set_config c = config.file_handlers <- c.file_handlers
(** Sets how log files are created when using make_file_handler *)

let generate_prefix () =
  match config.file_handlers.date_prefix with
  | None -> ""
  | Some f ->
    let open CalendarLib in
    let initial_tz = Time_Zone.current () in
    Time_Zone.change Local;
    let now = Calendar.now () in
    let prefix = (Printer.Calendar.sprint f now) in
    Time_Zone.change initial_tz;
    prefix

let generate_filename base =
  let rec find_versioned pattern i =
    let name = Printf.sprintf pattern i in
    if Sys.file_exists name
    then find_versioned pattern (i+1)
    else name
  in
  let prefix = generate_prefix ()
  and suffix = config.file_handlers.suffix
  and folder = config.file_handlers.logs_folder
  in
  match config.file_handlers.versioning
  with
  | None -> Filename.concat folder prefix^base^suffix
  | Some i ->
    let filename_pattern_string =
      Filename.concat folder
        (Printf.sprintf "%s%s_%%0%ii%s" prefix base i suffix) in
    let filename_pattern  = Scanf.format_from_string filename_pattern_string "%i" in
    find_versioned filename_pattern 0

let make_file_handler level filename_base =

  if not (Sys.file_exists config.file_handlers.logs_folder)
  then
    Unix.mkdir config.file_handlers.logs_folder 0o775;

  let filename = generate_filename filename_base in
  let open_flags =
    if config.file_handlers.truncate
    then [Open_wronly; Open_creat;Open_trunc]
    else [Open_wronly; Open_creat]
  in
  let oc =
    open_out_gen open_flags
      config.file_handlers.file_perms filename

  in
  {fmt = format_default;
   level = level;
   output = (fun s -> output_string oc s; flush oc);
   filters = [];
  }


type desc = | Cli of level | File of string * level

let make d = match d with
  | Cli lvl -> make_cli_handler lvl
  | File (f, lvl) -> make_file_handler lvl f
(** Used for quick handler creation, e.g.


    - Cli handler: outputs colored messages to stdout
    {[ let h = Handlers.make (Cli Debug) ]}
    - File handler : outputs messages to a given file
    {[ let h = Handlers.make (File ("filename", Debug)) ]}
*)



(** {1 Handlers setup } *)


(** Sets the level of a handler. *)
let set_level (h:t) lvl =
  h.level <- lvl

(** Sets the formatter of a handler. *)
let set_formatter h fmt =
  h.fmt <- fmt

(** Adds a filter to a handler. *)
let add_filter h filter =
  h.filters <- filter::h.filters


(** Auxiliary function.*)
let apply (h : t) (item: log_item) =

  if item.level >= h.level && (reduce (&&) (List.map (fun f -> f item) h.filters) true)
  then
    (
      h.output (Printf.sprintf "%s\n" (h.fmt item));
    )
