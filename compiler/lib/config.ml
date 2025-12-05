(* Project configuration for Shotgun compiler *)

type project_config = {
  name: string;
  root: string;  (* Directory containing shotgun.toml *)
}

(* Simple TOML parser for shotgun.toml - only handles name = "value" *)
let parse_toml content =
  let lines = String.split_on_char '\n' content in
  let result = Hashtbl.create 8 in
  List.iter (fun line ->
    let line = String.trim line in
    (* Skip empty lines and comments *)
    if String.length line > 0 && line.[0] <> '#' then begin
      match String.split_on_char '=' line with
      | [key; value] ->
        let key = String.trim key in
        let value = String.trim value in
        (* Strip quotes from value *)
        let value =
          if String.length value >= 2 && value.[0] = '"' then
            String.sub value 1 (String.length value - 2)
          else
            value
        in
        Hashtbl.replace result key value
      | _ -> ()
    end
  ) lines;
  result

(* Find shotgun.toml by searching up from the given path *)
let rec find_config_file dir =
  let config_path = Filename.concat dir "shotgun.toml" in
  if Sys.file_exists config_path then
    Some config_path
  else
    let parent = Filename.dirname dir in
    if parent = dir then
      None  (* Reached root *)
    else
      find_config_file parent

(* Load project configuration *)
let load_config start_path =
  let start_dir =
    if Sys.is_directory start_path then start_path
    else Filename.dirname start_path
  in
  (* Convert to absolute path *)
  let abs_start_dir =
    if Filename.is_relative start_dir then
      Filename.concat (Sys.getcwd ()) start_dir
    else
      start_dir
  in
  match find_config_file abs_start_dir with
  | None -> None
  | Some config_path ->
    let ic = open_in config_path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let config = parse_toml content in
    match Hashtbl.find_opt config "name" with
    | None -> None
    | Some name ->
      (* Make root absolute *)
      let root = Filename.dirname config_path in
      let abs_root =
        if Filename.is_relative root then
          Filename.concat (Sys.getcwd ()) root
        else
          root
      in
      Some {
        name;
        root = abs_root;
      }

(* Resolve an import path to a file path *)
let resolve_import config import_path =
  match import_path with
  | [] -> None
  | first :: rest ->
    (* Check if first segment matches project name *)
    if first = config.name then
      (* Project-relative import: myapp.utils -> ./utils.bs *)
      let rel_path = String.concat "/" rest ^ ".bs" in
      let full_path = Filename.concat config.root rel_path in
      if Sys.file_exists full_path then
        Some full_path
      else begin
        (* Try mod.bs convention: myapp.utils -> ./utils/mod.bs *)
        let dir_path = String.concat "/" rest in
        let mod_path = Filename.concat (Filename.concat config.root dir_path) "mod.bs" in
        if Sys.file_exists mod_path then
          Some mod_path
        else
          None
      end
    else if first = "std" then
      (* Standard library - TODO: implement *)
      None
    else
      (* Sibling import: utils -> ./utils.bs *)
      let rel_path = String.concat "/" import_path ^ ".bs" in
      let full_path = Filename.concat config.root rel_path in
      if Sys.file_exists full_path then
        Some full_path
      else
        None

(* Get the namespace (last segment) from an import path *)
let namespace_of_import = function
  | [] -> None
  | path -> Some (List.hd (List.rev path))
