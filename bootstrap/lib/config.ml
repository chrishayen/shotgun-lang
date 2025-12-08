(* Project configuration for Shotgun compiler *)

type project_config = {
  name: string;
  root: string;  (* Directory containing shotgun.toml *)
}

(* Simple TOML parser for shotgun.toml - only handles key = "value" *)
let parse_toml content =
  let lines = String.split_on_char '\n' content in
  let result = Hashtbl.create 8 in
  List.iter (fun line ->
    let line = String.trim line in
    (* Skip empty lines and comments *)
    if String.length line > 0 && line.[0] <> '#' then begin
      match String.index_opt line '=' with
      | None -> failwith ("Invalid config line (expected key = \"value\"): " ^ line)
      | Some idx ->
        let key = String.trim (String.sub line 0 idx) in
        let value = String.trim (String.sub line (idx + 1) (String.length line - idx - 1)) in
        if key = "" || value = "" then
          failwith ("Invalid config line (missing key or value): " ^ line);
        (* Strip quotes from value; require balanced quotes when present *)
        let value =
          if String.length value >= 2 && value.[0] = '"' && value.[String.length value - 1] = '"' then
            String.sub value 1 (String.length value - 2)
          else if String.contains value '"' then
            failwith ("Invalid quoted value in config: " ^ line)
          else
            value
        in
        Hashtbl.replace result key value
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
    (try
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
     with Failure _ ->
       None)

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
    else if first = "bootstrap" then
      (* Bootstrap stdlib for self-hosting experiments *)
      (* Search up from config.root for bootstrap directory *)
      let rec find_bootstrap dir =
        let candidate = Filename.concat dir "bootstrap" in
        if Sys.file_exists candidate && Sys.is_directory candidate then
          Some candidate
        else
          let parent = Filename.dirname dir in
          if parent = dir then None
          else find_bootstrap parent
      in
      (match find_bootstrap config.root with
      | None -> None
      | Some stdlib_root ->
        let rel_path = String.concat "/" rest ^ ".bs" in
        let full_path = Filename.concat stdlib_root rel_path in
        if Sys.file_exists full_path then
          Some full_path
        else begin
          (* Try mod.bs convention *)
          let dir_path = String.concat "/" rest in
          let mod_path = Filename.concat (Filename.concat stdlib_root dir_path) "mod.bs" in
          if Sys.file_exists mod_path then
            Some mod_path
          else
            None
        end)
    else
      (* Sibling import: utils -> ./utils.bs *)
      let rel_path = String.concat "/" import_path ^ ".bs" in
      let full_path = Filename.concat config.root rel_path in
      if Sys.file_exists full_path then
        Some full_path
      else
        None

(* Resolve an import path relative to a base directory (no project config needed) *)
let resolve_import_relative base_dir import_path =
  match import_path with
  | [] -> None
  | "bootstrap" :: rest ->
    (* Bootstrap stdlib - search up from base_dir for bootstrap directory *)
    let rec find_bootstrap dir =
      let candidate = Filename.concat dir "bootstrap" in
      if Sys.file_exists candidate && Sys.is_directory candidate then
        Some candidate
      else
        let parent = Filename.dirname dir in
        if parent = dir then None
        else find_bootstrap parent
    in
    (match find_bootstrap base_dir with
    | None -> None
    | Some bootstrap_root ->
      let rel_path = String.concat "/" rest ^ ".bs" in
      let full_path = Filename.concat bootstrap_root rel_path in
      if Sys.file_exists full_path then
        Some full_path
      else begin
        (* Try mod.bs convention *)
        let dir_path = String.concat "/" rest in
        let mod_path = Filename.concat (Filename.concat bootstrap_root dir_path) "mod.bs" in
        if Sys.file_exists mod_path then
          Some mod_path
        else
          None
      end)
  | _ ->
    (* Convert import path to file path: foo.bar.baz -> foo/bar/baz.bs *)
    let rel_path = String.concat "/" import_path ^ ".bs" in
    let full_path = Filename.concat base_dir rel_path in
    if Sys.file_exists full_path then
      Some full_path
    else begin
      (* Try mod.bs convention: foo.bar -> foo/bar/mod.bs *)
      let dir_path = String.concat "/" import_path in
      let mod_path = Filename.concat (Filename.concat base_dir dir_path) "mod.bs" in
      if Sys.file_exists mod_path then
        Some mod_path
      else
        None
    end

(* Get the namespace (last segment) from an import path *)
let namespace_of_import = function
  | [] -> None
  | path -> Some (List.hd (List.rev path))

(* Find all .bs files in a directory *)
let find_bs_files_in_dir dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then
    []
  else
    let entries = Sys.readdir dir in
    Array.to_list entries
    |> List.filter (fun f -> String.ends_with ~suffix:".bs" f)
    |> List.map (fun f -> Filename.concat dir f)
    |> List.sort String.compare

(* Resolve an import path to a package directory *)
let resolve_import_to_package config import_path =
  match import_path with
  | [] -> None
  | first :: rest ->
    (* Check if first segment matches project name *)
    if first = config.name then
      (* Project-relative import: myapp.parser -> ./parser/ *)
      let dir_path = String.concat "/" rest in
      let full_path = Filename.concat config.root dir_path in
      if Sys.file_exists full_path && Sys.is_directory full_path then
        let files = find_bs_files_in_dir full_path in
        if files <> [] then Some (full_path, files) else None
      else
        None
    else if first = "std" then
      (* Standard library - TODO: implement *)
      None
    else if first = "bootstrap" then
      (* Bootstrap stdlib for self-hosting experiments *)
      let rec find_bootstrap dir =
        let candidate = Filename.concat dir "bootstrap" in
        if Sys.file_exists candidate && Sys.is_directory candidate then
          Some candidate
        else
          let parent = Filename.dirname dir in
          if parent = dir then None
          else find_bootstrap parent
      in
      (match find_bootstrap config.root with
      | None -> None
      | Some stdlib_root ->
        let dir_path = String.concat "/" rest in
        let full_path = Filename.concat stdlib_root dir_path in
        if Sys.file_exists full_path && Sys.is_directory full_path then
          let files = find_bs_files_in_dir full_path in
          if files <> [] then Some (full_path, files) else None
        else
          None)
    else
      (* Sibling import: parser -> ./parser/ *)
      let dir_path = String.concat "/" import_path in
      let full_path = Filename.concat config.root dir_path in
      if Sys.file_exists full_path && Sys.is_directory full_path then
        let files = find_bs_files_in_dir full_path in
        if files <> [] then Some (full_path, files) else None
      else
        None

(* Get the package name from a directory path *)
let package_name_of_dir dir =
  Filename.basename dir
