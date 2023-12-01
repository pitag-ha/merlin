let read_line_opt () =
  try Some (read_line ()) with End_of_file -> None

let delimiter1 = Sys.argv.(1)
let delimiter2 = Sys.argv.(2)
let diff_file = Sys.argv.(3)
let rec loop () =
  let rec stdin_to_file ~until file_oc =
      match read_line_opt () with
      | None -> None
      | Some line ->
          if String.equal line until then Some ()
          else
            (output_string file_oc (line ^ "\n");
            stdin_to_file ~until file_oc)
    in
  let label1 = read_line_opt () in
  let tmp1 = Filename.temp_file "tmp1" "" in
  let oc = open_out tmp1 in
  let reached_delimiter1 =  stdin_to_file ~until:delimiter1 oc in
  close_out oc;
  let label2 = read_line_opt () in
  let tmp2 = Filename.temp_file "tmp2" "" in
  let oc = open_out tmp2 in
  let reached_delimiter2 = stdin_to_file ~until:delimiter2 oc in
  close_out oc;
  match label1, label2, reached_delimiter1, reached_delimiter2 with
    | Some label1, Some label2, Some (), Some () ->
      let diff_cmd = Printf.sprintf "diff -U 5 --label=\"%s\" --label=\"%s\" \"%s\" \"%s\" >> %s" label1 label2 tmp1 tmp2
      diff_file in
      let _ = Sys.command diff_cmd in
      loop ()
    | None, _, _, _ -> ()
    | Some label1, None, _, _ -> raise (Failure ("strange 1 " ^ label1))
    | _, _, None, _ -> raise (Failure ("strange 2"))
    | _, _, _, None -> raise (Failure ("strange 3"))
    (* | _ -> raise (Failure ("Unexpected end of file")) *)

let () = loop ()
