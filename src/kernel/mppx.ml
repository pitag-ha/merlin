open Mconfig

let {Logger. log} = Logger.for_section "Mppx"

let with_include_dir path f =
  let saved = !Clflags.include_dirs in
  let restore () = Clflags.include_dirs := saved in
  Clflags.include_dirs := path;
  let result =
    begin
      try
        f ()
      with
      | e ->
         restore ();
         raise e
    end
  in
  restore ();
  result

let rewrite parsetree cfg =
  let ppx = cfg.ocaml.ppx in
  (* add include path attribute to the parsetree *)
  with_include_dir (Mconfig.build_path cfg) @@ fun () ->
  match
    Pparse.apply_rewriters ~restore:false ~ppx ~tool_name:"merlin" parsetree
  with
  | parsetree -> parsetree
  | exception exn ->
    log ~title:"rewrite" "failed with %a" Logger.fmt (fun fmt ->
      match Location.error_of_exn exn with
      | None | Some `Already_displayed ->
        Format.fprintf fmt "%s" (Printexc.to_string exn)
      | Some (`Ok err) ->
        Location.print_main fmt err
    );
    Msupport.raise_error exn;
    parsetree

let parse_and_rewrite cfg =
  let ppx = match cfg.ocaml.ppx with
    | hd :: _ -> hd
    | _ -> failwith "when checking out the PPX phase, there should be a PPX phase..."
  in
  (* add include path attribute to the parsetree *)
  with_include_dir (Mconfig.build_path cfg) @@ fun () ->
  match
    (* this should use stdin (I'm totally ignoring the pp phase and am assuming that the code compiles) *)
    Pparse.parse_and_apply_one_rewriter ~ppx
  with
  | parsetree -> parsetree
  | exception exn ->
    log ~title:"rewrite" "failed with %a" Logger.fmt (fun fmt ->
      match Location.error_of_exn exn with
      | None | Some `Already_displayed ->
        Format.fprintf fmt "%s" (Printexc.to_string exn)
      | Some (`Ok err) ->
        Location.print_main fmt err
    );
    Msupport.raise_error exn;
    failwith "oups"
