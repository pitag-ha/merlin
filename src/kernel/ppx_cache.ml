type t = Mreader.parsetree

let {Logger. log} = Logger.for_section ("Ppx_cache")

module Fingerprint = struct
    type t = {binary_id : File_id.t; args : string list; workdir : string}
    let make ~binary ~args ~workdir =
        let qualified_binary =
            let dir = if String.ends_with ~suffix:"/" workdir then workdir else workdir ^ "/" in
            dir ^ "/" ^ binary in
        File_id.get_opt qualified_binary
            |> Option.map (fun binary_id -> {binary_id; args; workdir})
    let equal {binary_id = b1; args = a1; workdir = w1 } {binary_id = b2; args = a2; workdir = w2 } =
        File_id.check b1 b2 && List.equal String.equal a1 a2 && String.equal w1 w2
end

type cache = { fingerprints : Fingerprint.t list; result : Mreader.parsetree}

let cache = ref None

let with_cache ~source_was_cached (config : Mconfig.t) f =
  let title = "ppx" in
  let get_fingerprints = 
    List.map (fun 
    { Std.workdir; workval } ->
        match Std.String.split_on_char ~sep:' ' workval with
        | [] -> raise (Invalid_argument workval)
        | binary :: args ->
        Option.get @@ Fingerprint.make ~binary ~args ~workdir)
  in
  match get_fingerprints config.ocaml.ppx with
  | new_fingerprints ->
    (match !cache with
        | Some { fingerprints = old_fingerprints; result}
            when source_was_cached 
            && List.equal Fingerprint.equal new_fingerprints old_fingerprints
        ->
            log ~title "parsetree was reused\n"
            ;
            result
        | _ ->
        log ~title "parsetree was expanded: ppx prelimiaries have changed";
        let result = f config in
        cache := Some { fingerprints = new_fingerprints; result};
        result)
  | exception Invalid_argument a -> 
    log ~title "parsetree was expanded: cache workflow is incomplete: %s" a;
        let result = f config in
        cache := None;
        result
