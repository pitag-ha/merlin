open Std

let {Logger. log} = Logger.for_section "Pipeline"

let time_shift = ref (Misc.Time.initial ())

let timed_lazy r x =
  lazy (
    let start = Misc.Time.time_spent () in
    let time_shift0 = !time_shift in
    let update () =
      let delta = Misc.Time.(sub (time_spent ()) start) in
      let shift = Misc.Time.sub !time_shift time_shift0 in
      time_shift := Misc.Time.add time_shift0 delta;
      r := Misc.Time.(sub ( add !r delta ) shift);
    in
    match Lazy.force x with
    | x -> update (); x
    | exception exn -> update (); Std.reraise exn
  )

module Cache = struct
  let cache = ref []

  (* Values from configuration that are used as a key for the cache.
     These values should:
     - allow to maximize reuse; associating a single typechecker instance to a
       filename and directory is natural, but keying also based on verbosity
       makes no sense
     - prevent reuse in different environments (if there is a change in
       loadpath, a new typechecker should be produced).

     It would be better to guarantee that the typechecker was well-behaved
     when the loadpath changes (so that we can reusing the same instance, and
     let the typechecker figure which part of its internal state should be
     invalidated).
     However we already had many bug related to that.  There are subtle changes
     in the type checker behavior across the different versions of OCaml.
     It is simpler to create new instances upfront.
  *)

  let key config =
    Mconfig.(
      config.query.filename,
      config.query.directory,
      config.ocaml,
      {config.merlin with log_file = None; log_sections = []}
    )

  let get config =
    let title = "pop_cache" in
    let key = key config in
    match List.assoc key !cache with
    | state ->
      cache := (key, state) :: List.remove_assoc key !cache;
      log ~title "found entry for this configuration";
      state
    | exception Not_found ->
      log ~title "nothing cached for this configuration";
      let state = Mocaml.new_state () in
      cache := (key, state) :: List.take_n 5 !cache;
      state
end

module Typer = struct
  type t = {
    errors : exn list lazy_t;
    result : Mtyper.result;
  }
end

module Ppx = struct
  type t = {
    config : Mconfig.t;
    errors : exn list;
    parsetree : Mreader.parsetree;
  }
end

type t = {
  config : Mconfig.t;
  state  : Mocaml.typer_state;
  raw_source : Msource.t;
  source : (Msource.t * Mreader.parsetree option) lazy_t;
  reader : (Mreader.result * Mconfig.t * bool) lazy_t;
  ppx    : Ppx.t lazy_t;
  typer  : Typer.t lazy_t;

  pp_time     : Misc.Time.t ref;
  pp_lazy     : bool ref;
  reader_time : Misc.Time.t ref;
  reader_lazy : bool ref;
  ppx_time    : Misc.Time.t ref;
  ppx_lazy    : bool ref;
  typer_time  : Misc.Time.t ref;
  typer_lazy  : bool ref;
  error_time  : Misc.Time.t ref;
  error_lazy  : bool ref;
}

let raw_source t = t.raw_source

let input_config t = t.config
let input_source t = fst (Lazy.force t.source)

let with_pipeline t f =
  Mocaml.with_state t.state @@ fun () ->
  Mreader.with_ambient_reader t.config (input_source t) f

let get_lexing_pos t pos =
  Msource.get_lexing_pos
    (input_source t) ~filename:(Mconfig.filename t.config) pos

let reader t = Lazy.force t.reader

let ppx    t = Lazy.force t.ppx
let typer  t = Lazy.force t.typer

let reader_config    t = let (_, config, _) = (reader t) in config
let reader_parsetree t = let (pt, _, _) = (reader t) in pt.Mreader.parsetree
let reader_comments  t = let (pt, _, _) = reader t in pt.Mreader.comments
let reader_lexer_keywords  t = let (pt, _, _) = reader t in pt.Mreader.lexer_keywords
let reader_lexer_errors  t = let (pt, _, _) = reader t in pt.Mreader.lexer_errors
let reader_parser_errors t = let (pt, _, _) = reader t in pt.Mreader.parser_errors
let reader_no_labels_for_completion t =
  let (pt, _, _) = reader t in pt.Mreader.no_labels_for_completion

let ppx_parsetree t = (ppx t).Ppx.parsetree
let ppx_errors    t = (ppx t).Ppx.errors

let final_config  t = (ppx t).Ppx.config

let typer_result t = (typer t).Typer.result
let typer_errors t = Lazy.force (typer t).Typer.errors

let process
    ?state
    ?(pp_time=ref (Misc.Time.initial ()))
    ?(pp_lazy=ref true)
    ?(reader_time=ref (Misc.Time.initial ()))
    ?(reader_lazy=ref true)
    ?(ppx_time=ref (Misc.Time.initial ()))
    ?(ppx_lazy=ref true)
    ?(typer_time=ref (Misc.Time.initial ()))
    ?(typer_lazy=ref true)
    ?(error_time=ref (Misc.Time.initial ()))
    ?(error_lazy=ref true)
    ?for_completion
    config raw_source =
  let state = match state with
    | None -> Cache.get config
    | Some state -> state
  in
  let source = timed_lazy pp_time (lazy (
      pp_lazy := false;
      match Mconfig.(config.ocaml.pp) with
      | None -> raw_source, None
      | Some { workdir; workval } ->
        let source = Msource.text raw_source in
        match
          Pparse.apply_pp
            ~workdir ~filename:Mconfig.(config.query.filename)
            ~source ~pp:workval
        with
        | `Source source -> Msource.make source, None
        | (`Interface _ | `Implementation _) as ast ->
          raw_source, Some ast
    )) in
  let reader = timed_lazy reader_time (lazy (
      reader_lazy := false;
      let lazy source = source in
      let config = Mconfig.normalize config in
      Mocaml.setup_reader_config config;
      let {Source_cache.result; was_cached} = Source_cache.with_cache source (Mreader.parse ?for_completion config) in
      result, config, was_cached
    )) in
  let ppx = timed_lazy ppx_time (lazy (
      ppx_lazy := false;
      let lazy ({Mreader.parsetree; _}, config, source_was_cached) = reader in
      let caught = ref [] in
      Msupport.catch_errors Mconfig.(config.ocaml.warnings) caught @@ fun () ->
      let parsetree = Ppx_cache.with_cache ~source_was_cached config (Mppx.rewrite parsetree) in
      (* let parsetree = Ppx_cache.with_cache ~source_was_cached:false config (Mppx.parse_and_rewrite) in *)
      { Ppx. config; parsetree; errors = !caught }
    )) in
  let typer = timed_lazy typer_time (lazy (
      typer_lazy := false;
      let lazy { Ppx. config; parsetree; _ } = ppx in
      Mocaml.setup_typer_config config;
      let result = Mtyper.run config parsetree in
      let errors = timed_lazy error_time (lazy (error_lazy := false; Mtyper.get_errors result)) in
      { Typer. errors; result }
    )) in
  { config; state; raw_source; source; reader; ppx; typer;
    pp_time; pp_lazy; reader_time; reader_lazy; ppx_time; ppx_lazy; typer_time; typer_lazy; error_time; error_lazy }

let make config source =
  process (Mconfig.normalize config) source

let for_completion position
    {config; state; raw_source;
     pp_time; reader_time; ppx_time; typer_time; error_time; _} =
  process config raw_source ~for_completion:position
    ~state ~pp_time ~reader_time ~ppx_time ~typer_time ~error_time

let timing_information t = [
  "pp"     , !(t.pp_time);
  "reader" , !(t.reader_time);
  "ppx"    , !(t.ppx_time);
  "typer"  , !(t.typer_time);
  "error"  , !(t.error_time);
]

let lazy_information l = [
  "pp"          , !(l.pp_lazy);
  "reader" , !(l.reader_lazy);
  "ppx"    , !(l.ppx_lazy);
  "typer"  , !(l.typer_lazy);
  "error"  , !(l.error_lazy);
]
