type t = {result : Mreader.result; was_cached : bool}

let {Logger. log} = Logger.for_section ("Source_cache")

type cache = { last_source : Msource.Digest.t; last_result : Mreader.result}

let cache = ref None

let with_cache (source, pp_result) f =
  let title = "reader" in
  let result, was_cached = match !cache, pp_result with
    | Some { last_source; last_result}, None when Msource.Digest.(equal last_source @@ make source) ->
      log ~title "reusing source";
      last_result, true
    | _->
      log ~title "source was parsed";
      f (source, pp_result), false
  in
  cache := Some {last_source = Msource.Digest.make source; last_result = result};
  { result; was_cached }
