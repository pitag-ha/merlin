type t = Mreader.parsetree

val with_cache : source_was_cached:bool -> Mconfig.t -> (Mconfig.t -> t) -> t
(** [with_cache ~source_was_cached config f] levarages a cache mechanism to return the previous result of
    the parsetree expansion configured by [config] when possible. When not possible, it runs [f config]
    to do the expansion. *)
