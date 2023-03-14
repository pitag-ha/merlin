type t = {result : Mreader.result; was_cached : bool}

val with_cache : Msource.t * Mreader.parsetree option -> (Msource.t * Mreader.parsetree option -> Mreader.result) -> t
(** [with_cache source f] levarages a cache mechanism to return the previous reader
    result on [source] when possible. When not possible, it runs [f source] to do the reading. *)
