let run ?loc ~fallback:_ fn = Eio_posix.run ?loc (fun env -> fn (env :> Eio.Stdenv.t))
