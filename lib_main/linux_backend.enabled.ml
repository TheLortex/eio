let run ?loc ~fallback fn = Eio_linux.run ?loc ~fallback (fun env -> fn (env :> Eio.Stdenv.t))
