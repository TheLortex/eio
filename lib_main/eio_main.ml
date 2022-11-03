let force run fn =
  run ~fallback:(fun (`Msg msg) -> failwith msg) fn

let run ~loc fn =
  match Sys.getenv_opt "EIO_BACKEND" with
  | Some ("io-uring" | "linux") -> force (Linux_backend.run ~loc) fn
  | Some "posix" -> force (Posix_backend.run ~loc) fn
  | Some "luv" -> force Luv_backend.run fn
  | None | Some "" ->
    Linux_backend.run ~loc fn ~fallback:(fun _ ->
        Posix_backend.run ~loc fn ~fallback:(fun _ ->
            force Luv_backend.run fn
          )
      )
  | Some x -> Fmt.failwith "Unknown Eio backend %S (from $EIO_BACKEND)" x

let[@inline] run fn =
  let loc = Eio.Private.Ctf.get_caller () in
  run ~loc fn