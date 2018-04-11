open Core

let get_inchan = function
  | "-"      -> In_channel.stdin
  | filename -> In_channel.create ~binary:true filename

let do_hash hash_length filename =
  let open Cryptokit in
  get_inchan filename
  |> hash_channel (Hash.md5 ())
  |> transform_string (Hexa.encode ())
  |> (fun s -> String.prefix s hash_length)
  |> print_endline

let regular_file =
  Command.Arg_type.create
    (fun filename ->
      match Sys.is_file filename with
      | `Yes -> filename
      | `No | `Unknown ->
        eprintf "'%s' is not a regular file.\n%!" filename;
        exit 1
    )

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Let_syntax.(
     let%map_open
       hash_length = anon ("hash_length" %: int)
     and filename  = anon (maybe_with_default "-" ("filename" %: regular_file))
     in
     fun () -> do_hash hash_length filename
    )

let () =
  Command.run ~version:"1.0" ~build_info:"RWD" command
