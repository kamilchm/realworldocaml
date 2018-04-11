open Core

let add_days base days = Date.(
  add_days base days |> to_string |> print_endline
)

let prompt_for_string name of_string =
  printf "enter %s: %!" name;
  match In_channel.(input_line stdin) with
  | None -> failwith "no value entered. aborting."
  | Some line -> of_string line

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    Command.Let_syntax.(
      let%map_open
        base = anon ("base" %: date)
      and days = anon (maybe ("days" %: int))
      in
      let days =
        match days with
        | Some x -> x
        | None -> prompt_for_string "days" Int.of_string
      in
      fun () -> add_days base days
    )

let diff =
  Command.basic
    ~summary:"Show days between [date1] and [date2]"
    Command.Let_syntax.(
      let%map_open
        date1 = anon ("date1" %: date)
      and date2 = anon ("date2" %: date)
      in
      fun () -> Date.diff date1 date2 |> printf "%d days\n"
    )

let command =
  Command.group ~summary:"Manipulate dates"
    [ "add", add
    ; "diff", diff
    ]

let () = Command.run command
