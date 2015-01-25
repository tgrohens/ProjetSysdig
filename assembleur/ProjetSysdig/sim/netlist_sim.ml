let number_steps = ref (-1)

let exec filename =
  try
    let p = Netlist.read_file filename in
    Sim.simule p !number_steps;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
     ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    exec
    ""
;;

main ()
