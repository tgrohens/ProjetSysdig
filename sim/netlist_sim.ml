let number_steps = ref (-1)
let real_time = ref false 

let exec filename =
  try
    let p = Netlist.read_file filename in
    Sim.simule p !number_steps !real_time;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error occurred: %s@." s; exit 2

let main () =
  Arg.parse
     ["-n", Arg.Set_int number_steps, "Number of steps to simulate";
      "--real-time", Arg.Set real_time, "Should it run real-time"]
    exec
    ""
;;

main ()
