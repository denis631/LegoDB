open Lego_db

let _ =
  Exec.Runner.benchmark Bootstrapper.bootstrap;
  Repl.loop ()
