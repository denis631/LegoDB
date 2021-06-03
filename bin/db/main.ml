open Lego_db

let _ =
  Bootstrapper.bootstrap () ;
  Repl.run ()
