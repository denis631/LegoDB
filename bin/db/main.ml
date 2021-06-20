open Lego_db

let _ =
  Query.Driver.benchmark Bootstrapper.bootstrap;
  Repl.loop ()
