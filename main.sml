fun main () =
  case CommandLine.arguments() of
      [input] => TFC.compileFile input
    | _ => raise Fail "Bad invocation"

val _ = main ()
