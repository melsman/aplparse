fun rFile f =
    let val is = TextIO.openIn f
        val s = TextIO.inputAll is
    in TextIO.closeIn is;
       s
    end

val f =
    case CommandLine.arguments() of
      [f] => f
    | _ => (print ("Usage: " ^ CommandLine.name() ^ " file.apl\n"); OS.Process.exit OS.Process.failure)

val c = rFile f
val ts = AplLex.lex c

fun prln s = print(s ^ "\n")
val () = prln "File lexed:"
val () = prln(" " ^ AplLex.pr_tokens ts)

val () = prln "Parsing tokens..."
val () = case AplParse.parse AplParse.env0 ts of
           SOME (e,_) => (prln("Success:\n " ^ AplAst.pr_exp e);
                          OS.Process.exit OS.Process.success)
         | NONE => (prln "Parse error.";
                    OS.Process.exit OS.Process.failure)
                    
