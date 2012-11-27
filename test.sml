fun rFile f =
    let val is = TextIO.openIn f
        val s = TextIO.inputAll is
    in TextIO.closeIn is;
       s
    end

val c = rFile "test.apl"

val ts = AplLex.lex c

fun prln s = print(s ^ "\n")
val () = prln "File lexed:"
val () = prln(" " ^ AplLex.pr_tokens ts)


val () = prln "Parsing tokens..."
val () = case AplParse.parse ts of
           SOME e => prln("Success:\n " ^ AplParse.pr_exp e)
         | NONE => prln "Parse error."
