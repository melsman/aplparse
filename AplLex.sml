structure AplLex = struct

datatype token =
         Alpha
       | Omega
       | Alphaalpha
       | Omegaomega
       | Iota
       | Rho
       | Quad
       | Quaddiv
       | Quotquad
       | Quot
       | Max | Min
       | Enclose | Disclose
       | Slash | Backslash
       | Slashbar | Backslashbar
       | Gradeup | Gradedown
       | Each
       | Add
       | Sub
       | Times
       | Div
       | Pow
       | Lbra | Rbra
       | Lpar | Rpar
       | Lsqbra | Rsqbra
       | Semicolon | Colon
       | Cat | Vcat
       | Trans
       | Rot | Vrot
       | Larrow | Rarrow
       | Lt | Gt | Lteq | Gteq | Eq | Neq
       | Zilde
       | Circ
       | Circstar
       | Take | Drop
       | Or | And
       | Nor | Nand
       | Match | Nmatch
       | Qmark
       | Ring
       | Dot
       | Macron
       | Diamond
       | In
       | Nabla
       | Tilde
       | Intersect | Union
       | Lamp
       | Comment of token list
       | Newline
       | Letter of char
       | Digit of char
       | Id of string
       | Int of string
       | Double of string
       | Chars of word list
       | Dollar
       | Underscore
       | StarDia
       | TildeDia
       | Pipe
       | Fac
       | EOF

(* pr_chars : word list -> string *)
fun pr_chars ws =
    if List.all (fn w => w < 0w128) ws then
      "'" ^ implode (List.map (Char.chr o Word.toInt) ws) ^ "'"
    else "Chars(" ^ String.concatWith "," (List.map Word.toString ws) ^ ")"

val ppw = String.str o Char.chr o Word.toInt

(* pp_token : token -> string *)
fun pp_token t =
    case t of
         Alpha => ppw 0wx237A
       | Omega => ppw 0wx2375
       | Alphaalpha => pp_token Alpha ^ pp_token Alpha
       | Omegaomega => pp_token Omega ^ pp_token Omega
       | Iota => ppw 0wx2373
       | Rho => ppw 0wx2374
       | Quad => ppw 0wx2395
       | Quaddiv => ppw 0wx2339
       | Quotquad => ppw 0wx235E
       | Quot => "'"
       | Max => ppw 0wx2308
       | Min => ppw 0wx230A
       | Enclose => ppw 0wx2282
       | Disclose => ppw 0wx2283
       | Slash => "/"
       | Backslash => "\\"
       | Slashbar => ppw 0wx233F
       | Backslashbar => ppw 0wx2340
       | Gradeup => ppw 0wx234B
       | Gradedown => ppw 0wx2352
       | Each => ppw 0wxA8
       | Add => "+"
       | Sub => "-"
       | Times => ppw 0wxD7
       | Div => ppw 0wxF7
       | Pow => "*"
       | Lbra => "{"
       | Rbra => "}"
       | Lpar => "("
       | Rpar => ")"
       | Lsqbra => "["
       | Rsqbra => "]"
       | Semicolon => ";"
       | Colon => ":"
       | Cat => ","
       | Vcat => ppw 0wx236A
       | Trans => ppw 0wx2349
       | Rot => ppw 0wx233D
       | Vrot => ppw 0wx2296
       | Larrow => ppw 0wx2190
       | Rarrow => ppw 0wx2192
       | Lt => "<"
       | Gt => ">"
       | Lteq => ppw 0wx2264
       | Gteq => ppw 0wx2265
       | Eq => "="
       | Neq => ppw 0wx2260
       | Zilde => ppw 0wx236C
       | Circ => ppw 0wx25CB
       | Circstar => ppw 0wx235F
       | Take => ppw 0wx2191
       | Drop => ppw 0wx2193
       | Or => ppw 0wx2228
       | And => ppw 0wx2227
       | Nor => ppw 0wx2371
       | Nand => ppw 0wx2372
       | Match => ppw 0wx2261
       | Nmatch => ppw 0wx2262
       | Qmark => "?"
       | Ring => ppw 0wx2218
       | Dot => "."
       | Macron => ppw 0wxAF
       | Diamond => ppw 0wx22C4
       | In => ppw 0wx220A
       | Nabla => ppw 0wx2207
       | Tilde => "~"
       | Intersect => ppw 0wx2229
       | Union => ppw 0wx222A
       | Lamp => ppw 0wx235D
       | Comment ts => pp_token Lamp ^ pp_tokens ts
       | Newline => "\n"
       | Letter c => String.str c
       | Digit c => String.str c
       | Id s => s
       | Int i => i
       | Double r => r
       | Chars ws => pr_chars ws
       | Dollar => "$"
       | Underscore => "_"
       | StarDia => ppw 0wx2363
       | TildeDia => ppw 0wx2368
       | Pipe => "|"
       | Fac => "!"
       | EOF => "\\EOF"
and pp_tokens ts = String.concatWith "" (List.map pp_token ts)

(* pr_token : token -> string *)
fun pr_token t =
    case t of
         Alpha => "Alpha"
       | Omega => "Omega"
       | Alphaalpha => "Alphaalpha"
       | Omegaomega => "Omegaomega"
       | Iota => "Iota"
       | Rho => "Rho"
       | Quad => "Quad"
       | Quaddiv => "Quaddiv"
       | Quotquad => "Quotquad"
       | Quot => "Quot"
       | Max => "Max"
       | Min => "Min"
       | Enclose => "Enclose"
       | Disclose => "Disclose"
       | Slash => "Slash"
       | Backslash => "Backslash"
       | Slashbar => "Slashbar"
       | Backslashbar => "Backslashbar"
       | Gradeup => "Gradeup"
       | Gradedown => "Gradedown"
       | Each => "Each"
       | Add => "Add"
       | Sub => "Sub"
       | Times => "Times"
       | Div => "Div"
       | Pow => "Pow"
       | Lbra => "Lbra"
       | Rbra => "Rbra"
       | Lpar => "Lpar"
       | Rpar => "Rpar"
       | Lsqbra => "Lsqbra"
       | Rsqbra => "Rsqbra"
       | Semicolon => "Semicolon"
       | Colon => "Colon"
       | Cat => "Cat"
       | Vcat => "Vcat"
       | Trans => "Trans"
       | Rot => "Rot"
       | Vrot => "Vrot"
       | Larrow => "Larrow"
       | Rarrow => "Rarrow"
       | Lt => "Lt"
       | Gt => "Gt"
       | Lteq => "Lteq"
       | Gteq => "Gteq"
       | Eq => "Eq"
       | Neq => "Neq"
       | Zilde => "Zilde"
       | Circ => "Circ"
       | Circstar => "Circstar"
       | Take => "Take"
       | Drop => "Drop"
       | Or => "Or"
       | And => "And"
       | Nor => "Nor"
       | Nand => "Nand"
       | Match => "Match"
       | Nmatch => "Nmatch"
       | Qmark => "Qmark"
       | Ring => "Ring"
       | Dot => "Dot"
       | Macron => "Macron"
       | Diamond => "Diamond"
       | In => "In"
       | Nabla => "Nabla"
       | Tilde => "Tilde"
       | Intersect => "Intersect"
       | Union => "Union"
       | Lamp => "Lamp"
       | Comment ts => "Comment [" ^ pr_tokens ts ^ "]"
       | Newline => "Newline"
       | Letter c => "Letter(" ^ String.str c ^ ")"
       | Digit c => "Digit(" ^ String.str c ^ ")"
       | Id s => "Id(" ^ s ^ ")"
       | Int i => i
       | Double r => r
       | Chars ws => pr_chars ws
       | Dollar => "Dollar"
       | Underscore => "Underscore"
       | StarDia => "StarDia"
       | TildeDia => "TildeDia"
       | Pipe => "Pipe"
       | Fac => "Fac"
       | EOF => "End-of-file"
(* pr_tokens : token list -> string *)
and pr_tokens ts = String.concatWith " " (List.map pr_token ts)

type filename = Region.filename
type loc = Region.loc
type reg = Region.reg
fun loc0 f : loc = (1,0,f) (* line 1, char 0 *)

datatype state = CommentS of token list * loc * loc
               | StartS
               | SymbS of token * loc * loc   (* for lexing Alphaalpha, Omegaomega, Quad-Id *)
               | IntS of string * loc * loc
               | DoubleS of string * loc * loc
               | CharsS of word list * loc * loc
               | IdS of string * loc * loc
               | EndS

(* getChar : word -> char option *)
fun getChar w =
    if w < 0w128 then SOME(Char.chr(Word.toInt w))
    else NONE

(* lexWord : word -> token option *)
fun lexWord w =
    case w of
        0wx237A => SOME Alpha
      | 0wx2373 => SOME Iota
      | 0wx2375 => SOME Omega
      | 0wx2374 => SOME Rho
      | 0wx2363 => SOME StarDia
      | 0wx22C6 => SOME Pow
      | 0wx2368 => SOME TildeDia
      | 0wxAF => SOME Macron
      | 0wx236C => SOME Zilde
      | 0wxA8 => SOME Each
      | 0wx233F => SOME Slashbar
      | 0wx2340 => SOME Backslashbar
      | 0wx2264 => SOME Lteq
      | 0wx2265 => SOME Gteq
      | 0wx2260 => SOME Neq
      | 0wx2228 => SOME Or
      | 0wx2227 => SOME And
      | 0wx2371 => SOME Nor
      | 0wx2372 => SOME Nand
      | 0wxF7 => SOME Div
      | 0wxD7 => SOME Times
      | 0wx2212 => SOME Sub
      | 0wx220A => SOME In
      | 0wx2208 => SOME In
      | 0wx2191 => SOME Take
      | 0wx2193 => SOME Drop
      | 0wx25CB => SOME Circ
      | 0wx2308 => SOME Max
      | 0wx230A => SOME Min
      | 0wx2207 => SOME Nabla
      | 0wx2218 => SOME Ring
      | 0wx2282 => SOME Enclose
      | 0wx2283 => SOME Disclose
      | 0wx2229 => SOME Intersect
      | 0wx222A => SOME Union
      | 0wx2352 => SOME Gradedown
      | 0wx234B => SOME Gradeup
      | 0wx2349 => SOME Trans
      | 0wx233D => SOME Rot
      | 0wx2296 => SOME Vrot
      | 0wx235F => SOME Circstar
      | 0wx2339 => SOME Quaddiv
      | 0wx236A => SOME Vcat
      | 0wx2261 => SOME Match
      | 0wx2262 => SOME Nmatch
      | 0wx22C4 => SOME Diamond
      | 0wx2190 => SOME Larrow
      | 0wx2192 => SOME Rarrow
      | 0wx235D => SOME Lamp
      | 0wx2395 => SOME Quad
      | 0wx235E => SOME Quotquad
      | 0wx0003 => SOME EOF
      | _ =>
        case getChar w of
          SOME #"{" => SOME Lbra
        | SOME #"}" => SOME Rbra
        | SOME #"(" => SOME Lpar
        | SOME #")" => SOME Rpar
        | SOME #"." => SOME Dot
        | SOME #"," => SOME Cat
        | SOME #"*" => SOME Pow
        | SOME #"/" => SOME Slash
        | SOME #"\\" => SOME Backslash
        | SOME #"?" => SOME Qmark
        | SOME #"=" => SOME Eq
        | SOME #"<" => SOME Lt
        | SOME #">" => SOME Gt
        | SOME #"+" => SOME Add
        | SOME #"-" => SOME Sub
        | SOME #"~" => SOME Tilde
        | SOME #"\n" => SOME Newline
        | SOME #"[" => SOME Lsqbra
        | SOME #"]" => SOME Rsqbra
        | SOME #":" => SOME Colon
        | SOME #";" => SOME Semicolon
        | SOME #"$" => SOME Dollar
        | SOME #"_" => SOME Underscore
        | SOME #"|" => SOME Pipe
        | SOME #"!" => SOME Fac
        | SOME #"'" => SOME Quot
        | SOME c =>
          if Char.isDigit c then SOME(Digit c)
          else if Char.isAlpha c then SOME(Letter c)
          else NONE
        | _ => NONE

(* isWhiteSpace : word -> bool *)
fun isWhiteSpace w =
    case getChar w of
      SOME c => Char.isSpace c
    | NONE => false

(* lexError : loc -> string -> 'a *)
fun lexError loc s = 
    let val msg = "Lexical error at location " ^ Region.ppLoc loc ^ ": " ^ s
    in raise Fail msg
    end

type procstate = (token * reg) list * state * loc

(* process : word * procstate -> procstate *)
fun process0 (w,(tokens,state,loc)) =
    let val elem = lexWord w
        fun process (tokens,state,loc) =
            case (state, elem) of
              (StartS, SOME EOF)                 => (tokens, EndS, loc)
            | (CommentS(ts,l0,l1), SOME EOF)     => ((Comment(rev ts),(l0,loc))::tokens, EndS, Region.newline loc)
            | (CommentS(ts,l0,l1), SOME Newline) => let val nloc = Region.next loc
                                                    in ((Newline,(nloc,nloc))::(Comment(rev ts),(l0,loc))::tokens, StartS, Region.newline nloc)
                                                    end
            | (CommentS(ts,l0,l1), SOME t)       => (tokens, CommentS (t::ts,l0,loc), Region.next loc)
            | (CommentS(ts,l0,l1), NONE)         => (tokens, CommentS (Chars [w]::ts,l0,loc), Region.next loc)
            | (EndS, _)                          => lexError loc "Content after End-of-file (should not be possible)"
            | (CharsS _,      SOME EOF)          => lexError loc "End-of-file reached while parsing string"
            | (StartS,        SOME Macron)       => (tokens, IntS("-",loc,loc), Region.next loc)
            | (StartS,        SOME (Digit c))    => (tokens, IntS(String.str c,loc,loc), Region.next loc)
            | (IntS(s,l0,_),  SOME (Digit c))    => (tokens, IntS(s ^ String.str c,l0,loc), Region.next loc)
            | (DoubleS(s,l0,_), SOME (Digit c))  => (tokens, DoubleS(s ^ String.str c,l0,loc), Region.next loc)
            | (IntS(s,l0,_),  SOME (Letter c))   => lexError loc "ilformed integer"
            | (DoubleS(s,l0,_), SOME (Letter c)) => lexError loc "ilformed double"
            | (IntS(s,l0,_),  SOME Dot)          => (tokens, DoubleS(s ^ ".",l0,loc), Region.next loc)
            | (StartS,        SOME (Letter c))   => (tokens, IdS(String.str c,loc,loc), Region.next loc)
            | (StartS,        SOME Dollar)       => (tokens, IdS("$",loc,loc), Region.next loc)
            | (StartS,        SOME Underscore)   => (tokens, IdS("_",loc,loc), Region.next loc)
            | (IdS(s,l0,_),   SOME (Letter c))   => (tokens, IdS(s ^ String.str c,l0,loc), Region.next loc)
            | (IdS(s,l0,_),   SOME Underscore)   => (tokens, IdS(s ^ "_",l0,loc), Region.next loc)
            | (IdS(s,l0,_),   SOME (Digit c))    => (tokens, IdS(s ^ String.str c,l0,loc), Region.next loc)
            | (StartS,        SOME Alpha)        => (tokens, SymbS(Alpha,loc,loc), Region.next loc)
            | (StartS,        SOME Omega)        => (tokens, SymbS(Omega,loc,loc), Region.next loc)
            | (StartS,        SOME Quad)         => (tokens, SymbS(Quad,loc,loc), Region.next loc)
            | (SymbS(Alpha,l0,_), SOME Alpha)    => ((Alphaalpha,(l0,loc))::tokens, StartS, Region.next loc)
            | (SymbS(Omega,l0,_), SOME Omega)    => ((Omegaomega,(l0,loc))::tokens, StartS, Region.next loc)
            | (SymbS(Quad,l0,_), SOME (Letter c)) => (tokens, IdS("Quad$" ^ String.str c,l0,loc), Region.next loc)
            | (SymbS(t,l0,l1), _)                => process' ((t,(l0,l1))::tokens, StartS, loc)
            | (IntS(s,l0,l1), _)                 =>
              (case Int.fromString s of
                 SOME _ => process' ((Int s,(l0,l1))::tokens, StartS, loc)
               | NONE => lexError loc ("illformed integer " ^ s))
            | (DoubleS(s,l0,l1), _)              =>
              (case Real.fromString s of
                 SOME _ => process' ((Double s,(l0,l1)) :: tokens, StartS, loc)
               | NONE => lexError loc ("illformed double " ^ s))
            | (IdS(s,l0,l1),  _)                 => process' ((Id s, (l0, l1)) :: tokens, StartS, loc)
            | (StartS,        SOME Lamp)         => (tokens, CommentS (nil, loc, loc), Region.next loc)
            | (StartS,        SOME Quot)         => (tokens, CharsS (nil, loc, loc), Region.next loc)
            | (CharsS(ws,l0,l1),SOME Quot)       => ((Chars(rev ws),(l0,loc))::tokens, StartS, Region.next loc)
            | (CharsS(ws,l0,l1), _)              => (tokens,CharsS(w::ws,l0,loc), Region.next loc)
            | (StartS,        SOME s)            => ((s,(loc,loc))::tokens,StartS,
                                                     if s = Newline then Region.newline loc
                                                     else Region.next loc)
            | (StartS,        NONE)              => if isWhiteSpace w then (tokens,state,Region.next loc)
                                                    else lexError loc ("don't know what to do with " ^ Word.toString w)
        and process'(tokens,s,loc) =
            case elem of
              SOME Lamp => (tokens, CommentS (nil, loc, loc), Region.next loc)
            | _ => process(tokens,s,loc)
    in
      process(tokens,state,loc)
    end

(* lex : string -> string -> (token * reg) list *)
fun lex filename s =
    let val s = Utf8.fromString (s^"\u0003")  (* pad some whitespace to keep the lexer happy *)
        val (tokens,state,_) = Utf8.foldl process0 (nil, StartS, Region.loc0 filename) s
    in case state of
           EndS => rev tokens
         | _    => raise Fail "Reached end-of-file in non final-state (should not happen)"
    end
end
