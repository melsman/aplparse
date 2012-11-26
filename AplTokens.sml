structure AplLex = struct

datatype token =
         Alpha
       | Iota
       | Omega
       | Rho
       | Quad
       | Quaddiv
       | Quotquad
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
       | Ident | Nident
       | Qmark
       | Ring
       | Dot
       | Macron
       | Diamond
       | In
       | Nabla
       | Tilde
       | Newline
       | Letter of char
       | Digit of char
       | Id of string
       | Int of int
       | Double of real

datatype state = CommentS
               | StartS
               | NumS of string
               | IdS of string

fun getChar w =
    if w < 0w128 then SOME(Char.chr(Word.toInt w))
    else NONE

fun lexWord w =
    case w of
        0wx237A => SOME Alpha
      | 0wx2373 => SOME Iota
      | 0wx2375 => SOME Omega
      | 0wx2374 => SOME Rho
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
      | 0wxF7 => SOME Div
      | 0wxD7 => SOME Times
      | 0wx220A => SOME In
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
      | 0wx2261 => SOME Ident
      | 0wx2262 => SOME Nident
      | 0wx22C4 => SOME Diamond
      | 0wx2190 => SOME Larrow
      | 0wx2192 => SOME Rarrow
      | 0wx235D => SOME Comment
      | 0wx2395 => SOME Quad
      | 0wx235E => SOME Quotquad
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
        | SOME c =>
          if Char.isDigit c then SOME(Digit c) (* (Int(Char.ord c - 48)) *)
          else if Char.isLetter c then SOME(Letter c)
          else NONE
        | _ => NONE

fun classify f w =
    case getChar w of
      SOME c => if f c then SOME c
                else NONE
    | NONE => NONE

fun getLetter w = classify Char.isAlpha w 
fun getDigit w = classify Char.isDigit w
fun getLetterOrDigit w = classify Char.isAlphaNum w
fun isWhiteSpace w =
    case getChar w of
      SOME c => Char.isSpace c
    | NONE => false

fun process0 (w,(tokens,state)) =
    let val elem = lexWord w
        fun process (tokens,state) =
            case (state, elem) of
              (CommentS,      SOME Newline)      => (tokens, StartS)
            | (CommentS,      _           )      => (tokens, state)
            | (StartS,        SOME (Digit c))    => (tokens, NumS(Char.str c))
            | (NumS s,        SOME (Digit c))    => (tokens, NumS(s ^ Char.str c))
            | (NumS s,        SOME (Letter c))   => raise Fail "lex error: ilformed number"
            | (NumS s,        SOME Dot)          => (tokens, NumS(s ^ "."))
            | (StartS,        SOME (Letter c))   => (tokens, IdS(Char.str c))
            | (IdS s,         SOME (Letter c))   => (tokens, IdS(s ^ Char.str c))
            | (IdS s,         SOME (Digit c))    => (tokens, IdS(s ^ Char.str c))
            | (NumS s,        _)                 =>
              (case Int.fromString s of
                 SOME i => process'(Int i :: tokens, StartS)
               | NONE => case Real.fromString s of
                           SOME r => process'(Double r :: tokens, StartS)
                         | NONE => raise Fail ("lex error: ilformed number" ^ s))
            | (IdS s,         _)                 => process'(Id s :: tokens,StartS)
            | (StartS,        SOME s)            => (s::tokens,StartS)
            | (StartS,        NONE)              => if isWhiteSpace w then (tokens,state)
                                                    else raise Fail "lex error: hmmm"
                                                          
        and process'(tokens,s) =
            case elem of
              SOME Comment => (tokens,CommentS)
            | _ => process(tokens,s)
    in
      process(tokens,state)
    end

fun lex s = List.rev(Utf8.foldl process0 nil s)
end
