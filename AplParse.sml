structure AplParse : APL_PARSE = struct

val p_debug = false
fun debug f =
    if p_debug then print(f())
    else ()

open AplAst
structure PComb = ParseComb(type token=token 
                            val pr_token = AplLex.pr_token)

type reg = Region.reg
val botreg = (Region.botloc,Region.botloc)

open PComb infix >>> ->> >>- ?? ??? || oo oor

(* eat Newline's from the list of tokens *)
(* p_ws : unit p  *)
fun p_ws ts = (eat L.Newline ?? p_ws) #1 ts

(* Seperators: either whitespace or a Diamond *)
(* p_sep : unit p *)
val p_sep = p_ws || eat L.Diamond

(* p_id : string p *)
fun p_id nil = NO (Region.botloc,fn () => "expecting identifier but found end-of-file")
  | p_id ((L.Id id,r)::ts) = OK(id,r,ts)
  | p_id ((t,r:reg)::_) = NO (#1 r,fn() => ("expecting identifier but found token " ^ AplLex.pr_token t))

(* p_quad : string p *)
fun p_quad nil = NO (Region.botloc,fn () => "expecting Quad or identifier but found end-of-file")
  | p_quad ((L.Quad,r)::ts) = OK("$Quad",r,ts)
  | p_quad ((t,r:reg)::_) = NO (#1 r,fn() => ("expecting Quad or identifier but found token " ^ AplLex.pr_token t))

(* p_double : double p *)
fun p_double nil = NO (Region.botloc,fn () => "expecting double but found end-of-file")
  | p_double ((L.Double d,r)::ts) = OK(d,r,ts)
  | p_double ((t,r:reg)::_) = NO (#1 r, fn() => ("expecting double but found token " ^ AplLex.pr_token t))

(* p_complex : complex p *)
fun p_complex nil = NO (Region.botloc,fn () => "expecting complex number but found end-of-file")
  | p_complex ((L.Complex d,r)::ts) = OK(d,r,ts)
  | p_complex ((t,r:reg)::_) = NO (#1 r, fn() => ("expecting complex number but found token " ^ AplLex.pr_token t))
                                 
(* p_int : int p *)
fun p_int nil = NO (Region.botloc,fn () => "expecting integer but found end-of-file")
  | p_int ((L.Int i,r)::ts) = OK(i,r,ts)
  | p_int ((t,r:reg)::_) = NO (#1 r, fn() => ("expecting integer but found token " ^ AplLex.pr_token t))

(* p_string : word list p *)
fun p_string nil = NO (Region.botloc,fn () => "expecting string but found end-of-file")
  | p_string ((L.Chars ws,r)::ts) = OK(ws,r,ts)
  | p_string ((t,r:reg)::_) = NO (#1 r, fn() => ("expecting string but found token " ^ AplLex.pr_token t))

(* is_symb : Lexer.token -> bool *)
fun is_symb t =
    case t of
      L.Alpha => true
    | L.Alphaalpha => true
    | L.Omega => true
    | L.Omegaomega => true
    | L.Rho => true
    | L.Rtack => true
    | L.Ltack => true
    | L.Iota => true
    | L.Max => true
    | L.Min => true
    | L.Enclose => true
    | L.Disclose => true
    | L.Slash => true
    | L.Slashbar => true
    | L.Backslash => true
    | L.Squad => true
    | L.Gradeup => true
    | L.Gradedown => true
    | L.Each => true
    | L.Add => true
    | L.Sub => true
    | L.Times => true
    | L.Div => true
    | L.Dot => true
    | L.Pow => true
    | L.Qmark => true
    | L.Cat => true
    | L.Vcat => true
    | L.Trans => true
    | L.Rot => true
    | L.Vrot => true
    | L.Lt => true
    | L.Gt => true
    | L.Lteq => true
    | L.Gteq => true
    | L.Eq => true
    | L.Neq => true
    | L.Zilde => true
    | L.Circ => true
    | L.Circstar => true
    | L.Take => true
    | L.Drop => true
    | L.Or => true
    | L.And => true
    | L.Nor => true
    | L.Nand => true
    | L.Match => true
    | L.Nmatch => true
    | L.Tilde => true
    | L.Intersect => true
    | L.Union => true
    | L.StarDia => true
    | L.Ring => true
    | L.Pipe => true
    | L.Fac => true
    | L.In => true
    | L.Thorn => true
    | _ => false

(* p_symb : token p *)
fun p_symb nil = NO (Region.botloc,fn()=>"reached end-of-file")
  | p_symb ((t,r:reg)::ts) = 
    if is_symb t then OK(t,r,ts) 
    else NO (#1 r,
             fn () => ("expecting symbol but found token " ^ 
                       AplLex.pr_token t))

(* APL Parsing *)

(* Grammar:

   SEP := DIAMOND | NEWLINE

   body ::= guard <SEP body>
          | SEP body

   guard ::= expr <: expr>

   expr ::= assignment
          | seq <assignment>

   assignment ::= ID < [ indices ] > LARROW expr

   seq ::= item < seq >

   item ::= indexable < [ indices ] >

   indices ::= expr < ; indices >
             | ; indices

   indexable ::= INTEGER | DOUBLE | STRING | SYMBOL 
               | ( expr ) | { body }
*)

(* seq : exp * exp -> exp *)
fun seq (SeqE (es1,r1), SeqE (es2,r2)) = SeqE(es1@es2,Region.plus "seq1" r1 r2)
  | seq (SeqE (es,r), e) = SeqE(es @ [e],Region.plus "seq2" r (reg_exp e))
  | seq (e, SeqE (es,r)) = SeqE(e::es,Region.plus "seq3" (reg_exp e) r)
  | seq (e1,e2) = SeqE([e1,e2],Region.plus "seq4"(reg_exp e1)(reg_exp e2))

(* unres : exp * exp -> exp *)
fun unres (UnresE (es1,r1), UnresE (es2,r2)) = UnresE(es1@es2,Region.plus "unres1"r1 r2)
  | unres (UnresE (es,r), e) = UnresE(es @ [e],Region.plus "unres2" r (reg_exp e))
  | unres (e, UnresE (es,r)) = UnresE(e::es,Region.plus "unres3" (reg_exp e) r)
  | unres (e1,e2) = UnresE([e1,e2],Region.plus "unres4" (reg_exp e1)(reg_exp e2))

(* exp parsers *)
fun p_body ts =
    (((((p_guard ?? (p_sep ->> p_body)) seq) ?? p_ws) #1)
     || (p_sep ->> p_body)) ts

and p_guard ts =
    (p_expr ??? (eat L.Colon ->> p_expr)) GuardE ts

and p_expr ts =
    (  p_assignment 
    || (p_seq ?? p_assignment) seq
    ) ts

and p_assignment ts =
    ( ((((p_id || p_quad) oo (fn x => (x,nil))) ?? p_sqindices) (fn ((x,_),xs) => (x,xs))) >>- 
      eat L.Larrow >>> p_expr oor (fn (((x,xs),b),r) => AssignE(x,xs,b,r)) ) ts

and p_seq ts =
    (p_item ?? p_seq) unres ts

and p_sqindices ts = 
    (eat L.Lsqbra ->> p_indices >>- eat L.Rsqbra) ts

and p_item ts =
    (p_indexable ??? p_sqindices) IndexE ts

and p_indices ts =
    (  (p_expr oo (fn x => [SOME x]) ?? ((eat L.Semicolon ->> p_indices) || (eat L.Semicolon oo (fn() => [NONE])))) (op @)
    || (eat L.Semicolon oo (fn () => [NONE]) ?? p_indices) (op @)
    ) ts

and p_indexable ts =
    (  (p_int oor IntE)
    || (p_double oor DoubleE)
    || (p_complex oor ComplexE)
    || (p_string oor StrE)
    || (p_symb oor (fn (a,r) => IdE(Symb a,r)))
    || (p_id oor (fn (a,r) => IdE(Var a,r)))
    || ((eat L.Lpar ->> p_expr >>- eat L.Rpar) oor ParE)
    || ((eat L.Lbra ->> p_body >>- eat L.Rbra) oor (fn (e,r) => LambE((~1,~1),e,r)))
    ) ts

(* parse0 : (token * reg) list -> (exp, locerr) either *)
fun parse0 ts =
    case p_body ts of
      OK(ast,r,ts) => 
      (case ts of nil => OK ast
                | ((t,r)::_) => NO (#1 r, fn() => ("token " ^ AplLex.pr_token t 
                                                   ^ " not expected")))
    | NO l => NO l

structure Class = struct

(* Operators may take functions or arrays as arguments.
 *
 * We divide LambE bodies into functions and operators depending on
 * what operator- and function-argument references they contain:
 *   Omega  Alpha  Omegaomega  Alphaalpha  Class  Desciption
 *     _      0        0           0       (0,1)  Monadic function
 *     _      x        0           0       (0,2)  Dyadic function
 *     _      0        0           x       (1,1)  Monadic operator generating monadic function
 *     _      x        0           x       (1,2)  Monadic operator generating dyadic function
 *     _      0        x           _       (2,1)  Dyadic operator generating monadic function
 *     _      x        x           _       (2,2)  Dyadic operator generating dyadic function
 *)

type class = int * int (* partial order of valence pairs *)
val bot = (0,0)
val omega = (0,1)
val alpha = (0,2)
val alphaalpha = (1,0)
val omegaomega = (2,0)
fun mx x y = if Int.>(x,y) then x else y
fun lub (x1,y1) (x2,y2) = (mx x1 x2, mx y1 y2)   (* least upper bound *)

(* classify: to be applied to the body of a lambda expression; returns a pair
 * of an operator valence and a function valence (0, 1, or 2). *)

fun classify e : class =
    case e of
      IntE s => bot
    | DoubleE s => bot
    | ComplexE s => bot
    | VecE _ => bot
    | StrE s => bot
    | IdE(Symb L.Omega,_) => omega
    | IdE(Symb L.Alpha,_) => alpha
    | IdE(Symb L.Alphaalpha,_) => alphaalpha
    | IdE(Symb L.Omegaomega,_) => omegaomega
    | IdE _ => bot
    | LambE _ => bot (* don't go under a lambda *)
    | App1E (e0,e1,_) => lub (classify e0) (classify e1)
    | App2E (e0,e1,e2,_) => lub (classify e0) (lub (classify e1) (classify e2))
    | AppOpr1E (_,e0,e1,_) => lub (classify e0) (classify e1)
    | AppOpr2E (_,e0,e1,e2,_) => lub (classify e0) (lub (classify e1) (classify e2))
    | AssignE (v,is,e,_) => 
      foldl(fn (NONE, a) => a
             | (SOME e, a) => lub a (classify e)) (classify e) is
    | SeqE(es,_) => foldl (fn (e,a) => lub a (classify e)) bot es
    | ParE(e,_) => classify e
    | GuardE (e1,e2,_) => lub (classify e1) (classify e2)
    | IndexE (e0,is,_) =>
      foldl(fn (NONE, a) => a
             | (SOME e, a) => lub a (classify e)) (classify e0) is
    | UnresE(es,_) => foldl (fn (e,a) => lub a (classify e)) bot es
    | GenericE _ => raise Fail "classify:Generic"

fun pr_class (x,y) = "Cls(" ^ Int.toString x ^ "," ^ Int.toString y ^ ")"

val fun1 = (0,1)
val fun2 = (0,2)
val value = (0,0)
val opr1fun1 = (1,1)
val opr1fun2 = (1,2)
val opr2fun1 = (2,1)
val opr2fun2 = (2,2)

fun pFun1 c = c=fun1
fun pFun2 c = c=fun2
fun pVal c = c=value
fun pOpr1 c = c=opr1fun1 orelse c=opr1fun2
fun pOpr2 c = c=opr2fun1 orelse c=opr2fun2
fun appopr (_,n) = (0,n)

end

(* Utility function for resolving vectors in sequences of symbols and
 * immediate values *)
fun resolve_vectors gs =
    let fun vec [(e,s)] = (e,s)
          | vec nil = raise Fail "resolve_vectors.vec"
          | vec ((e,s)::gs) = 
            let val es = List.map #1 gs
                val r = reg_exps (reg_exp e) es
            in (VecE(e::es,r),s)
            end
        fun isValue (e,s) = s = [Class.value]
        val (gs,opt) =
            foldl (fn (g, (gs,NONE)) => if isValue g then (gs,SOME[g])
                                        else (g::gs,NONE)
                    | (g, (gs,SOME values)) =>
                      if isValue g then (gs,SOME(g::values))
                      else (g :: vec(rev values) :: gs,NONE))
                  (nil,NONE) gs
        val gs = case opt of
                   SOME values => vec(rev values) :: gs
                 | NONE => gs
    in rev gs
    end

(* Resolve and eliminate Unres nodes in the tree. The resolution is done
 * right-to-left. Each term is ascribed a specifier (a set of classes). The specifiers
 * are used to resolve terms *)

type spec = Class.class list
type env = (id * spec) list
val emp = []
fun lookup (E:env) id : spec option = 
    case List.find (fn (id',_) => id=id') E of
      SOME (_,s) => SOME s
    | NONE => NONE
fun add E (id,spec) = (id,spec)::E
fun plus (E,E') = E'@E
fun pr_spec s = "{" ^ String.concatWith "," (List.map Class.pr_class s) ^ "}"
fun pr_env E = 
    "{" ^ 
    String.concatWith "," (List.map (fn (id,s) => (pr_id id ^ ":" ^ pr_spec s)) E)
    ^ "}"

fun isKind p spec = List.exists p spec
fun isFunKind p E id =
    case List.find (fn (id',_) => id = id') E of
      SOME (_,s) => isKind p s
    | NONE => false
fun isVal (_,s) = isKind Class.pVal s
fun isFun1 (_,s) = isKind Class.pFun1 s
fun isFun2 (_,s) = isKind Class.pFun2 s
fun isOpr1 (_,s) = isKind Class.pOpr1 s
fun isOpr2 (_,s) = isKind Class.pOpr2 s
fun isOpr g = isOpr1 g orelse isOpr2 g
fun isFun g = isFun1 g orelse isFun2 g
fun appopr s = List.map Class.appopr s
val valuespec = [Class.value]
val lamb_env =
    let open Class
        open L
    in [(Symb Alpha, valuespec),
        (Symb Omega, valuespec),
        (Symb Alphaalpha, [value,fun1,fun2]),
        (Symb Omegaomega, [value,fun1,fun2])]
    end
fun pr_g (e,_) = pr_exp e
fun reg_g (e,_) = reg_exp e
val alpha_g = (IdE(Symb L.Alpha,botreg), valuespec)
val omega_g = (IdE(Symb L.Omega,botreg), valuespec)

(* Here is an example resolution:

 x1 x2  x3  f1(x4) : f1(x5) v(x6)
 x1 x2  1o2(x3) :  f1(x4) v(app1E(x5,x6))
 x1 x2  1o2(x3) :  v(app1E(x4,app1E(x5,x6)))
 x1 f1(x2)  1o2(x3) :  v(app1E(x4,app1E(x5,x6)))
 v(x1) f1(x2)  1o2(x3) :  v(app1E(x4,app1E(x5,x6)))
 v(x1) f2(App1E(x3,x2)) v(app1E(x4,app1E(x5,x6)))
 v(App2E(App1E(x3,x2),app1E(x4,app1E(x5,x6)),x1))
*)

fun resolveErr r msg =
    raise Fail ("Resolve Error: " ^ Region.pp r ^ ".\n  " ^ msg)

fun resolve E e =
    case e of
      IntE _ => (e,emp,valuespec)
    | DoubleE _ => (e,emp,valuespec)
    | ComplexE _ => (e,emp,valuespec)
    | VecE _ => (e,emp,valuespec)
    | StrE s => (e,emp,valuespec)
    | IdE (id,r) => 
      (case lookup E id of
         SOME s => (e,emp,s)
       | NONE => resolveErr r ("The identifier " ^ pr_id id ^ " is not in the environment"))
    | LambE (_,e,r) => 
      let val c = Class.classify e
          val (e,_,_) = resolve (lamb_env@E) e          
      in (LambE(c,e,r),emp,[c])
      end
    | App1E _ => raise Fail "resolve:App1"
    | App2E _ => raise Fail "resolve:App1"
    | AppOpr1E _ => raise Fail "resolve:App1"
    | AppOpr2E _ => raise Fail "resolve:App1"
    | AssignE (v,is,e,r) =>
      let val (e,E',s) = resolve E e
          val (is,E1) =
              foldl (fn (NONE, (is,E0)) => (NONE::is,E0)
                      | (SOME i, (is, E0)) => 
                        let val (i,E2,_) = resolve (E0@E'@E) i  (* memo: maybe check for valuespec *)
                        in (SOME i::is,E2@E0)
                        end) (nil,emp) is
          val E' = [(Var v,s)]
      in (AssignE(v,rev is,e,r),E',s)
      end
    | SeqE (es,r) => 
      let val (es,E,s) =
              foldl (fn (e,(es,E0,_)) =>
                        let val () = debug(fn () => "Resolving:\n " ^ pr_exp e ^ "\n")
                            val (e,E2,s) = resolve (E0@E) e
                        in (e::es,E2@E0,s)
                        end) (nil,emp,valuespec) es
      in (SeqE (rev es,r),E,s)
      end
    | ParE (e,_) => 
      let val (e,E,s) = resolve E e
      in (e,E,s)
      end
    | GuardE (e1,e2,r) =>
      let val (e1,E1,_) = resolve E e1                       (* memo: maybe check for valuespec *)
          val (e2,E2,s) = resolve (E1@E) e2
      in (GuardE(e1,e2,r),E2@E1,s)
      end
    | IndexE (e0,is,r) =>
      let val (is,E1) =
              foldl (fn (NONE, (is,E0)) => (NONE::is,E0)
                      | (SOME i, (is, E0)) => 
                        let val (i,E2,_) = resolve (E0@E) i  (* memo: maybe check for valuespec *)
                        in (SOME i::is,E2@E0)
                        end) (nil,emp) is
          val (e0,E0,_) = resolve (E1@E) e0                  (* memo: maybe check for valuespec *)
      in (IndexE(e0,rev is,r),E0@E1,valuespec)
      end
    | UnresE (es,r) =>
      let val (gs, E') = foldl (fn (e,(gs,E')) => 
                                   let val (e,E'',s) = resolve (E'@E) e
                                   in ((e,s)::gs,E''@E')
                                   end) (nil,emp) (rev es)
          val gs = resolve_vectors gs
          val (e,s) = res0 r (rev gs)
      in (e,E',s)
      end
    | GenericE _ => raise Fail "resolve:Generic"

and appOpr1((e1,s1),(e2,_)) =
    let val derivedfunvalences = List.map #2 (appopr s1)
        val r = Region.plus "appOpr1" (reg_exp e2) (reg_exp e1)
    in (AppOpr1E(derivedfunvalences,e1,e2,r),appopr s1)
    end
and appOpr2((e1,s1),(e2,_),(e3,_)) =
    let val derivedfunvalences = List.map #2 (appopr s1)
        val r = Region.plus "appOpr2" (reg_exp e2) (reg_exp e3)
    in (AppOpr2E(derivedfunvalences,e1,e2,e3,r),appopr s1)
    end
and app1((e1,_),(e2,_)) =
    let val r = Region.plus "app1" (reg_exp e1) (reg_exp e2)
    in (App1E(e1,e2,r),valuespec)
    end
and app2((e1,_),(e2,_),(e3,_)) =
    let val r1 = reg_exp e1
        val r2 = reg_exp e2
        val r3 = reg_exp e3
        val r = if r2 = botreg then
                  if r3 = botreg then r1
                  else Region.plus "app2.1" r1 r3
                else if r3 = botreg then Region.plus "app2.2" r2 r1
                else Region.plus "app2" r2 r3
    in (App2E(e1,e2,e3,r),valuespec)
    end
and res0 r gs =
    case gs of
      [] => raise Fail "res0: empty Unres node"
    | [g] => g
    | [g1,g2] =>
      (case resFun gs of
           SOME [g] => g
         | SOME [g1,g2] => if isFun g2 then
                             let val r = Region.plus "atop" (reg_g g2) (reg_g g1)
                                 fun lam c g = (LambE(c,#1(app1(g2,g)),r), [c])
                                 val g_m = if isFun1 g1 then SOME(lam Class.fun1 (app1(g1,omega_g)))
                                           else NONE
                                 val g_d = if isFun2 g1 then SOME(lam Class.fun2 (app2(g1,alpha_g,omega_g)))
                                           else NONE
                             in case (g_m, g_d) of
                                    (SOME (e_m,s_m), SOME (e_d,s_d)) => (GenericE(fn MONADIC => e_m | DYADIC => e_d, r), s_m @ s_d)
                                  | (SOME g,NONE) => g
                                  | (NONE, SOME g) => g
                                  | (NONE, NONE) => resolveErr r "res0.impossible - g1 should be a function"
                             end
                           else resolveErr (reg_g g2) "expecting a function"
         | SOME _ => resolveErr r "res0.impossible"
         | NONE => if isFun1 g2 andalso isVal g1 then res0 r [app1(g2,g1)]
                   (*else if isOpr1 g1 andalso isFun g2 then res0 r [appOpr1(g1,g2)]*)
                   else resolveErr (Region.plus "res0.1" (reg_g g2) (reg_g g1)) "could not resolve Unres node")
    | g1::g2::g3::gs =>   
      let fun cont() =
              if isFun1 g2 then res0 r (app1(g2,g1)::g3::gs)   (* ... b f1 a ==> ... b f1(a) *)
              else if isOpr1 g2 then
                case resFun (g3::gs) of
                  SOME (g3::gs) => res0 r (g1::appOpr1(g2,g3)::gs)
                | SOME nil => raise Fail "res0: impossible"
                | NONE => res0 r (g1::appOpr1(g2,g3)::gs) (* pass value as argument to monadic operator! *)
              else
                resolveErr (Region.plus "res0.2" (reg_g g3) (reg_g g1))
                           ("dyadic operator not yet supported for e2: " ^ pr_g g2 ^ "; e1: " ^ pr_g g1)
      in case resFun (g1::g2::g3::gs) of
             SOME (g1::g2::g3::gs) => (* it is a train *)
             (case resFun (g2::g3::gs) of
                  SOME nil => resolveErr (reg_g g3) "train.impossible1"
                | SOME [_] => resolveErr (reg_g g3) "train.impossible2"
                | SOME (g2::g3::gs) =>
                  if isFun2 g2 then
                    case try3Train g1 g2 g3 gs of
                        (SOME (e_m,s_m), SOME (e_d,s_d), r) => (GenericE(fn MONADIC => e_m | DYADIC => e_d, r), s_m @ s_d)
                      | (NONE, SOME g_d, _) => g_d
                      | (SOME g_m, NONE, _) => g_m
                      | (NONE, NONE, r) => resolveErr r ("expecting f and h in an fgh-train both to be either monadic or dyadic; got " ^ pr_g g3 ^ " and " ^ pr_g g1)
                  else resolveErr (reg_g g2) ("expecting dyadic function in train but got: " ^ pr_g g2)
                | NONE => resolveErr (reg_g g2) ("expecting function in train but got: " ^ pr_g g2))
          | _ =>  (* it is not a train *) 
            if isOpr2 g3 then                                     (* ... o2 f a *)
              case resFun gs of
                  SOME(g4::gs) => res0 r (g1::appOpr2(g3,g4,g2)::gs)
                | _ => case gs of
                           g4::gs => res0 r (g1::appOpr2(g3,g4,g2)::gs)
                         | nil => raise Fail "res0: expecting argument to dyadic operator"
(*
            else if isOpr1 g3 andalso List.null gs then
              res0 r (g1::appOpr1(g3,g2)::gs)
*)
            else if isFun2 g2 andalso isVal g1 andalso isVal g3 then
              res0 r (app2(g2,g3,g1)::gs)        (* ... b f2 a ==> ... f2(a,b) *)
            else if isOpr2 g2 then
              if not(isOpr g1) then
                case resFun (g3::gs) of
                    SOME (g3::gs) => res0 r (appOpr2(g2,g3,g1)::gs)
                  | SOME nil => raise Fail "res0: impossible2"
                  | NONE => if isVal g3 then res0 r (appOpr2(g2,g3,g1)::gs)
                            else resolveErr (reg_g g3) ("expecting value or function as left argument to dyadic operator - got " ^ pr_g g3)
              else resolveErr (reg_g g1) ("operators cannot take operators as arguments - got " ^ pr_g g1)
            else cont()
      end
and trainWrap f c r gs =
    let val g = (LambE(c,#1(f()),r), [c])
    in SOME(res0 r (g::gs))
    end handle _ => NONE
and try3Train g1 g2 g3 gs =
    let val r21 = Region.plus "train2" (reg_g g2) (reg_g g1)
    in case resFun (g3::gs) of
           SOME nil => resolveErr (reg_g g3) "try3Train.impossible"
         | SOME (g3::gs) =>
           let val r = Region.plus "train1" (reg_g g3) r21
               val g_m = if isFun1 g1 andalso isFun1 g3 then (* monadic fgh-train *)
                           trainWrap (fn () => app2(g2,app1(g3,omega_g),app1(g1,omega_g))) Class.fun1 r gs
                         else NONE
               val g_d = if isFun2 g1 andalso isFun2 g3 then 
                           trainWrap (fn () => app2(g2,app2(g3,alpha_g,omega_g),app2(g1,alpha_g,omega_g))) Class.fun2 r gs
                         else NONE
           in (g_m, g_d, r)
           end
         | NONE => 
           let val r = Region.plus "train1" (reg_g g3) r21
               val g_m = if isFun1 g1 andalso isVal g3 then (* monadic Agh-train *)
                           trainWrap (fn () => app2(g2,g3,app1(g1,omega_g))) Class.fun1 r gs
                         else NONE
               val g_d = if isFun2 g1 andalso isVal g3 then (* dyadic Agh-train *)
                           trainWrap (fn () => app2(g2,g3,app2(g1,alpha_g,omega_g))) Class.fun2 r gs
                         else if isFun2 g1 andalso List.null gs andalso isOpr1 g3 then (* dyadic fgh-train with / to the left, e.g. *)
                           trainWrap (fn () => app2(g2,app1(appOpr1(g3,alpha_g),omega_g),app2(g1,alpha_g,omega_g))) Class.fun2 r gs
                         else NONE
           in (g_m, g_d, r)
           end
    end
and resFun gs =
    case gs of
      [] => raise Fail "resFun: impossible"
    | [g] => if isFun g then SOME gs else
             if isOpr1 g then SOME [(LambE(Class.fun2,#1(app1(appOpr1(g,alpha_g),omega_g)),reg_g g), [Class.fun2])] else 
             NONE
    | g1::g2::gs' =>
      if isOpr2 g2 then NONE  (*raise Fail "resFun: dyadic operators not yet supported"*)
      else if isFun g1 then SOME gs 
      else if isOpr1 g1 then
        (case resFun (g2::gs') of
           SOME(g2::gs') => SOME(appOpr1(g1,g2)::gs')
         | SOME nil => raise Fail "resFun: impossible"
         | NONE => if isVal g2 then (* convert g1 to a fun2 function *) 
                     let val c = Class.fun2
                         val r = reg_g g1
                         val g = app1(appOpr1(g1,alpha_g),omega_g)
                         val g1' = (LambE(c,#1 g,r), [c])
                     in SOME (g1'::g2::gs')
                     end
                   else NONE)
      else NONE

val env0 =
    let open Class
        open L
    in List.map (fn (t,l) => (Symb t,l))
       [(Zilde,     valuespec),
        (Rho,       [fun1,fun2]),
        (Rtack,     [fun1,fun2]),
        (Ltack,     [fun2]),
        (Max,       [fun1,fun2]),
        (Min,       [fun1,fun2]),
        (Iota,      [fun1]),
        (Tilde,     [fun1]),
        (TildeDia,  [opr1fun1,opr1fun2]),
        (Trans,     [fun1,fun2]),
        (Enclose,   [fun1]),
        (Disclose,  [fun1,fun2]),
        (Gradeup,   [fun1]),
        (Gradedown, [fun1]),
        (Add,       [fun1,fun2]),
        (Sub,       [fun1,fun2]),
        (Times,     [fun1,fun2]),
        (Div,       [fun1,fun2]),
        (Cat,       [fun1,fun2]),
        (Rot,       [fun1,fun2]),
        (Vrot,      [fun1,fun2]),
        (Cat,       [fun1,fun2]),
        (Pipe,      [fun1,fun2]),
        (In,        [fun1,fun2]),
        (Qmark,     [fun1,fun2]),
        (Fac,       [fun1,fun2]),
        (Circstar,  [fun1,fun2]),
        (Pow,       [fun1,fun2]),
        (Vcat,      [fun2]),
        (Lt,        [fun2]),
        (Gt,        [fun2]),
        (Lteq,      [fun2]),
        (Gteq,      [fun2]),
        (Eq,        [fun2]),
        (Neq,       [fun2]),
        (Take,      [fun2]),
        (Drop,      [fun2]),
        (Or,        [fun2]),
        (And,       [fun2]),
        (Nor,       [fun2]),
        (Nand,      [fun2]),
        (Match,     [fun2]),
        (Nmatch,    [fun1,fun2]),
        (Intersect, [fun2]),
        (Union,     [fun2]),
        (Ring,      [fun1,fun2,opr2fun1,opr2fun2]),     (* fun1,fun2: hack to resolve Ring Dot (outer product) as an application of a dyadic operator *)
        (Each,      [opr1fun1,opr1fun2]),
        (StarDia,   [opr2fun1]),
        (Circ,      [fun1,fun2]),
        (Slash,     [opr1fun1]),
        (Slashbar,  [opr1fun1]),
        (Backslash, [opr1fun1]),
        (Squad,     [fun1]),
        (Thorn,     [fun1,fun2]),
        (Dot,       [opr2fun2])  (* MEMO: back to opr2fun2 *)
       ]
    end

exception ParseErr of Region.loc * string
fun parse E ts =
    case parse0 ts of
      OK e =>
      let val () = debug (fn () => "AST is\n " ^ pr_exp e ^ "\n")
          val (e',E',_) = resolve E e
      in (e',E')
      end
    | NO (l,f) => raise ParseErr (l,f())

open Class
fun add (id,l) e = (Var id, l) :: e 
end
