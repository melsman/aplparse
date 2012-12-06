structure AplParse : APL_PARSE = struct

val p_debug = true
fun debug f =
    if p_debug then print(f())
    else ()

open AplAst
structure PComb = ParseComb(type token=token)

open PComb infix >>> ->> >>- ?? || oo

val p_sep = eat L.Newline || eat L.Diamond

fun p_ws ts = (eat L.Newline ?? p_ws) #1 ts

fun p_id nil = NONE
  | p_id (L.Id id::ts) = SOME(id,ts)
  | p_id _ = NONE

fun p_double nil = NONE
  | p_double (L.Double d::ts) = SOME(d,ts)
  | p_double _ = NONE

fun p_int nil = NONE
  | p_int (L.Int i::ts) = SOME(i,ts)
  | p_int _ = NONE

fun is_symb t =
    case t of
      L.Alpha => true
    | L.Omega => true
    | L.Rho => true
    | L.Iota => true
    | L.Max => true
    | L.Min => true
    | L.Enclose => true
    | L.Disclose => true
    | L.Slash => true
    | L.Gradeup => true
    | L.Gradedown => true
    | L.Each => true
    | L.Add => true
    | L.Sub => true
    | L.Times => true
    | L.Div => true
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
    | L.Match => true
    | L.Nmatch => true
    | L.Tilde => true
    | L.Intersect => true
    | L.Union => true
    | _ => false

fun p_symb nil = NONE
  | p_symb (t::ts) = if is_symb t then SOME(t,ts) else NONE

(* APL Parsing *)

(* Grammar:

   SEP := DIAMOND | NEWLINE

   body ::= guard <SEP body>
          | SEP body

   guard ::= expr <: expr>

   expr ::= assignment
          | seq <assignment>

   assignment ::= ID LARROW expr

   seq ::= item < seq >

   item ::= indexable < [ indices ] >

   indices ::= expr < ; indices >
             | ; indices

   indexable ::= INTEGER | DOUBLE | STRING | SYMBOL 
               | ( expr ) | { body }
*)

fun seq (SeqE es1, SeqE es2) = SeqE(es1@es2)
  | seq (SeqE es, e) = SeqE(es @ [e])
  | seq (e, SeqE es) = SeqE(e::es)
  | seq (e1,e2) = SeqE[e1,e2]

fun unres (UnresE es1, UnresE es2) = UnresE(es1@es2)
  | unres (UnresE es, e) = UnresE(es @ [e])
  | unres (e, UnresE es) = UnresE(e::es)
  | unres (e1,e2) = UnresE[e1,e2]

fun p_body ts =
    (((p_guard ?? (p_sep ->> p_body)) seq) ?? p_ws) #1 ts

and p_guard ts =
    (p_expr ?? (eat L.Colon ->> p_expr)) GuardE ts

and p_expr ts =
    (  p_assignment 
    || (p_seq ?? p_assignment) seq
    ) ts

and p_assignment ts =
    (p_id >>- eat L.Larrow >>> p_expr oo AssignE) ts

and p_seq ts =
    (p_item ?? p_seq) unres ts

and p_item ts =
    (p_indexable ?? (eat L.Lsqbra ->> p_indices >>- eat L.Rsqbra)) IndexE ts

and p_indices ts =
    (  (p_expr oo (fn x => [SOME x]) ?? (eat L.Semicolon ->> p_indices)) (op @)
    || (eat L.Semicolon oo (fn () => [NONE]))
    ) ts

and p_indexable ts =
    (  (p_int oo IntE)
    || (p_double oo DoubleE)
    || (p_symb oo (IdE o Symb))
    || (p_id oo (IdE o Var))
    || ((eat L.Lpar ->> p_expr >>- eat L.Rpar) oo ParE)
    || ((eat L.Lbra ->> p_expr >>- eat L.Rbra) oo LambE)
    ) ts

fun parse0 ts =
    case p_body ts of
      SOME(ast,ts) => 
      (case ts of nil => SOME ast
                | _ => NONE)
    | NONE => NONE

fun resolve_vectors es =
    let
      fun immed (DoubleE _) = true
        | immed (IntE _) = true
        | immed _ = false

      fun vec [e] = e
        | vec nil = raise Fail "resolve_vectors.vec"
        | vec es = VecE es
      
      val (es,opt) =
            foldl (fn (e, (es,NONE)) => if immed e then (es,SOME[e])
                                        else (e::es,NONE)
                    | (e, (es,SOME immeds)) =>
                      if immed e then (es,SOME(e::immeds))
                      else (e :: vec(rev immeds) :: es,NONE))
                  (nil,NONE) es
        val es = case opt of
                   SOME immeds => vec(rev immeds) :: es
                 | NONE => es
    in rev es
    end

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
    | VecE _ => bot
    | StrE s => bot
    | IdE(Symb L.Omega) => omega
    | IdE(Symb L.Alpha) => alpha
    | IdE(Symb L.Alphaalpha) => alphaalpha
    | IdE(Symb L.Omegaomega) => omegaomega
    | IdE _ => bot
    | LambE e => bot (* don't go under a lambda *)
    | App1E (e0,e1) => lub (classify e0) (classify e1)
    | App2E (e0,e1,e2) => lub (classify e0) (lub (classify e1) (classify e2))
    | AssignE (v,e) => classify e
    | SeqE es => foldl (fn (e,a) => lub a (classify e)) bot es
    | ParE e => classify e
    | GuardE (e1,e2) => lub (classify e1) (classify e2)
    | IndexE (e0,is) =>
      foldl(fn (NONE, a) => a
             | (SOME e, a) => lub a (classify e)) (classify e0) is
    | UnresE es => foldl (fn (e,a) => lub a (classify e)) bot es

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
fun isVal s = isKind Class.pVal s
fun isFun1 s = isKind Class.pFun1 s
fun isFun2 s = isKind Class.pFun2 s
fun isOpr1 s = isKind Class.pOpr1 s
fun isOpr2 s = isKind Class.pOpr2 s
fun isFun s = isFun1 s orelse isFun2 s
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

(* Here is an example of resolution:

 x1 x2  x3  f1(x4) : f1(x5) v(x6)
 x1 x2  1o2(x3) :  f1(x4) v(app1E(x5,x6))
 x1 x2  1o2(x3) :  v(app1E(x4,app1E(x5,x6)))
 x1 f1(x2)  1o2(x3) :  v(app1E(x4,app1E(x5,x6)))
 v(x1) f1(x2)  1o2(x3) :  v(app1E(x4,app1E(x5,x6)))
 v(x1) f2(App1E(x3,x2)) v(app1E(x4,app1E(x5,x6)))
 v(App2E(App1E(x3,x2),app1E(x4,app1E(x5,x6)),x1))
*)

fun resolve E e =
    case e of
      IntE s => (e,emp,valuespec)
    | DoubleE s => (e,emp,valuespec)
    | VecE _ => (e,emp,valuespec)
    | StrE s => (e,emp,valuespec)
    | IdE id => 
      (case lookup E id of
         SOME s => (e,emp,s)
       | NONE => raise Fail ("resolve: identifier " ^ pr_id id ^ " not in the environment"))
    | LambE e => 
      let val c = Class.classify e
          val (e,_,_) = resolve (lamb_env@E) e          
      in (LambE e,emp,[c])
      end
    | App1E (e0,e) => raise Fail "resolve:App1"
    | App2E (e0,e1,e2) => raise Fail "resolve:App1"
    | AssignE (v,e) =>
      let val (e,E,s) = resolve E e
          val E' = [(Var v,s)]
      in (AssignE(v,e),E',s)
      end
    | SeqE es => 
      let val (es,E,s) =
              foldl (fn (e,(es,E0,_)) =>
                        let val () = debug(fn () => "Resolving:\n " ^ pr_exp e ^ "\n")
                            val (e,E2,s) = resolve (E0@E) e
                        in (e::es,E2@E0,s)
                        end) (nil,emp,valuespec) es
      in (SeqE (rev es),E,s)
      end
    | ParE e => 
      let val (e,E,s) = resolve E e
      in (e,E,s)
      end
    | GuardE (e1,e2) =>
      let val (e1,E1,_) = resolve E e1                       (* memo: maybe check for valuespec *)
          val (e2,E2,s) = resolve (E1@E) e2
      in (GuardE(e1,e2),E2@E1,s)
      end
    | IndexE (e0,is) =>
      let val (is,E1) =
              foldl (fn (NONE, (is,E0)) => (NONE::is,E0)
                      | (SOME i, (is, E0)) => 
                        let val (i,E2,_) = resolve (E0@E) i  (* memo: maybe check for valuespec *)
                        in (SOME i::is,E2@E0)
                        end) (nil,emp) is
          val (e0,E0,_) = resolve (E1@E) e0                  (* memo: maybe check for valuespec *)
      in (IndexE(e0,rev is),E0@E1,valuespec)
      end
    | UnresE es =>
      let val es = resolve_vectors es
          val (gs, E') = foldl (fn (e,(gs,E')) => 
                                   let val (e,E'',s) = resolve (E'@E) e
                                   in ((e,s)::gs,E''@E')
                                   end) (nil,emp) (rev es)
          val (e,s) = res0 (rev gs)                         
      in (e,E',s)
      end
and res0 gs =
    case gs of
      [] => raise Fail "res0: empty Unres node"
    | [g] => g
    | [(e1,s1),(e2,s2)] =>
      if isFun1 s2 andalso isVal s1 then
        res0 [(App1E(e2,e1),valuespec)]
      else if isOpr1 s1 andalso isFun s2 then
        res0 [(App1E(e1,e2),appopr s1)]
      else raise Fail ("res0: could not resolve Unres node")
    | (e1,s1)::(e2,s2)::(e3,s3)::gs =>   
      if isOpr2 s3 then                                     (* ... o2 f a *)
        raise Fail "res0: dyadic operators not yet supported for e3"
      else
        if isVal s1 then
          if isFun2 s2 then
            if isVal s3 then res0 ((App2E(e2,e3,e1),valuespec)::gs)        (* ... b f2 a ==> ... f2(a,b) *)
            else raise Fail "res0: expecting value for e3"
          else if isFun1 s2 then res0 ((App1E(e2,e1),valuespec)::(e3,s3)::gs)   (* ... b f1 a ==> ... b f1(a) *)
          else if isOpr1 s2 then
            (case resFun ((e3,s3)::gs) of
               SOME ((e3,s3)::gs) => res0 ((e1,s1)::(App1E(e2,e3),appopr s2)::gs)
             | SOME nil => raise Fail "res0: impossible"
             | NONE => res0 ((e1,s1)::(App1E(e2,e3),appopr s2)::gs)) (* pass value as argument to monadic operator! *)
          else raise Fail ("res0: dyadic operator not yet supported for e2: " ^ pr_exp e2 ^ "; e1: " ^ pr_exp e1)
        else raise Fail "res0: expecting value for e1"
and resFun gs =
    case gs of
      [] => raise Fail "resFun: impossible"
    | [(e1,s1)] => if isFun s1 then SOME gs else NONE
    | (e1,s1)::(e2,s2) :: _ =>
      if isOpr2 s2 then
        raise Fail "resFun: dyadic operators not yet supported"
      else if isFun s1 then SOME gs else NONE    

val env0 =
    let open Class
        open L
    in List.map (fn (t,l) => (Symb t,l))
       [(Zilde,     valuespec),
        (Rho,       [fun1,fun2]),
        (Max,       [fun1,fun2]),
        (Min,       [fun1,fun2]),
        (Iota,      [fun1]),
        (Trans,     [fun1]),
        (Enclose,   [fun1]),
        (Disclose,  [fun1]),
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
        (Each,      [fun2]),
        (Pow,       [fun2]),
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
        (Match,     [fun2]),
        (Nmatch,    [fun2]),
        (Intersect, [fun2]),
        (Union,     [fun2]),
        (Slash,     [opr1fun1]),
        (Dot,       [opr2fun2])
       ]
    end

fun parse E ts =
    case parse0 ts of
      SOME e =>
      let val () = debug (fn () => "AST is\n " ^ pr_exp e ^ "\n")
          val (e',E',_) = resolve E e
      in SOME(e',E')
      end
    | NONE => NONE
end
