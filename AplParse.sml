structure AplParse = struct

val p_debug = true
fun debug f =
    if p_debug then print(f())
    else ()

structure L = AplLex
type token = L.token
type var = string

datatype id = Symb of token
            | Var of var

datatype exp =
         IntE of string
       | DoubleE of string
       | StrE of string
       | VecE of exp list
       | VarE of var
       | LambE of exp
       | App0E of id                  (* apply niladic function *)
       | App1E of id * exp            (* apply monadic function *)
       | App2E of id * exp * exp      (* apply dyadic function *) 
       | AssignE of var * exp
       | SeqE of exp list
       | GuardE of exp * exp
       | IndexE of exp * exp option list
       | UnresE of exp list
       | SymbE of token
       | ParE of exp

fun pr_id (Var v) = v
  | pr_id (Symb s) = L.pr_token s

fun pr_exp e =
    case e of
      IntE s => s
    | DoubleE s => s
    | StrE s => "\"" ^ String.toString s ^ "\""
    | VecE es => "Vec[" ^ pr_exps es ^ "]"
    | VarE s => s
    | LambE e => "Lam(" ^ pr_exp e ^ ")"
    | App0E id => "App0(" ^ pr_id id ^ ")"
    | App1E (id,e) => "App1(" ^ pr_id id ^ "," ^ pr_exp e ^ ")"
    | App2E (id,e1,e2) => "App2(" ^ pr_id id ^ "," ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
    | AssignE (v,e) => "Assign(" ^ v ^ "," ^ pr_exp e ^ ")"
    | SeqE es => "[" ^ pr_exps es ^ "]"
    | ParE e => "Par(" ^ pr_exp e ^ ")"
    | GuardE (e1,e2) => "Guard(" ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
    | IndexE (e,is) => "Index(" ^ pr_exp e ^ "," ^ pr_indices is ^ ")"
    | SymbE t => L.pr_token t
    | UnresE es => "Unres[" ^ pr_exps es ^ "]"

and pr_exps nil = ""
  | pr_exps [e] = pr_exp e
  | pr_exps (e::es) = pr_exp e ^ "," ^ pr_exps es

and pr_indices nil = ""
  | pr_indices (NONE::is) = "; " ^ pr_indices is
  | pr_indices (SOME e :: is) = pr_exp e ^ pr_indices is

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
    || (p_symb oo SymbE)
    || (p_id oo VarE)
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

type env = (id * int) list
val emp = []

fun isFunKind i E id =
    List.exists (fn p => p = (id,i)) E

fun idOfExp (SymbE t) = SOME(Symb t)
  | idOfExp (VarE v) = SOME(Var v)
  | idOfExp _ = NONE

fun resolve E e =
    case e of
      IntE s => (e,emp)
    | DoubleE s => (e,emp)
    | VecE _ => (e,emp)
    | StrE s => (e,emp)
    | VarE s => (e,emp)
    | LambE e => (LambE (#1(resolve E e)), emp)
    | App0E id => raise Fail "resolve:App0"
    | App1E (id,e) => raise Fail "resolve:App1"
    | App2E (id,e1,e2) => raise Fail "resolve:App1"
    | AssignE (v,e) =>
      let val (e,E) = resolve E e
      in (AssignE(v,e),[(Var v,1)])  (* MEMO: support other kinds of variables *)
      end
    | SeqE es => 
      let val (es,E) =
              foldl (fn (e,(es,E0)) =>
                        let val () = debug(fn () => "Resolving:\n " ^ pr_exp e ^ "\n")
                            val (e,E2) = resolve (E0@E) e
                        in (e::es,E2@E0)
                        end) (nil,emp) es
      in (SeqE (rev es),E)
      end
    | ParE e => 
      let val (e,E) = resolve E e
      in (e, E)
      end
    | GuardE (e1,e2) =>
      let val (e1,E1) = resolve E e1
          val (e2,E2) = resolve (E1@E) e2
      in (GuardE(e1,e2),E2@E1)
      end
    | IndexE (e0,is) =>
      let val (is,E1) =
              foldl (fn (NONE, (is,E0)) => (NONE::is,E0)
                      | (SOME i, (is, E0)) => 
                        let val (i,E2) = resolve (E0@E) i
                        in (SOME i::is,E2@E0)
                        end) (nil,emp) is
          val (e0,E0) = resolve (E1@E) e0
      in (IndexE(e0,rev is),E0@E1)
      end
    | SymbE t => (e,emp)
    | UnresE es =>
      let val es = resolve_vectors es
      in res E es
      end
and resolveFun i E e =
    case idOfExp e of
      SOME id => if isFunKind i E id then SOME id
                 else NONE
    | NONE => NONE
and resolveDyadic E e = resolveFun 2 E e
and resolveMonadic E e = resolveFun 1 E e
and resolveNiladic E e = resolveFun 0 E e
and isArg E e =
    (case (resolveDyadic E e, resolveMonadic E e, resolveNiladic E e) of
       (NONE, NONE, NONE) => true
     | _ =>  false)
and res0 E (e,(es,E')) =
    case es of
      [] => (case resolveNiladic (E'@E) e of
               SOME id => ([App0E id],emp)
             | NONE =>
               let val (e,E'') = resolve (E'@E) e
               in ([e],E''@E')
               end)
    | [e1,e2] =>
      let fun monadic s =
              case resolveMonadic (E'@E) e1 of  (* check if e1 is also monadic *)
                SOME id => res0 E (e,([App1E(id,e2)],E'))
              | NONE => raise Fail ("expecting either dyadic or monadic operator: " ^ s)
      in case resolveDyadic (E'@E) e1 of
           SOME id =>
           if isArg (E'@E) e then
             let val (e,E'') = resolve (E'@E) e
             in ([App2E(id,e,e2)],E''@E')
             end
           else monadic "1"
         | NONE => monadic "2"
      end
    | [e1] =>
      (case resolveDyadic (E'@E) e of
         SOME _ => ([e,e1],E')
       | NONE =>
         case resolveMonadic (E'@E) e of
           SOME id => ([App1E(id,e1)],E')
         | NONE => raise Fail ("expecting either dyadic or monadic operator: - got: " ^ pr_exp e ^ "; e1 is " ^ pr_exp e1))
    | _ => raise Fail "res0: impossible"
    
and res E es =
    let val es = rev es
        val (es, E') = foldl (res0 E) (nil,emp) es
    in case es of
         [e] => (e,E')
       | _ => raise Fail "resolvation failed"
    end

val monadic =
    let open L 
    in [Rho, Iota, Max, Min, Enclose, Disclose, Gradeup, Gradedown, Add, Sub, Cat, 
        Trans, Rot, Vrot]
    end

val dyadic =
    let open L 
    in [Rho, Max, Min, Each, Add, Sub, Times, Div, Pow, Cat, Vcat, Rot, Vrot, Lt, Gt, 
        Lteq, Gteq, Eq, Neq, Take, Drop, Or, And, Match, Nmatch, Intersect, Union]
    end

val niladic =
    let open L 
    in [Qmark]
    end

val env0 =
    map (fn t => (Symb t,1)) monadic @
    map (fn t => (Symb t,2)) dyadic @
    map (fn t => (Symb t,0)) niladic

fun parse ts =
    case parse0 ts of
      SOME e =>
      let val () = debug (fn () => "AST is\n " ^ pr_exp e ^ "\n")
      in SOME(#1 (resolve env0 e))
      end
    | NONE => NONE
end
