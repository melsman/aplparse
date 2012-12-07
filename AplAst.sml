structure AplAst = struct
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
         | IdE of id
         | LambE of (int*int) * exp               (* ints specify valence of operator and derived function *)
         | App1E of exp * exp                     (* apply monadic function or operator *)
         | App2E of exp * exp * exp               (* apply dyadic function or operator *) 
         | AppOpr1E of int list * exp * exp       (* apply monadic operator; int list contains the possible valences of the resulting function *)
         | AppOpr2E of int list * exp * exp * exp (* apply dyadic operator *)
         | AssignE of var * exp
         | SeqE of exp list
         | ParE of exp
         | GuardE of exp * exp
         | IndexE of exp * exp option list
         | UnresE of exp list
                   
  fun pr_id (Var v) = v
    | pr_id (Symb s) = L.pr_token s
                       
  fun pr_ints vs = String.concatWith "," (List.map Int.toString vs)

  fun pr_exp e =
      case e of
        IntE s => s
      | DoubleE s => s
      | StrE s => "\"" ^ String.toString s ^ "\""
      | VecE es => "Vec[" ^ pr_exps es ^ "]"
      | IdE id => pr_id id
      | LambE ((v1,v2),e) => "Lam[" ^ pr_ints[v1,v2] ^ "](" ^ pr_exp e ^ ")"
      | App1E (e0,e) => "App1(" ^ pr_exp e0 ^ "," ^ pr_exp e ^ ")"
      | App2E (e0,e1,e2) => "App2(" ^ pr_exp e0 ^ "," ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
      | AppOpr1E (vs,e0,e1) => "AppOpr1[" ^ pr_ints vs ^ "](" ^ pr_exp e0 ^ "," ^ pr_exp e1 ^ ")"
      | AppOpr2E (vs,e0,e1,e2) => "AppOpr2[" ^ pr_ints vs ^ "](" ^ pr_exp e0 ^ "," ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
      | AssignE (v,e) => "Assign(" ^ v ^ "," ^ pr_exp e ^ ")"
      | SeqE es => "[" ^ pr_exps es ^ "]"
      | ParE e => "Par(" ^ pr_exp e ^ ")"
      | GuardE (e1,e2) => "Guard(" ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
      | IndexE (e,is) => "Index(" ^ pr_exp e ^ "," ^ pr_indices is ^ ")"
      | UnresE es => "Unres[" ^ pr_exps es ^ "]"
                     
  and pr_exps nil = ""
    | pr_exps [e] = pr_exp e
    | pr_exps (e::es) = pr_exp e ^ "," ^ pr_exps es
                        
  and pr_indices nil = ""
    | pr_indices (NONE::is) = "; " ^ pr_indices is
    | pr_indices (SOME e :: is) = pr_exp e ^ pr_indices is
end
