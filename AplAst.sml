structure AplAst = struct
  structure L = AplLex
  type token = L.token
  type reg = Region.reg
  type var = string
             
  datatype id = Symb of token
              | Var of var
                       
  datatype exp =
           IntE of string * reg
         | DoubleE of string * reg
         | StrE of word list * reg
         | VecE of exp list * reg
         | IdE of id * reg
         | LambE of (int*int) * exp * reg         (* ints specify valence of operator and derived function *)
         | App1E of exp * exp * reg               (* apply monadic function or operator *)
         | App2E of exp * exp * exp * reg         (* apply dyadic function or operator *) 
         | AppOpr1E of int list * exp * exp * reg (* apply monadic operator; int list contains the possible valences of the resulting function *)
         | AppOpr2E of int list * exp * exp * exp * reg (* apply dyadic operator *)
         | AssignE of var * exp * reg
         | SeqE of exp list * reg
         | ParE of exp * reg
         | GuardE of exp * exp * reg
         | IndexE of exp * exp option list * reg
         | UnresE of exp list * reg
                   
  fun pr_id (Var v) = v
    | pr_id (Symb s) = L.pr_token s
                       
  fun pr_ints vs = String.concatWith "," (List.map Int.toString vs)

  fun pr_exp e =
      case e of
        IntE (s,_) => s
      | DoubleE (s,_) => s
      | StrE (ws,_) => L.pr_chars ws
      | VecE (es,_) => "Vec[" ^ pr_exps es ^ "]"
      | IdE (id,_) => pr_id id
      | LambE ((v1,v2),e,_) => "Lam[" ^ pr_ints[v1,v2] ^ "](" ^ pr_exp e ^ ")"
      | App1E (e0,e,_) => "App1(" ^ pr_exp e0 ^ "," ^ pr_exp e ^ ")"
      | App2E (e0,e1,e2,_) => "App2(" ^ pr_exp e0 ^ "," ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
      | AppOpr1E (vs,e0,e1,_) => "AppOpr1[" ^ pr_ints vs ^ "](" ^ pr_exp e0 ^ "," ^ pr_exp e1 ^ ")"
      | AppOpr2E (vs,e0,e1,e2,_) => "AppOpr2[" ^ pr_ints vs ^ "](" ^ pr_exp e0 ^ "," ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
      | AssignE (v,e,_) => "Assign(" ^ v ^ "," ^ pr_exp e ^ ")"
      | SeqE (es,_) => "[" ^ pr_exps es ^ "]"
      | ParE (e,_) => "Par(" ^ pr_exp e ^ ")"
      | GuardE (e1,e2,_) => "Guard(" ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
      | IndexE (e,is,_) => "Index(" ^ pr_exp e ^ "," ^ pr_indices is ^ ")"
      | UnresE (es,_) => "Unres[" ^ pr_exps es ^ "]"
                     
  and pr_exps nil = ""
    | pr_exps [e] = pr_exp e
    | pr_exps (e::es) = pr_exp e ^ "," ^ pr_exps es
                        
  and pr_indices nil = ""
    | pr_indices (NONE::is) = "; " ^ pr_indices is
    | pr_indices (SOME e :: is) = pr_exp e ^ pr_indices is

  fun reg_exp e =
      case e of
        IntE (_,r) => r
      | DoubleE (_,r) => r
      | StrE (_,r) => r
      | VecE (_,r) => r
      | IdE (_,r) => r
      | LambE (_,_,r) => r
      | App1E (_,_,r) => r
      | App2E (_,_,_,r) => r
      | AppOpr1E (_,_,_,r) => r
      | AppOpr2E (_,_,_,_,r) => r
      | AssignE (_,_,r) => r
      | SeqE (_,r) => r
      | ParE (_,r) => r
      | GuardE (_,_,r) => r
      | IndexE (_,_,r) => r
      | UnresE (_,r) => r
                     
  and reg_exps r nil = r
    | reg_exps r (e::es) = reg_exps (Region.plus "reg_exps" r (reg_exp e)) es

end
