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
         | LambE of exp
         | App0E of exp                  (* apply niladic function *)
         | App1E of exp * exp            (* apply monadic function *)
         | App2E of exp * exp * exp      (* apply dyadic function *) 
         | AssignE of var * exp
         | SeqE of exp list
         | ParE of exp
         | GuardE of exp * exp
         | IndexE of exp * exp option list
         | UnresE of exp list
         | Opr1E of token * exp          (* e.g., each, reduce, scan *)
         | Opr2E of token * exp * exp    (* e.g., inner-, outer-product *)
                   
  fun pr_id (Var v) = v
    | pr_id (Symb s) = L.pr_token s
                       
  fun pr_exp e =
      case e of
        IntE s => s
      | DoubleE s => s
      | StrE s => "\"" ^ String.toString s ^ "\""
      | VecE es => "Vec[" ^ pr_exps es ^ "]"
      | IdE id => pr_id id
      | LambE e => "Lam(" ^ pr_exp e ^ ")"
      | App0E e0 => "App0(" ^ pr_exp e0 ^ ")"
      | App1E (e0,e) => "App1(" ^ pr_exp e0 ^ "," ^ pr_exp e ^ ")"
      | App2E (e0,e1,e2) => "App2(" ^ pr_exp e0 ^ "," ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
      | AssignE (v,e) => "Assign(" ^ v ^ "," ^ pr_exp e ^ ")"
      | SeqE es => "[" ^ pr_exps es ^ "]"
      | ParE e => "Par(" ^ pr_exp e ^ ")"
      | GuardE (e1,e2) => "Guard(" ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
      | IndexE (e,is) => "Index(" ^ pr_exp e ^ "," ^ pr_indices is ^ ")"
      | UnresE es => "Unres[" ^ pr_exps es ^ "]"
      | Opr1E (t,e1) => "Opr1(" ^ L.pr_token t ^ "," ^ pr_exp e1 ^ ")"
      | Opr2E (t,e1,e2) => "Opr2(" ^ L.pr_token t ^ "," ^ pr_exp e1 ^ "," ^ pr_exp e2 ^ ")"
                     
  and pr_exps nil = ""
    | pr_exps [e] = pr_exp e
    | pr_exps (e::es) = pr_exp e ^ "," ^ pr_exps es
                        
  and pr_indices nil = ""
    | pr_indices (NONE::is) = "; " ^ pr_indices is
    | pr_indices (SOME e :: is) = pr_exp e ^ pr_indices is
end
