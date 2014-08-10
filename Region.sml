structure Region :> REGION = struct
  type loc = int * int
  type reg = loc * loc
  val loc0 = (1,1)
  fun newline (l,_) = (l+1,1)
  fun next (l,n) = (l,n+1)

  fun lt (l1:loc) (l2:loc) =
      #1 l1 < #1 l2 orelse (#1 l1 = #1 l2 andalso #2 l1 < #2 l2)
  fun wf (r:reg) =
      #1 r = #2 r orelse lt (#1 r) (#2 r)
                            
  fun ppLoc (a,b) = Int.toString a ^ "." ^ Int.toString b
  fun pp (a,b) = "[" ^ ppLoc a ^ "-" ^ ppLoc b ^ "]"
  fun plus s r1 r2 =
      if wf r1 andalso wf r2 andalso lt (#2 r1) (#1 r2) then (#1 r1, #2 r2)
      else raise Fail ("ParseComb: Region " ^ pp r1 ^ " cannot be merged with region " ^ pp r2 ^ " at " ^ s)
end
