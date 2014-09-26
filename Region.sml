structure Region :> REGION = struct
  type filename = string
  type loc = int * int * filename
  type reg = loc * loc
  fun loc0 f = (1,1,f)
  fun newline (l,_,f) = (l+1,1,f)
  fun next (l,n,f) = (l,n+1,f)

  fun lt (l1:loc) (l2:loc) =
      #1 l1 < #1 l2 orelse (#1 l1 = #1 l2 andalso #2 l1 < #2 l2)
  fun wf (r:reg) =
      #3 (#1 r) <> #3 (#2 r) orelse
      #1 r = #2 r orelse lt (#1 r) (#2 r)
  fun ppLoc0 (a,b,_) = Int.toString a ^ "." ^ Int.toString b
  fun ppLoc (l:loc) = #3 l ^ ":" ^ ppLoc0 l
  fun pp (a,b) =
      if a = b then ppLoc a
      else if #3 a = #3 b then #3 a ^ ":" ^ ppLoc0 a ^ "-" ^ ppLoc0 b
      else ppLoc a ^ "-" ^ ppLoc b
  fun plus s r1 r2 =
      if wf r1 andalso wf r2 andalso (lt (#2 r1) (#1 r2) orelse #3 (#2 r1) <> #3 (#1 r2)) then
        (#1 r1, #2 r2)
      else raise Fail ("ParseComb: Region " ^ pp r1 ^ " cannot be merged with region " ^ pp r2 ^ " at " ^ s)
end