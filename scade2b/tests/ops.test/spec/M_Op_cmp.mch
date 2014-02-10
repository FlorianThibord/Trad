MACHINE M_Op_cmp
SEES M_Enum

OPERATIONS

rr <-- Op_cmp(aa, bb, cc, dd, ee, ff) =
 PRE
   ff : INT &
   ee : INT &
   dd : INT &
   cc : INT &
   bb : INT &
   aa : INT
 THEN
   rr :: { ii | ii : BOOL }
 END 
END
