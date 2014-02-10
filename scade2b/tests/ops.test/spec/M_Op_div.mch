MACHINE M_Op_div
SEES M_Enum

OPERATIONS

rr <-- Op_div(aa, bb) =
 PRE
   bb : INT &
   aa : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
