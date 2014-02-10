MACHINE M_Op_unops
SEES M_Enum

OPERATIONS

rr <-- Op_unops(aa) =
 PRE
   aa : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
