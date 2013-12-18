MACHINE M_Op_minus
SEES M_Enum

OPERATIONS

rr <-- Op_minus(xx, yy) =
 PRE
   yy : INT &
   xx : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
