MACHINE M_Op_plus
SEES M_Enum

OPERATIONS

rr <-- Op_plus(xx, yy) =
 PRE
   yy : INT &
   xx : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
