MACHINE M_Op_xor
SEES M_Enum

OPERATIONS

rr <-- Op_xor(xx, yy) =
 PRE
   yy : INT &
   xx : INT
 THEN
   rr :: { ii | ii : INT }
 END 
END
