MACHINE M_vect_int_op_def
SEES M_Enum

OPERATIONS

yy <-- vect_int_op_def(xx, zz) =
 PRE
   zz : INT &
   xx : INT
 THEN
   yy :: { ii | ii : seq(INT) }
 END 
END
