MACHINE M_vect_real_op_concat
SEES M_Enum

OPERATIONS

yy <-- vect_real_op_concat(xx, zz) =
 PRE
   zz : seq(REAL) &
   xx : seq(REAL)
 THEN
   yy :: { ii | ii : seq(REAL) }
 END 
END
