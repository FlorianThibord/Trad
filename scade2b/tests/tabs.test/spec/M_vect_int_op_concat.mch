MACHINE M_vect_int_op_concat
SEES M_Enum

OPERATIONS

yy <-- vect_int_op_concat(xx, zz) =
 PRE
   zz : seq(INT) &
   xx : seq(INT)
 THEN
   yy :: { ii | ii : seq(INT) }
 END 
END
