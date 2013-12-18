MACHINE M_Id_int

OPERATIONS

yy <-- Id_int(xx) =
 PRE
   xx : INT
 THEN
   yy :: { ii | ii : INT }
 END 
END
