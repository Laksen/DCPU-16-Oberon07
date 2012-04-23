MODULE Random;
   
   IMPORT Out;
   
   VAR
      x: INTEGER;
   
   PROCEDURE Next(): INTEGER;
   BEGIN
      x := LSL(x,1) + (((ROR(x, 15) MOD 2) + (ROR(x, 14) MOD 2) + 1) MOD 2)*19+5;
      
      RETURN x
   END Next;
   
   PROCEDURE LCG*(l: INTEGER): INTEGER;
      RETURN Next() MOD l
   END Random;

BEGIN
   x := 0ACE1H;
END Random.