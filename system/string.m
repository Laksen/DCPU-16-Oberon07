MODULE String;
   
   PROCEDURE Length*(S: ARRAY OF CHAR): INTEGER;
   VAR r: INTEGER;
   BEGIN
      r := 0;
      WHILE (r < LEN(S)) & (S[r] # 00X) DO
         INC(r);
      END;
      
      RETURN r
   END Length;
   
   PROCEDURE Concat*(VAR S: ARRAY OF CHAR; C: ARRAY OF CHAR);
   VAR sLen, cLen, cI: INTEGER;
   BEGIN
      sLen := Length(S);
      cLen := Length(C);
      cI := 0;
      
      WHILE (sLen < (LEN(S)-1)) & (cI < cLen) DO
         S[sLen] := C[cI];
         INC(sLen);
         INC(cI);
      END;
      
      S[sLen] := 00X;
   END Concat;
   
   PROCEDURE ConcatChar*(VAR S: ARRAY OF CHAR; C: CHAR);
   VAR sLen: INTEGER;
   BEGIN
      sLen := Length(S);
      
      S[sLen] := c;
      S[sLen+1] := 00X;
   END ConcatChar;
   
   PROCEDURE ConcatInt*(VAR S: ARRAY OF CHAR; i: INTEGER);
   VAR Buffer: ARRAY 5 OF CHAR;
       BufPos: INTEGER;
   BEGIN
      IF i < 0 THEN
         i := -i;
         ConcatChar(S, "-");
      END;
      
      BufPos := LEN(Buffer)-1;
      
      IF i = 0 THEN
         Concat(S, "0");
      ELSE
         WHILE i > 0 DO
            Buffer[BufPos] := CHR(ORD("0") + i MOD 10);
            Dec(BufPos);
            i := i DIV 10;
         END;
         
         WHILE BufPos < LEN(Buffer)-1 DO
            INC(BufPos);
            ConcatChar(S, Buffer[BufPos]);
         END;
      END;
   END ConcatInt;
   
   PROCEDURE Assign*(VAR S: ARRAY OF CHAR; C: ARRAY OF CHAR);
   BEGIN
      S[0] := 00X;
      COncat(S, C);
   END Assign;

END String.