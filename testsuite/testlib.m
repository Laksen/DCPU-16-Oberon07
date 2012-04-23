MODULE testlib;

   IMPORT SYSTEM;
   
   CONST
      VideoBase = 08000H;
      VideoWidth = 32;
      VideoHeight = 12;
   
   VAR
      PassCount,
      FailCount: INTEGER;
      
      x,y: INTEGER;
   
   PROCEDURE WriteChar*(c: CHAR; Color: INTEGER);
   VAR h: INTEGER;
   BEGIN
      h := VideoBase+X+Y*VideoWidth;
      
      SYSTEM.PUT(h, ord(c) + Color);
      INC(X);
   END WriteChar;
   
   PROCEDURE WriteStr(s: ARRAY OF CHAR; Color: INTEGER);
   VAR i: INTEGER;
   BEGIN
      i := 0;
      WHILE i < LEN(s) DO
         WriteChar(s[i], Color);
         
         INC(i);
      END;
   END WriteStr;
   
   PROCEDURE WriteInt(i, Color: INTEGER);
   VAR Buffer: ARRAY 5 OF CHAR;
       BufPos: INTEGER;
   BEGIN
      IF i < 0 THEN
         i := -i;
         WriteChar("-");
      END;
      
      BufPos := LEN(Buffer)-1;
      
      IF i = 0 THEN
         WriteChar("0", Color);
      ELSE
         WHILE i > 0 DO
            Buffer[BufPos] := CHR(ORD("0") + i MOD 10);
            Dec(BufPos);
            i := i DIV 10;
         END;
         
         WHILE BufPos < LEN(Buffer)-1 DO
            INC(BufPos);
            WriteChar(Buffer[BufPos], Color);
         END;
      END;
   END WriteInt;
   
   PROCEDURE Pass*;
   BEGIN
      INC(PassCount);
   END Pass;
   
   PROCEDURE Fail*(Msg: ARRAY OF CHAR);
   BEGIN
      INC(FailCount);
      
      WriteStr("Fail: ", 0C000H);
      
      WriteStr(Msg, 0C000H);
      
      INC(y);
      x := 0;
   END Fail;
   
   PROCEDURE FailBinaryInt*(Msg: ARRAY OF CHAR; Result, Expected: INTEGER);
   BEGIN
      INC(FailCount);
      
      WriteStr("Fail: ", 0C000H);
      
      WriteStr(Msg, 0C000H);
      
      WriteChar("(", 0C000H);
      WriteInt(Result, 0C000H);
      WriteChar("#", 0C000H);
      WriteInt(Expected, 0C000H);
      WriteChar(")", 0C000H);
      
      INC(y);
      x := 0;
   END Fail;
   
   PROCEDURE StartSuite*(Msg: ARRAY OF CHAR);
   BEGIN
      PassCount := 0;
      FailCount := 0;
      
      WriteStr("Starting: ", 0F000H);
      WriteStr(Msg, 0F000H);
      INC(y);
      x := 0;
   END Done;
   
   PROCEDURE DoneSuite*;
   BEGIN
      WriteStr("Done. Passed: ", 0F000H);
      WriteInt(PassCount, 0A000H);
      WriteStr(" Failed: ", 0C000H);
      WriteInt(FailCount, 0C000H);
      INC(y);
      x := 0;
   END Done;

BEGIN
   x := 0;
   y := 0;
END testlib.