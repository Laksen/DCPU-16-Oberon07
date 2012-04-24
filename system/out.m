MODULE out;

   IMPORT SYSTEM;
   
   CONST
      VideoBase* = 08000H;
      VideoWidth* = 32;
      VideoHeight* = 12;

   VAR Color,
       X*,Y*: INTEGER;

   PROCEDURE CheckScroll;
   VAR i,i2,c: INTEGER;
   BEGIN
      IF x >= VideoWidth THEN
         x := 0;
         INC(y);
      END;
      
      IF y >= VideoHeight THEN
         FOR i := 0 TO 10 DO
            FOR i2 := 0 TO 31 DO
               SYSTEM.Get(VideoBase+(i+1)*32+i2, c);
               SYSTEM.Put(VideoBase+i*32+i2, c);
            END;
         END;
         
         FOR i2 := 0 TO 31 DO
            SYSTEM.Put(VideoBase+11*32+i2, 0);
         END;
         
         DEC(y);
      END;
   END CheckScroll;

   PROCEDURE WriteChar*(c: CHAR);
   BEGIN
      SYSTEM.PUT(VideoBase+X+Y*VideoWidth, ord(c) + Color);
      INC(X);
      
      CheckScroll();
   END WriteChar;

   PROCEDURE Inside(x,y: INTEGER): BOOLEAN;
   VAR b: BOOLEAN;
   BEGIN
      b := TRUE;
      
      IF x < 0 THEN b := FALSE; END;
      IF x >= VideoWidth THEN b := FALSE; END;
      
      IF y < 0 THEN b := FALSE; END;
      IF y >= VideoHeight THEN b := FALSE; END;
      
      RETURN b
   END Inside;
   
   PROCEDURE PutChar*(x,y: INTEGER; c: CHAR);
   BEGIN
      IF Inside(x,y) THEN
         SYSTEM.PUT(VideoBase+X+Y*VideoWidth, ord(c) + Color);
      END;
   END PutChar;
   
   PROCEDURE WriteHex*(v, digits: INTEGER);
   VAR i, d: INTEGER;
   BEGIN
      FOR i := Digits-1 TO 0 BY -1 DO
         d := ASR(v, i*4) MOD 16;
         
         IF d > 9 THEN
            WriteChar(CHR(ORD("A")+d-10));
         ELSE
            WriteChar(CHR(ORD("0")+d));
         END;
      END;
   END WriteHex;
   
   PROCEDURE WriteInt*(i: INTEGER);
   VAR Buffer: ARRAY 5 OF CHAR;
       BufPos: INTEGER;
   BEGIN
      IF i < 0 THEN
         i := -i;
         WriteChar("-");
      END;
      
      BufPos := LEN(Buffer)-1;
      
      IF i = 0 THEN
         WriteChar("0");
      ELSE
         WHILE i > 0 DO
            Buffer[BufPos] := CHR(ORD("0") + i MOD 10);
            Dec(BufPos);
            i := i DIV 10;
         END;
         
         WHILE BufPos < LEN(Buffer)-1 DO
            INC(BufPos);
            WriteChar(Buffer[BufPos]);
         END;
      END;
   END WriteInt;
   
   PROCEDURE WriteReal*(r: REAL);
   VAR q: INTEGER;
   BEGIN
      IF r < 0.0 THEN
         WriteChar("-");
         r := -r;
      END;
      
      q := Floor(r);
      
      WriteInt(q);
      WriteChar(".");
      
      r := r-Flt(q);
      
      IF r = 0.0 THEN
         WriteChar("0");
      ELSE
         WHILE r # 0.0 DO
            r := r*10.0;
            q := Floor(r);
            r := r-Flt(q);
            WriteChar(CHR(ORD("0")+q));
         END;
      END;
   END WriteReal;

   PROCEDURE WriteStr*(c: ARRAY OF CHAR);
   VAR i: INTEGER;
   BEGIN
      i := 0;
      WHILE c[i] # 00X DO
         WriteChar(c[i]);
         INC(i);
      END;
   END WriteStr;
   
   PROCEDURE WriteBool*(b: BOOLEAN);
   BEGIN
      IF b THEN
         WriteStr("TRUE");
      ELSE
         WriteStr("FALSE");
      END;
   END WriteBool;
   
   PROCEDURE WriteLn*;
   BEGIN
      INC(Y);
      X := 0;
      
      CheckScroll();
   END WriteLn;
   
   PROCEDURE SetColor*(FG, BG: INTEGER);
   BEGIN
      Color := LSL(FG, 12) + LSL(BG, 8);
   END SetColor;
   
   PROCEDURE SetPosition*(AX,AY: INTEGER);
   BEGIN
      IF AX >= VideoWidth THEN
         X := VideoWidth-1;
      ELSIF AX < 0 THEN
         X := 0;
      ELSE
         X := AX;
      END;
      
      IF AY >= VideoHeight THEN
         Y := VideoHeight-1;
      ELSIF AY < 0 THEN
         Y := 0;
      ELSE
         Y := AY;
      END;
   END SetPosition;
   
   PROCEDURE ClearScreen*;
   VAR i: INTEGER;
   BEGIN
      FOR i := 0 TO VideoWidth*VideoHeight-1 DO
         SYSTEM.Put(VideoBase+i, 0);
      END;
   END ClearScreen;

BEGIN
   X := 0;
   Y := 0;
   SetColor(7, 0);
END out.