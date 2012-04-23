MODULE testExpr;

   IMPORT testlib;
   
   PROCEDURE Power(x: INTEGER): INTEGER;
      RETURN x*x
   END Power;
   
   PROCEDURE TestForLoop;
   VAR i,i2,a,b,cnt: INTEGER;
       r1,r2,r3: REAL;
   BEGIN
      Cnt := 0;
      FOR i := 0 TO 9 DO
         Cnt := Cnt+i;
      END;
      IF Cnt = 45 THEN
         testLib.Pass();
      ELSE
         testlib.FailBinaryInt("ForLoop 1, Count", cnt, 45);
      END;
      
      Cnt := 0;
      FOR i := -10 TO 10 BY 2 DO
         Cnt := Cnt+i;
      END;
      IF Cnt = 0 THEN
         testLib.Pass();
      ELSE
         testlib.FailBinaryInt("ForLoop 1, Negstart, Count", cnt, 0);
      END;
      
      Cnt := 0;
      FOR i := 10 TO -10 BY -2 DO
         Cnt := Cnt+i;
      END;
      IF Cnt = 0 THEN
         testLib.Pass();
      ELSE
         testlib.FailBinaryInt("ForLoop 1, Neg, Count", cnt, 0);
      END;
      
      a := 0;
      b := 10;
      
      Cnt := 0;
      FOR i := a TO b DO
         Cnt := Cnt+i;
      END;
      IF Cnt = 55 THEN
         testLib.Pass();
      ELSE
         testlib.FailBinaryInt("ForLoop 1, Var, Count", cnt, 55);
      END;
      
      a := 0;
      b := -10;
      
      Cnt := 0;
      FOR i := a TO b DO
         Cnt := Cnt+i;
      END;
      IF Cnt = 0 THEN
         testLib.Pass();
      ELSE
         testlib.FailBinaryInt("ForLoop 1, Var, Neg, Count", cnt, 0);
      END;
      
      a := 0;
      b := 5;
      
      Cnt := 0;
      FOR i := a TO b DO
         Cnt := Cnt+Power(i);
         r1 := Flt(i)*10.0-Flt(i)*Flt(i)/10.4*(Flt(i+2)*Flt(i-2));
         r2 := Flt(i)-r1/10.0;
      END;
      IF Cnt = 55 THEN
         testLib.Pass();
      ELSE
         testlib.FailBinaryInt("ForLoop 1, Var, Complex", cnt, 55);
      END;
   END TestForLoop;

BEGIN
   TestLib.StartSuite("testFor");
   TestForLoop();
   TestLib.DoneSuite();
END testExpr.