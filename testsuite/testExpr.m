MODULE testExpr;

   IMPORT testlib;
   
   PROCEDURE TestAdd(x,y,r: INTEGER);
   VAR res: INTEGER;
   BEGIN
      res := x+y;
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.FailBinaryInt("TestAdd",res,r);
      END;
   END TestAdd;

   PROCEDURE TestSub(x,y,r: INTEGER);
   VAR res: INTEGER;
   BEGIN
      res := x-y;
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.FailBinaryInt("TestSub",res,r);
      END;
   END TestSub;

   PROCEDURE TestMul(x,y,r: INTEGER);
   VAR res: INTEGER;
   BEGIN
      res := x*y;
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.FailBinaryInt("TestMul",res,r);
      END;
   END TestMul;

   PROCEDURE TestDiv(x,y,r: INTEGER);
   VAR res: INTEGER;
   BEGIN
      res := x DIV y;
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.FailBinaryInt("TestDiv",res,r);
      END;
   END TestDiv;

   PROCEDURE TestMod(x,y,r: INTEGER);
   VAR res: INTEGER;
   BEGIN
      res := x MOD y;
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.FailBinaryInt("TestMod",res,r);
      END;
   END TestMod;
   
   PROCEDURE TestNumbers(x,y, r0,r1,r2,r3,r4: INTEGER);
   BEGIN
      TestAdd(x,y,r0);
      TestSub(x,y,r1);
      TestMul(x,y,r2);
      TestDiv(x,y,r3);
      TestMod(x,y,r4);
   END TestNumbers;
   
   PROCEDURE TestAddR(x,y,r: REAL);
   VAR res: REAL;
   BEGIN
      res := x+y;
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.Fail("TestAddR");
      END;
   END TestAddR;

   PROCEDURE TestSubR(x,y,r: REAL);
   VAR res: REAL;
   BEGIN
      res := x-y;
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.Fail("TestSubR");
      END;
   END TestSubR;

   PROCEDURE TestMulR(x,y,r: REAL);
   VAR res: REAL;
   BEGIN
      res := x*y;
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.Fail("TestMulR");
      END;
   END TestMulR;

   PROCEDURE TestDivR(x,y,r: REAL);
   VAR res: REAL;
   BEGIN
      res := x / y;
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.Fail("TestDivR");
      END;
   END TestDivR;
   
   PROCEDURE TestNumbersR(x,y, r0,r1,r2,r3: REAL);
   BEGIN
      TestAddR(x,y,r0);
      TestSubR(x,y,r1);
      TestMulR(x,y,r2);
      TestDivR(x,y,r3);
   END TestNumbers;
   
   PROCEDURE TestLsl(x,y, r: INTEGER);
   VAR res: INTEGER;
   BEGIN
      res := LSL(x, y);
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.FailBinaryInt("TestLsl",res,r);
      END;
   END TestLsl;
   
   PROCEDURE TestAsr(x,y, r: INTEGER);
   VAR res: INTEGER;
   BEGIN
      res := ASR(x, y);
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.FailBinaryInt("TestAsr",res,r);
      END;
   END TestAsr;
   
   PROCEDURE TestRor(x,y, r: INTEGER);
   VAR res: INTEGER;
   BEGIN
      res := ROR(x, y);
      
      IF res = r THEN
         TestLib.Pass();
      ELSE
         TestLib.FailBinaryInt("TestRor",res,r);
      END;
   END TestRor;
   
   PROCEDURE TestShift(x,y, r0,r1,r2: INTEGER);
   BEGIN
      TestLsl(x,y,r0);
      TestAsr(x,y,r1);
      TestRor(x,y,r2);
   END TestShift;

BEGIN
   TestLib.StartSuite("testArith");
   
   TestNumbers(0,1, 1,-1,0,0,0);
   TestNumbers(10,1, 11,9,10,10,0);
   TestNumbers(10,5, 15,5,50,2,0);
   TestNumbers(10,8, 18,2,80,1,2);
   TestNumbers(10,-8, 2,18,-80,-1,2);
   TestNumbers(-100,-100, -200,0,10000,1,0);
   TestNumbers(-100,100, 0,-200,-10000,-1,0);
   
   TestLib.DoneSuite();
   
   TestLib.StartSuite("testReal");
   
   TestNumbersR(0.0,1.0,         1.0,    -1.0,  0.0,     0.0);
   TestNumbersR(10.0,1.0,        11.0,    9.0,  10.0,    10.0);
   TestNumbersR(10.0,5.0,        15.0,    5.0,  50.0,    2.0);
   TestNumbersR(10.0,8.0,        18.0,    2.0,  80.0,    10.0/8.0);
   TestNumbersR(10.0,-8.0,       2.0,     18.0, -80.0,   10.0/(-8.0));
   TestNumbersR(-10.0,-10.0,     -20.0,   0.0,  100.0,   1.0);
   TestNumbersR(-10.0,10.0,      0.0,    -20.0,-100.0,   -1.0);
   
   TestLib.DoneSuite();
   
   TestLib.StartSuite("testShift");
   
   TestShift(00000H,4, 00000H,00000H,00000H);
   TestShift(00001H,4, 00010H,00000H,01000H);
   TestShift(00010H,4, 00100H,00001H,00001H);
   TestShift(0FFF0H,4, 0FF00H,0FFFFH,00FFFH);
   TestShift(08001H,1, 00002H,0C000H,0C000H);
   
   TestLib.DoneSuite();
END testExpr.