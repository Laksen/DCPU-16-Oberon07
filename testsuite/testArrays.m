MODULE testArrays;

   IMPORT testlib;
   
   VAR
      xg: ARRAY 4,4,4 OF INTEGER;
   
   PROCEDURE TestArray;
   VAR x: ARRAY 4,4,4 OF INTEGER;
       i,i2,i3,cnt: INTEGER;
   BEGIN
      FOR i := 0 TO 3 DO
         FOR i2 := 0 TO 3 DO
            FOR i3 := 0 TO 3 DO
               x[i,i2,i3] := i+i2+i3;
            END;
         END;
      END;
      
      cnt := 0;
      FOR i := 0 TO 3 DO
         FOR i2 := 0 TO 3 DO
            FOR i3 := 0 TO 3 DO
               INC(cnt,x[i,i2,i3]);
            END;
         END;
      END;
      
      IF cnt = 288 THEN
         testlib.pass();
      ELSE
         Testlib.FailBinaryInt("Array, local, cnt", cnt, 288);
      END;
      
      FOR i := 0 TO 3 DO
         FOR i2 := 0 TO 3 DO
            FOR i3 := 0 TO 3 DO
               xg[i,i2,i3] := i+i2+i3;
            END;
         END;
      END;
      
      cnt := 0;
      FOR i := 0 TO 3 DO
         FOR i2 := 0 TO 3 DO
            FOR i3 := 0 TO 3 DO
               INC(cnt,xg[i,i2,i3]);
            END;
         END;
      END;
      
      IF cnt = 288 THEN
         testlib.pass();
      ELSE
         Testlib.FailBinaryInt("Array, global, cnt", cnt, 288);
      END;
      
      FOR i := 0 TO 3 DO
         FOR i2 := 0 TO 3 DO
            FOR i3 := 0 TO 3 DO
               x[i,i2,i3] := x[i,i2,i3] + i+i2+i3;
            END;
         END;
      END;
      
      cnt := 0;
      FOR i := 0 TO 3 DO
         FOR i2 := 0 TO 3 DO
            FOR i3 := 0 TO 3 DO
               INC(cnt,x[i,i2,i3]);
            END;
         END;
      END;
      
      IF cnt = 2*288 THEN
         testlib.pass();
      ELSE
         Testlib.FailBinaryInt("Array, local, cnt2", cnt, 2*288);
      END;
      
      FOR i := 0 TO 3 DO
         FOR i2 := 0 TO 3 DO
            FOR i3 := 0 TO 3 DO
               xg[i,i2,i3] := xg[i,i2,i3] + i+i2+i3;
            END;
         END;
      END;
      
      cnt := 0;
      FOR i := 0 TO 3 DO
         FOR i2 := 0 TO 3 DO
            FOR i3 := 0 TO 3 DO
               INC(cnt,xg[i,i2,i3]);
            END;
         END;
      END;
      
      IF cnt = 2*288 THEN
         testlib.pass();
      ELSE
         Testlib.FailBinaryInt("Array, global, cnt2", cnt, 2*288);
      END;
   END TestArray;

BEGIN
   TestLib.StartSuite("testArrays");
   TestArray();
   TestLib.DoneSuite();
END testArrays.