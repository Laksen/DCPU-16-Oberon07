MODULE Math;
   
   IMPORT Out;
   
   CONST
      Pi* = 3.14159265;
      PiHalf* = Pi/2.0;
   
   PROCEDURE Round*(x: REAL): INTEGER;
   VAR res: INTEGER;
   BEGIN
      IF x < 0.0 THEN
         res := -Floor(-x+0.5);
      ELSE
         res := Floor(x+0.5);
      END;
      
      RETURN res
   END Round;
   
   PROCEDURE FMod*(x, y: REAL): REAL;
   VAR r: INTEGER;
       q: REAL;
       s: BOOLEAN;
   BEGIN
      IF (x > y) OR (x < -y) THEN
         s := x < 0.0;
         IF s THEN
            x := -x;
         END;
         
         q := x/y;
         
         r := Floor(q);
         
         IF r = 0 THEN
            q := x;
         ELSE
            q := x-Flt(r)*y;
         END;
         
         IF s THEN
            q := -q;
         END;
      ELSE
         q := x;
      END;
      
      RETURN q
   END FMod;

   PROCEDURE Sin*(x: REAL): REAL;
   VAR xs, res: REAL;
   BEGIN
      x := FMod(x, 2.0*Pi);
      IF x >= Pi THEN
         x := x-2.0*Pi;
      ELSIF x <= -Pi THEN
         x := x+2.0*Pi;
      END;
      
      IF x >= PiHalf THEN
         x := Pi-x;
      ELSIF x <= -PiHalf THEN
         x := -Pi-x;
      END;
      
      xs := x*x/2.0;
      
      res := x;
      
      x := x*xs;
      res := res - x/3.0;
      
      x := x*xs;
      res := res + x/30.0;
      
      x := x*xs/4.0;
      res := res - x/157.0;
      
      RETURN res
   END Sin;

   PROCEDURE Cos*(x: REAL): REAL;
      RETURN Sin(PiHalf-x)
   END Cos;
   
   PROCEDURE Atan66s(x: REAL): REAL;
   CONST
      c1 = 1.6867629106;
      c2 = 0.4378497304;
      c3 = 1.6867633134;
   VAR x2: REAL;
   BEGIN
      x2 := x*x;
      
      RETURN x*(c1+x2*c2)/(c3+x2)
   END Atan66s;
   
   PROCEDURE Atan*(x: REAL): REAL;
   CONST
      TanPi12 = 0.2679491924311;
      TanPi6  = 0.5773502691896;
   VAR Sgn, Cmp, Reg: BOOLEAN;
       y, tmp: REAL;
   BEGIN
      Sgn := FALSE;
      Cmp := FALSE;
      Reg := FALSE;
      
      IF x < 0.0 THEN
         x := -x;
         Sgn := TRUE;
      END;
      
      IF x > 1.0 THEN
         x := 1.0/x;
         Cmp := TRUE;
      END;
      
      IF x > TanPi12 THEN
         y := x-TanPi6;
         tmp := 1.0+TanPi6*x;
         x := y/tmp;
         
         Reg := TRUE;
      END;
      
      y := Atan66s(x);
      
      IF Reg THEN
         y := y + Pi/6.0;
      END;
      
      IF Cmp THEN
         y := Pi/2.0 - y;
      END;
      
      IF Sgn THEN
         y := -y;
      END;
      
      RETURN y
   END Atan;
   
   PROCEDURE Atan2*(y,x: REAL): REAL;
   VAR tmp: REAL;
   BEGIN
      IF x = 0.0 THEN
         IF y > 0.0 THEN
            tmp := Pi/2.0;
         ELSE
            tmp := -Pi/2.0;
         END;
      ELSE
         tmp := y/x;
         tmp := Atan(tmp);
         
         IF x < 0.0 THEN
            IF y >= 0.0 THEN
               tmp := tmp + Pi;
            ELSE
               tmp := tmp - Pi;
            END;
         END;
      END;
      
      RETURN tmp
   END Atan2;

END Math.