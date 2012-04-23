MODULE Dither;

   IMPORT System, math;
   
   CONST
      TriWidth = 100;
      TriHeight = 70;
   
   TYPE
      TPoint = RECORD
         x,y: INTEGER
      END;
   
   VAR
      Pixels: ARRAY 32*12, 2 OF SET;
      
      Entropy: ARRAY 32*12 OF INTEGER;
      LinesUsed: SET;
      
      CharCount: INTEGER;
      Charset: ARRAY 128, 2 OF SET;
      Chars: ARRAY 32*12 OF INTEGER;
      
      Cnt: INTEGER;
      r: REAL;
      
      Cube: ARRAY 8, 32 OF TPoint;
   
   PROCEDURE Swap(VAR x,y: INTEGER);
   VAR t: INTEGER;
   BEGIN
      t := x;
      x := y;
      y := t;
   END Swap;
   
   PROCEDURE Min3(a,b,c: INTEGER): INTEGER;
   VAR r: INTEGER;
   BEGIN
      r := A;
      IF b < r THEN r := b; END;
      IF c < r THEN r := c; END;
      
      RETURN r
   END Min3;
   
   PROCEDURE Max3(a,b,c: INTEGER): INTEGER;
   VAR r: INTEGER;
   BEGIN
      r := A;
      IF b > r THEN r := b; END;
      IF c > r THEN r := c; END;
      
      RETURN r
   END Max3;
   
   PROCEDURE Clear;
   VAR i,i2: INTEGER;
   BEGIN
      i := 0;
      WHILE i < 32*12 DO
         Pixels[i,0] := {};
         Pixels[i,1] := {};
         INC(i);
      END;
   END ClearPixels;
   
   PROCEDURE SetPixel(x,y: INTEGER; On: BOOLEAN);
   VAR ix,iy: INTEGER;
   BEGIN
      iy := y MOD 8;
      
      IF ~ODD(x) THEN
         INC(iy,8);
      END;
      
      x := ROR(x,1);
      ix := x MOD 2;
      x := ROR(x,1) MOD 32;
      
      y := ROR(y,3) MOD 16;
      y := y*32;
      
      IF On THEN
         INCL(Pixels[x+y, ix], iy);
      ELSE
         EXCL(Pixels[x+y, ix], iy);
      END;
   END SetPixel;
   
   PROCEDURE Scanline(x0,x1,y: INTEGER; HalfColor: BOOLEAN);
   VAR ix,iy,x,oy: INTEGER;
   BEGIN
      iy := y MOD 8;
      oy := y;
      
      IF x0 > x1 THEN
         Swap(x0,x1);
      END;
      
      y := ROR(y,3) MOD 16;
      y := y*32;
      
      IF ~ODD(x0) THEN
         iy := (iy+8) MOD 16;
      END;
      
      WHILE x0 <= x1 DO
         x := ROR(x0,1);
         ix := x MOD 2;
         x := ROR(x,1) MOD 32;
         
         IF HalfColor THEN
            IF ODD(x0+oy) THEN
               INCL(Pixels[x+y, ix], iy);
            ELSE
               EXCL(Pixels[x+y, ix], iy);
            END;
         ELSE
            INCL(Pixels[x+y, ix], iy);
         END;
         
         iy := (iy+8) MOD 16;
         INC(x0);
      END;
   END Scanline;
   
   PROCEDURE DrawLine(x0,y0,x1,y1: INTEGER);
   VAR dx,dy,
       sx,sy,
       err,e2: INTEGER;
       DoRun: BOOLEAN;
   BEGIN
      IF x0 > x1 THEN
         Swap(x0,x1);
         Swap(y0,y1);
      END;
      
      dx := ABS(x1-x0);
      dy := ABS(y1-y0);
      
      IF x0 < x1 THEN
         sx := 1;
      ELSE
         sx := -1;
      END;
      
      IF y0 < y1 THEN
         sy := 1;
      ELSE
         sy := -1;
      END;
      
      err := dx-dy;
      
      DoRun := TRUE;
      
      WHILE DoRun DO
         SetPixel(x0,y0, TRUE);
         
         IF (x0 = x1) & (y0 = y1) THEN
            DoRun := FALSE;
         ELSE
            e2 := 2*err;
            
            IF e2 > -dy THEN
               err := err - dy;
               x0 := x0 + sx;
            END;
            
            IF e2 <  dx THEN
               err := err + dx;
               y0 := y0 + sy;
            END;
         END;
      END;
   END DrawLine;
   
   PROCEDURE DrawTri(x0,y0,x1,y1,x2,y2: INTEGER);
   BEGIN
      DrawLine(x0,y0,x1,y1);
      DrawLine(x2,y2,x1,y1);
      DrawLine(x2,y2,x0,y0);
   END DrawTri;
   
   PROCEDURE FillTriHS(x1,y1, x2,y2, x3,y3: INTEGER; HalfColor: BOOLEAN);
   VAR miny,maxy,
       minx,maxx,
       dx1,dx2,dx3,
       cx1,cx2,cx3,
       dy1,dy2,dy3,
       cy1,cy2,cy3,
       cy01,cy02,cy03,
       x,y: INTEGER;
   BEGIN
      miny := Min3(y1,y2,y3);
      maxy := Max3(y1,y2,y3);
      
      minx := Min3(x1,x2,x3);
      maxx := Max3(x1,x2,x3);
      
      dx1 := x1-x2;
      dx2 := x2-x3;
      dx3 := x3-x1;
      
      dy1 := y1-y2;
      dy2 := y2-y3;
      dy3 := y3-y1;
      
      y := miny;
      
      cx1 := dx1 * (y - y1);
      cx2 := dx2 * (y - y2);
      cx3 := dx3 * (y - y3);
      
      cy01 := cx1 - dy1 * (minx - x1);
      cy02 := cx2 - dy2 * (minx - x2);
      cy03 := cx3 - dy3 * (minx - x3);
      
      WHILE y < maxy DO
         x := minx;
         
         cy1 := cy01;
         cy2 := cy02;
         cy3 := cy03;
         
         WHILE x < maxx DO
            IF (cy1 > 0) THEN
               IF (cy2 > 0) THEN
                  IF (cy3 > 0) THEN
                     IF HalfColor THEN
                        SetPixel(x,y,ODD(x+y));
                     ELSE
                        SetPixel(x,y,TRUE);
                     END;
                  END;
               END;
            END;
            
            DEC(cy1, dy1);
            DEC(cy2, dy2);
            DEC(cy3, dy3);
            
            INC(x);
         END;
         
         INC(cy01, dx1);
         INC(cy02, dx2);
         INC(cy03, dx3);
         
         INC(y);
      END;
   END FillTriHS;
   
   PROCEDURE Reorder(VAR x1,y1, x2,y2, x3,y3: INTEGER);
   VAR miny,maxy: INTEGER;
   BEGIN
      miny := Min3(y1,y2,y3);
      maxy := Max3(y1,y2,y3);
      
      IF y2 = miny THEN
         Swap(y1,y2);
         Swap(x1,x2);
      ELSIF y3 = miny THEN
         Swap(y1,y3);
         Swap(x1,x3);
      END;
      
      IF y2 = maxy THEN
         Swap(y2,y3);
         Swap(x2,x3);
      END;
   END Reorder;
   
   PROCEDURE FillTriScan(x1,y1, x2,y2, x3,y3: INTEGER; HalfColor: BOOLEAN);
   VAR dx1,dx2,
       cx1,cx2: REAL;
       y: INTEGER;
   BEGIN
      Reorder(x1,y1, x2,y2, x3,y3);
      
      cx1 := Flt(x1);
      cx2 := Flt(x1);
      
      dx1 := Flt(x2-x1)/Flt(y2-y1);
      dx2 := Flt(x3-x1)/Flt(y3-y1);
      
      y := y1;
      
      WHILE y < y2 DO
         Scanline(Floor(cx1),Floor(cx2),y, HalfColor);
         
         cx1 := cx1+dx1;
         cx2 := cx2+dx2;
         
         INC(y);
      END;
      
      dx1 := Flt(x3-x2)/Flt(y3-y2);
      
      cx1 := Flt(x2);
      WHILE y < y3 DO
         Scanline(Floor(cx1),Floor(cx2),y, HalfColor);
         
         cx1 := cx1+dx1;
         cx2 := cx2+dx2;
         
         INC(y);
      END;
   END FillTriScan;
   
   PROCEDURE FillRect(x0,y0,w,h: INTEGER; Color: BOOLEAN);
   VAR t: INTEGER;
   BEGIN
      WHILE h > 0 DO
         t := x0+w;
         WHILE t > x0 DO
            DEC(t);
            SetPixel(t,y0, Color);
         END;
         
         DEC(h);
         INC(y0);
      END;
   END DrawRect;
   
   PROCEDURE CalcEntropy(a,b: SET; VAR Error: INTEGER): INTEGER;
   VAR r: INTEGER;
   BEGIN
      r := ABS(a)+ABS(b);
      
      Error := r;
      
      r := 16-ABS(r-16);
      
      RETURN r
   END CalcEntropy;
   
   PROCEDURE CalcBlockEntropy(): INTEGER;
   VAR i,i2,base, h, cnt, tmp: INTEGER;
   BEGIN
      LinesUsed := {};
      
      Cnt := 0;
      
      i := 0;
      base := 0;
      
      WHILE i < 12 DO
         i2 := 0;
         WHILE i2 < 32 DO
            Chars[base] := 0;
            
            h := CalcEntropy((Pixels[base,0]), (Pixels[base,1]), tmp);
            
            Entropy[base] := h;
            
            IF h > 0 THEN
               INCL(LinesUsed, i);
            END;
            
            IF h > 0 THEN
               INC(Cnt);
            END;
            
            INC(i2);
            INC(base);
         END;
         
         INC(i);
      END;
      
      RETURN cnt
   END CalcBlockEntropy;
   
   PROCEDURE SetBorder(f: INTEGER);
   BEGIN
      SYSTEM.PUT(08280H, f);
   END SetBorder;
   
   PROCEDURE UpdateRound(): INTEGER;
   VAR i,i2,base,
       bestx,besty,besth,
       H,He,
       Cnt,
       Berr: INTEGER;
       C0, C1: SET;
   BEGIN
      Cnt := 0;
      
      besth := 0;
      bestx := 0;
      besty := 0;
      
      i2 := 0;
      base := 0;
      WHILE i2 < 12 DO
         IF i2 IN LinesUsed THEN
            i := 0;
            WHILE i < 32 DO
               H := Entropy[base];
               
               IF ((besth > H) OR (besth = 0)) & (H > 0) THEN
                  bestx := i;
                  besty := i2;
                  besth := H;
               END;
               
               INC(base);
               INC(i);
            END;
         ELSE
            INC(base,32);
         END;
         
         INC(i2);
      END;
      
      C0 := Pixels[bestx+32*besty,0];
      C1 := Pixels[bestx+32*besty,1];
      
      Charset[CharCount,0] := C0;
      Charset[CharCount,1] := C1;
      
      i2 := 0;
      base := 0;
      WHILE i2 < 12 DO
         IF i2 IN LinesUsed THEN
            i := 0;
            WHILE i < 32 DO
               IF Entropy[base] > 0 THEN
                  He := CalcEntropy((Pixels[base,0]/C0), (Pixels[base,1]/C1), Berr);
                  
                  IF He <= 1 THEN
                     Entropy[base] := 0;
                     
                     IF Berr >= 16 THEN
                        Chars[base] := CharCount + 00F00H;
                     ELSE
                        Chars[base] := CharCount + 0F000H;
                     END;
                  ELSE
                     INC(Cnt);
                  END;
               END;
               
               INC(base);
               INC(i);
            END;
         ELSE
            INC(Base, 32);
         END;
         
         INC(i2);
      END;
      
      INC(CharCount);
      
      RETURN Cnt
   END UpdateRound;
   
   PROCEDURE UpdateRounds;
   VAR R, Count: INTEGER;
   BEGIN
      CharCount := 1;
      
      R := 1;
      
      Count := 1;
      
      WHILE (R < 128) & (Count > 0) DO
         Count := UpdateRound();
         
         INC(R);
      END;
   END UpdateRounds;
   
   PROCEDURE SimpleRound;
   VAR i,i2,base,b: INTEGER;
   BEGIN
      CharCount := 1;
      
      i2 := 0;
      b := 08000H;
      base := 0;
      WHILE i2 < 12 DO
         IF i2 IN LinesUsed THEN
            i := 0;
            WHILE i < 32 DO
               IF Entropy[base] > 0 THEN
                  Charset[CharCount,0] := Pixels[base,0];
                  Charset[CharCount,1] := Pixels[base,1];
                  
                  SYSTEM.PUT(b, CharCount + 0F000H);
                  
                  INC(CharCount);
               ELSIF Pixels[base,0] # {} THEN
                  SYSTEM.PUT(b, 00F00H);
               ELSE
                  SYSTEM.PUT(b, 00000H);
               END;
               
               INC(i);
               INC(base);
               INC(b);
            END;
         ELSE
            i := 0;
            WHILE i < 32 DO
               SYSTEM.PUT(b, 00000H);
               INC(b);
               INC(i);
            END;
            INC(base, 32);
         END;
         
         INC(i2);
      END;
   END SimpleRound;
   
   PROCEDURE UpdateScreen;
   VAR I,I2, base: INTEGER;
   BEGIN
      Base := 08180H;
      
      i := 0;
      WHILE i < CharCount DO
         SYSTEM.PUT(Base, Charset[i,0]);
         SYSTEM.PUT(Base+1, Charset[i,1]);
         
         INC(i);
         INC(Base,2);
      END;
      
      Base := 08000H;
      
      i := 0;
      WHILE i < 32*12 DO
         SYSTEM.PUT(Base, Chars[i]);
         INC(Base);
         INC(i);
      END;
   END UpdateScreen;
   
   PROCEDURE UpdateScreen2;
   VAR I,I2, base: INTEGER;
   BEGIN
      Base := 08180H;
      
      i := 0;
      WHILE i < CharCount DO
         SYSTEM.PUT(Base, Charset[i,0]);
         SYSTEM.PUT(Base+1, Charset[i,1]);
         
         INC(i);
         INC(Base,2);
      END;
   END UpdateScreen;
   
   PROCEDURE DoDither;
   VAR cnt: INTEGER;
   BEGIN
      cnt := CalcBlockEntropy();
      
      IF cnt < 128 THEN
         SimpleRound();
         UpdateScreen2();
      ELSE
         SetBorder(15);
         UpdateRounds();
         UpdateScreen();
      END;
   END DoDither;
   
   PROCEDURE Rotate(VAR cub: TPoint; x,y,z, C,S: REAL);
   VAR zr, r0,r1,r2: REAL;
   BEGIN
      r0 := -s;
      r1 := s*c;
      r2 := c*c;
      
      zr := x*r0+y*r1+z*r2;
      zr := zr*(0.5*0.8);
      zr := zr+1.0;
      
      r0 := c*c;
      r1 := -c*s+s*s*c;
      r2 := s*s+c*s*c;
      
      cub.x := Floor((x*r0+y*r1+z*r2)*zr*20.0)+64;
      
      r0 := c*s;
      r1 := c*c+s*s*s;
      r2 := -s*c+c*s*s;
      
      cub.y := Floor((x*r0+y*r1+z*r2)*zr*20.0)+48;
      
   END Rotate;
   
   PROCEDURE InitCube;
   VAR i: INTEGER;
       ct, st: REAL;
   BEGIN
      i := 0;
      WHILE i < 32 DO
         ct := Math.Cos(Flt(i)*(Math.Pi/16.0));
         st := Math.Sin(Flt(i)*(Math.Pi/16.0));
         
         Rotate(Cube[0,i], -1.0,-1.0,-1.0, ct, st);
         Rotate(Cube[1,i],  1.0,-1.0,-1.0, ct, st);
         Rotate(Cube[2,i],  1.0, 1.0,-1.0, ct, st);
         Rotate(Cube[3,i], -1.0, 1.0,-1.0, ct, st);
         Rotate(Cube[4,i], -1.0,-1.0, 1.0, ct, st);
         Rotate(Cube[5,i],  1.0,-1.0, 1.0, ct, st);
         Rotate(Cube[6,i],  1.0, 1.0, 1.0, ct, st);
         Rotate(Cube[7,i], -1.0, 1.0, 1.0, ct, st);
         
         INC(i);
      END;
   END InitCube;
   
   PROCEDURE DrawCL(x,y: INTEGER);
   VAR x0,x1,y0,y1: INTEGER;
   BEGIN
      x0 := Cube[x,Cnt].x;
      y0 := Cube[x,Cnt].y;
      x1 := Cube[y,Cnt].x;
      y1 := Cube[y,Cnt].y;
      
      DrawLine(x0,y0,x1,y1);
   END DrawCL;
   
   PROCEDURE DrawCube;
   BEGIN
      DrawCL(0,1);
      DrawCL(1,2);
      DrawCL(2,3);
      DrawCL(3,0);
      
      DrawCL(4,5);
      DrawCL(5,6);
      DrawCL(6,7);
      DrawCL(7,4);
      
      DrawCL(0,4);
      DrawCL(1,5);
      DrawCL(2,6);
      DrawCL(3,7);
      
      Cnt := (Cnt+1) MOD 32;
   END DrawCube;
   
BEGIN
   Charset[0,0] := {};
   Charset[0,1] := {};
   
   SYSTEM.PUT(08000H, ORD("P")+0F000H);
   SYSTEM.PUT(08001H, ORD("r")+0F000H);
   SYSTEM.PUT(08002H, ORD("e")+0F000H);
   SYSTEM.PUT(08003H, ORD("c")+0F000H);
   SYSTEM.PUT(08004H, ORD("a")+0F000H);
   SYSTEM.PUT(08005H, ORD("l")+0F000H);
   SYSTEM.PUT(08006H, ORD("c")+0F000H);
   
   InitCube();
   
   Cnt := 0;
   WHILE TRUE DO
      clear();
      DrawCube();
      DoDither();
   END;
END Dither.