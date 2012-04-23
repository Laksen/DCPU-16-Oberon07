MODULE GC;

   IMPORT Internals, SYSTEM, Out;
   
   VAR
      HeapStart,
      HeapMax: INTEGER;
      
      Block16,
      Block32,
      Block128: INTEGER;
      
      Records: INTEGER;
   
   PROCEDURE AllocHeap(Size: INTEGER): INTEGER;
   VAR res: INTEGER;
   BEGIN
      res := HeapStart;
      INC(HeapStart, Size);
      SYSTEM.PUT(res, Size);
      INC(res, 1);
      
      RETURN res
   END AllocHeap;
   
   PROCEDURE AllocList(VAR Lst: INTEGER; Size: INTEGER): INTEGER;
   VAR Res: INTEGER;
   BEGIN
      res := Lst;
      
      IF res = 0 THEN
         res := AllocHeap(16);
      ELSE
         SYSTEM.GET(Lst, Lst);
         INC(res);
      END;
      
      RETURN res
   END AllocList;
   
   PROCEDURE FreeList(VAR Lst: INTEGER; Block: INTEGER);
   BEGIN
      SYSTEM.PUT(Block, Lst);
      Lst := Block;
   END FreeList;
   
   PROCEDURE GetBlock(Size: INTEGER): INTEGER;
   VAR Res: INTEGER;
   BEGIN
      IF Size < 16 THEN
         res := AllocList(Block16, 16);
      ELSIF Size < 32 THEN
         res := AllocList(Block32, 32);
      ELSIF Size < 128 THEN
         res := AllocList(Block128, 128);
      ELSE
         res := AllocHeap(Size);
      END;
      
      RETURN Res
   END GetBlock;
   
   PROCEDURE FreeBlock(Addr: INTEGER);
   VAR Size: INTEGER;
   BEGIN
      DEC(Addr);
      SYSTEM.GET(Addr, Size);
      
      IF Size = 16 THEN
         FreeList(Block16, Addr);
      ELSIF Size = 32 THEN
         FreeList(Block32, Addr);
      ELSIF Size = 128 THEN
         FreeList(Block128, Addr);
      END;
   END FreeBlock;
   
   PROCEDURE InitVptrs(Rec, Vptr: INTEGER);
   VAR PtrCnt, PtrOfs,
       i: INTEGER;
   BEGIN
      WHILE Vptr # 0 DO
         SYSTEM.GET(Vptr+1, PtrCnt);
         
         FOR i := 0 TO PtrCnt-11 DO
            SYSTEM.GET(Vptr+2+i, PtrOfs);
            SYSTEM.PUT(Rec+PtrOfs, 0);
         END;
         
         SYSTEM.GET(Vptr, Vptr);
      END;
   END InitVptrs;
   
   PROCEDURE NewImpl(VAR Addr: INTEGER; Size, Vptr: INTEGER);
   BEGIN
      Addr := GetBlock(Size);
      
      SYSTEM.PUT(Addr, Vptr);
      SYSTEM.PUT(Addr+1, 0);
      SYSTEM.PUT(Addr+2, Records);
      
      InitVptrs(Addr, Vptr);
      
      Records := Addr;
   END NewImpl;
   
   PROCEDURE Mark(Rec: INTEGER);
   VAR F, PtrCnt, ofs, i, value, Vptr: INTEGER;
   BEGIN
      SYSTEM.GET(Rec+1, F);
      
      IF F = 0 THEN
         SYSTEM.PUT(Rec+1, 1);
         
         SYSTEM.GET(Rec, Vptr);
         
         WHILE Vptr # 0 DO
            SYSTEM.GET(Vptr+1, PtrCnt);
            
            i := 0;
            WHILE PtrCnt > 0 DO
               SYSTEM.GET(Vptr+3+i, Ofs);
               SYSTEM.GET(Rec+ofs, Value);
               
               IF Value # 0 THEN
                  Mark(Value);
               END;
               
               INC(i);
               DEC(PtrCnt);
            END;
            
            SYSTEM.GET(Vptr, Vptr);
         END;
      END;
   END Mark;
   
   PROCEDURE Collect*;
   VAR R,F,
       Prev,Next: INTEGER;
   BEGIN
      R := Records;
      Prev := 0;
      
      WHILE R # 0 DO
         SYSTEM.GET(R+1, F);
         SYSTEM.GET(R+2, Next);
         
         IF F = 0 THEN
            IF Prev = 0 THEN
               Records := Next;
            ELSE
               SYSTEM.PUT(Prev+2, Next);
            END;
            
            FreeBlock(R);
         ELSE
            SYSTEM.PUT(R+1, 0);
            Prev := R;
         END;
         
         R := Next;
      END;
   END Collect;

BEGIN
   Block16 := 0;
   Block32 := 0;
   Block128 := 0;
   
   Records := 0;
   
   SYSTEM.GET(Internals.NrHEAPStart, HeapStart);
   SYSTEM.GET(Internals.NrHEAPEnd, HeapMax);
   
   SYSTEM.PUT(Internals.NrNEW, SYSTEM.ADR(NewImpl));
END GC.