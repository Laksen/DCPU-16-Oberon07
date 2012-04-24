MODULE Asm;
   
   IMPORT String, SYSTEM;
   
   PROCEDURE FetchPC(VAR PC: INTEGER): INTEGER;
   VAR V: INTEGER;
   BEGIN
      SYSTEM.GET(PC, V);
      INC(PC);
      RETURN V
   END FetchPC;
   
   PROCEDURE DecodeOperand(VAR Buffer: ARRAY OF CHAR; Oper: INTEGER; VAR Addr: INTEGER);
   BEGIN
      Oper := Oper MOD 64;
      
      IF Oper < 8 THEN
         IF Oper = 0 THEN
            String.Concat(Buffer, "A");
         ELSIF Oper = 1 THEN
            String.Concat(Buffer, "B");
         ELSIF Oper = 2 THEN
            String.Concat(Buffer, "C");
         ELSIF Oper = 3 THEN
            String.Concat(Buffer, "X");
         ELSIF Oper = 4 THEN
            String.Concat(Buffer, "Y");
         ELSIF Oper = 5 THEN
            String.Concat(Buffer, "Z");
         ELSIF Oper = 6 THEN
            String.Concat(Buffer, "I");
         ELSE
            String.Concat(Buffer, "J");
         END;
      ELSIF Oper < 16 THEN
         Oper := Oper-8;
         IF Oper = 0 THEN
            String.Concat(Buffer, "[A]");
         ELSIF Oper = 1 THEN
            String.Concat(Buffer, "[B]");
         ELSIF Oper = 2 THEN
            String.Concat(Buffer, "[C]");
         ELSIF Oper = 3 THEN
            String.Concat(Buffer, "[X]");
         ELSIF Oper = 4 THEN
            String.Concat(Buffer, "[Y]");
         ELSIF Oper = 5 THEN
            String.Concat(Buffer, "[Z]");
         ELSIF Oper = 6 THEN
            String.Concat(Buffer, "[I]");
         ELSE
            String.Concat(Buffer, "[J]");
         END;
      ELSIF Oper < 24 THEN
         Oper := Oper-16;
         
         String.Concat(Buffer, "[");
         String.ConcatInt(Buffer, FetchPC(Addr));
         
         IF Oper = 0 THEN
            String.Concat(Buffer, "+A]");
         ELSIF Oper = 1 THEN
            String.Concat(Buffer, "+B]");
         ELSIF Oper = 2 THEN
            String.Concat(Buffer, "+C]");
         ELSIF Oper = 3 THEN
            String.Concat(Buffer, "+X]");
         ELSIF Oper = 4 THEN
            String.Concat(Buffer, "+Y]");
         ELSIF Oper = 5 THEN
            String.Concat(Buffer, "+Z]");
         ELSIF Oper = 6 THEN
            String.Concat(Buffer, "+I]");
         ELSE
            String.Concat(Buffer, "+J]");
         END;
      ELSIF Oper = 018H THEN
         String.Concat(Buffer, "POP");
      ELSIF Oper = 019H THEN
         String.Concat(Buffer, "PEEK");
      ELSIF Oper = 01AH THEN
         String.Concat(Buffer, "PUSH");
      ELSIF Oper = 01BH THEN
         String.Concat(Buffer, "SP");
      ELSIF Oper = 01CH THEN
         String.Concat(Buffer, "PC");
      ELSIF Oper = 01DH THEN
         String.Concat(Buffer, "O");
      ELSIF Oper = 01EH THEN
         String.Concat(Buffer, "[");
         String.ConcatInt(Buffer, FetchPC(Addr));
         String.Concat(Buffer, "]");
      ELSIF Oper = 01FH THEN
         String.ConcatInt(Buffer, FetchPC(Addr));
      ELSE
         Oper := Oper-020H;
         String.ConcatInt(Buffer, Oper);
      END;
   END DecodeOperand;
   
   PROCEDURE DisassembleAddr*(VAR Buffer: ARRAY OF CHAR; VAR OpcodeAddr: INTEGER): BOOLEAN;
   VAR OP, c: INTEGER;
       r: BOOLEAN;
   BEGIN
      r := TRUE;
      
      OP := FetchPC(OpcodeAddr);
      
      c := OP MOD 16;
      
      IF c = 0 THEN
         c := ROR(OP, 4) MOD 64;
         
         IF c = 1 THEN
            String.Concat(Buffer, "JSR ");
            DecodeOperand(Buffer, ROR(Op, 10), OpcodeAddr);
         ELSE
            r := FALSE;
         END;
      ELSE
         IF c = 1 THEN
            String.Concat(Buffer, "SET ");
         ELSIF c = 2 THEN
            String.Concat(Buffer, "ADD ");
         ELSIF c = 3 THEN
            String.Concat(Buffer, "SUB ");
         ELSIF c = 4 THEN
            String.Concat(Buffer, "MUL ");
         ELSIF c = 5 THEN
            String.Concat(Buffer, "DIV ");
         ELSIF c = 6 THEN
            String.Concat(Buffer, "MOD ");
         ELSIF c = 7 THEN
            String.Concat(Buffer, "SHL ");
         ELSIF c = 8 THEN
            String.Concat(Buffer, "SHR ");
         ELSIF c = 9 THEN
            String.Concat(Buffer, "AND ");
         ELSIF c = 10 THEN
            String.Concat(Buffer, "BOR ");
         ELSIF c = 11 THEN
            String.Concat(Buffer, "XOR ");
         ELSIF c = 12 THEN
            String.Concat(Buffer, "IFE ");
         ELSIF c = 13 THEN
            String.Concat(Buffer, "IFN ");
         ELSIF c = 14 THEN
            String.Concat(Buffer, "IFG ");
         ELSIF c = 15 THEN
            String.Concat(Buffer, "IFB ");
         ELSE
            r := FALSE;
         END;
         
         DecodeOperand(Buffer, ROR(Op, 4), OpcodeAddr);
         String.Concat(Buffer, ", ");
         DecodeOperand(Buffer, ROR(Op, 10), OpcodeAddr);
      END;
      
      RETURN r
   END Disassemble;

   PROCEDURE Disassemble*(VAR Buffer: ARRAY OF CHAR; Opcode: ARRAY OF INTEGER);
   VAR adr: INTEGER;
   BEGIN
      adr := SYSTEM.ADR(Opcode[0]);
      DisassembleAddr(Buffer, adr);
   END Disassemble;

END Asm.