program doc;

{$mode objfpc}{$H+}

uses classes, sysutils, math,
     scanner, symtab, ntypes, instr, ninbuilt, nsystem;

const
 RealFractionBits = 7;
 RealFractionValue = 1 shl RealFractionBits;

var code: array[0..$FFFF] of word;
    codeCounter: word = InitSize+SystemFuncs;
    dataCounter: word = $8000;

    initFuncs: array[0..1023] of word;
    initCnt: longint;

    globals,
    mods: TSymTable;

procedure EmitWord(w: word);
begin
   code[codeCounter] := w;
   inc(codeCounter);
end;

function AllocateData(size: word): word;
begin
   dec(dataCounter, size);
   result := dataCounter;
end;

function AllocateString(const str: string): word;
var i: longint;
begin
   result := AllocateData(length(str)+1);
   for i := 1 to length(str) do
      code[result+i-1] := ord(str[i]);
   code[result+length(str)] := 0;
end;

function GetRecordId(a: PType): word;
var i, cnt: longint;
begin
   if a^.RecordID <> 0 then
      result := a^.RecordID
   else
   begin
      cnt := 0;

      for i := 0 to high(a^.Fields) do
         if a^.Fields[i].Typ^.TypeKind = typPointer then
            inc(cnt);

      result := AllocateData(3+Cnt);
      a^.RecordID := result;

      if assigned(a^.BaseType) then
         code[result] := GetRecordId(a^.BaseType)
      else
         code[result] := 0;

      code[result+1] := cnt;

      cnt := result+3;

      for i := 0 to high(a^.Fields) do
         if a^.Fields[i].Typ^.TypeKind = typPointer then
         begin
            FindField(a, a^.Fields[i].Name, code[cnt]);
            inc(cnt);
         end;
   end;
end;

procedure AddInit(w: word);
begin
   initFuncs[initCnt] := w;
   inc(initCnt);
end;

function ConstLoc(a: longint): TExpr;
begin
   result.Typ := @TypeInteger;
   result.Loc.Typ := ltConst;
   result.Loc.val := a;
end;

function RegLoc(a: longint): TExpr;
begin
   result.Typ := nil;
   result.Loc.Typ := ltReg;
   result.Loc.reg := a;
end;

function RefLoc(a: longint): TExpr;
begin
   result.Typ := nil;
   result.Loc.Typ := ltGlobVar;
   result.Loc.Addr := a;
end;

function ParamLoc(a: longint): TExpr;
begin
   result.Typ := nil;
   result.Loc.Typ := ltParam;
   result.Loc.Offset := a;
end;

function VarLoc(a: longint): TExpr;
begin
   result.Typ := nil;
   result.Loc.Typ := ltVar;
   result.Loc.Offset := a;
end;

function PushLoc: TExpr;
begin
   result.Typ := nil;
   result.Loc.Typ := ltPush;
end;

function PopLoc: TExpr;
begin
   result.Typ := nil;
   result.Loc.Typ := ltPop;
end;

function Compile(const AName: string): PSymbol; forward;

function DoCompile(const AName: string): PSymbol;
var s: PSymbol;
begin
   result := mods.FindSymbol(AName);
   if not assigned(result) then
   begin
      s := Compile(AName);
      mods.AddSymbol(s);
      result := s;
   end;
end;

function encode(const a: TExpr; out w: word; out emitword: boolean): byte;
begin
   case a.Loc.Typ of
      ltConst:
         begin
            if a.loc.val <= $1F then
            begin
               emitword := false;
               w := 0;
               result := $20+a.loc.val;
            end
            else
            begin
               emitword := true;
               w := a.loc.val;
               result := $1F;
            end;
         end;
      ltReg:
         begin
            emitword := false;
            result := a.loc.reg;
         end;
      ltRegInd:
         begin
            emitword := false;
            result := a.loc.reg+$8;
         end;
      ltRegOffset:
         begin
            emitword := true;
            w := a.Loc.Offset;
            result := a.loc.reg+$10;
         end;
      ltGlobVar:
         begin
            emitword := true;
            w := a.Loc.Addr;
            result := $1E;
         end;
      ltParamRef,
      ltParam:
         begin
            emitword := true;
            w := a.Loc.Offset+2;
            result := $17;
         end;
      ltVar:
         begin
            emitword := true;
            w := a.Loc.Offset;
            result := $17;
         end;
      ltPush:
         begin
            emitword := false;
            result := $1A;
         end;
      ltPop:
         begin
            emitword := false;
            result := $18;
         end;
   else
      raise exception.Create('Not implemented');
   end;
end;

function LocSame(const a,b: TExpr): boolean;
begin
   result := (a.Loc.Typ = b.Loc.Typ);

   if not result then exit;

   case a.Loc.Typ of
      ltReg: result := a.Loc.reg = b.loc.reg;
      ltParam,
      ltVar: result := a.Loc.Offset = b.loc.Offset;
      ltGlobVar: result := a.Loc.Addr = b.loc.Addr;
   else
      result := false;
   end;
end;

function Dumploc(const a: TLocation): string;
const reglut: array[0..$7] of string = ('A','B','C','X','Y','Z','I','J');
begin
   result := '';
   case a.Typ of
      ltConst: result := '0x'+inttohex(a.val, 4);
      ltReg:
         begin
            if a.reg < 8 then
               result := RegLut[a.reg]
            else if a.reg = V_PC then
               result := 'PC'
            else if a.reg = V_SP then
               result := 'SP'
            else if a.reg = V_O then
               result := 'O';
         end;
      ltRegOffset:
         begin
            if a.reg < 8 then
               result := RegLut[a.reg]
            else if a.reg = V_PC then
               result := 'PC'
            else if a.reg = V_SP then
               result := 'SP'
            else if a.reg = V_O then
               result := 'O';

            result := '['+result+'+0x'+IntToHex(a.Offset, 4)+']';
         end;
      ltGlobVar:
         result := '[0x'+inttohex(a.Addr, 4)+']';
      ltVar:
         if a.Offset >= $8000 then
            result := '[FP-'+inttohex(word(-a.Offset), 4)+']'
         else
            result := '[FP+'+inttohex(a.Offset, 4)+']';
      ltParam:
         result := '[FP+'+inttohex(a.Offset+2, 4)+']';
      ltParamRef:
         result := '[FP+'+inttohex(a.Offset+2, 4)+']';
      ltPush:
         result := 'PUSH';
      ltPop:
         result := 'POP';
   else
      WriteStr(result, a.typ);
   end;
end;

procedure Instruction(AOp: TOp; const A,B: TExpr);
var oper, va,vb: word;
    wa, wb: word;
    ea, eb: boolean;
begin
   if (AOp = A_SET) and
      LocSame(a,b) then exit;

   oper := ord(AOp);

   wa := Encode(A, va, ea);
   wb := Encode(B, vb, eb);

   EmitWord(oper or (wa shl OP_A) or (wb shl OP_B));
   if ea then EmitWord(va);
   if eb then EmitWord(vb);

   //writeln(AOp, ' ', dumploc(a.loc):8,', ', dumploc(b.loc));
end;

procedure ExtInstruction(AOp: TExtOp; const A: TExpr);
var oper, va,vb: word;
    wa, wb: word;
    ea, eb: boolean;
begin
   oper := ord(AOp);

   wa := Encode(A, va, ea);

   EmitWord((oper shl OP_A) or (wa shl OP_B));
   if ea then EmitWord(va);

   //writeln(AOp, ' ', dumploc(a.loc));
end;

procedure EnterProc(AdjSP: longint);
begin
   Instruction(A_SET, PushLoc, RegLoc(V_FP));
   Instruction(A_SET, RegLoc(V_FP), RegLoc(V_SP));
   if AdjSP <> 0 then
      Instruction(A_SUB, RegLoc(V_SP), ConstLoc(-AdjSP));
end;

procedure LeaveProc;
begin
   Instruction(A_SET, RegLoc(V_SP), RegLoc(V_FP));
   Instruction(A_SET, RegLoc(V_FP), PopLoc);
   Instruction(A_SET, RegLoc(V_PC), PopLoc);
end;

function Compile(const AName: string): PSymbol;
var fs: TStream;
    sc: TScanner;
    sym: PSymbol;
    syms: TSymTable;

    regs: array[0..6] of boolean;
    spills: array[0..1023] of longint;
    spillCnt: longint;

   function AllocReg: longint;
   var i: longint;
   begin
      for i := 0 to high(regs) do
         if not regs[i] then
         begin
            regs[i] := true;
            exit(i);
         end;

      result := spillCnt mod 7;
      spills[spillCnt] := result;
      inc(spillCnt);

      Instruction(A_SET, PushLoc, RegLoc(result));
   end;

   function SpillUsed: longword;
   var i: longint;
   begin
      result := 0;
      for i := 0 to high(regs) do
         if regs[i] then
         begin
            Instruction(A_SET, PushLoc, RegLoc(i));

            result := result or (1 shl i);
            regs[i] := false;
         end;
   end;

   procedure ClearUsed;
   var i: longint;
   begin
      for i := 0 to high(regs) do
            regs[i] := false;
   end;

   procedure Recover(OldRegs: longint);
   var i: longint;
   begin
      for i := high(regs) downto 0 do
         if (OldRegs and (1 shl i)) <> 0 then
         begin
            Instruction(A_SET, RegLoc(i), PopLoc);

            regs[i] := true;
         end;
   end;

   procedure Unspill(Level: longint);
   var i: longint;
   begin
      for i := spillcnt-1 downto Level do Instruction(A_SET, RegLoc(spills[i]), PopLoc);
      spillcnt := Level;
   end;

   procedure Error(s: string);
   begin
      //writeln('[',sc.Line,']', s);
      raise exception.CreateFmt('[%d] %s', [sc.Line,s]);
      //halt;
   end;

   function EncodeReal(r: double): word;
   begin
      result := round(r*RealFractionValue);
   end;

   function DecodeReal(w: word): double;
   begin
      result := smallint(w)/RealFractionValue;
   end;

   procedure MakeReadable(var loc: TExpr);
   var old: TExpr;
   begin
      case loc.Loc.Typ of
         ltParamRef:
            begin
               old := loc;

               loc.loc.typ := ltRegInd;
               loc.loc.Reg := AllocReg;

               old.Loc.Typ := ltParam;

               Instruction(A_SET, regloc(loc.loc.reg), old);
               //Instruction(A_SET, regloc(loc.loc.reg), loc);

               //loc.loc.typ := ltReg;
            end;
         ltSkipTrue:
            begin
               loc.Loc.Typ := ltReg;
               loc.loc.reg := AllocReg;

               {
                  IFx
                  ADD PC, 2 ; JMP false
               true:
                  SET r, 1
                  ADD PC, 1 ; JMP exit
               false:
                  SET r, 0
               exit:
               }

               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(2));
               Instruction(A_SET, loc, ConstLoc(1));
               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(1));
               Instruction(A_SET, loc, ConstLoc(0));
            end;
         ltSkipFalse:
            begin
               loc.Loc.Typ := ltReg;
               loc.loc.reg := AllocReg;

               {
                  IFx
                  ADD PC, 2 ; JMP true
               false:
                  SET r, 0
                  ADD PC, 1 ; JMP exit
               true:
                  SET r, 1
               exit:
               }

               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(2));
               Instruction(A_SET, loc, ConstLoc(0));
               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(1));
               Instruction(A_SET, loc, ConstLoc(1));
            end;
      end;
   end;

   procedure MakeWritable(var loc: TExpr);
   var old: TExpr;
   begin
      case loc.Loc.Typ of
         ltReg: ;

         ltVar,
         ltRegOffset,
         ltRegInd,
         ltGlobVar,
         ltParam,
         ltConst:
            begin
               old := loc;

               loc.Loc.Typ := ltReg;
               loc.loc.reg := AllocReg;

               Instruction(A_SET, loc, old);
            end;
         ltParamRef:
            begin
               old := loc;

               loc.Loc.Typ := ltRegInd;
               loc.loc.reg := AllocReg;

               Instruction(A_SET, RegLoc(loc.loc.reg), old);
               Instruction(A_SET, RegLoc(loc.loc.reg), loc);

               loc.Loc.Typ := ltReg;
            end;
         ltSkipTrue:
            begin
               loc.Loc.Typ := ltReg;
               loc.loc.reg := AllocReg;

               {
                  IFx
                  ADD PC, 2 ; JMP false
               true:
                  SET r, 1
                  ADD PC, 1 ; JMP exit
               false:
                  SET r, 0
               exit:
               }

               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(2));
               Instruction(A_SET, loc, ConstLoc(1));
               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(1));
               Instruction(A_SET, loc, ConstLoc(0));
            end;
         ltSkipFalse:
            begin
               loc.Loc.Typ := ltReg;
               loc.loc.reg := AllocReg;

               {
                  IFx
                  ADD PC, 2 ; JMP true
               false:
                  SET r, 0
                  ADD PC, 1 ; JMP exit
               true:
                  SET r, 1
               exit:
               }

               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(2));
               Instruction(A_SET, loc, ConstLoc(0));
               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(1));
               Instruction(A_SET, loc, ConstLoc(1));
            end;
         {ltSkipTrue:
            begin
               loc.Loc.Typ := ltReg;
               loc.loc.reg := AllocReg;

               {
                  IFx
                  ADD PC, 2 ; JMP true
               false:
                  SET r, 0
                  ADD PC, 1 ; JMP exit
               true:
                  SET r, 1
               exit:
               }

               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(2));
               Instruction(A_SET, loc, ConstLoc(0));
               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(1));
               Instruction(A_SET, loc, ConstLoc(1));
            end;
         ltSkipFalse:
            begin
               loc.Loc.Typ := ltReg;
               loc.loc.reg := AllocReg;

               {
                  IFx
                  ADD PC, 2 ; JMP false
               true:
                  SET r, 1
                  ADD PC, 1 ; JMP exit
               false:
                  SET r, 0
               exit:
               }

               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(2));
               Instruction(A_SET, loc, ConstLoc(0));
               Instruction(A_ADD, RegLoc(V_PC), ConstLoc(1));
               Instruction(A_SET, loc, ConstLoc(1));
            end;}
      else
         Error('Not writable');
      end;
   end;

   function MakeAddr(e: TExpr): TExpr;
   begin
      case e.Loc.Typ of
         ltReg:
            begin
               result := e;
               result.Loc.Typ := ltRegInd;
            end;
         ltConst:
            begin
               result := e;
               result.Loc.Typ := ltGlobVar;
               result.Loc.Addr := Result.Loc.Val;
            end;
         ltVar:
            begin
               result := e;
               result.Loc.Typ := ltRegInd;
               result.Loc.reg := AllocReg;

               Instruction(A_SET, RegLoc(result.Loc.Reg), e);
            end;
         ltParam:
            begin
               result := e;
               result.Loc.Typ := ltRegInd;
               result.Loc.reg := AllocReg;

               Instruction(A_SET, RegLoc(result.Loc.Reg), e);
            end;
         ltParamRef:
            begin
               result := e;
               result.Loc.Typ := ltRegInd;
               result.Loc.reg := AllocReg;

               e.Loc.Typ := ltParam;

               Instruction(A_SET, RegLoc(result.Loc.Reg), e);
               Instruction(A_SET, RegLoc(result.Loc.Reg), result);
            end;
         ltGlobVar:
            begin
               result := e;
               result.Loc.Typ := ltRegInd;
               result.Loc.reg := AllocReg;

               Instruction(A_SET, RegLoc(result.Loc.Reg), e);
            end;
         ltRegInd:
            begin
               result := e;
               result.Loc.Typ := ltRegInd;
               result.Loc.reg := AllocReg;

               Instruction(A_SET, RegLoc(result.Loc.Reg), e);
            end;
         ltRegOffset:
            begin
               result := e;
               result.Loc.Typ := ltRegInd;
               result.Loc.reg := AllocReg;

               Instruction(A_SET, RegLoc(result.Loc.Reg), e);
            end
      else
         Error('Cannot get address of symbol');
      end;
   end;

   function GetAddr(e: TExpr): TExpr;
   begin
      case e.Loc.Typ of
         ltConst:
            begin
               result := e;
            end;
         ltGlobVar:
            begin
               result := ConstLoc(e.Loc.Addr);
            end;
         ltVar:
            begin
               result := e;
               result.Loc.Typ := ltReg;
               result.Loc.reg := AllocReg;

               Instruction(A_SET, result, RegLoc(V_FP));
               if e.Loc.Offset <> 0 then
                  Instruction(A_ADD, result, ConstLoc(e.Loc.Offset));
            end;
         ltParam:
            begin
               result := e;
               result.Loc.Typ := ltReg;
               result.Loc.reg := AllocReg;

               e.Loc.Typ := ltConst;

               Instruction(A_SET, RegLoc(result.Loc.Reg), RegLoc(V_FP));
               Instruction(A_ADD, RegLoc(result.Loc.Reg), ConstLoc(e.Loc.Offset+2));
            end;
         ltParamRef:
            begin
               result := e;
               result.Loc.Typ := ltReg;
               result.Loc.reg := AllocReg;

               e.Loc.Typ := ltParam;

               Instruction(A_SET, RegLoc(result.Loc.Reg), e);
            end;
         ltRegOffset:
            begin
               result := e;
               result.Loc.Typ := ltReg;

               if result.Loc.Offset <> 0 then
               begin
                  if result.Loc.Offset >= $8000 then
                     Instruction(A_SUB, result, constloc(-e.Loc.Offset))
                  else
                     Instruction(A_ADD, result, ConstLoc(e.Loc.Offset));
               end;
            end;
         ltRegInd:
            begin
               result := e;
               result.Loc.Typ := ltReg;
            end
      else
         result := MakeAddr(e);
      end;
   end;

   function AddOffset(AExpr: TExpr; Ofs: word): TExpr;
   begin
      result := AExpr;

      case AExpr.Loc.Typ of
         ltVar: inc(result.Loc.Offset, ofs);
         ltRegOffset: inc(result.Loc.Offset, ofs);
         ltRegInd:
            begin
               if ofs = 0 then
                  result := AExpr
               else
               begin
                  result.Loc.Typ := ltRegOffset;
                  result.Loc.reg := AExpr.Loc.reg;
                  result.Loc.Offset := ofs;
               end;
            end;
         ltGlobVar: inc(result.Loc.Addr, ofs);
         ltParam:
            begin
               result.Loc.Typ := ltRegOffset;
               result.Loc.Reg := AllocReg;
               result.Loc.Offset := ofs;

               Instruction(A_SET, RegLoc(result.loc.reg), aexpr);
            end;
         ltParamRef:
            begin
               result.Loc.Typ := ltRegOffset;

               result.Loc.Reg := AllocReg;
               result.Loc.Offset := ofs;

               Instruction(A_SET, RegLoc(result.loc.reg), aexpr);
            end;
      else
         error('Cannot add offset');
      end;
   end;

   function GetLen(l: TExpr): TExpr;
   begin
      if l.Typ^.TypeKind = typOpenArray then
      begin
         result := l;
         result.Typ := @TypeInteger;
         result.Loc.Typ := ltParam;
         inc(result.Loc.Offset);
      end
      else
         result := ConstLoc(l.Typ^.ArrayLen);
   end;

   procedure MakeLWritable(var loc: TExpr);
   var old: TExpr;
   begin
      case loc.Loc.Typ of
         ltReg:
            begin
               loc.Loc.Typ := ltRegInd;
            end;
         ltParamRef:
            begin
               old := loc;

               loc.Loc.Typ := ltRegInd;
               loc.loc.reg := AllocReg;

               old.Loc.Typ := ltParam;

               Instruction(A_SET, RegLoc(loc.loc.reg), old);
            end;
      end;
   end;

   function ToAddr(e: TExpr): TExpr;
   begin
      case e.Loc.Typ of
         ltGlobVar:
            begin
               result := e;
               result.Loc.Typ := ltConst;
               result.Loc.val := result.Loc.Addr;
            end;
         ltParam: result := e;
         ltParamRef:
            begin
               result := e;
               result.loc.Typ := ltReg;
               result.Loc.Reg := AllocReg;

               Instruction(A_SET, result, e);
            end;
         ltVar:
            begin
               result.loc.Typ := ltReg;
               result.Loc.Reg := AllocReg;
               Instruction(A_SET, result, RegLoc(V_FP));
               Instruction(A_ADD, result, ConstLoc(e.Loc.Offset));
            end;
      else
         Error('Cannot get address of symbol');
      end;
   end;

   function IsSpilled(r: longint): boolean;
   var i: longint;
   begin
      result := false;

      for i := 0 to spillCnt-1 do
         if spills[i] = r then
            exit(true);
   end;

   procedure Unuse(x: TExpr; DeferUnspill: boolean = false);
   begin
      case x.Loc.Typ of
         ltReg,
         ltRegInd,
         ltRegOffset:
            begin
               if x.Loc.Reg < 7 then
               begin
                  if IsSpilled(x.Loc.Reg) then
                  begin
                     if not DeferUnspill then
                        if x.Loc.Reg = spills[spillCnt-1] then
                        begin
                           dec(spillcnt);
                           Instruction(A_SET, RegLoc(spills[spillCnt]), PopLoc);
                        end;

                  end
                  else
                     regs[x.Loc.Reg] := false;
               end;
            end;
      end;
   end;

   function AddJmp: word;
   begin
      result := codeCounter+2;
      Instruction(A_ADD, RegLoc(V_PC), ConstLoc($FFFF));
   end;

   procedure FixJmp(l: word);
   begin
      code[l-1] := codeCounter-l+1;
   end;

   function Token: TToken;
   begin
      result := sc.Token;
   end;

   procedure Consume(ATok: TToken);
   begin
      if sc.Token = ATok then
         sc.Next
      else
         Error('Wrong token');
   end;

   function Pr(tk: TToken): string;
   begin
      result := sc.TokenStr;
      consume(tk);
   end;

   function Ident: string;
   begin
      result := uppercase(pr(tkIdent));
   end;

   function IdentDef: PSymbol;
   begin
      new(Result);
      result^.name := Ident;
      result^.exported := Token = tkMul;
      if token = tkMul then
         Consume(tkMul);
   end;

   function QualIdent: PSymbol;
   var t, x: String;
   begin
      t := Ident;
      result := syms.FindSymbol(t);
      if not assigned(result) then
         Error('No symbol found called '+t);

      if result^.typ = stModule then
      begin
         Consume(tkDot);
         x := Ident;
         result := result^.symtab.FindPublicSymbol(x);
         if not assigned(result) then
            Error('No symbol found called '+x+' in '+t);
      end;
   end;

   function ConstExpr(var Val: word): PType; forward;

   function ConstElement: word;
   var a,b: word;
       i: longint;
   begin
      if not Same(ConstExpr(a), @TypeInteger) then error('Set element must be integer');
      b := a;

      if token = tkDotDot then
      begin
         consume(tkDotDot);

         if not Same(ConstExpr(b), @TypeInteger) then error('Set element must be integer');
      end;

      if a > b then
      begin
         i := a;
         a := b;
         b := i;
      end;

      result := 0;
      for i := a to b do
         result := result or (1 shl i);
   end;

   function ConstSet: word;
   begin
      Consume(tkLBracket);

      result := ConstElement;
      while token = tkComma do
      begin
         Consume(tkComma);
         result := result or ConstElement;
      end;

      Consume(tkRBracket);
   end;

   function ConstFactor(var Val: word): PType;
   var s: PSymbol;
   begin
      case token of
         tkInteger:
            begin
               result := @TypeInteger;
               val := strtoint(pr(tkInteger));
            end;
         tkReal:
            begin
               result := @TypeReal;
               val := EncodeReal(strtofloat(pr(tkReal)));
            end;
         tkNil:
            begin
               result := @TypeNil;
               val := 0;
            end;
         tkTrue,
         tkFalse:
            begin
               result := @TypeBool;
               val := ord(token = tkTrue);
            end;
         tkLBracket:
            begin
               val := ConstSet;
               result := @TypeSet;
            end;
         tkIdent:
            begin
               s := QualIdent;
               if s^.typ <> stConst then
                  Error('Non-constant constant expression not supported');

               result := s^.symType;
               val := s^.val;
            end;
         tkLParan:
            begin
               Consume(tkLParan);
               result := ConstExpr(val);
               Consume(tkRParan);
            end;
      end;
   end;

   function ConstTerm(var Val: word): PType;
   var tok: TToken;
       r2: PType;
       v2: word;
   begin
      result := ConstFactor(val);

      while token in [tkMul,tkSlash,tkDiv,tkMod,tkAnd] do
      begin
         tok := Token;

         consume(tok);

         r2 := ConstFactor(v2);

         if not same(result, r2) then
            Error('Cannot multiply, divide, or "and" non-equal types');

         case tok of
            tkMul:
               begin
                  if same(result, @TypeInteger) then
                     val := val*v2
                  else if same(result, @TypeReal) then
                     val := EncodeReal(DecodeReal(val)*DecodeReal(v2))
                  else if same(result, @TypeSet) then
                     val := val and v2
                  else
                     error('* operator does not apply to types');
               end;
            tkSlash:
               begin
                  if same(result, @TypeSet) then
                     val := val xor v2
                  else if same(result, @TypeReal) then
                     val := EncodeReal(DecodeReal(val)/DecodeReal(v2))
                  else
                     error('/ operator does not apply to types');
               end;
            tkDiv:
               begin
                  if same(result, @TypeInteger) then
                     val := val div v2
                  else
                     error('DIV operator does not apply to types');
               end;
            tkMod:
               begin
                  if same(result, @TypeInteger) then
                     val := val div v2
                  else
                     error('DIV operator does not apply to types');
               end;
            tkAnd:
               begin
                  if same(result, @TypeBool) then
                     val := val and v2
                  else
                     error('& operator does not apply to types');
               end;
         end;
      end;
   end;

   function ConstSimpExpr(var Val: word): PType;
   var neg: boolean;
       tok: TToken;
       v2: word;
       r2: PType;
   begin
      neg := token = tkMinus;

      if token in [tkPlus, tkMinus] then
         Consume(token);

      result := ConstTerm(val);

      if neg then
      begin
         if Same(result, @TypeInteger) then
            val := (not val)+1
         else if same(result, @TypeReal) then
            val := EncodeReal(-DecodeReal(val))
         else if Same(result, @TypeSet) then
            val := not val
         else
            error('Unary negate does not apply to type');
      end;

      while token in [tkPlus, tkMinus, tkOr] do
      begin
         tok := Token;

         consume(tok);

         r2 := ConstTerm(v2);

         if not same(result, r2) then
            Error('Cannot add, subtract, or "or" non-equal types');

         case tok of
            tkPlus:
               begin
                  if same(result, @TypeInteger) then
                     val := val+v2
                  else if same(result, @TypeReal) then
                     val := EncodeReal(DecodeReal(val)+DecodeReal(v2))
                  else if same(result, @TypeSet) then
                     val := val or v2
                  else
                     error('Add operator does not apply to types');
               end;
            tkMinus:
               begin
                  if same(result, @TypeInteger) then
                     val := val-v2
                  else if same(result, @TypeReal) then
                     val := EncodeReal(DecodeReal(val)-DecodeReal(v2))
                  else if same(result, @TypeSet) then
                     val := val and (not v2)
                  else
                     error('Subtract operator does not apply to types');
               end;
            tkOr:
               begin
                  if same(result, @TypeBool) then
                     val := val or v2
                  else
                     error('OR operator does not apply to types');
               end;
         end;
      end;
   end;

   function ConstExpr(var Val: word): PType;
   var tok: TToken;
       v2: word;
       r2: PType;
   begin
      result := ConstSimpExpr(val);

      if token in [tkEqual, tkNEqual, tkLess, tkLEqual, tkGreater, tkGEqual, tkIn] then
      begin
         tok := token;
         consume(tok);

         r2 := ConstSimpExpr(v2);

         case tok of
            tkEqual, tkNEqual, tkLess, tkLEqual, tkGreater, tkGEqual:
               begin
                  if not same(result, r2) then
                     Error('Cannot compare non-equal types');
                  result := @TypeBool;

                  case tok of
                     tkEqual: val := ord(val = v2);
                     tkNEqual: val := ord(val <> v2);
                     tkLess: val := ord(val < v2);
                     tkLEqual: val := ord(val <= v2);
                     tkGreater: val := ord(val > v2);
                     tkGEqual: val := ord(val >= v2);
                  end;
               end;
            tkIn:
               begin
                  if not same(r2, @TypeSet) then
                     Error('R-value of IN operator must be a set');
                  if not same(result, @TypeInteger) then
                     Error('L-value of IN operator must be a set');
                  result := @TypeBool;

                  val := (v2 shr val) and 1;
               end;
         end;
      end;
   end;

   function len: longint;
   var v: word;
       p: PType;
   begin
      p := ConstExpr(v);
      if not same(p, @TypeInteger) then error('Array length must be integer');
      result := v;
   end;

   function GetType(p: PSymbol): PType;
   begin
      if p^.typ = stType then
         result := p^.symType
      else
         Error('Symbol is not a type declaration');
   end;

   function StrucType: PType; forward;

   function _Type(var Decl: PType): PType;
   begin
      if token = tkIdent then
      begin
         decl := nil;
         result := GetType(QualIdent);
      end
      else
      begin
         result := StrucType;
         Decl := result;
      end;
   end;

   function FormalParameters: PType;

      function FormalType(var decl: PType): PType;
      begin
         if token = tkArray then
         begin
            consume(tkArray);
            consume(tkOf);
            result := GetType(QualIdent);
            new(decl);
            decl^.TypeKind := typOpenArray;
            decl^.BaseType := result;
            result := decl;
         end
         else
         begin
            decl := nil;
            result := GetType(QualIdent);
         end;
      end;

      procedure FPSection(Func: PType);
      var isvar: Boolean;
          s: TStringList;
          i: longint;
          pt, dt: PType;
      begin
         isvar := (Token = tkVar);
         if isvar then consume(tkVar);

         s := TStringList.Create;

         s.add(ident);
         while token = tkComma do
         begin
            consume(tkcomma);
            s.add(ident);
         end;

         consume(tkColon);

         pt := FormalType(dt);

         for i := 0 to s.Count-1 do
            AddField(func, s[i], false, isvar, dt, pt);

         s.free;
      end;

   begin
      new(result);
      result^.TypeKind := typFunc;

      Consume(tkLParan);
      if token in [tkVar, tkIdent] then
      begin
         FPSection(result);
         while token = tkSemiColon do
         begin
            consume(tkSemiColon);
            FPSection(result);
         end;
      end;
      Consume(tkRParan);

      if Token = tkColon then
      begin
         consume(tkColon);
         Result^.FuncRet := GetType(QualIdent);
      end;
   end;

   function FieldList(typ: PType): PType;
   var n: string;
       exported: boolean;
       td: PType;
   begin
      n := Ident;
      exported := (token = tkMul);
      if exported then consume(tkMul);

      if token = tkColon then
      begin
         consume(tkColon);
         result := _Type(td);

         AddField(typ, n, exported, false, td, result);
      end
      else
      begin
         consume(tkComma);
         result := FieldList(typ);
         AddField(typ, n, exported, false, nil, result);
      end;
   end;

   function StrucType: PType;
   var t, x: PType;
       tmp, tmp2: string;
       tmpSym: PSymbol;
   begin
      if token = tkArray then
      begin
         consume(tkArray);
         new(result);
         result^.TypeKind := typArray;
         result^.ArrayLen := len;
         t := result;
         while token = tkComma do
         begin
            Consume(tkComma);
            new(x);
            x^.TypeKind := typArray;
            x^.ArrayLen := len;
            t^.BaseType := x;
            t := x;
         end;
         consume(tkOf);
         t^.BaseType := _Type(t^.decl);
      end
      else if token = tkRecord then
      begin
         consume(tkRecord);
         new(result);
         result^.BaseType := nil;
         result^.TypeKind := typRecord;
         result^.RecordID := 0;

         if token = tkLParan then
         begin
            Consume(tkLParan);
            result^.BaseType := GetType(QualIdent);
            Consume(tkRParan);
         end;

         if token = tkIdent then
         begin
            FieldList(result);
            while token = tkSemiColon do
            begin
               consume(tkSemiColon);
               FieldList(result);
            end;
         end;

         Consume(tkEnd);
      end
      else if token = tkPointer then
      begin
         Consume(tkPointer);
         Consume(tkTo);
         new(result);
         result^.TypeKind := typPointer;

         if token = tkIdent then
         begin
            tmp := Ident;
            tmpSym := syms.FindSymbol(tmp);
            if not assigned(tmpSym) then
            begin
               result^.ForwardType := tmp;
               result^.decl := nil;
               result^.BaseType := nil;
            end
            else
            begin
               if tmpSym^.typ = stModule then
               begin
                  Consume(tkDot);
                  tmp2 := Ident;
                  tmpSym := tmpSym^.symtab.FindPublicSymbol(tmp2);
                  if not assigned(tmpSym) then
                     Error('No symbol found called '+tmp2+' in '+tmp);
               end;

               result^.decl := nil;
               result^.BaseType := GetType(tmpSym);
            end;
         end
         else
            result^.BaseType := _Type(result^.decl);

         if assigned(result^.BaseType) and
            (result^.BaseType^.TypeKind <> typRecord) then error('Pointer types can only be to records');
      end
      else if token = tkProcedure then
      begin
         consume(tkProcedure);
         if token = tkLParan then
         begin
            result := FormalParameters;
         end
         else
         begin
            new(result);
            Result^.TypeKind := typFunc;
            result^.FuncRet := nil;
         end;
      end;
   end;

   function Expression: TExpr; forward;
   function Designator(LValue: boolean; var sym: PSymbol): TExpr; forward;

   function Element: TExpr;
   var a,b, negmask: TExpr;
   begin
      a := Expression;
      if not same(a.Typ, @TypeInteger) then error('Set element has to be integer');

      if token = tkDotDot then
      begin
         consume(tkDotDot);

         // Generate negative mask
         if a.Loc.Typ = ltConst then
            negmask := ConstLoc($FFFF shl a.Loc.Val)
         else
         begin
            negmask := ConstLoc($FFFF);
            MakeWritable(negmask);
            MakeReadable(a);
            Instruction(A_SHL, negmask, a);
            unuse(a);
         end;
         negmask.Typ := @TypeSet;

         b := Expression;

         if b.Loc.Typ = ltConst then
            result := ConstLoc(not ($FFFE shl b.Loc.Val))
         else
         begin
            result := ConstLoc($FFFE);
            MakeWritable(result);
            MakeReadable(b);
            Instruction(A_SHL, result, b);
            Instruction(A_XOR, result, ConstLoc($FFFF));
            unuse(b);
         end;

         if (negmask.Loc.Typ = ltConst) and
            (result.loc.typ = ltConst) then
            result := ConstLoc(Result.Loc.Val and negmask.Loc.Val)
         else
         begin
            Instruction(A_AND, result, negmask);
            Unuse(negmask);
         end;

         result.Typ := @TypeSet;
      end
      else
      begin
         if a.Loc.Typ = ltConst then
            result := ConstLoc(1 shl a.Loc.Val)
         else
         begin
            result := ConstLoc(1);
            MakeWritable(result);
            MakeReadable(a);
            Instruction(A_SHL, result, a);
            unuse(a);
         end;

         result.Typ := @TypeSet;
      end;
   end;

   function Factor: TExpr;
   var tmp, r2, r3: TExpr;
       t: string;
       i, cnt: longint;
       mpt: PSymbol;
       tmp2: PSymbol;
       l, l2: word;
       r: LongWord;
   begin
      if token = tkInteger then
      begin
         result.Loc.Typ := ltConst;
         result.Loc.val := strtoint(pr(tkInteger));
         result.Typ := @TypeInteger;
      end
      else if token = tkReal then
      begin
         result.Loc.Typ := ltConst;
         result.Loc.val := encodeReal(StrToFloat(pr(tkReal)));
         result.Typ := @TypeReal;
      end
      else if token = tkNil then
      begin
         consume(tkNil);
         result.Loc.Typ := ltGlobVar;
         result.Loc.addr := 0;
         result.Typ := @TypeNil;
      end
      else if token in [tkTrue, tkFalse] then
      begin
         result.Loc.Typ := ltConst;
         result.Loc.val := ord(token = tkTrue);
         result.Typ := @TypeBool;
         consume(token);
      end
      else if token = tkCharConst then
      begin
         result.Loc.Typ := ltConst;
         result.Loc.val := strtoint(Pr(tkCharConst));
         result.Typ := @TypeChar;
      end
      else if token = tkString then
      begin
         result.Loc.Typ := ltConst;
         t := pr(tkString);
         result.Loc.val := AllocateString(t);
         result.Typ := ArrayType(length(t), @TypeChar);
      end
      else if token = tkLBracket then
      begin
         Consume(tkLBracket);
         if token <> tkRBracket then
         begin
            result := Element;
            while token = tkComma do
            begin
               MakeWritable(result);

               Consume(tkComma);

               r2 := Element;

               if (result.Loc.Typ = ltConst) and
                  (r2.Loc.Typ = ltConst) then
               begin
                  result := ConstLoc(result.Loc.Val or r2.Loc.Val);
                  result.Typ := @TypeSet;
               end
               else
               begin
                  Instruction(A_BOR, result, r2);
                  Unuse(r2);
               end;
            end;
         end
         else
         begin
            result := ConstLoc(0);
            result.Typ := @TypeSet;
         end;
         Consume(tkRBracket);
      end
      else if token = tkIdent then
      begin
         result := designator(false, mpt);

         if assigned(mpt) and
            (mpt^.typ = stInbFunc) then
         begin
            Consume(tkLParan);

            case TInbuiltFunc(mpt^.val) of
               inbAbs:
                  begin
                     result := Expression;

                     if Same(result.Typ, @TypeInteger) then
                     begin
                        if result.Loc.Typ = ltConst then
                           result := ConstLoc(abs(result.Loc.Val))
                        else
                        begin
                           MakeWritable(result);
                           Instruction(A_IFG, result, ConstLoc($7FFF));
                           Instruction(A_MUL, result, ConstLoc(-1));
                        end;
                     end
                     else if same(result.typ, @TypeReal) then
                     begin
                        if result.Loc.Typ = ltConst then
                        begin
                           result := ConstLoc(EncodeReal(abs(DecodeReal(result.Loc.Val))));
                           result.Typ := @TypeReal;
                        end
                        else
                        begin
                           MakeWritable(result);
                           Instruction(A_IFG, result, ConstLoc($7FFF));
                           Instruction(A_MUL, result, ConstLoc(-1));
                        end;
                     end
                     else if same(result.typ, @TypeSet) then
                     begin
                        {
                           SET r2, 0
                           IFE result, 0
                           ADD PC, exit
                           IFB result, 1
                           ADD r2, 1
                           SHR result, 1
                           SUB PC,
                        }

                        r2 := RegLoc(AllocReg);
                        MakeWritable(result);

                        Instruction(A_SET, r2, ConstLoc(0));
                        l2 := codeCounter;
                        Instruction(A_IFE, result, ConstLoc(0));
                        l := AddJmp;
                        Instruction(A_IFB, result, ConstLoc(1));
                        Instruction(A_ADD, r2, ConstLoc(1));
                        Instruction(A_SHR, result, ConstLoc(1));
                        Instruction(A_IFB, result, ConstLoc(1));
                        Instruction(A_ADD, r2, ConstLoc(1));
                        Instruction(A_SHR, result, ConstLoc(1));
                        Instruction(A_SUB, RegLoc(V_PC), ConstLoc(codeCounter-l2+1));
                        FixJmp(l);

                        unuse(result);
                        result := r2;
                        result.Typ := @TypeInteger;
                     end
                     else
                        Error('Not implemented');
                  end;
               inbOdd:
                  begin
                     result := Expression;

                     if not Same(result.Typ, @TypeInteger) then
                        error('ODD does only apply to integer types');

                     Instruction(A_IFB, result, ConstLoc(1));

                     result.Loc.Typ := ltSkipFalse;
                     result.Typ := @TypeBool;
                  end;
               inbLen:
                  begin
                     result := Designator(true, mpt);

                     if not (result.Typ^.TypeKind in [typArray, typOpenArray]) then
                        error('LEN function expects array type');

                     if Result.Typ^.TypeKind = typArray then
                        result := ConstLoc(result.Typ^.ArrayLen)
                     else
                        result := GetLen(result);
                  end;
               inbLsl:
                  begin
                     result := Expression;
                     if not same(result.Typ, @TypeInteger) then Error('Shift can only be applied to integers');

                     Consume(tkComma);
                     tmp := Expression;
                     if not same(tmp.Typ, @TypeInteger) then Error('Shift can only be applied to integers');

                     MakeWritable(result);

                     Instruction(A_SHL, result, tmp);

                     Unuse(tmp);
                  end;
               inbLsr:
                  begin
                     result := Expression;
                     if not same(result.Typ, @TypeInteger) then Error('Shift can only be applied to integers');

                     Consume(tkComma);
                     tmp := Expression;
                     if not same(tmp.Typ, @TypeInteger) then Error('Shift can only be applied to integers');

                     MakeWritable(result);

                     Instruction(A_SHR, result, tmp);

                     Unuse(tmp);
                  end;
               inbAsr:
                  begin
                     result := Expression;
                     if not same(result.Typ, @TypeInteger) then Error('Shift can only be applied to integers');

                     Consume(tkComma);
                     tmp := Expression;
                     if not same(tmp.Typ, @TypeInteger) then Error('Shift can only be applied to integers');

                     MakeWritable(result);

                     {
                        IFG x, $8000
                        SET PC, l
                        SHR x, y
                        SET PC, l2
                     l:
                        XOR x, 0xFFFF
                        SHR x,y
                        XOR x, 0xFFFF
                     l2:
                     }
                     Instruction(A_IFG, result, ConstLoc($7FFF));
                     l := AddJmp;
                     Instruction(A_SHR, result, tmp);
                     l2 := AddJmp;
                     FixJmp(l);
                     Instruction(A_XOR, result, ConstLoc($FFFF));
                     Instruction(A_SHR, result, tmp);
                     Instruction(A_XOR, result, ConstLoc($FFFF));
                     FixJmp(l2);

                     Unuse(tmp);
                  end;
               inbRor:
                  begin
                     result := Expression;
                     if not same(result.Typ, @TypeInteger) then Error('Shift can only be applied to integers');

                     Consume(tkComma);
                     tmp := Expression;
                     if not same(tmp.Typ, @TypeInteger) then Error('Shift can only be applied to integers');

                     MakeWritable(result);

                     Instruction(A_SHR, result, tmp);
                     Instruction(A_BOR, result, RegLoc(V_O));

                     Unuse(tmp);
                  end;
               inbFloor:
                  begin
                     result := Expression;
                     if not (same(result.Typ, @TypeReal) or
                             same(result.typ, @TypeLongReal)) then Error('FLOOR can only be applied to REALs');

                     if result.Loc.Typ = ltConst then
                        result.Loc.Val := floor(DecodeReal(result.Loc.Val))
                     else
                     begin
                        MakeWritable(result);
                        {
                           IFG x, $8000
                           SET PC, l
                           SHR x, y
                           SET PC, l2
                        l:
                           XOR x, 0xFFFF
                           SHR x,y
                           XOR x, 0xFFFF
                        l2:
                        }
                        Instruction(A_IFG, result, ConstLoc($7FFF));
                        l := AddJmp;
                        Instruction(A_SHR, result, ConstLoc(RealFractionBits));
                        l2 := AddJmp;
                        FixJmp(l);
                        Instruction(A_XOR, result, ConstLoc($FFFF));
                        Instruction(A_SHR, result, ConstLoc(RealFractionBits));
                        Instruction(A_XOR, result, ConstLoc($FFFF));
                        FixJmp(l2);
                     end;

                     result.Typ := @TypeInteger;
                  end;
               inbFlt:
                  begin
                     result := Expression;
                     if not same(result.Typ, @TypeInteger) then Error('FLT can only be applied to INTEGERs');

                     if result.Loc.Typ = ltConst then
                        result.Loc.Val := EncodeReal(SmallInt(result.Loc.Val))
                     else
                     begin
                        MakeWritable(result);
                        Instruction(A_SHL, result, ConstLoc(RealFractionBits));
                     end;

                     result.Typ := @TypeReal;
                  end;
               inbOrd:
                  begin
                     result := Expression;
                     if not (same(result.Typ, @TypeChar) or
                             same(result.typ, @TypeBool) or
                             same(result.typ, @TypeSet)) then Error('Can only convert CHAR, BOOL, or SETs to ordinal values');

                     MakeReadable(result);

                     result.Typ := @TypeInteger;
                  end;
               inbChr:
                  begin
                     result := Expression;
                     if not same(result.Typ, @TypeInteger) then Error('Can only convert INTEGERs to CHARs');

                     result.Typ := @TypeChar;
                  end;
               inbLong:
                  begin
                     result := Expression;
                     if not same(result.Typ, @TypeReal) then Error('LONG can only be applied to REALs');

                     result.Typ := @TypeLongReal;
                  end;
               inbShort:
                  begin
                     result := Expression;
                     if not same(result.Typ, @TypeLongReal) then Error('SHORT can only be applied to LONGREALs');

                     result.Typ := @TypeReal;
                  end;

               inbAdr:
                  begin
                     result := GetAddr(Expression);
                     result.Typ := @TypeInteger;
                  end;
               inbSize:
                  begin
                     result := Designator(false,mpt);
                     result := ConstLoc(TypeSize(result.Typ));
                  end;
               inbMultShift:
                  begin
                     Result := Expression;
                     Consume(tkComma);
                     r2 := Expression;

                     tmp.Loc.Typ := ltReg;
                     tmp.Loc.Reg := AllocReg;

                     MakeWritable(result);
                     MakeReadable(r2);

                     Instruction(A_MUL, result, r2);
                     Instruction(A_SET, tmp, RegLoc(V_O));

                     Unuse(r2);

                     consume(tkComma);

                     r2 := Expression;

                     MakeReadable(r2);

                     if r2.Loc.Typ = ltConst then
                     begin
                        Instruction(A_SHL, tmp, ConstLoc(16-r2.Loc.Val));
                        Instruction(A_SHR, result, ConstLoc(r2.loc.val));
                        Instruction(A_BOR, result, tmp);
                     end
                     else
                     begin
                        Instruction(A_SHR, result, r2);
                        Instruction(A_SHR, tmp, r2);
                        Instruction(A_BOR, result, RegLoc(V_O));

                        Unuse(r2);
                     end;
                  end;
            else
               Error('Bad inbuilt function');
            end;

            Consume(tkRParan);
         end
         else if assigned(mpt) and
                 ((mpt^.typ = stFunc) or
                  (mpt^.symType^.TypeKind = typFunc)) then
         begin
            if token = tkLParan then
            begin
               Consume(tkLParan);

               r2 := result;

               result.Typ := result.Typ^.FuncRet;
               result.Loc.Typ := ltReg;
               result.Loc.Reg := AllocReg;

               regs[Result.Loc.Reg] := false;

               r := SpillUsed;

               i := 0;
               cnt := 0;

               if token <> tkRParan then
               begin
                  if length(mpt^.symType^.Fields) <= 0 then
                     error('Function does not take any parameters');

                  if IsByref(mpt^.symType^.Fields[i]) then
                  begin
                     tmp := Expression;

                     if mpt^.symType^.Fields[i].Typ^.TypeKind = typOpenArray then
                     begin
                        Instruction(A_SET, PushLoc, GetLen(tmp));
                        inc(cnt);
                     end;

                     Instruction(A_SET, PushLoc, GetAddr(tmp));
                  end
                  else
                  begin
                     tmp := Expression;

                     if not AssignableTo(mpt^.symType^.Fields[i].Typ, tmp.typ) then
                        Error('Wrong type in argument');

                     MakeReadable(tmp);
                     Instruction(A_SET, PushLoc, tmp);
                  end;
                  inc(i);
                  inc(cnt);

                  ClearUsed;

                  while token = tkComma do
                  begin
                     pr(tkComma);

                     if length(mpt^.symType^.Fields) <= i then
                        error('Too many parameters');

                     if IsByref(mpt^.symType^.Fields[i]) then
                     begin
                        tmp := Expression;

                        if mpt^.symType^.Fields[i].Typ^.TypeKind = typOpenArray then
                        begin
                           Instruction(A_SET, PushLoc, GetLen(tmp));
                           inc(cnt);
                        end;

                        Instruction(A_SET, PushLoc, GetAddr(tmp))
                     end
                     else
                     begin
                        tmp := Expression;

                        if not AssignableTo(mpt^.symType^.Fields[i].Typ, tmp.typ) then
                           Error('Wrong type in argument');

                        MakeReadable(tmp);
                        Instruction(A_SET, PushLoc, tmp);
                     end;
                     inc(i);
                     inc(cnt);

                     ClearUsed;
                  end;

                  if i < length(mpt^.symType^.Fields) then
                     writeln('Expected more parameters');
               end;

               ExtInstruction(A_JSR, r2);

               if cnt > 0 then Instruction(A_ADD, RegLoc(V_SP), ConstLoc(cnt));

               Instruction(A_SET, result, RegLoc(0));

               Recover(r);
               regs[Result.Loc.Reg] := true;

               Consume(tkRParan);
            end;
         end;
      end
      else if token = tkLParan then
      begin
         consume(tkLParan);
         result := Expression;
         consume(tkRParan);
      end
      else if token = tkNot then
      begin
         consume(tkNot);
         result := Factor();
         MakeWritable(result);

         tmp.typ := nil;
         tmp.loc.typ := ltConst;

         if Same(result.Typ, @TypeBool) then
            tmp.Loc.val := $1
         else
            tmp.Loc.val := $FFFF;

         Instruction(A_XOR, result, tmp);
      end
      else
         error('Error while parsing factor');
   end;

   function Term: TExpr;
   var tok: TToken;
       r2,tmp, tmp2: TExpr;
       l: word;
   begin
      result := Factor;

      while token in [tkMul,tkSlash,tkDiv,tkMod,tkAnd] do
      begin
         tok := token;
         consume(tok);

         if result.Loc.Typ in [ltSkipFalse,ltSkipTrue] then
            MakeReadable(result);

         r2 := Factor;

         if r2.Loc.Typ in [ltSkipFalse,ltSkipTrue] then
            MakeReadable(r2);

         if (result.Loc.Typ = ltConst) and
            (r2.loc.typ = ltConst) then
         begin
            case tok of
               tkMul:
                  if same(result.Typ, @TypeInteger) then
                     result.Loc.val := result.Loc.val*r2.loc.val
                  else if same(result.Typ, @TypeReal) then
                     result.Loc.val := EncodeReal(DecodeReal(result.Loc.val)*DecodeReal(r2.Loc.val))
                  else if same(result.Typ, @TypeSet) then
                     result.Loc.val := result.Loc.val and r2.loc.val
                  else
                     error('Syntax error');
               tkSlash:
                  if same(result.Typ, @TypeReal) then
                     result.Loc.val := EncodeReal(DecodeReal(result.Loc.val)/DecodeReal(r2.Loc.val))
                  else if same(result.Typ, @TypeSet) then
                     result.Loc.val := result.Loc.val xor r2.loc.val
                  else
                     error('Syntax error');
               tkDiv:
                  if same(result.Typ, @TypeInteger) then
                     result.Loc.val := result.Loc.val div r2.loc.val
                  else
                     error('Syntax error');
               tkMod:
                  if same(result.Typ, @TypeInteger) then
                     result.Loc.val := result.Loc.val mod r2.loc.val
                  else
                     error('Syntax error');
               tkAnd:
                  if same(result.Typ, @TypeBool) then
                     result.Loc.val := result.Loc.val and r2.loc.val
                  else
                     error('Syntax error');
            end;
         end
         else
         begin
            MakeWritable(result);

            case tok of
               tkMul:
                  if same(result.Typ, @TypeInteger) then
                     Instruction(A_MUL, result, r2)
                  else if same(result.Typ, @TypeReal) then
                  begin
                     MakeWritable(r2);

                     tmp := RegLoc(AllocReg);
                     Instruction(A_SET, tmp, ConstLoc(0));

                     Instruction(A_IFG, ConstLoc($8000), result);
                     l := AddJmp;
                     Instruction(A_XOR, result, ConstLoc($FFFF));
                     Instruction(A_ADD, result, ConstLoc(1));
                     Instruction(A_XOR, tmp, ConstLoc(1));
                     FixJmp(l);

                     Instruction(A_IFG, ConstLoc($8000), r2);
                     l := AddJmp;
                     Instruction(A_XOR, r2, ConstLoc($FFFF));
                     Instruction(A_ADD, r2, ConstLoc(1));
                     Instruction(A_XOR, tmp, ConstLoc(1));
                     FixJmp(l);

                     tmp2 := RegLoc(AllocReg);
                     Instruction(A_MUL, result, r2);
                     Instruction(A_SET, tmp2, RegLoc(V_O));
                     Instruction(A_SHR, result, ConstLoc(RealFractionBits));
                     Instruction(A_SHL, tmp2, ConstLoc(16-RealFractionBits));
                     Instruction(A_BOR, result, tmp2);
                     Unuse(tmp2);

                     Instruction(A_IFN, tmp,  ConstLoc(1));
                     l := AddJmp;
                     Instruction(A_XOR, result, ConstLoc($FFFF));
                     Instruction(A_ADD, result, ConstLoc(1));
                     FixJmp(l);

                     Unuse(tmp);
                  end
                  else if same(result.Typ, @TypeBool) then
                     Instruction(A_AND, result, r2)
                  else if same(result.Typ, @TypeSet) then
                     Instruction(A_AND, result, r2)
                  else
                     error('Syntax error');
               tkSlash:
                  if same(result.Typ, @TypeSet) then
                     Instruction(A_XOR, result, r2)
                  else if same(result.Typ, @TypeReal) then
                  begin
                     MakeWritable(r2);

                     tmp := RegLoc(AllocReg);
                     Instruction(A_SET, tmp, ConstLoc(0));

                     Instruction(A_IFG, ConstLoc($8000), result);
                     l := AddJmp;
                     Instruction(A_XOR, result, ConstLoc($FFFF));
                     Instruction(A_ADD, result, ConstLoc(1));
                     Instruction(A_XOR, tmp, ConstLoc(1));
                     FixJmp(l);

                     Instruction(A_IFG, ConstLoc($8000), r2);
                     l := AddJmp;
                     Instruction(A_XOR, r2, ConstLoc($FFFF));
                     Instruction(A_ADD, r2, ConstLoc(1));
                     Instruction(A_XOR, tmp, ConstLoc(1));
                     FixJmp(l);

                     tmp2 := RegLoc(AllocReg);
                     Instruction(A_DIV, result, r2);
                     Instruction(A_SET, tmp2, RegLoc(V_O));
                     Instruction(A_SHL, result, ConstLoc(RealFractionBits));
                     Instruction(A_SHR, tmp2, ConstLoc(16-RealFractionBits));
                     Instruction(A_BOR, result, tmp2);
                     Unuse(tmp2);

                     Instruction(A_IFN, tmp,  ConstLoc(1));
                     l := AddJmp;
                     Instruction(A_XOR, result, ConstLoc($FFFF));
                     Instruction(A_ADD, result, ConstLoc(1));
                     FixJmp(l);
                     Unuse(tmp);
                  end
                  else
                     error('Syntax error');
               tkDiv:
                  if same(result.Typ, @TypeInteger) then
                  begin
                     MakeWritable(r2);

                     tmp := RegLoc(AllocReg);
                     Instruction(A_SET, tmp, ConstLoc(0));

                     Instruction(A_IFG, ConstLoc($8000), result);
                     l := AddJmp;
                     Instruction(A_XOR, result, ConstLoc($FFFF));
                     Instruction(A_ADD, result, ConstLoc(1));
                     Instruction(A_XOR, tmp, ConstLoc(1));
                     FixJmp(l);

                     Instruction(A_IFG, ConstLoc($8000), r2);
                     l := AddJmp;
                     Instruction(A_XOR, r2, ConstLoc($FFFF));
                     Instruction(A_ADD, r2, ConstLoc(1));
                     Instruction(A_XOR, tmp, ConstLoc(1));
                     FixJmp(l);

                     Instruction(A_DIV, result, r2);

                     Instruction(A_IFN, tmp,  ConstLoc(1));
                     l := AddJmp;
                     Instruction(A_XOR, result, ConstLoc($FFFF));
                     Instruction(A_ADD, result, ConstLoc(1));
                     FixJmp(l);
                     Unuse(tmp);
                  end
                  else
                     error('Syntax error');
               tkMod:
                  if same(result.Typ, @TypeInteger) then
                     Instruction(A_MOD, result, r2)
                  else
                     error('Syntax error');
               tkAnd:
                  if same(result.Typ, @TypeBool) then
                  begin
                     MakeReadable(r2);
                     Instruction(A_AND, result, r2)
                  end
                  else
                     error('Syntax error');
            end;
         end;

         Unuse(r2);
      end;
   end;

   function SimpExpr: TExpr;
   var neg: Boolean;
       r2: TExpr;
       tok: TToken;
   begin
      neg := (token = tkMinus);
      if token in [tkPlus, tkMinus] then consume(token);

      result := Term;
      if neg then
      begin
         MakeReadable(result);
         if same(result.Typ, @TypeSet) then
         begin
            if result.Loc.Typ = ltConst then
            begin
               result := ConstLoc(not result.Loc.Val);
               result.Typ := @TypeSet;
            end
            else
            begin
               MakeWritable(result);
               Instruction(A_XOR, result, ConstLoc($FFFF));
            end;
         end
         else if same(result.Typ, @TypeReal) then
         begin
            if result.Loc.Typ = ltConst then
            begin
               result := ConstLoc(EncodeReal(-DecodeReal(result.Loc.Val)));
               result.Typ := @TypeReal;
            end
            else
            begin
               MakeWritable(result);
               Instruction(A_XOR, result, ConstLoc($FFFF));
               Instruction(A_ADD, result, ConstLoc(1));
            end;
         end
         else if same(result.typ, @TypeInteger) then
         begin
            if result.Loc.Typ = ltConst then
               result := ConstLoc(-result.Loc.Val)
            else
            begin
               MakeWritable(result);
               Instruction(A_XOR, result, ConstLoc($FFFF));
               Instruction(A_ADD, result, ConstLoc(1));
            end;
         end
         else
            Error('Negation operator does not apply to type');
      end;

      while token in [tkPlus, tkMinus, tkOr] do
      begin
         tok := token;
         consume(tok);

         if result.Loc.Typ in [ltSkipFalse,ltSkipTrue] then
            MakeReadable(result);

         r2 := Term;
         if r2.Loc.Typ in [ltSkipFalse,ltSkipTrue] then
            MakeReadable(r2);

         if not same(result.Typ, r2.Typ) then
            error('Simple expression requires equal types');

         if (result.Loc.Typ = ltConst) and
            (r2.Loc.Typ = ltConst) then
         begin
            case tok of
               tkPlus:
                  begin
                     if same(result.Typ, @TypeInteger) then
                        result := ConstLoc(result.Loc.Val+r2.Loc.Val)
                     else
                        result := ConstLoc(result.Loc.Val or r2.Loc.Val);
                  end;
               tkMinus:
                  begin
                     if same(result.Typ, @TypeInteger) then
                        result := ConstLoc(result.Loc.Val-r2.Loc.Val)
                     else
                        result := ConstLoc(result.Loc.Val and (not r2.Loc.Val));
                  end;
               tkOr:
                  begin
                     if not same(result.Typ, @TypeBool) then
                        Error('Expected boolean');
                     result := ConstLoc(result.Loc.Val or r2.Loc.Val);
                  end;
            end;
         end
         else
         begin
            MakeWritable(result);
            MakeReadable(r2);

            case tok of
               tkPlus:
                  begin
                     if same(result.Typ, @TypeInteger) then
                        Instruction(A_ADD, result, r2)
                     else if same(result.Typ, @TypeReal) then
                        Instruction(A_ADD, result, r2)
                     else if same(result.Typ, @TypeSet) then
                        Instruction(A_BOR, result, r2)
                     else
                        error('Type error');
                  end;
               tkMinus:
                  begin
                     if same(result.Typ, @TypeInteger) then
                        Instruction(A_SUB, result, r2)
                     else if same(result.Typ, @TypeReal) then
                        Instruction(A_SUB, result, r2)
                     else if same(result.Typ, @TypeSet) then
                     begin
                        MakeWritable(r2);
                        Instruction(A_XOR, r2, ConstLoc($FFFF));
                        Instruction(A_AND, result, r2);
                     end
                     else
                        error('Type error');
                  end;
               tkOr:
                  begin
                     if not same(result.Typ, @TypeBool) then
                        Error('Expected boolean');
                     Instruction(A_BOR, result, r2);
                  end;
            end;
         end;

         Unuse(r2);
      end;
   end;

   function Expression: TExpr;
   var tok: TToken;
       r2: TExpr;
   begin
      result := SimpExpr;

      if token in [tkEqual, tkNEqual, tkLess, tkLEqual, tkGreater, tkGEqual, tkIn] then
      begin
         tok := token;
         consume(tok);

         if same(result.typ, @TypeBool) then
            MakeReadable(result);

         r2 := SimpExpr;

         if (tok = tkIn) and
            (not same(r2.typ, @TypeSet)) and
            (not same(result.typ, @TypeInteger)) then
            Error('IN operator works on sets')
         else if (tok <> tkIn) and
                 (not Same(result.Typ, r2.Typ)) and
                 (not same(r2.typ, @TypeNil)) and
                 (not same(Result.typ, @TypeNil)) then
            Error('Cannot compare types');

         if same(r2.Typ, @TypeNil) then
         begin
            if result.typ^.TypeKind <> typPointer then
               error('Can only compare NIL to pointer type');

            MakeReadable(result);

            if tok = tkEqual then
               Instruction(A_IFE, result, ConstLoc(0))
            else if tok = tkNEqual then
               Instruction(A_IFN, result, ConstLoc(0))
            else
               Error('Operator not applicable to pointer type');
         end
         else if same(result.Typ, @TypeNil) then
         begin
            if r2.typ^.TypeKind <> typPointer then
               error('Can only compare NIL to pointer type');

            if tok = tkEqual then
               Instruction(A_IFE, r2, ConstLoc(0))
            else if tok = tkNEqual then
               Instruction(A_IFN, r2, ConstLoc(0))
            else
               Error('Operator not applicable to pointer type');
         end
         else if same(r2.Typ, @TypeBool) then
         begin
            MakeReadable(r2);

            if tok = tkEqual then
               Instruction(A_IFE, result, r2)
            else if tok = tkNEqual then
               Instruction(A_IFN, result, r2)
            else
               Error('Operator not applicable to boolean');
         end
         else if same(result.Typ, @TypeSet) then
         begin
            if tok = tkEqual then
               Instruction(A_IFE, result, r2)
            else if tok = tkNEqual then
               Instruction(A_IFN, result, r2)
            else
               Error('Operator not implemented');
         end
         else if same(result.Typ, @TypeInteger) or
                 Same(Result.Typ, @TypeReal) or
                 Same(Result.Typ, @TypeLongReal) then
         begin
            MakeReadable(result);
            MakeReadable(r2);

            if tok = tkEqual then
               Instruction(A_IFE, result, r2)
            else if tok = tkNEqual then
               Instruction(A_IFN, result, r2)
            else if tok = tkLess then
            begin
               MakeWritable(result);
               Instruction(A_SUB, result, r2);
               Instruction(A_IFB, result, ConstLoc($8000));
            end
            else if tok = tkLEqual then
            begin
               MakeWritable(result);
               Instruction(A_SUB, result, r2);
               Instruction(A_SUB, result, ConstLoc(1));
               Instruction(A_IFB, result, ConstLoc($8000));
            end
            else if tok = tkGreater then
            begin
               MakeWritable(r2);
               Instruction(A_SUB, r2, result);
               Instruction(A_IFB, r2, ConstLoc($8000));
            end
            else if tok = tkGEqual then
            begin
               MakeWritable(r2);
               Instruction(A_SUB, r2, result);
               Instruction(A_SUB, r2, ConstLoc(1));
               Instruction(A_IFB, r2, ConstLoc($8000));
            end
            else if tok = tkIn then
            begin
               MakeReadable(result);
               MakeWritable(r2);

               Instruction(A_SHR, r2, result);
               Instruction(A_IFB, r2, ConstLoc(1));
            end
            else
               Error('Error while comparing');
         end
         else
            Error('Error while comparing');

         Unuse(r2, true);

         result.Typ := @TypeBool;
         result.Loc.Typ := ltSkipFalse;
      end;
   end;

   procedure ReturnStatement(funcdecl: PType);
   var r: TExpr;
       i: longint;
   begin
      for i := 0 to high(regs) do regs[i] := false;
      Unspill(0);

      regs[0] := true;
      r := Expression;

      if not same(r.Typ, funcdecl^.FuncRet) then error('Return statement does not match function return type');

      Instruction(A_SET, RegLoc(0), r);
   end;

   procedure Statements; forward;

   procedure IfStatement;
   var lEnds: array of word;
       i: longint;

      procedure AddEnd;
      begin
         setlength(lEnds, high(lEnds)+2);
         lEnds[high(lEnds)] := AddJmp;
      end;

      procedure CondStatements;
      var c: TExpr;
          lTrue, lFalse: word;
          i: longint;
      begin
         c := Expression;
         if not Same(c.Typ, @TypeBool) then error('Expression must be boolean');

         Consume(tkThen);

         if not (c.Loc.Typ in [ltSkipFalse, ltSkipTrue]) then
         begin
            MakeReadable(c);
            Instruction(A_IFN, c, ConstLoc(1));
            Unuse(c, true);
            c.Loc.Typ := ltSkipTrue;
         end;

         if c.Loc.Typ = ltSkipFalse then
         begin
            {
               SET PC, True
               SET PC, False
            True
               {true}
               SET PC, End
            False
               ...
            End
            }

            lTrue := AddJmp;
            lFalse := AddJmp;
            FixJmp(lTrue);

            { true}
            for i := 0 to high(regs) do regs[i] := false;
            Unspill(0);

            Statements;
            AddEnd;

            FixJmp(lFalse);
         end
         else
         begin
            {
               SET PC, False
               SET PC, True
            True
               {true}
               SET PC, End
            False
               ...
            End
            }

            lFalse := AddJmp;

            { true}
            for i := 0 to high(regs) do regs[i] := false;
            Unspill(0);

            Statements;
            AddEnd;

            FixJmp(lFalse);
         end;
      end;

   begin
      Consume(tkIf);

      CondStatements;

      while token = tkElsif do
      begin
         Consume(tkElsif);

         CondStatements;
      end;

      if token = tkElse then
      begin
         Consume(tkElse);
         Statements;
      end;

      Consume(tkEnd);

      for i := 0 to high(lEnds) do
         FixJmp(lEnds[i]);
   end;

   procedure WhileStatement;
   var start: word;
       i: longint;

      procedure CondStatements(AStart: word);
      var c: TExpr;
          lTrue, lFalse: word;
          i: longint;
      begin
         c := Expression;
         if not Same(c.Typ, @TypeBool) then error('Expression must be boolean');

         Consume(tkDo);

         if not (c.Loc.Typ in [ltSkipFalse, ltSkipTrue]) then
         begin
            MakeReadable(c);
            Instruction(A_IFN, c, ConstLoc(1));
            Unuse(c, true);
            c.Loc.Typ := ltSkipTrue;
         end;

         if c.Loc.Typ = ltSkipFalse then
         begin
            {
               SET PC, True
               SET PC, False
            True
               {true}
               SET PC, AStart
            False
               ...
            End
            }

            lTrue := AddJmp;
            lFalse := AddJmp;
            FixJmp(lTrue);

            { true}
            for i := 0 to high(regs) do regs[i] := false;
            Unspill(0);

            Statements;
            Instruction(A_SET, RegLoc(V_PC), ConstLoc(AStart));

            FixJmp(lFalse);
         end
         else
         begin
            {
               SET PC, False
               {true}
               SET PC, AStart
            False
               ...
            End
            }

            lFalse := AddJmp;

            { true}
            for i := 0 to high(regs) do regs[i] := false;
            Unspill(0);

            Statements;
            Instruction(A_SET, RegLoc(V_PC), ConstLoc(AStart));

            FixJmp(lFalse);
         end;
      end;

   begin
      Consume(tkWhile);

      start := codeCounter;

      CondStatements(start);

      while token = tkElsif do
      begin
         Consume(tkElsif);

         CondStatements(start);
      end;

      Consume(tkEnd);
   end;

   procedure ForStatement;
   var sym: PSymbol;
       d, d2, beg, lim: TExpr;
       by, l, tJmp, fJmp: Word;
       i: longint;
   begin
      consume(tkFor);
      d := Designator(true, sym);

      if not same(d.typ, @TypeInteger) then Error('FOR loop iterator variable must be integer');

      consume(tkAssign);

      beg := Expression;

      if not same(beg.typ, @TypeInteger) then Error('FOR loop iterator bounds must be integer');

      MakeLWritable(d);
      Instruction(A_SET, d, beg);
      Unuse(beg);

      Consume(tkTo);

      lim := Expression;

      if not same(lim.typ, @TypeInteger) then Error('FOR loop iterator bounds must be integer');

      by := 1;
      if token = tkBy then
      begin
         Consume(tkBy);
         if not same(ConstExpr(by), @TypeInteger) then error('BY value must be integer');
      end;

      Consume(tkDo);

      if by = 0 then error('FOR loop increment cannot be 0')
      else if by < $8000 then
      begin
         l := codeCounter;

         {for i := 0 to high(regs) do regs[i] := false;
         Unspill;}

         d2 := d;
         MakeWritable(d2);

         Instruction(A_SUB, d2, lim);
         Instruction(A_SUB, d2, ConstLoc(1));
         Instruction(A_IFB, d2, ConstLoc($8000));
         Unuse(d2, true);
         tJmp := AddJmp;
         fJmp := AddJmp;
         FixJmp(tJmp);

         Statements;

         Instruction(A_ADD, d, ConstLoc(by));

         Instruction(A_SET, RegLoc(V_PC), ConstLoc(l));

         FixJmp(fJmp);
      end
      else
      begin
         l := codeCounter;

         {for i := 0 to high(regs) do regs[i] := false;
         Unspill;}

         d2 := d;
         MakeWritable(d2);

         Instruction(A_SUB, d2, lim);
         Instruction(A_IFB, d2, ConstLoc($8000));
         Unuse(d2, true);
         fJmp := AddJmp;

         Statements;

         Instruction(A_ADD, d, ConstLoc(by));

         Instruction(A_SET, RegLoc(V_PC), ConstLoc(l));

         FixJmp(fJmp);
      end;

      Unuse(lim);

      Consume(tkEnd);
   end;

   function Selector(LValue: boolean; Typ: TExpr): TExpr;
   var t: String;
       ofs: word;
       r: TExpr;
       newt: PType;
       l: word;
   begin
      case token of
         tkDot:
            begin
               Consume(tkDot);

               if Typ.Loc.Typ = ltParamRef then
                  MakeReadable(typ);

               if assigned(typ.Typ) and
                  (typ.Typ^.TypeKind = typPointer) then
               begin
                  // "Dereference"
                  newt := typ.typ^.BaseType;
                  typ := MakeAddr(typ);
                  typ.Typ := newt;
               end;

               t := ident;
               if (not assigned(typ.Typ)) or
                  (not IsRecord(typ.typ)) or
                  (not HasField(typ.typ, t)) then
                  Error('Type is not a record, or does not have field "'+t+'"');

               result := typ;
               result.Typ := FindField(typ.typ, t, ofs);
               result := AddOffset(result, ofs);
            end;
         tkLSquare:
            begin
               Consume(tkLSquare);

               r := Expression;
               if not Same(r.Typ, @TypeInteger) then Error('Array index must be integer');
               result := ToAddr(Typ);
               MakeWritable(r);
               if TypeSize(typ.Typ^.BaseType) <> 1 then
                  Instruction(A_MUL, r, ConstLoc(TypeSize(typ.Typ^.BaseType)));
               Instruction(A_ADD, r, result);
               result := r;
               result.Typ := typ.Typ^.BaseType;

               while token = tkComma do
               begin
                  Consume(tkComma);

                  r := Expression;
                  if not Same(r.Typ, @TypeInteger) then Error('Array index must be integer');
                  if TypeSize(result.Typ^.BaseType) <> 1 then
                  begin
                     MakeWritable(r);
                     Instruction(A_MUL, r, ConstLoc(TypeSize(result.Typ^.BaseType)));
                  end
                  else
                     MakeReadable(r);
                  Instruction(A_ADD, result, r);
                  result.Typ := result.Typ^.BaseType;
               end;

               Consume(tkRSquare);

               result := MakeAddr(result);
            end;
         tkDeref:
            begin
               Consume(tkDeref);

               if assigned(typ.Typ) and
                  (typ.Typ^.TypeKind = typPointer) then
               begin
                  // "Dereference"
                  newt := typ.typ^.BaseType;
                  typ := MakeAddr(typ);
                  typ.Typ := newt;
               end
               else
                  error('Cannot dereference non-pointer');

               result := typ;
            end;
         tkLParan:
            begin
               Consume(tkLParan);
               newt := GetType(QualIdent);

               if not (assigned(typ.Typ) and
                       (typ.Typ^.TypeKind = typPointer)) then
                  error('Typeguards not implemented for non-pointer types');

               if not Extends(newt, result.Typ) then
                  error('Typeguard is not applicable to type');

               Instruction(A_IFN, GetAddr(result), ConstLoc(0));
               l := AddJmp;

               Instruction(A_SET, PushLoc, ConstLoc(0));
               Instruction(A_SET, PushLoc, ConstLoc(AC_NIL_POINTER));
               ExtInstruction(A_JSR, RefLoc(NR_ASSERT));
               Instruction(A_ADD, RegLoc(V_SP), ConstLoc(2));

               FixJmp(l);

               Instruction(A_IFN, result, ConstLoc(GetRecordId(newt)));
               l := AddJmp;

               Instruction(A_SET, PushLoc, ConstLoc(0));
               Instruction(A_SET, PushLoc, ConstLoc(AC_TYPEGUARD));
               ExtInstruction(A_JSR, RefLoc(NR_ASSERT));
               Instruction(A_ADD, RegLoc(V_SP), ConstLoc(2));

               FixJmp(l);

               result.Typ := newt;

               Consume(tkRParan);
            end;
      else
         error('Not implemented');
      end;
   end;

   function Designator(LValue: boolean; var sym: PSymbol): TExpr;
   var s: PSymbol;
   begin
      s := QualIdent;

      sym := s;

      if s^.typ = stInbFunc then
      begin
         result.Typ := nil;
         result.Loc.typ := ltConst;
         result.Loc.Val := 0;
         exit;
      end
      else if s^.typ = stFunc then
      begin
         result.Loc.Typ := ltConst;
         result.loc.val := s^.ofs;
      end
      else if s^.typ = stVar then
      begin
         if s^.local then
         begin
            result.loc.Typ := ltVar;
            result.Loc.Offset := s^.ofs;
         end
         else
         begin
            result.Loc.Typ := ltGlobVar;
            result.Loc.Addr := s^.ofs;
         end;
      end
      else if s^.typ = stConst then
      begin
         result.Loc.Typ := ltConst;
         result.Loc.val := s^.val;
      end
      else if s^.typ = stParam then
      begin
         if s^.byref then
         begin
            result.Loc.Typ := ltParamRef;
            result.Loc.Offset := s^.ofs;
         end
         else
         begin
            result.Loc.Typ := ltParam;
            result.Loc.Offset := s^.ofs;
         end;
      end;

      result.Typ := s^.symType;

      if s^.typ = stInbFunc then exit;

      while token in [tkDot, tkLSquare, tkDeref, tkLParan] do
      begin
         if (token = tkLParan) and
            ((assigned(result.typ) and
             (result.Typ^.TypeKind = typFunc)) or
             (s^.typ = stFunc)) then
            break;
         result := Selector(LValue, result);
      end;
   end;

   procedure RepeatStatement;
   var expr: TExpr;
       l, l2: Word;
   begin
      consume(tkRepeat);

      l := codeCounter;

      Statements;

      consume(tkUntil);
      expr := Expression;

      if not same(expr.typ, @TypeBool) then
         error('Repeat until requires boolean expression');

      if not (expr.Loc.Typ in [ltSkipFalse, ltSkipTrue]) then
      begin
         MakeReadable(expr);
         Instruction(A_IFE, expr, ConstLoc(0));
         expr.Loc.Typ := ltSkipFalse;
      end;

      if expr.Loc.Typ = ltSkipFalse then
      begin
         {
         IFE r, 0
         SET PC, loop
         }
         Instruction(A_SET, RegLoc(V_PC), ConstLoc(l));
      end
      else
      begin
         {
         IFE r, 1
         SET PC, exit
         SET PC, loop
         }
         l2 := AddJmp;
         Instruction(A_SET, RegLoc(V_PC), ConstLoc(l));
         FixJmp(l2);
      end;
   end;

   procedure CaseStatement;
   begin


      {Consume(tkCase);
      expr := Expression;
      Consume(tkOf);



      Consume(tkEnd); }
   end;

   procedure Statement;
   var d, expr: TExpr;
       i, cnt: longint;
       r: longword;
       l,l2: word;
       sym, sym2: PSymbol;
   begin
      if token = tkIdent then
      begin
         d := Designator(true, sym);

         if assigned(sym) and
            (sym^.typ = stInbFunc) then
         begin
            Consume(tkLParan);

            case TInbuiltFunc(sym^.val) of
               inbInc:
                  begin
                     d := Designator(true, sym);

                     if not (sym^.typ in [stVar, stParam]) then
                        error('Cannot increment non-variable symbol');

                     expr := ConstLoc(1);
                     if token = tkComma then
                     begin
                        consume(tkComma);
                        expr := Expression;
                     end;

                     MakeLWritable(d);

                     Instruction(A_ADD, d, expr);
                  end;
               inbDec:
                  begin
                     d := Designator(true, sym);

                     if not (sym^.typ in [stVar, stParam]) then
                        error('Cannot decrement non-variable symbol');

                     expr := ConstLoc(1);
                     if token = tkComma then
                     begin
                        consume(tkComma);
                        expr := Expression;
                     end;

                     MakeLWritable(d);

                     Instruction(A_SUB, d, expr);
                  end;
               inbIncl:
                  begin
                     d := Designator(true, sym);

                     if not (sym^.typ in [stVar, stParam]) then
                        error('Cannot INCL non-variable set');

                     consume(tkComma);
                     expr := Expression;

                     if expr.Loc.Typ = ltConst then
                        expr := ConstLoc(1 shl expr.Loc.Val)
                     else
                     begin
                        i := AllocReg;
                        Instruction(A_SET, RegLoc(i), ConstLoc(1));
                        Instruction(A_SHL, RegLoc(i), expr);
                        expr := RegLoc(i);
                     end;

                     MakeLWritable(d);

                     Instruction(A_BOR, d, expr);
                  end;
               inbExcl:
                  begin
                     d := Designator(true, sym);

                     if not (sym^.typ in [stVar, stParam]) then
                        error('Cannot EXCL non-variable set');

                     consume(tkComma);
                     expr := Expression;

                     if expr.Loc.Typ = ltConst then
                        expr := ConstLoc(not (1 shl expr.Loc.Val))
                     else
                     begin
                        i := AllocReg;
                        Instruction(A_SET, RegLoc(i), ConstLoc(1));
                        Instruction(A_SHL, RegLoc(i), expr);
                        Instruction(A_XOR, RegLoc(i), ConstLoc($FFFF));
                        expr := RegLoc(i);
                     end;

                     MakeLWritable(d);

                     Instruction(A_AND, d, expr);
                  end;
               inbCopy:
                  begin
                     r := SpillUsed;

                     expr := Expression;

                     consume(tkComma);
                     d := Designator(true, sym);

                     if not (sym^.typ in [stVar, stParam]) then
                        error('Cannot COPY to non-variable symbol');

                     Instruction(A_SET, PushLoc, GetAddr(d));
                     Instruction(A_SET, PushLoc, expr);

                     ExtInstruction(A_JSR, RefLoc(NR_COPY));

                     Instruction(A_ADD, RegLoc(V_SP), ConstLoc(2));

                     Recover(r);
                  end;
               inbNew:
                  begin
                     r := SpillUsed;

                     d := Designator(true, sym);

                     if not (sym^.typ in [stVar, stParam]) then
                        error('Cannot COPY to non-variable symbol');

                     if d.Typ^.TypeKind <> typPointer then Error('Can only allocate a pointer to a record');

                     Instruction(A_SET, PushLoc, GetAddr(d));
                     Instruction(A_SET, PushLoc, ConstLoc(TypeSize(d.Typ^.BaseType)));
                     Instruction(A_SET, PushLoc, ConstLoc(GetRecordId(d.Typ^.BaseType)));

                     ExtInstruction(A_JSR, RefLoc(NR_NEW));

                     Instruction(A_ADD, RegLoc(V_SP), ConstLoc(3));

                     recover(r);
                  end;
               inbAssert:
                  begin
                     expr := Expression;

                     MakeReadable(expr);

                     Instruction(A_IFE, expr, ConstLoc(1));
                     l := AddJmp;

                     if token = tkComma then
                     begin
                        Consume(tkComma);
                        d := Expression;
                     end
                     else
                        d := ConstLoc(0);

                     Instruction(A_SET, PushLoc, d);
                     unuse(d);
                     Instruction(A_SET, PushLoc, ConstLoc(AC_ASSERTION));
                     ExtInstruction(A_JSR, RefLoc(NR_ASSERT));
                     Instruction(A_ADD, RegLoc(V_SP), ConstLoc(2));

                     FixJmp(l);
                  end;
               inbPack:
                  begin
                     d := Designator(true, sym);
                     Consume(tkComma);
                     expr := Expression;

                     if not Same(expr.Typ, @TypeInteger) then error('Second arg of PACK must be integer');

                     if Same(d.Typ, @TypeReal) then
                     begin
                        error('Not implemented');
                     end
                     else
                        Error('Type error');
                  end;
               inbUnpack:
                  begin
                     d := Designator(true, sym);
                     Consume(tkComma);
                     expr := Designator(true, sym2);

                     if not Same(expr.Typ, @TypeInteger) then error('Second arg of PACK must be integer');

                     if Same(d.Typ, @TypeReal) then
                     begin
                        error('Not implemented');
                     end
                     else
                        Error('Type error');
                  end;
               inbChunkMove:
                  begin

                  end;
               inbPut:
                  begin
                     expr := Expression;
                     Consume(tkComma);
                     d := Expression;

                     MakeReadable(d);
                     MakeReadable(expr);
                     Instruction(A_SET, MakeAddr(expr), d);
                  end;
               inbGet:
                  begin
                     expr := Expression;
                     Consume(tkComma);
                     d := Designator(true, sym);

                     MakeLWritable(d);
                     MakeReadable(expr);
                     Instruction(A_SET, d, MakeAddr(expr));
                  end;
            else
               Error('Proper procedure called');
            end;

            Consume(tkRParan);
         end
         else if token = tkAssign then
         begin
            consume(tkAssign);
            expr := Expression;

            if d.Typ^.TypeKind = typPointer then
            begin
               if (not Extends(expr.Typ, d.Typ^.BaseType)) and
                  (not ((expr.Typ^.TypeKind = typPointer) and
                        (Extends(expr.Typ^.BaseType, d.Typ^.BaseType)))) and
                  (not same(expr.Typ, @TypeNil)) then
                  Error('Right hand side does not extend record type of left hand side pointer');

               MakeLWritable(d);

               if expr.Typ^.TypeKind = typPointer then
               begin
                  MakeReadable(expr);

                  Instruction(A_SET, d, expr)
               end
               else
               begin
                  Instruction(A_SET, d, GetAddr(expr));
               end;
            end
            else if d.typ^.TypeKind = typBasic then
            begin
               if not same(d.Typ, expr.Typ) then
                  Error('Cannot assign expression to left hand side value');

               MakeLWritable(d);
               MakeReadable(expr);

               Instruction(A_SET, d, expr);
            end
            else if d.typ^.TypeKind = typFunc then
            begin
               {if not same(d.Typ, expr.Typ) then
                  Error('Cannot assign expression to left hand side value');}

               MakeLWritable(d);
               MakeReadable(expr);

               Instruction(A_SET, d, expr);
            end
            else
               error('Cannot assign non-basic types');
         end
         else
         begin
            Consume(tkLParan);

            r := SpillUsed;

            i := 0;
            cnt := 0;

            if token <> tkRParan then
            begin
               if length(sym^.symType^.Fields) <= 0 then
                  error('Function does not take any parameters');

               if IsByref(sym^.symType^.Fields[i]) then
               begin
                  expr := Expression;

                  if sym^.symType^.Fields[i].Typ^.TypeKind = typOpenArray then
                  begin
                     Instruction(A_SET, PushLoc, GetLen(expr));
                     inc(cnt);
                  end;

                  Instruction(A_SET, PushLoc, GetAddr(expr))
               end
               else
               begin
                  expr := Expression;

                  if not AssignableTo(sym^.symType^.Fields[i].Typ, expr.typ) then
                     Error('Wrong type in argument');

                  MakeReadable(expr);
                  Instruction(A_SET, PushLoc, expr);
               end;
               inc(i);
               inc(cnt);

               while token = tkComma do
               begin
                  consume(tkComma);

                  if length(sym^.symType^.Fields) <= i then
                     error('Too many parameters');

                  if IsByref(sym^.symType^.Fields[i]) then
                  begin
                     expr := Expression;

                     if sym^.symType^.Fields[i].Typ^.TypeKind = typOpenArray then
                     begin
                        Instruction(A_SET, PushLoc, GetLen(expr));
                        inc(cnt);
                     end;

                     Instruction(A_SET, PushLoc, GetAddr(expr))
                  end
                  else
                  begin
                     expr := Expression;

                     if not same(sym^.symType^.Fields[i].Typ, expr.typ) then
                        Error('Wrong type in argument');

                     MakeReadable(expr);
                     Instruction(A_SET, PushLoc, expr);
                  end;
                  inc(i);
                  inc(cnt);
               end;

               if i < length(sym^.symType^.Fields) then
                  writeln('Expected more parameters');
            end;

            ExtInstruction(A_JSR, d);

            if cnt > 0 then Instruction(A_ADD, RegLoc(V_SP), ConstLoc(cnt)); // Cleanup

            consume(tkRParan);

            Recover(r);
         end;
      end
      else if token = tkIf then
         IfStatement
      else if token = tkWhile then
         WhileStatement
      else if token = tkCase then
         CaseStatement
      else if token = tkRepeat then
         RepeatStatement
      else if token = tkFor then
         ForStatement;

      for i := 0 to high(regs) do regs[i] := false;
      Unspill(0);
   end;

   procedure Statements;
   begin
      Statement;
      while token = tkSemiColon do
      begin
         consume(tkSemiColon);
         statement;
      end;
   end;

   function VarDecl(var VarOffs: word; Global: boolean): PType;
   var n: PSymbol;
   begin
      n := IdentDef;
      n^.typ := stVar;

      if token = tkColon then
      begin
         consume(tkColon);
         result := _Type(n^.typDecl);
         n^.symType := result;
      end
      else
      begin
         consume(tkComma);
         result := VarDecl(VarOffs, Global);
         n^.symType := result;
      end;

      n^.local := not global;

      if global then
      begin
         n^.ofs := AllocateData(TypeSize(n^.symType));
      end
      else
      begin
         dec(VarOffs, TypeSize(n^.symType));
         n^.ofs := varoffs;
      end;

      syms.AddSymbol(n);
   end;

   procedure Decls(var vars: word; Global: boolean);
   var id, sym: PSymbol;
       x: TSymTable;
       varCnt,
       paramOfs: word;
       lowValue, i: longint;

      procedure RegisterParam(const name: string; isvar: boolean; typ: PType; var OutOfs: longint);
      var p: PSymbol;
      begin
         new(p);
         p^.name := name;
         p^.exported := false;
         p^.typ := stParam;
         p^.local := true;
         p^.ofs := paramOfs;
         outOfs := paramOfs;
         if typ^.TypeKind = typOpenArray then inc(paramOfs); // Implicitly pass length
         inc(paramOfs);
         p^.symType := typ;
         p^.byref := isvar or (typ^.TypeKind in [typRecord, typArray, typOpenArray]);
         syms.AddSymbol(p);
      end;

   begin
      if token = tkConst then
      begin
         consume(tkConst);

         while token = tkIdent do
         begin
            id := IdentDef;
            id^.typ := stConst;
            Consume(tkEqual);
            id^.symType := ConstExpr(id^.val);
            consume(tkSemiColon);

            syms.AddSymbol(id);
         end;
      end;

      if token = tkType then
      begin
         consume(tkType);

         lowValue := length(syms.symbols);

         while token = tkIdent do
         begin
            id := IdentDef;
            id^.typ := stType;
            Consume(tkEqual);
            id^.typDecl := structype;
            id^.symType := id^.typDecl;
            consume(tkSemiColon);

            syms.AddSymbol(id);
         end;

         for i := lowValue to high(syms.symbols) do
         begin
            if (syms.symbols[i]^.symType^.TypeKind = typPointer) and
               (not assigned(syms.symbols[i]^.symType^.BaseType)) then
            begin
               sym := syms.FindSymbol(syms.symbols[i]^.symType^.ForwardType);
               if not assigned(sym) then
                  error('Forward pointer type declaration not resolved: '+syms.symbols[i]^.name);

               syms.symbols[i]^.symType^.BaseType := GetType(sym);
            end;
         end;
      end;

      if token = tkVar then
      begin
         consume(tkVar);

         while token = tkIdent do
         begin
            VarDecl(vars, Global);
            consume(tkSemiColon);
         end;
      end;

      while token = tkProcedure do
      begin
         Consume(tkProcedure);
         id := IdentDef;

         for i := 0 to high(regs) do regs[i] := false;
         SpillCnt := 0;

         id^.typ := stFunc;
         id^.symType := nil;
         if token = tkLParan then
            id^.symType := FormalParameters
         else
         begin
            new(id^.symType);
            id^.symType^.TypeKind := typFunc;
            id^.symType^.FuncRet := nil;
         end;

         consume(tkSemiColon);

         id^.typDecl := id^.symType;
         id^.symtab := TSymTable.Create(syms);
         id^.ofs := codeCounter;

         syms.AddSymbol(id);

         x := syms;
         syms := id^.symtab;

         if assigned(id^.symType) then
         begin
            paramOfs := 0;

            if assigned(id^.symType) then
               for i := high(id^.symType^.Fields) downto 0 do
                  RegisterParam(id^.symType^.Fields[i].Name, id^.symType^.Fields[i].IsVar, id^.symType^.Fields[i].Typ, id^.symType^.Fields[i].Ofs);
         end;

         varCnt := 0;
         Decls(varCnt, false);

         EnterProc(varCnt);

         for i := 0 to high(id^.symtab.symbols) do
            if (id^.symtab.symbols[i]^.typ = stVar) and
               (id^.symtab.symbols[i]^.symType^.TypeKind = typPointer) then
               Instruction(A_SET, VarLoc(id^.symtab.symbols[0]^.ofs), ConstLoc(0));

         if token = tkbegin then
         begin
            Consume(tkBegin);
            Statements;
         end;

         if token = tkReturn then
         begin
            Consume(tkReturn);
            ReturnStatement(id^.symType);
         end;

         consume(tkEnd);
         consume(tkIdent);
         consume(tkSemiColon);

         syms := x;

         LeaveProc;
      end;
   end;

   procedure Import;
   var n,m: string;
       s: PSymbol;
   begin
      n := Ident;
      m := n;
      if token = tkAssign then
      begin
         consume(tkAssign);
         m := ident;
      end;

      s := DoCompile(m);
      s^.Name := n;
      s^.exported := false;
      syms.AddSymbol(s);
   end;

   procedure ImportList;
   begin
      Consume(tkImport);
      Import;
      while token = tkComma do
      begin
         consume(tkComma);
         Import;
      end;
      Consume(tkSemiColon);
   end;

   procedure Module;
   var varCnt: word;
       i: longint;
       name: String;
   begin
      consume(tkModule);
      name := ident;
      consume(tkSemiColon);

      if Token = tkImport then
         ImportList;

      writeln('Compiling: ', name);

      Decls(varCnt, true);

      if token = tkBegin then
      begin
         for i := 0 to high(regs) do regs[i] := false;
         for i := 0 to high(spills) do spills[i] := 0;
         spillCnt := 0;

         Consume(tkBegin);
         AddInit(codeCounter);
         Statements;
         Instruction(A_SET, RegLoc(V_PC), PopLoc);
      end;

      consume(tkEnd);
      consume(tkIdent);
      consume(tkDot);
   end;

var i: longint;
begin
   for i := 0 to high(regs) do regs[i] := false;
   for i := 0 to high(spills) do spills[i] := 0;
   spillCnt := 0;

   fs := TFileStream.Create(AName+'.m', fmOpenRead);
   sc := TScanner.Create(fs);
   syms := TSymTable.Create(globals);

   new(sym);
   sym^.name := uppercase(AName);
   sym^.exported := true;
   sym^.typ := stModule;
   sym^.symtab := syms;

   try
      Module;

      //syms.CullPrivate;
   finally
      sc.free;
      fs.free;
   end;

   result := sym;
end;

procedure Dump(s: PSymbol); forward;

procedure DumpSyms(s: TSymTable);
var i: longint;
begin
   for i := 0 to high(s.symbols) do
   begin
      write(' ');
      dump(s.symbols[i]);
   end;
end;

procedure Dump(s: PSymbol);
begin
   if not assigned(s) then exit;

   if s^.typ = stModule then
   begin
      writeln('MODULE: ', s^.name);
      DumpSyms(s^.symtab);
   end
   else if s^.typ = stConst then
      writeln(s^.name,': ',s^.symType^.SimpleKind,' = ', s^.val)
   else if s^.typ = stType then
      writeln(s^.name,' = Type')
   else if s^.typ = stVar then
      writeln(s^.name,' = Var(',inttohex(s^.ofs,4),')')
   else if s^.typ = stFunc then
      writeln(s^.name,' = func(',inttohex(s^.ofs,4),')')
   else
      writeln;
end;

function GetSystemModule: PSymbol;
begin
   new(result);
   result^.name := 'SYSTEM';
   result^.typ := stModule;
   result^.symtab := TSymTable.Create(nil);

   result^.symtab.AddSymbol(InbFunc('ADR',  inbAdr));
   result^.symtab.AddSymbol(InbFunc('SIZE', inbSize));
   result^.symtab.AddSymbol(InbFunc('BIT',  inbBit));
   result^.symtab.AddSymbol(InbFunc('MULTSHIFT',  inbMultShift));

   result^.symtab.AddSymbol(InbFunc('MOVE', inbChunkMove));
   result^.symtab.AddSymbol(InbFunc('GET', inbGet));
   result^.symtab.AddSymbol(InbFunc('PUT', inbPut));
end;

var i, cnt: longint;
    startAddr: word;
    fs: TStream;
    s: string;
begin
   mods := TSymTable.Create(nil);
   globals := TSymTable.Create(nil);

   globals.AddSymbol(TypSym('INTEGER',TypeInteger));
   globals.AddSymbol(TypSym('CHAR',TypeChar));
   globals.AddSymbol(TypSym('BOOLEAN',TypeBool));
   globals.AddSymbol(TypSym('SET',TypeSet));
   globals.AddSymbol(TypSym('REAL',TypeReal));
   globals.AddSymbol(TypSym('LONGREAL',TypeLongReal));

   globals.AddSymbol(InbFunc('INC', inbInc));
   globals.AddSymbol(InbFunc('DEC', inbDec));
   globals.AddSymbol(InbFunc('INCL', inbIncl));
   globals.AddSymbol(InbFunc('EXCL', inbExcl));
   globals.AddSymbol(InbFunc('COPY', inbCopy));
   globals.AddSymbol(InbFunc('NEW', inbNew));
   globals.AddSymbol(InbFunc('ASSERT', inbAssert));
   globals.AddSymbol(InbFunc('PACK', inbPack));
   globals.AddSymbol(InbFunc('UNPK', inbUnpack));

   globals.AddSymbol(InbFunc('ABS', inbAbs));
   globals.AddSymbol(InbFunc('ODD', inbOdd));
   globals.AddSymbol(InbFunc('LEN', inbLen));
   globals.AddSymbol(InbFunc('LSL', inbLsl));
   globals.AddSymbol(InbFunc('LSR', inbLsr));
   globals.AddSymbol(InbFunc('ASR', inbAsr));
   globals.AddSymbol(InbFunc('ROR', inbRor));

   globals.AddSymbol(InbFunc('FLOOR', inbFloor));
   globals.AddSymbol(InbFunc('FLT', inbFlt));
   globals.AddSymbol(InbFunc('ORD', inbOrd));
   globals.AddSymbol(InbFunc('CHR', inbChr));
   globals.AddSymbol(InbFunc('LONG', inbLong));
   globals.AddSymbol(InbFunc('SHORT', inbShort));

   mods.AddSymbol(GetSystemModule);

   try
      if Paramcount < 1 then
      begin
         Writeln('Please specify filename');
      end
      else
         DoCompile(ParamStr(1));

      startAddr := codeCounter;

      for i := 0 to initCnt-1 do
         ExtInstruction(A_JSR, ConstLoc(initFuncs[i]));
      Instruction(A_SUB, RegLoc(V_PC), ConstLoc(1));

      for i := 0 to SystemFuncs-1 do code[i+InitSize] := startAddr;
      code[NR_HEAP_Start] := codeCounter;

      writeln('Code used(words): ', codeCounter);
      writeln('Data used(words): ', $8000-dataCounter);

      codeCounter := 0;
      Instruction(A_SET, RegLoc(V_SP), ConstLoc(0));
      Instruction(A_SET, RegLoc(V_PC), RefLoc(NR_RESET));

      code[NR_HEAP_End] := dataCounter-1;
   except
      on e: exception do
      begin
         writeln(e.message);
         readln;
      end;
   end;

   for i := $FFFF downto 0 do
      if code[i] <> 0 then
      begin
         cnt := i+1;
         break;
      end;

   fs := TFileStream.Create('mem.dcpu16', fmCreate or fmOpenWrite);
   fs.WriteBuffer(code[0], cnt*2);
   fs.free;

   fs := TFileStream.Create('mem.dmp', fmCreate or fmOpenWrite);
   for i := 0 to cnt-1 do
   begin
      fs.writebuffer(NtoBE(code[i]), 2);
   end;
   fs.free;

   fs := TFileStream.Create('mem.dhex', fmCreate or fmOpenWrite);
   for i := 0 to cnt-1 do
   begin
      s := 'DAT 0x'+inttohex(code[i], 4)+#13#10;
      fs.writebuffer(s[1], length(s));
   end;
   fs.free;

   mods.Free;
   globals.Free;
end.

