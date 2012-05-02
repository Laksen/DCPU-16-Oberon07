unit ninbuilt;

{$mode objfpc}{$H+}

interface

uses symtab;

const
 InitSize = 3;
 SystemFuncs = 8;

 NR_RESET      = InitSize+0;
 NR_COPY       = InitSize+1;
 NR_ASSERT     = InitSize+2;

 NR_NEW        = InitSize+3;

 NR_PACK       = InitSize+4;
 NR_UNPACK     = InitSize+5;

 NR_HEAP_Start = InitSize+6;
 NR_HEAP_End   = InitSize+7;

 AC_NIL_POINTER = 0;
 AC_ASSERTION   = 1;
 AC_TYPEGUARD   = 2;

type
 TInbuiltFunc = (
                 // Proper procedures
                 inbInc,
                 inbDec,
                 inbIncl,
                 inbExcl,
                 inbCopy,
                 inbNew,
                 inbAssert,
                 inbPack,
                 inbUnpack,
                 // Function procedures
                 inbAbs,
                 inbOdd,
                 inbLen,
                 inbLsl,
                 inbLsr,
                 inbAsr,
                 inbRor,
                 inbAnd,
                 // Type conversion
                 inbFloor,
                 inbFlt,
                 inbOrd,
                 inbChr,
                 inbLong,
                 inbShort,
                 // System function
                 inbMultShift,
                 inbDivShift,
                 inbAdr,
                 inbSize,
                 inbBit,

                 inbHWN,
                 inbINT,
                 inbIAS,
                 inbIAQ,
                 // System proper
                 inbMoveForward,
                 inbMoveBackward,
                 inbGet,
                 inbPut,

                 inbHWQ,
                 inbHWI,
                 inbIAG);

function InbFunc(const AName: string; Func: TInbuiltFunc): PSymbol;

implementation

function InbFunc(const AName: string; Func: TInbuiltFunc): PSymbol;
begin
   new(result);
   result^.name := AName;
   result^.exported := true;
   result^.typ := stInbFunc;
   result^.symType := nil;
   result^.val := ord(func);
end;

end.

