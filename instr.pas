unit instr;

{$mode objfpc}{$H+}

interface

type
 TOp = (A_None,
        A_SET,A_ADD,A_SUB,A_MUL,A_DIV,A_MOD,
        A_SHL,A_SHR,A_AND,A_BOR,A_XOR,
        A_IFE,A_IFN,A_IFG,A_IFB);

 TExtOp = (A_Res,
           A_JSR);

const
 OP_A = 4;
 OP_B = 10;

 V_FP = 7;

 V_POP  = $18;
 V_PEEK = $19;
 V_PUSH = $1A;
 V_SP   = $1B;
 V_PC   = $1C;
 V_O    = $1D;

 OP_Ret = ord(A_SET) or (V_PC shl OP_A) or (V_POP shl OP_B);

implementation

function Op(AOp: TOp; AA, AB: word): word;
begin
   result := ord(AOp) or (AA shl 4) or (AB shl 6);
end;

end.

