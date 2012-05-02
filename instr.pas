unit instr;

{$mode objfpc}{$H+}

interface

type
 TOp = (A_None,
        A_SET,A_ADD,A_SUB,A_MUL,A_MLI,A_DIV,A_DVI,A_MOD,A_MDI,
        A_AND,A_BOR,A_XOR,A_SHR,A_ASR,A_SHL,

        A_IFB,A_IFC,A_IFE,A_IFN,A_IFG,A_IFA,A_IFL,A_IFU,

        A_r0,A_r1,
        A_ADX,A_SBX,
        A_r2,A_r3,
        A_STI,A_STD);

 TExtOp = (A_JSR,
           A_INT,
           A_IAG,
           A_IAS,
           A_RFI,
           A_IAQ,
           A_HWN,
           A_HWQ,
           A_HWI);

const
 OP_A = 10;
 OP_B = 5;

 V_FP = 7;

 V_POP  = $18;
 V_PUSH = $18;
 V_PEEK = $19;
 V_SPrel= $1A;
 V_SP   = $1B;
 V_PC   = $1C;
 V_EX    = $1D;

 OP_Ret = ord(A_SET) or (V_PC shl OP_A) or (V_POP shl OP_B);

 ExtOps: array[TExtOp] of word = (1,$8,$9,$A,$B,$C,$10,$11,$12);

implementation

end.

