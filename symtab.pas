unit symtab;

{$mode objfpc}{$H+}

interface

uses classes, sysutils;

type
 TSymType = (stFunc,
             stInbFunc,
             stInterruptFunc,
             stVar,
             stParam,
             stConst,
             stType,
             stModule);

 TTypeKind = (typBasic, typRecord, typFunc, typPointer, typArray, typOpenArray);
 TSimpleKind = (skNil, skInt, skChar, skBool, skSet, skReal, skLongReal);

 PType = ^TType;

 TField = record
  Name: string;
  IsVar,
  Exported: boolean;
  Typ,
  Decl: PType;
  Ofs: longint;
 end;

 PSymbol = ^TSymbol;

 TType = record
  TypeKind: TTypeKind;
  SimpleKind: TSimpleKind;
  ArrayLen: longint;

  decl: PType;

  FuncRet,
  BaseType: PType;

  RecordID: longint;
  Fields: array of TField;

  ForwardType: string;
 end;

 TSymTable = class;

 TSymbol = record
  name: string;
  exported: boolean;
  typ: TSymType;
  module: PSymbol;

  typDecl,
  symType: PType;

  byref,
  local: boolean;
  val,
  ofs: word;
  symtab: TSymTable;
 end;

 { TSymTable }

 TSymTable = class
 private
  fParent: TSymTable;
 public
  name: string;

  symbols: array of PSymbol;

  function FindSymbol(const AName: string): PSymbol;
  function FindPublicSymbol(const AName: string): PSymbol;

  procedure AddSymbol(Sym: PSymbol);
  procedure CullPrivate;

  constructor Create(AParent: TSymTable);
  destructor Destroy; override;
 end;

 TLocType = (ltParam, ltParamRef, ltVar,
             ltGlobVarRef, ltGlobVarAddr,
             ltFunc,
             ltAbsoluteAddr, ltAbsoluteRef,
             ltPush, ltPop, ltPick,
             ltReg, ltRegInd, ltConst, ltRegOffset,
             ltSkipFalse, ltSkipTrue);
 TLocation = record
  Typ: TLocType;

  Reg: word;
  Offset,
  Addr,
  Val: Word;
 end;

 PExpr = ^TExpr;
 TExpr = record
  Typ: PType;
  Loc: TLocation;
  Module: PSymbol;
 end;

function TypSym(const AName: string; var Typ: TType): PSymbol;

implementation

{ TSymTable }

procedure FreeSym(s: PSymbol);
begin
   freemem(s);
end;

function TSymTable.FindSymbol(const AName: string): PSymbol;
var i: longint;
begin
   result := nil;
   for i := 0 to high(symbols) do
      if symbols[i]^.name = AName then
         exit(symbols[i]);
   if assigned(fParent) then
      result := fParent.FindSymbol(AName);
end;

function TSymTable.FindPublicSymbol(const AName: string): PSymbol;
var i: longint;
begin
   result := nil;
   for i := 0 to high(symbols) do
      if (symbols[i]^.name = AName) and
         symbols[i]^.exported then
         exit(symbols[i]);
end;

procedure TSymTable.AddSymbol(Sym: PSymbol);
begin
   setlength(symbols, high(symbols)+2);
   symbols[high(symbols)] := sym;
end;

procedure TSymTable.CullPrivate;
var i: longint;
begin
   for i := high(symbols) downto 0 do
   begin
      if not symbols[i]^.exported then
      begin
         FreeSym(symbols[i]);

         if high(symbols) > i then
            move(symbols[i+1], symbols[i], (high(symbols)-i)*sizeof(symbols[0]));
         setlength(symbols, high(symbols));
      end;
   end;
end;

constructor TSymTable.Create(AParent: TSymTable);
begin
   inherited Create;
   setlength(symbols, 0);
   fParent := AParent;
end;

destructor TSymTable.Destroy;
var i: longint;
begin
   for i := 0 to high(symbols) do
      FreeSym(symbols[i]);

   inherited Destroy;
end;

function TypSym(const AName: string; var Typ: TType): PSymbol;
begin
   new(result);
   result^.name := AName;
   result^.exported := true;
   result^.typ := stType;
   result^.symType := @typ;
end;

end.

