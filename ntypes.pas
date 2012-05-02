unit ntypes;

{$mode objfpc}{$H+}

interface

uses symtab;

var TypeNil,
    TypeInteger,
    TypeChar,
    TypeBool,
    TypeSet,
    TypeReal,
    TypeLongReal: TType;

function BaseType(a: PType): PType;
function Same(a,b: PType): boolean;
function AssignableTo(a,b: PType): boolean;
function Extends(a, b: PType): boolean;

function TypeSize(a: PType): longint;

function ArrayType(Alen: longint; ABase: PType): PType;

function IsByref(const f: TField): boolean;

function IsRecord(a: PType): boolean;

procedure AddField(a: PType; const AName: string; Exported, IsVar: boolean; TypDecl, Typ: PType);
function HasField(a: PType; const AName: string): boolean;
function FindField(a: PType; const AName: string; out Ofs: word): PType;

implementation

function BaseType(a: PType): PType;
begin
   result := nil;

   if a^.TypeKind = typRecord then
      result := a^.BaseType;
end;

function Same(a, b: PType): boolean;
begin
   result := (a = b);
end;

function AssignableTo(a, b: PType): boolean;
begin
   result := same(a, b);
end;

function Extends(a, b: PType): boolean;
begin
   if a = nil then exit(false);
   result := Same(a,b) or
             Extends(BaseType(a), b);
end;

function TypeSize(a: PType): longint;
var i: longint;
begin
   result := 0;

   if not assigned(a) then exit;

   case a^.TypeKind of
      typBasic: result := 1;
      typArray: result := a^.ArrayLen*TypeSize(a^.BaseType);
      typRecord:
         begin
            if assigned(a^.BaseType) then
               result := sizeof(a^.BaseType)
            else
               result := 3;
            for i := 0 to high(a^.Fields) do
               inc(result, TypeSize(a^.Fields[i].Typ));
         end;
      typPointer,
      typFunc: result := 1;
   end;
end;

function ArrayType(Alen: longint; ABase: PType): PType;
begin
   new(result);
   result^.TypeKind := typArray;
   result^.BaseType := ABase;
   result^.ArrayLen := Alen;
end;

function IsByref(const f: TField): boolean;
begin
   result := f.IsVar or
             (f.Typ^.TypeKind = typRecord) or
             (f.Typ^.TypeKind = typArray) or
             (f.Typ^.TypeKind = typOpenArray);
end;

function IsRecord(a: PType): boolean;
begin
   result := assigned(a) and
             ((a^.TypeKind = typRecord) or
              (a^.TypeKind = typPointer));
end;

procedure AddField(a: PType; const AName: string; Exported, IsVar: boolean; TypDecl, Typ: PType);
begin
   setlength(a^.Fields, high(a^.Fields)+2);
   a^.Fields[high(a^.Fields)].Name := AName;
   a^.Fields[high(a^.Fields)].IsVar := IsVar;
   a^.Fields[high(a^.Fields)].Exported := Exported;
   a^.Fields[high(a^.Fields)].Typ := Typ;
   a^.Fields[high(a^.Fields)].Decl := TypDecl;
end;

function HasField(a: PType; const AName: string): boolean;
var i: longint;
begin
   result := false;
   for i := 0 to high(a^.Fields) do
      if a^.Fields[i].Name = AName then
         exit(true);

   a := BaseType(a);
   if not assigned(a) then
      exit;

   result := HasField(a, AName);
end;

function FindField(a: PType; const AName: string; out Ofs: word): PType;
var i: longint;
begin
   result := nil;
   if assigned(a^.BaseType) then
      ofs := TypeSize(a^.BaseType)
   else
      ofs := 3;
   for i := 0 to high(a^.Fields) do
   begin
      if a^.Fields[i].Name = AName then
         exit(a^.Fields[i].Typ);
      inc(ofs, TypeSize(a^.Fields[i].Typ));
   end;

   a := BaseType(a);
   if not assigned(a) then exit;

   result := FindField(a, AName, ofs);
end;

function BasicType(s: TSimpleKind): TType;
begin
   result.TypeKind := typBasic;
   result.SimpleKind := s;
   result.BaseType := nil;
   result.FuncRet := nil;
   result.ArrayLen := 0;
end;

initialization
   TypeNil := BasicType(skNil);
   TypeInteger := BasicType(skInt);
   TypeChar := BasicType(skChar);
   TypeBool := BasicType(skBool);
   TypeSet := BasicType(skSet);
   TypeReal := BasicType(skReal);
   TypeLongReal := BasicType(skLongReal);

end.

