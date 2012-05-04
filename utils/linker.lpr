program linker;

{$mode objfpc}{$H+}

uses sysutils, classes;

type
 TModule = record
  Name: string;
  Code,
  Data,
  Bss: word;
 end;

const
 REL_CODE = 0;
 REL_DATA = 1;
 REL_BSS  = 2;

var Memory: array[0..$FFFF] of word;
    CodeCounter: word = 3+8;

    initFuncs: array[0..1023] of word;
    initCount: word = 0;

    modules: array of TModule;

function Allocate(Size: longint): word;
begin
   result := CodeCounter;
   inc(CodeCounter, size);
end;

function FindModule(const AName: string): longint;
var i: longint;
begin
   result := -1;
   for i := 0 to high(modules) do
      if modules[i].Name = AName then
         exit(i);
end;

function ReadStr(s: TStream): string;
var i: longint;
begin
   setlength(result, s.ReadWord);
   for i := 1 to length(result) do
      result[i] := chr(s.ReadWord);
end;

function LoadObject(const AName: string): longint;
var fs: TStream;
    ImpCount, InitFunc, RelCount, CmdCount,
    CodeSize, Code, DataSize, Data, Bss: word;
    reltype, impidx, codeofs: word;
    Imports: array of longint;
    i: longint;
begin
   result := FindModule(AName);
   if result >= 0 then exit;

   fs := TFileStream.Create(ChangeFileExt(AName,'.mdx'), fmOpenRead);
   try
      if fs.ReadWord <> $0B07 then raise exception.Create('Wrong object header');

      InitFunc := fs.ReadWord;
      ImpCount := fs.ReadWord;
      CodeSize := fs.ReadWord;
      DataSize := fs.ReadWord;
      Code := Allocate(CodeSize);
      Data := Allocate(DataSize);
      Bss := Allocate(fs.ReadWord);
      RelCount := fs.ReadWord;
      CmdCount := fs.ReadWord;

      if InitFunc <> $FFFF then
      begin
         initFuncs[initCount] := InitFunc+Code;
         inc(initCount);
      end;

      setlength(imports, ImpCount);
      for i := 0 to ImpCount-1 do
         Imports[i] := LoadObject(ReadStr(fs));

      fs.ReadBuffer(Memory[Code], CodeSize*2);

      fs.ReadBuffer(Memory[Data], DataSize*2);

      for i := 0 to RelCount-1 do
      begin
         reltype := fs.ReadWord;
         impIdx := fs.ReadWord;
         codeOfs := fs.ReadWord;

         if impidx = $FFFF then
         begin
            case reltype of
               REL_CODE:
                  Memory[Code+codeofs] := Memory[Code+codeofs]+Code;
               REL_DATA:
                  Memory[Code+codeofs] := Memory[Code+codeofs]+Data;
               REL_BSS:
                  Memory[Code+codeofs] := Memory[Code+codeofs]+Bss;
            end;
         end
         else
         begin
            case reltype of
               REL_CODE:
                  Memory[Code+codeofs] := Memory[Code+codeofs]+Modules[Imports[impidx]].Code;
               REL_DATA:
                  Memory[Code+codeofs] := Memory[Code+codeofs]+Modules[Imports[impidx]].Data;
               REL_BSS:
                  Memory[Code+codeofs] := Memory[Code+codeofs]+Modules[Imports[impidx]].Bss;
            end;
         end;
      end;
   finally
      fs.Free;
   end;

   setlength(modules, high(modules)+2);
   modules[high(modules)].Name := AName;
   modules[high(modules)].Code := Code;
   modules[high(modules)].Data := Data;
   modules[high(modules)].Bss := Bss;

   result := high(modules);
end;

var i: longint;
    fs: TStream;
begin
   for i := 1 to Paramcount() do
      LoadObject(ParamStr(i));

   fs := TFileStream.Create('mem.dmp', fmopenwrite or fmcreate);

   Memory[0] := $1 or ($1B shl 5) or ($21 shl 10);
   Memory[1] := $1 or ($1C shl 5) or ($1F shl 10);
   Memory[2] := CodeCounter;

   for i := 0 to initCount-1 do
   begin
      Memory[CodeCounter+0] := $0 or (1 shl 5) or ($1F shl 10);
      Memory[CodeCounter+1] := initFuncs[i];
      inc(CodeCounter,2);
   end;

   Memory[CodeCounter] := $1 or ($1C shl 5) or ($20 shl 10);
   inc(CodeCounter);

   for i := 0 to CodeCounter-1 do
      fs.WriteWord(BEtoN(Memory[i]));

   //fs.writebuffer(memory[0], codecounter*2);

   fs.free;
end.

