unit paths;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
 TFileType = (ftMod, ftMox);

var OutputPath: string;

procedure AddPath(const Path: string);

function FindFile(Name: string; AType: TFileType): string;

implementation

const
 Extension: array[TFileType] of string = ('.m','.mox');

var Dirs: TStringList;

procedure AddPath(const Path: string);
begin
   Dirs.Add(IncludeTrailingBackslash(Path));
end;

function FindFile(Name: string; AType: TFileType): string;
var i: longint;
begin
   Name := ChangeFileExt(name, Extension[AType]);
   for i := 0 to dirs.Count-1 do
   begin
      if FileExists(dirs[i]+name) then
         exit(dirs[i]+name);
   end;

   result := '';
end;

initialization
   Dirs := TStringList.Create;
   AddPath('.');
finalization
   Dirs.Free;

end.

