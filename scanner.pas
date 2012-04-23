unit scanner;

{$mode objfpc}

interface

uses sysutils, classes;

type
 TToken = (tkEof,
           tkInteger, tkReal, tkCharConst, tkString, tkIdent, tkNil,
           tkMul,tkSlash,tkDiv,tkMod,tkAnd,
           tkPlus,tkMinus,tkOr,
           tkEqual, tkNEqual, tkLess, tkLEqual, tkGreater, tkGEqual, tkIn,
           tkPipe,tkAssign,
           tkLParan,tkRParan,tkLSquare,tkRSquare,tkLBracket,tkRBracket,
           tkComma,tkDot,tkDotDot,tkColon,tkSemiColon,tkDeref,
           tkProcedure,tkModule,tkIf,tkThen,tkElse,tkElsif,
           tkWhile,tkDo,
           tkRepeat,tkUntil,
           tkFor,tkTo,tkBy,
           tkReturn,
           tkCase,tkConst,tkVar,tkType,
           tkImport,
           tkBegin,tkEnd,
           tkArray,tkOf,
           tkRecord,tkPointer,
           tkTrue,tkFalse,tkNot);

 EScannerException = class(Exception);

 TScanner = class
 private
  ch: char;
  fStream: TStream;
  fToken: TToken;
  fTokenStr: string;
  fLine,fPos: longint;

  fLookBack: char;
  lookback: boolean;
  
  function Eof: boolean;
  function GetChar: char;
  function FindMatch: longint;
 public
  procedure Next;
  
  constructor Create(AStream: TStream);
  
  property Token: TToken read fToken;
  property TokenStr: string read fTokenStr;

  property Line: longint read fLine;
  property Position: longint read fPos;
 end;

const
 TokenStrs: array[TToken] of string = 
          ('',
           '', '', '', '', '', 'NIL',
           '*','/','DIV','MOD','&',
           '+','-','OR',
           '=', '#', '<', '<=', '>', '>=', 'IN',
           '|',':=',
           '(',')','[',']','{','}',
           ',','.','..',':',';','^',
           'PROCEDURE','MODULE','IF','THEN','ELSE','ELSIF',
           'WHILE','DO',
           'REPEAT','UNTIL',
           'FOR','TO','BY',
           'RETURN',
           'CASE','CONST','VAR','TYPE',
           'IMPORT',
           'BEGIN','END',
           'ARRAY','OF',
           'RECORD','POINTER',
           'TRUE','FALSE','~');
 TokenNames: array[TToken] of string = 
          ('',
           'Integer', 'Real', 'Char constant', 'String', 'Identifier', 'NIL',
           '*','/','DIV','MOD','&',
           '+','-','OR',
           '=', '#', '<', '<=', '>', '>=', 'IN',
           '|',':=',
           '(',')','[',']','{','}',
           ',','.','..',':',';','^',
           'PROCEDURE','MODULE','IF','THEN','ELSE','ELSIF',
           'WHILE','DO',
           'REPEAT','UNTIL',
           'FOR','TO','BY',
           'RETURN',
           'CASE','CONST','VAR','TYPE',
           'IMPORT',
           'BEGIN','END',
           'ARRAY','OF',
           'RECORD','POINTER',
           'TRUE','FALSE','~');

implementation

const
 BadChars = [' ',#0..#31];
 Letter = ['a'..'z','A'..'Z'];
 Digit = ['0'..'9'];
 HexDigit = ['A'..'F'];

function TScanner.Eof: boolean;
begin
   result := fStream.Position = fStream.Size;
end;

function TScanner.GetChar: char;
begin
   result := #0;

   if lookback then
   begin
      result := fLookback;
      lookback := false;
      exit;
   end;

   fStream.Read(result, 1);
   inc(fPos)
end;

function TScanner.FindMatch: longint;
var tk: TToken;
begin
   result := 0;
   for tk := low(TToken) to high(TToken) do
   begin
      if pos(fTokenStr, TokenStrs[tk]) = 1 then
      begin
         fToken := tk;
         inc(result);
      end;
   end;
end;

procedure TScanner.Next;
var tk: TToken;
    hex: boolean;
    old: String;
    sl,sp,commentlevel: longint;

   procedure Trim;
   begin
      if ch in BadChars then
      begin
         while (ch in BadChars) and (not eof) do
         begin
            if ch = #10 then
            begin
               fpos := 0;
               inc(fline);
            end;
            ch := GetChar;
         end;
         if Eof then
         begin
            fToken := tkEof;
            exit;
         end;
      end;
   end;

begin
   if Eof and (ch = #0) then
   begin
      fToken := tkEof;
      exit;
   end;
   
   Trim;

   commentlevel := 0;

   while ch = '(' do
   begin
      sp := Position;
      sl := Line;

      fToken := tkLParan;
      fTokenStr := '(';

      ch := GetChar;
      if ch = '*' then
      begin
         inc(commentlevel);
         while true do
         begin
            ch := GetChar;
            trim;

            if EOF and (ch = #0) then raise EScannerException.CreateFmt('Unterminated comment found at line %d, position %d', [sl,sp]);

            if ch = '(' then
            begin
               ch := GetChar;
               if ch = '*' then
               begin
                  inc(commentlevel);
               end;
            end
            else if ch = '*' then
            begin
               ch := GetChar;
               if ch = ')' then
               begin
                  dec(commentlevel);
                  if commentlevel = 0 then
                  begin
                     ch := GetChar;
                     break;
                  end;
               end;
            end;
         end;
      end
      else
         exit;
   end;

   Trim;

   if ch in Letter then
   begin
      fToken := tkIdent;
      fTokenStr := '';
      
      while ch in (letter+digit) do
      begin
         fTokenStr := fTokenStr+ch;
         ch := GetChar;
      end;
      
      for tk := low(TToken) to high(TToken) do
      begin
         if fTokenStr = TokenStrs[tk] then
         begin
            fToken := tk;
            break;
         end;
      end;
   end
   else if ch = '"' then
   begin
      fToken := tkString;
      fTokenStr := '';
      
      ch := GetChar;
      while ch <> '"' do
      begin
         if Eof then raise exception.Create('Unterminated string');
         fTokenStr := fTokenStr + ch;
         ch := GetChar;
      end;
      ch := GetChar;

      if length(fTokenStr) = 1 then
      begin
         fToken := tkCharConst;
         fTokenStr := inttostr(ord(fTokenStr[1]));
      end;
   end
   {else if ch = '''' then
   begin
      fToken := tkCharConst;
      fTokenStr := '';
      
      ch := GetChar;
      while ch <> '''' do
      begin
         if Eof then raise exception.Create('Unterminated string');
         fTokenStr := fTokenStr + ch;
         ch := GetChar;
      end;
      ch := GetChar;

      if length(fTokenStr) <> 1 then
         raise exception.Create('Invalid char constant');
   end}
   else if ch in digit then
   begin
      fTokenStr := ch;

      ch := GetChar;
      hex := false;
      while ch in digit+HexDigit do
      begin
         hex := (ch in HexDigit) or hex;
         fTokenStr := fTokenStr+ch;
         ch := GetChar;
      end;
      if hex or (ch in ['X', 'H']) then
      begin
         if ch = 'H' then
         begin
            fToken := tkInteger;
            fTokenStr := inttostr(StrToInt('$'+fTokenStr));
         end
         else if ch = 'X' then
         begin
            fToken := tkCharConst;
            fTokenStr := inttostr(StrToInt('$'+fTokenStr));
         end
         else
            raise exception.CreateFmt('Expected ["H" or "X"] but got "%s"', [ch]);
         ch := GetChar;
      end
      else
      begin
         if ch = '.' then
         begin
            ch := GetChar;
            if ch = '.' then
            begin
               lookback := true;
               fLookBack := '.';
               fToken := tkInteger;
               exit;
            end
            else
               fTokenStr:=fTokenStr+'.';

            while ch in Digit do
            begin
               fTokenStr:=fTokenStr+ch;
               ch := GetChar;
            end;
            fTokenStr := fTokenStr+'0';

            if ch in ['E','D'] then
            begin
               fTokenStr := fTokenStr+'E';
               ch := GetChar;

               if ch in ['+','-'] then
               begin
                  fTokenStr := fTokenStr+ch;
                  ch := GetChar;
               end
               else
                  fTokenStr := fTokenStr+'+';

               while ch in Digit do
               begin
                  fTokenStr:=fTokenStr+ch;
                  ch := GetChar;
               end;
            end;

            fToken := tkReal;
         end
         else
            fToken := tkInteger;
      end;
   end
   else
   begin
      fTokenStr := ch;
      
      while true do
      begin
         if FindMatch = 1 then
            break;
         if eof then
         begin
            fToken := tkEof;
            for tk := low(TToken) to high(TToken) do
            begin
               if fTokenStr = TokenStrs[tk] then
               begin
                  fToken := tk;
                  break;
               end;
            end;
            break;
         end;
         ch := getchar;
         if ch in badchars then
         begin
            for tk := low(TToken) to high(TToken) do
            begin
               if fTokenStr = TokenStrs[tk] then
               begin
                  fToken := tk;
                  break;
               end;
            end;
            break;
         end;
         old := fTokenStr;
         fTokenStr := fTokenStr + ch;
         if FindMatch = 0 then
         begin
            fTokenStr := old;
            for tk := low(TToken) to high(TToken) do
            begin
               if fTokenStr = TokenStrs[tk] then
               begin
                  fToken := tk;
                  exit;
               end;
            end;
            raise exception.CreateFmt('[%d,%d] Expected token but got "%s"', [fLine,fPos, fTokenStr]);
         end;
      end;
      ch := getchar;
   end;
end;

constructor TScanner.Create(AStream: TStream);
begin
   inherited Create;
   lookback := false;
   fLine := 1;
   fPos := 1;
   fStream := AStream;
   Next;
end;

initialization
   DefaultFormatSettings.DecimalSeparator := '.';

end.
