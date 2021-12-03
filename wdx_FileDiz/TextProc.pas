{$I-}
unit TextProc;

interface

function GetDesc(const fn, fnDesc: string{; descType: integer}): string;
function GetText(const fn: string{; descType: integer}): string;
function GetHtmlTag(const fn, tagName: string): string;
function GetHtmlMetaTag(const fn, tagName: string): string;

var
  fDescChar: string = ' ';
  fReplaces: array[1..20] of
    record sFrom, sTo: string; end;

implementation

uses Windows, SProc;

//-----------------------------------------
function GetDesc(const fn, fnDesc: string{; descType: integer}): string;
var
  f: Text;
  fn0, fn0s, s, ss: string;
  i: integer;
  IsOEM: boolean;
begin
  Result:= '';
  AssignFile(f, ChangeFileName(fn, fnDesc));
  Reset(f);
  if IOResult<>0 then Exit;

  IsOEM:= Pos('.bbs', fnDesc)>0; // Files.bbs - in OEM, Descript.ion - in ANSI
  fn0:= ExtractFileName(fn);     // fn - in ANSI
  fn0s:= ExtractFileName(FShortName(fn));
  if IsOEM then
    begin
    fn0:= ToOEM(fn0);
    fn0s:= ToOEM(fn0s);
    end;

  while not Eof(f) do
    begin
    Readln(f, s);
    i:= Pos(' ', s); if i=0 then Continue;
  
    if s[1]='"' then i:= Pos('"', Copy(s, 2, MaxInt))+2;
    ss:= Copy(s, 1, i-1);
    //Writeln('ss: ', ss);

    STrimLeft(ss, ['"']);
    STrimRight(ss, ['"']);
    if (StrIComp(PChar(fn0), PChar(ss))=0) or
       (StrIComp(PChar(fn0s), PChar(ss))=0) then
      begin
      Result:= Copy(s, i+1, MaxInt);
      Break;
      end;
    end;

  // multi-line descriptions: need to delete leading ' ' and '|'
  STrimLeft(Result, [' ']);
  //if descType=1 then
   while not Eof(f) do
     begin
     Readln(f, s);
     if (s='') or (s[1]<>' ') then Break;
     STrimLeft(s, [' ', '|']);
     Result:= Result+fDescChar+s;
     end;
  STrimRight(Result, [' ']);
  
  CloseFile(f);

  if IsOEM then
    Result:= ToANSI(Result);
end;

//-----------------------------------------
function GetText(const fn: string{; descType: integer}): string;
var
  i: integer;
begin
  Result:= SRead(fn, 100, fDescChar);

  for i:= Low(fReplaces) to High(fReplaces) do
    with fReplaces[i] do
      if sFrom<>'' then
        SReplaceAll(Result, sFrom, sTo);
end;

//-----------------------------------------
function GetHtmlTag(const fn, tagName: string): string;
var
  s: string;
begin
  s:= SRead(fn, 100, ' ');
  Result:= SBetweenI(s, '<'+tagName+'>', '</');
end;

//-----------------------------------------
function GetHtmlMetaTag(const fn, tagName: string): string;
var
  sName, sContent: string;

  procedure GetNames(const s: string);
  var
    n1, n2: integer;
  begin
    // name="description" content="..." 
    sName:= '';
    sContent:= '';

    n1:= Pos('name=', LowerCase(s)); if n1=0 then Exit;
    if s[n1+5]='"'
      then n2:= Pos('"', Copy(s, n1+6, MaxInt))
      else n2:= Pos(' ', Copy(s, n1+6, MaxInt));
    sName:= Copy(s, n1+5, n2);
    STrimLeft(sName, ['"']);
    //Writeln('sName: "', ToOEM(sName), '"');

    n1:= Pos('content=', LowerCase(s)); if n1=0 then Exit;
    if s[n1+8]='"'
      then n2:= Pos('"', Copy(s, n1+9, MaxInt))
      else n2:= Pos(' ', Copy(s, n1+9, MaxInt));
    sContent:= Copy(s, n1+8, n2);
    STrimLeft(sContent, ['"']);
    //Writeln('sContent: "', ToOEM(sContent), '"');
  end;

var
  s, ss: string;
  n: integer;
begin
  Result:= '';
  s:= SRead(fn, 100, ' ');
  repeat
    n:= Pos('<meta', LowerCase(s)); if n=0 then Break;
    Delete(s, 1, n+4);
    n:= Pos('>', s); if n=0 then Break;
    ss:= Copy(s, 1, n-1)+' ';
    GetNames(ss);
    if StrIComp(PChar(sName), PChar(tagName))=0 then
      begin Result:= sContent; Break end;
    Delete(s, 1, n);
  until false;
end;

end.
