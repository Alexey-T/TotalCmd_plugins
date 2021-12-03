{$apptype console}
{$I-}

program OdfToTxt;

uses
  Windows, SysUtils, OOData, SProc, XmlProc;

const
  sVersion = '1.2.1';

var
  OptAnsi: boolean = false;
  OptShowHead: boolean = true;
  OptOutFilename: string = '';


procedure ReadParams;
const
  sOut = '/out=';
var
  i: integer;
  s: string;
begin
  for i:= 1 to ParamCount do
    begin
    s:= ParamStr(i);

    if StrIComp(PChar(s), '/ansi')=0 then
      OptAnsi:= true;

    if StrIComp(PChar(s), '/nohead')=0 then
      OptShowHead:= false;

    if StrIComp(PChar(Copy(s, 1, Length(sOut))), sOut)=0 then
      OptOutFilename:= Copy(s, Length(sOut)+1, MaxInt);
    end;
end;


function SDecode(const s: string): string;
begin
  if OptAnsi
    then Result:= s
    else Result:= ToOEM(s);
end;

var
  OldTime: DWORD = 0;
  OldMsg: string = '';

procedure Progress(const Msg: string; N: integer);
begin
  if OldTime=0 then OldTime:= GetTickCount;
  if GetTickCount-OldTime>1000 then
    begin
    if OldMsg<>Msg then
      begin
      Writeln(Msg, '...');
      OldMsg:= Msg;
      end;
    Write(n, '% ');
    OldTime:= GetTickCount;
    end;
end;


var
  fn: string;
  f: System.Text;
begin
  fn:= ParamStr(1);
  if fn='' then
    begin
    Writeln('OdfToTxt V'+sVersion);
    Writeln('Converts OpenOffice.org documents to plain text');
    Writeln('Copyright (c) 2005-2007 Alexey Torgashin');
    Writeln('http://atorg.net.ru');
    Writeln;
    Writeln('Usage:');
    Writeln('  OdfToTxt.exe <FileName> [ Options ]');
    Writeln;
    Writeln('Options:');
    Writeln('  /ANSI            - Write resulting text in ANSI codepage (default is OEM)');
    Writeln('  /NoHead          - Skip document header: title and subject');
    Writeln('  /Out=<FileName>  - Specify resulting text filename (default is stdout)');
    Exit
    end;

  ReadParams;

  XmlProgress:= Progress;

  if not GetOODataAndText(fn) then
    begin
    Writeln('Error converting document "', fn, '"');
    Exit
    end;

  SReplaceAll(foooDesc, #13, ' ');
  SReplaceAll(foooDesc, #10, ' ');

  Assign(f, OptOutFilename);
  Rewrite(f);
  if IOResult<>0 then
    begin
    Writeln('Error creating file "', OptOutFilename, '"');
    Exit
    end;

  if OptShowHead then
    begin
    Writeln(f, 'Title: ', SDecode(foooTitle));
    Writeln(f, 'Subject: ', SDecode(foooSubject));
    Writeln(f, 'Description: ', SDecode(foooDesc));
    if OptAnsi
      then Writeln(f, '===============================================================================')
      else Writeln(f, '컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�');
    end;

  Writeln(f, SDecode(foooTextList.Text));
  Close(f);
end.
