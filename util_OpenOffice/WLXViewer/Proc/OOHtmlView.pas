{$define Test}
unit OOHtmlView;

interface

function ConvertXmlToHtml(const fn_xml, fn_xsl: string): boolean;
function ConvertOdtToHtml(const fn_odt, fn_xsl, fn_TempDir: string; var fn_xml: string): boolean;

implementation

uses
  SysUtils, Windows, SProc, FProc, UnzipDll;

const
  meta_xml: PChar = 'meta.xml';
  content_xml: PChar = 'content.xml';
  CR = #13#10;
  xsl_inc1 = CR+'<?xml-stylesheet href="';
  xsl_inc2 = '" type="text/xsl"?>'+CR;

procedure Msg(const s: string);
begin
  MessageBox(0, PChar(s), 'Viewer', mb_ok or mb_iconerror or mb_taskmodal);
end;


function ConvertXmlToHtml(const fn_xml, fn_xsl: string): boolean;
var
  sAll: string;
  s, sHead, sXSL: AnsiString; //Ansi!
  i, i2: integer;
begin
  Result:= false;
  if not FReadToString(fn_xml, sAll) then
    begin Msg('Cannot read file:'+CR+fn_xml); Exit end;

  //Delete !DOCTYPE tag:
  s:= sAll;
  i:= Pos('<!DOCTYPE', s);
  if i>0 then
    begin
    i2:= PosFrom('>', s, i);
    if i2>0 then
      Delete(s, i, i2-i+1);
    end;

  //Prepare XSL stub, also read encoding value in first 100 bytes:
  //Cannot use Format: it breaks UTF8 string
  sXSL:= xsl_inc1 + fn_xsl + xsl_inc2;
  sHead:= Copy(s, 1, 100);
  if Pos(LowerCase('encoding="UTF-8"'), LowerCase(sHead))>0 then
    sXSL:= UTF8Encode(sXSL);
  //messageboxA(0, PAnsiChar(Copy(sXSL,1,100)),'',0);

  //Insert XSL stub after first tag:
  i:= Pos('<', s);  if i=0 then begin Msg('Cannot find tag begin'); Exit end;
  i:= PosFrom('>', s, i);  if i=0 then begin Msg('Cannot find tag end'); Exit end;
  Insert(sXSL, s, i+1);

  if not FWriteString(fn_xml, s) then
    begin Msg('Cannot write xml file:'+CR+fn_xml); Exit end;
  Result:= true;
end;


function ConvertOdtToHtml(const fn_odt, fn_xsl, fn_TempDir: string; var fn_xml: string): boolean;
begin
  Result:= UnzipSingle(fn_odt, fn_TempDir, [content_xml]);
  if Result then
    begin
    fn_xml:= fn_TempDir + '\' + content_xml;
    Result:= ConvertXmlToHtml(fn_xml, fn_xsl);
    end
  else
    Msg('Cannot unpack file to temp:'+CR+fn_odt);
end;

end.
