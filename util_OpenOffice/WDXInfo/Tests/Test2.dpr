{$apptype console}
{$I-}

uses Windows, SProc, XmlProc;

const
  sDecodeRec: array[1..5] of TDecodeRec =
    ((SFrom: '&quot;'; STo: '"'),
     (SFrom: '&amp;';  STo: '&'),
     (SFrom: '&apos;'; STo: ''''),
     (SFrom: '&lt;';   STo: '<'),
     (SFrom: '&gt;';   STo: '>'));

var
  fn: string;
  //f: Text;
  //s: string;
  //XmlCont: string;
begin
  fn:= 'C:\Download\content.xml';

  {
  XmlCont:= '';
  AssignFile(f, fn);
  Reset(f);
  if IOResult<>0 then Exit;
  while not Eof(f) do
    begin
    Readln(f, s);
    XmlCont:= XmlCont+s;
    end;
  CloseFile(f);

  Writeln('GetText: "', ToOEM(ConvertFromUTF8(XmlCont)), '"');
  Exit;
  }

  ReadXmlFile(fn);
  Writeln(ToOEM(GetXmlText));

  {
  s:= '&quot;String&quot;&lt;&amp;&gt;';
  Writeln('Decode: ', SDecode(s, sDecodeRec));

  s:= 'TextText';
  Writeln('CopyTo: "', SCopyFromTo(s, 2, 'T'), '"');
  }
end.
