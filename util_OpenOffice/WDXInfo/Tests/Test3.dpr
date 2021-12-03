{$apptype console}
{$I-}

uses Windows, SProc;

const
  Rec: array[1..1] of TDecodeRec =
    ((SFrom: #13#10; STo: #13#10' '));

var
  //s: string = 'aabbaa';
  s: string = '<MS'#13#10'DLL'#13#10'Repair>';

begin
  Writeln('S: ', s);
  Writeln('SDecode: ', SDecode(s, Rec));

end.
