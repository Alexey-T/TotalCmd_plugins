{$apptype GUI}

{$I-}

program OONotepadView;

uses Windows, OOData, SProc, FProc1;

const
  ssCaption = 'OONotepadView';
  ssVersion = '1.1.1';

var
  fn, fnTemp: string;
  f: Text;

begin
  //Read parameters

  fn:= ParamStr(1);
  if fn='' then
    begin
    MessageBox(0, PChar(
      'OpenOffice.org Notepad Viewer v'+ssVersion+#13+
      'Copyright © 2006 Alexey Torgashin, http://atorg.net.ru'#13#13+
      'Usage:'#13+
      'OONotepadView.exe Filename.ODT'#13#13+
      'Note:'#13+
      'unzip32.dll must be in the same folder as this program'),
      PChar('About '+ssCaption),
      MB_OK or MB_ICONINFORMATION);
    Halt
    end;

  //Process file

  if not GetOODataAndText(fn) then
    begin
    MessageBox(0, PChar(
      'Error converting OpenOffice.org document "'+fn+'"'),
      ssCaption, MB_OK or MB_ICONERROR);
    Halt
    end;

  SReplaceAll(foooDesc, #13, ' ');
  SReplaceAll(foooDesc, #10, ' ');

  //Create temp file

  fnTemp:= SExpandVars('%TEMP%\OONotepadView.txt');

  Assign(f, fnTemp);
  Rewrite(f);
  if IOResult<>0 then
    begin
    MessageBox(0, PChar('Error creating file "'+fnTemp+'"'), ssCaption, MB_OK or MB_ICONERROR);
    Halt
    end;

  if foooTitle<>'' then Writeln(f, '* Title: ', foooTitle);
  if foooSubject<>'' then Writeln(f, '* Subject: ', foooSubject);
  if foooDesc<>'' then Writeln(f, '* Description: ', foooDesc);

  if (foooTitle<>'') or (foooSubject<>'') or (foooDesc<>'') then
    Writeln(f, '=============================================================================');

  Writeln(f, foooTextList.Text);

  Close(f);

  //Run Notepad and clean up

  FExecShell(fnTemp, '', SW_SHOW, true);
  DeleteFile(PChar(fnTemp));

end.
