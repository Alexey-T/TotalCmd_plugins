{$apptype console}

uses SysUtils, Windows, tcProc;

var
  fn, s: string;
begin
  tcSetPaths('C:\TC\Totalcmd.exe', 'C:\TC\TC_my.ini');
  
  fn:= 'C:\TC\Plugins\WLX\LinkInfo.wlx';
  Writeln('fn: ', fn);
  Writeln('DetectString: ', tcLSDetectString(fn));
  Writeln;

  fn:= 'C:\TC\Plugins\WCX\iso.wcx';
  Writeln('fn: ', fn);
  Writeln('PackerCaps: ', tcPackerCaps(fn));
  Writeln;

  {
  fn:= 'C:\TC\Plugins\POP3\POP3Plugin.dll';
  Writeln('fn: ', fn);
  Writeln('Type: ', Ord(tcTypeOf(fn)));
  Writeln;
  }

  fn:= 'C:\TC\Plugins\WDX\AudioInfo\AudioInfo.wdx';
  Writeln('fn: ', fn);
  Writeln('Type: ', Ord(tcTypeOf(fn)));
  Writeln;

  fn:= 'C:\TC\Plugins\WLX\WdxGuide\WDXGuideInLister.wlx';
  //fn:= 'C:\TC\Plugins\WLX\pdfview.wlx';
  Writeln('fn: ', fn);
  Writeln('Installed: ', tcInstalled(fn, s));
  if s<>'' then Writeln('Parameters: ', s);
  Writeln;

  Writeln('Format: ', Format('String: "%s"', ['S']));

end.
