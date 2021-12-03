{************************************************}
{                                                }
{  Project: TC Plugins Manager: FS-Config Tool   }
{  http://atorg.net.ru                           }
{                                                }
{************************************************}

// Command line parameters:
//  1: FS-plugin filename
//  2: folder of plugin ini file
//  3: msg shown before plugin unload
//  4: window handle of host application

{$apptype GUI}
program Config;
uses
  Windows, FSPlugin;
{$R *.res}
{$I ConfigProc}

function strToInt(const s: string): integer;
var c: integer;
begin
  Val(s, Result, c);
  if c>0 then Result:= 0;
end;

begin
  if ParamCount<2 then
    begin
    MessageBox(0,
      'Helper tool used by TC Plugins Manager to call FS plugins configuration dialog.'#13+
      'Usage: Config <filename.wfx> <ini folder> [ <window handle> ] [ <message> ]',
      cMainCaption, MB_OK or MB_ICONINFORMATION);
    Exit
    end;
  tcFSConfigure2(ParamStr(1), ParamStr(2), StrToInt(ParamStr(3)), ParamStr(4));
end.
