unit tcRegistration;

interface

procedure tcRegisterExtensions(AEnable: boolean; const AStrInstall, AStrUninstall: string);


implementation

uses
  Windows, SysUtils, RegProc;


//---------------------------------------------------------------------
procedure Register(const ext, key, descType, descInstall, descUninstall: string; IconNum: integer);
begin
  SetRegKeyStr(HKEY_CLASSES_ROOT, PChar(ext), '', PChar(key));
  SetRegKeyStr(HKEY_CLASSES_ROOT, PChar(key), '', PChar(descType));
  SetRegKeyStr(HKEY_CLASSES_ROOT, PChar(key+'\DefaultIcon'), '',
    PChar(Format('"%s",%d', [ParamStr(0), IconNum])));

  SetRegKeyStr(HKEY_CLASSES_ROOT, PChar(key+'\shell\open'), '',
    PChar(descInstall));
  SetRegKeyStr(HKEY_CLASSES_ROOT, PChar(key+'\shell\open\command'), '',
    PChar(Format('"%s" "%%1"', [ParamStr(0)])));

  SetRegKeyStr(HKEY_CLASSES_ROOT, PChar(key+'\shell\uninstall'), '',
    PChar(descUninstall));
  SetRegKeyStr(HKEY_CLASSES_ROOT, PChar(key+'\shell\uninstall\command'), '',
    PChar(Format('"%s" /Uninstall "%%1"', [ParamStr(0)])));
end;

//---------------------------------------------------------------------
procedure Unregister(const ext, key: string);
begin
  RegDeleteKey(HKEY_CLASSES_ROOT, PChar(key+'\shell\uninstall\command'));
  RegDeleteKey(HKEY_CLASSES_ROOT, PChar(key+'\shell\uninstall'));
  RegDeleteKey(HKEY_CLASSES_ROOT, PChar(key+'\shell\open\command'));
  RegDeleteKey(HKEY_CLASSES_ROOT, PChar(key+'\shell\open'));
  RegDeleteKey(HKEY_CLASSES_ROOT, PChar(key+'\shell'));
  RegDeleteKey(HKEY_CLASSES_ROOT, PChar(key+'\DefaultIcon'));
  RegDeleteKey(HKEY_CLASSES_ROOT, PChar(key));
  RegDeleteKey(HKEY_CLASSES_ROOT, PChar(ext));
end;

//---------------------------------------------------------------------
const
  cIconWLX = 0;
  cIconWCX = 1;
  cIconWFX = 2;
  cIconWDX = 3;

  cExtWLX = '.wlx';
  cExtWCX = '.wcx';
  cExtWFX = '.wfx';
  cExtWDX = '.wdx';

  ckeyWLX = 'TC_Plugin_WLX';
  cKeyWCX = 'TC_Plugin_WCX';
  cKeyWFX = 'TC_Plugin_WFX';
  cKeyWDX = 'TC_Plugin_WDX';

  cTypeWLX = 'Total Commander Lister Plugin';
  cTypeWCX = 'Total Commander Packer Plugin';
  cTypeWFX = 'Total Commander File-System Plugin';
  cTypeWDX = 'Total Commander Content Plugin';

procedure tcRegisterExtensions(AEnable: boolean; const AStrInstall, AStrUninstall: string);
var
  S: string;
  Registered: boolean;
begin
  S:= GetRegKeyStr(HKEY_CLASSES_ROOT, cExtWDX, '', '');
  S:= GetRegKeyStr(HKEY_CLASSES_ROOT, PChar(S+'\shell\open\command'), '', '');
  Registered:= Pos(ParamStr(0), S) > 0;
  
  if AEnable and (not Registered) then
    begin
    Register(cExtWFX, cKeyWFX, cTypeWFX, AStrInstall, AStrUninstall, cIconWFX);
    Register(cExtWLX, cKeyWLX, cTypeWLX, AStrInstall, AStrUninstall, cIconWLX);
    Register(cExtWCX, cKeyWCX, cTypeWCX, AStrInstall, AStrUninstall, cIconWCX);
    Register(cExtWDX, cKeyWDX, cTypeWDX, AStrInstall, AStrUninstall, cIconWDX);
    end;

  if (not AEnable) and (Registered) then
    begin
    Unregister(cExtWFX, cKeyWFX);
    Unregister(cExtWLX, cKeyWLX);
    Unregister(cExtWCX, cKeyWCX);
    Unregister(cExtWDX, cKeyWDX);
    end;
end;

end.
