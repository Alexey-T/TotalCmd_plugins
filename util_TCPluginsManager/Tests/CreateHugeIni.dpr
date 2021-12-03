{$apptype console}

uses
  SysUtils;

const
  sDetect =
    'EXT="ODT" | EXT="ODS" | EXT="ODP" | EXT="ODG" | EXT="ODF" | EXT="ODB" '+
    '| EXT="ODM" | EXT="OTT" | EXT="OTH" | EXT="OTS" | EXT="OTG" | EXT="OTP" | EXT="SXW" | EXT="SXC" | EXT="SXG" | EXT="SXI" | EXT="SXD" | EXT="SXM" | EXT="STW" | EXT="STC" | EXT="STD" | EXT="STI"';

var
  f: text;
  i: integer;
  s: string;
begin
  Assign(f, 'PluginsHuge.ini');
  Rewrite(f);

  Writeln(f, '[ListerPlugins]');

  for i:= 0 to 550-1 do
    begin
    s:= IntToStr(i);
    Writeln(f, s+'=C:\Program Files\Total Commander\Plugins\Lister\ListerPluginName'+s+'.wlx');
    Writeln(f, s+'_detect'+'=('+s+') '+sDetect);
    end;

  Close(f);
end.
