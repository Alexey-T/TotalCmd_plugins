{$I-}

unit tcReport;

interface

function tcSaveReport(const sVer, sTCExe, sTCIni, sTCPluginsPath: string): boolean;

var
  OptRpt: record
    FN: string;
    Names,
    Vers,
    States,
    Assocs,
    Files: boolean;
  end;

implementation

uses
  Windows, SysUtils, Classes,
  tcProc, RegProc, VersionInfo, SProc, SProc2, FProc, Msg;

const
  sFmt1 = '%0:-25s ';
  sFmt2 = '%1:-12s ';
  sFmt3 = '%2:-12s ';
  sFmt4 = '%3:s ';
  sFmt5 = '"%4:s"';

procedure ListItem(const f: Text; nIndex: integer; TypeFilter: TLibType);
var
  s: string;
begin
  s:= '';
  if OptRpt.Names then s:= s+sFmt1;
  if OptRpt.Vers then s:= s+sFmt2;
  if OptRpt.States then s:= s+sFmt3;
  if OptRpt.Assocs then s:= s+sFmt4;
  if OptRpt.Files then s:= s+sFmt5;

  with LibList[nIndex] do
    if fType=TypeFilter then
      Writeln(f, Format(s, [title, FileVersionInfo(fn, ''), LibState(nIndex), params, fn]));
end;

const
  CR = #13#10;

function tcSaveReport;
var
  list: TStringList;
  f: text;
  i: integer;
begin
  Result:= false;

  list:= TStringList.Create;

  try
    try
      list.Sorted:= true;
      list.Duplicates:= dupAccept;

      AssignFile(f, OptRpt.FN);
      Rewrite(f);

      if IOResult=0 then
        begin
        // sorting
        list.Clear;
        for i:= 1 to libNum do
          list.Add(Format('%s =%d', [libList[i].title, i]));

        // listing
        Writeln(f, Format(S0080, [sVer, DateToStr(Date), {TimeToStr(Time)}'']));
        Writeln(f, Format(S0081, [FileVersionInfo(sTCExe, vsProductVersion), sTCExe, sTCIni]));
        Writeln(f, Format(S0082, [SOsName, SOsVersion]));
        Writeln(f, Format(S0083, [sTCPluginsPath]));

        Writeln(f, S0084);
        for i:= 0 to list.Count-1 do
          ListItem(f, StrToInt(SParamVal(list[i])), fLister);

        Writeln(f, S0085);
        for i:= 0 to list.Count-1 do
          ListItem(f, StrToInt(SParamVal(list[i])), fPacker);

        Writeln(f, S0086);
        for i:= 0 to list.Count-1 do
          ListItem(f, StrToInt(SParamVal(list[i])), fFS);

        Writeln(f, S0087);
        for i:= 0 to list.Count-1 do
          ListItem(f, StrToInt(SParamVal(list[i])), fContent);

        Result:= true;
        end;

    finally
      CloseFile(f);
      list.Free;
    end;
  except
  end;
end;


end.
