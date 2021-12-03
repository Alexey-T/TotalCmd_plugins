//-----------------------------------------------
function tcInstalled(const fn: string; typ: TLibType): boolean; overload;
var
  Keys: TStringList;
  i: integer;
  s, section: string;
begin
  Result:= false;
  Keys:= TStringList.Create;
  try
    section:= cLibSections[typ];
    tcReadKeys(section, Keys);
    for i:= 0 to Keys.Count-1 do
      begin
      s:= tcOptionPath(section, Keys[i]);
      if StrIComp(PChar(s), PChar(fn))=0 then
        begin
        Result:= true;
        Break
        end;
      end;
  finally
    Keys.Free;
  end;
end;


//-----------------------------------------------
const
  sBoolean: array[boolean] of string = ('FALSE', 'TRUE');

function tcInstalled(const fn: string; var fParams: string): boolean; overload;
var
  n: integer;
begin
  tcSearchPlugins(false);
  n:= LibIndex(fn);
  Result:= n>0;
  if Result then
    fParams:= libList[n].params;
end;


//-----------------------------------------------
function tcUninstallPath2(const fn: string; DoDisable: boolean): boolean;
var
  t, typ: TLibType;
begin
  typ:= fUnknownType;
  for t:= Succ(fUnknownType) to High(TLibType) do
    if tcInstalled(fn, t) then
      begin typ:= t; Break end;

  Result:= typ <> fUnknownType;
  if Result then
    tcUninstallPath(cLibSections[typ], fn, DoDisable);
end;


//-----------------------------------------------
(*
function tcInstalled2(const fn: string; var fParams: string): boolean;
var
  Keys: TStringList;
  skey, sfn, section: string;
  nType: TLibType;
  i: integer;
begin
  Result:= false;
  fParams:= '';
  Keys:= TStringList.Create;
  try
    for nType:= Low(TLibType) to High(TLibType) do
      begin
      section:= cLibSections[nType];
      tcReadKeys(section);

      case nType of
        fFS,
        fPacker:
          for i:= 1 to _tcKeysNum do
            begin
            skey:= _tcKeys[i];
            sfn:= tcOptionPath(section, skey);
            if StrIComp(PChar(sfn), PChar(fn))=0 then
              begin
              Result:= true;
              fParams:= fParams+' '+skey;
              end;
            end;

        fLister,
        fContent:
          for i:= 1 to _tcKeysNum do
            if Pos('_', _tcKeys[i])=0 then
              begin
              skey:= _tcKeys[i];
              sfn:= tcOptionPath(section, skey);
              if StrIComp(PChar(sfn), PChar(fn))=0 then
                begin
                Result:= true;
                fParams:= fParams+' '+tcOption(section, skey+cDetect);
                end;
              end;    
      end;
      end;
  finally
    Keys.Free;
  end;
end;
*)


//-----------------------------------------------
procedure tcReport(const f: Text);
var
  n: integer;
const
  sTypes: array[TLibType] of string = (
    '?',
    'Lister',
    'Packer',
    'FS',
    'Content');
begin
  Writeln(f, 'TC plugins report');
  //Writeln(f, 'OS: ', SOsName, ' version ', SOsVersion);
  Writeln(f);
  Writeln(f, 'TC version: ', FileVersionInfo(tcDir+'\Totalcmd.exe', vsProductVersion));
  Writeln(f, 'TC dir: ', tcDir);
  Writeln(f, 'TC ini: ', tcIni);
  Writeln(f, 'Plugins: ', libNum);
  Writeln(f);
  for n:= 1 to libNum do
    with libList[n] do
      begin
      Writeln(f, Format('%-40.40s %-40s %-10s  %-10s "%s"',
        [Format('%s: %s', [sTypes[fType], title]),
         Format('"%s"', [fn]),
         version, LibState(n), params]));
      end;
end;


//---------------------------------------------------------------------
procedure tcReadPackerFlags(const fn: string; var Flags: TPackerFlags);
var
  Keys: TStringList;
  skey, sval, sfn, section: string;
  i, nflag: integer;
begin
  FillChar(Flags, SizeOf(Flags), 0);
  Flags.flagDefault:= tcPackerCaps(fn);

  Keys:= TStringList.Create;
  try
    section:= cLibSections[fPacker];
    tcReadKeys(section, Keys);
    for i:= 0 to Keys.Count-1 do
      begin
      skey:= Keys[i];
      sval:= SExpandVars(tcOption(section, skey));

      if Pos(',', sval)=0
        then
          begin
          nflag:= Flags.flagDefault;
          sfn:= sval
          end
        else
          begin
          nflag:= StrToIntDef(SDeleteFrom(sval, ','), 0);
          sfn:= SDeleteTo(sval, ',');
          end;

      if StrIComp(PChar(fn), PChar(sfn))=0 then
        with Flags do
          begin
          Inc(num);
          ext[num-1].str:= skey;
          ext[num-1].flag:= nflag;
          //MessageBox(0, PChar(Format('str: %s, flag: %d', [skey, nflag])), 'Flag', MB_OK);
          end;
      end;
  finally
    Keys.Free;
  end;
end;


//---------------------------------------------------------------------
function tcPackerFlagExist(const Flags: TPackerFlags; const fExt: string): boolean;
var
  i: integer;
begin
  Result:= false;
  with Flags do
    for i:= 0 to num-1 do
      if StrIComp(PChar(ext[i].str), PChar(fExt))=0 then
        begin Result:= true; Break end;
end;

//---------------------------------------------------------------------
procedure tcWritePackerFlags(const fn: string; const Flags: TPackerFlags);
var
  section: string;
  oldFlags: TPackerFlags;
  i: integer;
begin
  section:= cLibSections[fPacker];

  tcReadPackerFlags(fn, oldFlags);
  with oldFlags do
    for i:= 0 to num-1 do
      if not tcPackerFlagExist(Flags, ext[i].str) then
        tcOptionDel(section, ext[i].str, false);

  with Flags do
    for i:= 0 to num-1 do
      tcOptionSet(section, ext[i].str, Format('%d,%s', [ext[i].flag, fn])); 
end;

//---------------------------------------------------------------------
procedure InitLog(var Log: TLibLog);
begin
  with Log do
    begin
    Info:= TStringList.Create;
    Errors:= TStringList.Create;
    end;
end;

procedure ClearLog(const Log: TLibLog);
begin
  with Log do
    begin
    Info.Clear;
    Errors.Clear;
    end;
end;

procedure FreeLog(var Log: TLibLog);
begin
  with Log do
    begin
    FreeAndNil(Info);
    FreeAndNil(Errors);
    end;
end;

//---------------------------------------------------------------------
function tcDefButtonBar: string;
begin
  Result:= tcOption('Buttonbar', 'Buttonbar');
  Result:= SExpandVars(Result);
  if Result='' then
    Result:= tcDir+'\Default.bar';
end;

//---------------------------------------------------------------------
function tcEditor: string;
begin
  Result:= tcOption('Configuration', 'Editor');
  Result:= SExpandVars(Result);
  if Result='' then
    Result:= 'notepad.exe';
end;
