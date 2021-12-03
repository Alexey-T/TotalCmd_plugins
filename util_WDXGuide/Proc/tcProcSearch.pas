//-----------------------------------------------
procedure tcSearchPlugins2; forward;
procedure tcSearchPlugins(readDesc: boolean);
var
  skey, sval, sfn: string;
  i, n: integer;
  ffType: TLibType;
  section: string;
begin
  LibClear;

  //--------------------
  // Packer plugins
  ffType:= fPacker;
  section:= sLibSections[ffType];
  tcReadKeys(section);
  for i:= 1 to _tcKeysNum do
    begin
    skey:= _tcKeys[i];
    sval:= tcOption(section, skey);
    sfn:= tcOptionPath(section, skey);
    n:= LibIndex(sfn);
    if n=0
      then
        begin
        LibAdd;
        with libList[libNum] do
          begin
          fType:= ffType;
          fn:= sfn;
          params:= skey;
          end
        end
      else
        with libList[n] do
          params:= params+' '+skey;
    end;


  //--------------------
  // Lister plugins
  ffType:= fLister;
  section:= sLibSections[ffType];
  tcReadKeys(section);
  for i:= 1 to _tcKeysNum do
    begin
    skey:= _tcKeys[i];
    if Pos('_', skey)>0 then Continue;
    sval:= tcOption(section, skey);
    sfn:= tcOptionPath(section, skey);
    n:= StrToIntDef(skey, -1);
    if n<0 then Continue;

    LibAdd;
    with libList[libNum] do
      begin
      fType:= ffType;
      fIndex:= n;
      fn:= sfn;
      params:= tcOption(section, skey+'_detect');
      end;
    end;    


  //--------------------
  // Content plugins, new in TC 6.50!
  ffType:= fContent;
  section:= sLibSections[ffType];
  tcReadKeys(section);
  for i:= 1 to _tcKeysNum do
    begin
    skey:= _tcKeys[i];
    if Pos('_', skey)>0 then Continue;
    sval:= tcOption(section, skey);
    sfn:= tcOptionPath(section, skey);
    n:= StrToIntDef(skey, -1);
    if n<0 then Continue;

    LibAdd;
    with libList[libNum] do
      begin
      fType:= ffType;
      fIndex:= n;
      fn:= sfn;
      params:= tcOption(section, skey+'_detect');
      end;
    end;    


  //--------------------
  // FS plugins
  ffType:= fFS;
  section:= sLibSections[ffType];
  tcReadKeys(section);
  for i:= 1 to _tcKeysNum do
    begin
    skey:= _tcKeys[i];
    sval:= tcOption(section, skey);
    sfn:= tcOptionPath(section, skey);

    LibAdd;
    with libList[libNum] do
      begin
      fType:= ffType;
      fn:= sfn;
      params:= skey;
      end;
    end;    


  (* //enabled in TC Plugman
  //--------------------
  // Search for disabled plugins
  tcSearchPlugins2;

  //--------------------
  // Get descriptions and states
  if readDesc then
    for n:= 1 to libNum do
      with libList[n] do
        begin
        title:= LibDesc(fn);
        fDeleted:= not IsFileExist(fn);
        fLoaded:= IsFileUsed(fn);
        case fShowVersions of
          versionsNone: version:= '';
          versionsNum: version:= FileVersionInfo(fn, '');
          versionsStr: version:= FileVersionInfo(fn, vsProductVersion);
        end;
        end;
  *)
end;



//-----------------------------------------------
// search for disabled plugins
procedure tcSearchPlugins2;
var
  s, ssection, sfn, skey: string;
  i, n: integer;
  k: TLibType;
begin
  tcReadKeys(section_disabled);
  for i:= 1 to _tcKeysNum do
    begin
    //'section;nnn,fn;key;'
    s:= _tcKeys[i];
    //svalue:= tcOption(section_disabled, s);
    ssection:= SDeleteFrom(s, ';');  s:= SDeleteTo(s, ';');
    sfn:=      SDeleteFrom(s, ';');  s:= SDeleteTo(s, ';');
    skey:=     SDeleteFrom(s, ';');

    sfn:= SDeleteTo(sfn, ',');
    sfn:= SExpandVars(sfn); //%COMMANDER_PATH%

    //Writeln(Format('section: %s  fn: %s  key: %s', [ssection, sfn, skey]));
    n:= LibIndex(sfn);
    if n=0 then //показывать запрещенный плагин только один раз
      begin
      LibAdd;
      with libList[libNum] do
        begin
        fn:= sfn;
        params:= '';
        fType:= TLibType(0);
        for k:= Low(TLibType) to High(TLibType) do
          if ssection=sLibSections[k] then
            begin fType:= k; Break end;

        //Show params for disabled plugins:
        //if fType=fFS then params:= skey else
        // if fType=fLister then params:= svalue else
        //  if fType=fContent then params:= svalue;

        fDeleted:= not IsFileExist(fn);
        fLoaded:= IsFileUsed(fn);
        fDisabled:= true;
        end;
      end;
    end;
end;
