procedure tcSearchDisabledPlugins; forward;

//-----------------------------------------------
procedure tcSearchPlugins(readDesc: boolean);
var
  skey, sval, sfn: string;
  i, n: integer;
  typ: TLibType;
  section: string;
  Keys: TStringList;
begin
  LibClear;

  Keys:= TStringList.Create;

  //--------------------
  // Packer plugins
  typ:= fPacker;
  section:= cLibSections[typ];
  tcReadKeys(section, Keys);
  
  for i:= 0 to Keys.Count-1 do
    begin
    skey:= Keys[i];
    sval:= tcOption(section, skey);
    sfn:= tcOptionPath(section, skey);
    n:= LibIndex(sfn);
    if n=0
      then
        begin
        LibAdd;
        with libList[libNum] do
          begin
          fType:= typ;
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
  typ:= fLister;
  section:= cLibSections[typ];
  tcReadKeys(section, Keys);

  for i:= 0 to Keys.Count-1 do
    begin
    skey:= Keys[i];
    if Pos('_', skey)>0 then Continue;
    sval:= tcOption(section, skey);
    sfn:= tcOptionPath(section, skey);
    n:= StrToIntDef(skey, -1);
    if n<0 then Continue;

    LibAdd;
    with libList[libNum] do
      begin
      fType:= typ;
      fIndex:= n;
      fn:= sfn;
      params:= tcOption(section, skey+cDetect);
      end;
    end;    


  //--------------------
  // Content plugins, new in TC 6.50!
  typ:= fContent;
  section:= cLibSections[typ];
  tcReadKeys(section, Keys);

  for i:= 0 to Keys.Count-1 do
    begin
    skey:= Keys[i];
    if Pos('_', skey)>0 then Continue;
    sval:= tcOption(section, skey);
    sfn:= tcOptionPath(section, skey);
    n:= StrToIntDef(skey, -1);
    if n<0 then Continue;

    LibAdd;
    with libList[libNum] do
      begin
      fType:= typ;
      fIndex:= n;
      fn:= sfn;
      params:= tcOption(section, skey+cDetect);
      end;
    end;    


  //--------------------
  // FS plugins
  typ:= fFS;
  section:= cLibSections[typ];
  tcReadKeys(section, Keys);

  for i:= 0 to Keys.Count-1 do
    begin
    skey:= Keys[i];
    sval:= tcOption(section, skey);
    sfn:= tcOptionPath(section, skey);

    LibAdd;
    with libList[libNum] do
      begin
      fType:= typ;
      fn:= sfn;
      params:= skey;
      end;
    end;    


  //--------------------
  //Cleanup
  Keys.Free;

  //--------------------
  // Search for disabled plugins
  tcSearchDisabledPlugins;


  //--------------------
  // Get descriptions and states
  if readDesc then
    for n:= 1 to libNum do
      with libList[n] do
        title:= LibDesc(fn);
end;


//-----------------------------------------------
function tcTypeFromSection(const section: string): TLibType;
var
  typ: TLibType;
begin
  Result:= fUnknownType;
  for typ:= Low(TLibType) to High(TLibType) do
    if StrIComp(PChar(section), PChar(cLibSections[typ]))=0 then
      begin
      Result:= typ;
      Break
      end;
end;


//-----------------------------------------------
procedure tcSearchDisabledPlugins;
var
  s, ssection, sfn, skey, svalue: string;
  i, nIndex: integer;
  Keys: TStringList;

  procedure KeyError;
  begin
    MsgError(Format('Invalid key "%s" in [DisabledPlugins] section of "%s"', [Keys[i], tcIni]));
  end;

begin
  Keys:= TStringList.Create;
  try
    tcReadKeys(cSectionDisabled, Keys);
    for i:= 0 to Keys.Count-1 do
      begin
      //'section;nnn,fn;key;'
      s:= Keys[i];

      if SCharCount(s, ';') < 3 then
        begin KeyError; Continue; end;

      svalue:= tcOption(cSectionDisabled, s);

      ssection:= SDeleteFrom(s, ';');
      s:= SDeleteTo(s, ';');
      if (ssection='') then
        begin KeyError; Continue; end;
      
      sfn:= SDeleteFrom(s, ';');
      s:= SDeleteTo(s, ';');
      if (sfn='') then
        begin KeyError; Continue; end;
      
      skey:= SDeleteFrom(s, ';');
      if (skey='') then
        begin KeyError; Continue; end;

      sfn:= tcExpandPath(sfn);

      nIndex:= LibIndex(sfn, tcTypeFromSection(ssection)); //find listed only in same section
      if nIndex=0 then //Plugin is not listed
        begin
        LibAdd;
        with libList[libNum] do
          begin
          fn:= sfn;
          params:= '';
          fType:= tcTypeFromSection(ssection);

          //Show params for disabled plugins:
          case fType of
            fFS,
            fPacker:
              params:= skey;
            fLister,
            fContent:
              params:= svalue;
          end;

          if fType in [fLister, fContent] then
            fIndex:= StrToIntDef(skey, 0);

          fDisabled:= true;
          end;
        end
      else //Plugin is already listed (Packer plugin may have several records)
        begin
        if libList[nIndex].fDisabled then
          with libList[nIndex] do
            if tcTypeFromSection(ssection)=fPacker then
              params:= params+' '+skey;
        end;
      end;

  finally
    Keys.Free;
  end;
end;
