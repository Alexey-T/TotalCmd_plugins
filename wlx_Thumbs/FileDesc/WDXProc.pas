//--------------------------------------------
function WDXFieldType(n: integer): string;
begin
  case n of
    FT_NUMERIC_32:              Result:= 'FT_NUMERIC_32';
    FT_NUMERIC_64:              Result:= 'FT_NUMERIC_64';
    FT_NUMERIC_FLOATING:        Result:= 'FT_NUMERIC_FLOATING';
    FT_DATE:                    Result:= 'FT_DATE';
    FT_TIME:                    Result:= 'FT_TIME';
    FT_DATETIME:                Result:= 'FT_DATETIME';
    FT_BOOLEAN:                 Result:= 'FT_BOOLEAN';
    FT_MULTIPLECHOICE:          Result:= 'FT_MULTIPLECHOICE';
    FT_STRING:                  Result:= 'FT_STRING';
    FT_FULLTEXT:                Result:= 'FT_FULLTEXT';
    FT_NOSUCHFIELD:             Result:= 'FT_NOSUCHFIELD';
    FT_FILEERROR:               Result:= 'FT_FILEERROR';
    FT_FIELDEMPTY:              Result:= 'FT_FIELDEMPTY';
    FT_DELAYED:                 Result:= 'FT_DELAYED';
    FT_ONDEMAND:                Result:= 'FT_ONDEMAND';
    else Result:= '?';
  end;
end;

//--------------------------------------------
procedure WDXInitPlugin(PluginNum: integer; const DefIni: string);
var
  fieldsNum: integer;
  dps: TContentDefaultParamStruct;
  buf1, buf2: array[0..5*1024] of char;
  res, i: integer;
begin
  with DescPlugins[PluginNum] do
    if fType=TypeWDX then
      with fRecordWDX do
        begin
        @fContentGetDetectString:= GetProcAddress(fHandle, 'ContentGetDetectString');
        if Assigned(fContentGetDetectString) then
          begin
          FillChar(buf1, SizeOf(buf1), 0);
          fContentGetDetectString(buf1, SizeOf(buf1));
          end;

        @fContentSetDefaultParams:= GetProcAddress(fHandle, 'ContentSetDefaultParams');
        if Assigned(fContentSetDefaultParams) then
          begin
          FillChar(dps, SizeOf(dps), 0);
          dps.Size:= SizeOf(dps);
          dps.PluginInterfaceVersionLow:= 20;
          dps.PluginInterfaceVersionHi:= 1;
          lstrcpy(dps.DefaultIniName, PChar(DefIni));
          fContentSetDefaultParams(@dps);
          end;

        @fContentGetSupportedField:= GetProcAddress(fHandle, 'ContentGetSupportedField');
        if not Assigned(fContentGetSupportedField) then Exit;

        @fContentGetValue:= GetProcAddress(fHandle, 'ContentGetValue');
        if not Assigned(fContentGetValue) then Exit;

        @fContentPluginUnloading:= GetProcAddress(fHandle, 'ContentPluginUnloading');

        for i:= Low(fFields) to High(fFields) do
          fFields[i].fName:= '';
        fieldsNum:= -1;
        repeat
          FillChar(buf1, SizeOf(buf1), 0);
          FillChar(buf2, SizeOf(buf2), 0);
          try
            res:= fContentGetSupportedField(fieldsNum+1, buf1, buf2, SizeOf(buf1));
          except
            res:= ft_nomorefields;
            MessageBox(0,
              PChar(fFileName+':'#13'exception in ContentGetSupportedField'),
              PChar(fMainCaption), MB_OK or MB_ICONERROR or MB_SETFOREGROUND);
          end;
          if res=ft_nomorefields then Break;
          if fieldsNum=High(fFields) then Break;
          Inc(fieldsNum);
          fFields[fieldsNum].fType:= res;
          fFields[fieldsNum].fUnits:= buf2;
          fFields[fieldsNum].fName:= buf1;
        until false;
        end;
end;

//--------------------------------------------
procedure WDXShow(PluginNum: integer);
var
  i: integer;
  s: string;
begin
  s:= '';
  with DescPlugins[PluginNum] do
    begin
    if fType=TypeWDX then
      with fRecordWDX do
        for i:= Low(fFields) to High(fFields) do
          if fFields[i].fName<>'' then
            s:= s+fFields[i].fName+' ';
    MessageBox(0, PChar('Plugin: '+fFileName+#13+
      'Fields: '+s), 'Plugin', MB_OK);
    end;
end;

//--------------------------------------------
function SGetUnit(var sUnits: string): string;
var
  k: integer;
begin
  k:= Pos('|', sUnits); if k=0 then k:= MaxInt;
  Result:= Copy(sUnits, 1, k-1);
  Delete(sUnits, 1, k);
end;

//--------------------------------------------
function WDXGetField(PluginNum: integer; const sField, sFile: string): string;
const
  fFlag1 = $0001;
var
  buf1: array[0..5*1024] of char;
  fnval: integer absolute buf1;
  fnval64: Int64 absolute buf1;
  ffval: Double absolute buf1;
  fdate: TDateFormat absolute buf1;
  ftime: TTimeFormat absolute buf1;
  ffiletime: TFileTime absolute buf1;
  ltime: TFileTime;
  stime: TSystemTime;
  nField: integer;
  //sUnits, sUnit: string;
  //nUnits, nUnit: integer;
  nUnit, res, n: integer;
begin
  Result:= '';
  with DescPlugins[PluginNum] do
    if fType=TypeWDX then
      with fRecordWDX do
        begin
        nField:= -1;
        for n:= Low(fFields) to High(fFields) do
          if fFields[n].fName=sField then begin nField:= n; Break end;
        if nField=-1 then
          begin
          if (fFlags and fFlag1)=0 then
            MessageBox(0, PChar(SFormat('Cannot find field "%s" in plugin %s', [sField, ExtractFileName(fFileName)])),
              PChar(fMainCaption), MB_OK or MB_ICONERROR or MB_SETFOREGROUND);
          fFlags:= fFlags or fFlag1;
          Exit
          end;

        {
        sUnits:= fFields[nField].fUnits;
        if fType=ft_multiplechoice
          then nUnits:= 1
          else nUnits:= SCharCount(sUnits, '|')+1;

        for nn:= 0 to nUnits-1 do
          begin
          if nUnits=1
            then begin nUnit:= 0; sUnit:= ''; end
            else begin nUnit:= nn; sUnit:= SGetUnit(sUnits); end;
        }

        nUnit:= 0;
        FillChar(buf1, SizeOf(buf1), 0);
        try
          res:= fContentGetValue(PChar(sFile), nField, nUnit, @buf1, SizeOf(buf1), 0);
        except
          res:= ft_fileerror;
          MessageBox(0,
            PChar(fFileName+':'#13'exception in ContentGetValue'),
            PChar(fMainCaption), MB_OK or MB_ICONERROR or MB_SETFOREGROUND);
        end;

        case res of 
          ft_fieldempty:          Result:= '';
          ft_numeric_32:          Result:= IntToStr(fnval);
          ft_numeric_64:          Result:= IntToStr(fnval64);
          ft_numeric_floating:    Result:= FloatToStr(ffval);
          ft_date:                Result:= SFormat('%2.2d.%2.2d.%4.4d', [fdate.wDay, fdate.wMonth, fdate.wYear]);
          ft_time:                Result:= SFormat('%2.2d:%2.2d:%2.2d', [ftime.wHour, ftime.wMinute, ftime.wSecond]);
          
          ft_datetime:            begin
                                  FileTimeToLocalFileTime(ffiletime, ltime);
                                  FileTimeToSystemTime(ltime, stime);
                                  Result:= SFormat('%2.2d.%2.2d.%4.4d %2.2d:%2.2d:%2.2d',
                                          [stime.wDay, stime.wMonth, stime.wYear,
                                           stime.wHour, stime.wMinute, stime.wSecond]);
                                  end;

          ft_boolean:             if fnval=0 then Result:= 'FALSE' else Result:= 'TRUE';
          ft_string,
          ft_multiplechoice:      Result:= buf1;
          else Result:= ''; //WdxFieldType(res);
        end;

        end;
end;

//--------------------------------------------
function WDXGetDesc(PluginNum: integer; const sFile: string): string;
var
  s, sVal, sBefore, sField, sPrefix, sSuffix: string;
  n, nn, pos1, pos2: integer;
begin
  Result:= '';
  with DescPlugins[PluginNum] do
    if fType=TypeWDX then
      with fRecordWDX do
        begin
        s:= fFormat;
        repeat
          pos1:= PosFrom('[', s, 1);    if pos1=0 then Break;
          pos2:= PosFrom(']', s, pos1); if pos2=0 then Break;
          sBefore:= Copy(s, 1, pos1-1);
          sField:= Copy(s, pos1+1, pos2-pos1-1);
          Delete(s, 1, pos2);
          if sField='' then Continue;

          sPrefix:= '';
          sSuffix:= '';
          if sField[1]='"' then
            begin
            n:= PosFrom('"', sField, 2); if n=0 then Continue;
            sPrefix:= Copy(sField, 2, n-2);
            Delete(sField, 1, n);
            end;
          n:= Pos('"', sField);
          if n>0 then
            begin
            nn:= PosFrom('"', sField, n+1); if nn=0 then Continue;
            sSuffix:= Copy(sField, n+1, nn-n-1);
            Delete(sField, n, MaxInt);
            end;
          //Writeln('["', sPrefix, '"', sField, '"', sSuffix, '"]');

          sVal:= WDXGetField(PluginNum, sField, sFile);
          if sVal<>'' then
            Result:= Result+sBefore+sPrefix+sVal+sSuffix;
        until false;
        end;
end;
