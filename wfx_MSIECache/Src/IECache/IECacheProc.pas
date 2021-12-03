//----------------------------------------------
function GetPluginFilename: string;
var
  buf: array[0 .. Pred(MAX_PATH)] of char;
begin
  SetString(Result, buf,
    GetModuleFileName(hInstance, buf, SizeOf(buf)));
end;

//----------------------------------------------
// fMode = 0: '\domain\site\[NNN] /path'
// fMode = 1: '\domain\site\url'
function FilenameToUrl(const fn: string): string;
var
  i: integer;
begin
  Result:= fn;
  Result:= SDeleteTo(Result, '\');
  Result:= SDeleteTo(Result, '\');
  Result:= SDeleteTo(Result, '\');
  if fMode then Exit;

  i:= Pos(']', Result);
  if i > 0 then
    begin
    i:= StrToIntDef(Copy(Result, 2, i - 2), -1);
    if i >= 0 then
      Result:= Cache.Items^[i].sUrl
    else
      Result:= '';  
    end;  
end;

//----------------------------------------------
function UrlToFilename(const url: string): string;
var
  lpEntryInfo: PInternetCacheEntryInfo;
  dwEntrySize: DWORD;
  dwLastError: DWORD;
begin
  Result:= '';
  dwEntrySize:= 0;
  GetUrlCacheEntryInfo(PChar(url), TInternetCacheEntryInfo(nil^), dwEntrySize);
  dwLastError:= GetLastError;
  if (dwLastError=ERROR_FILE_NOT_FOUND) then Exit;
  if (dwLastError=ERROR_INSUFFICIENT_BUFFER) then
    begin
    GetMem(lpEntryInfo, dwEntrySize);
    if not (GetUrlCacheEntryInfo(PChar(url), lpEntryInfo^, dwEntrySize)) then
      begin FreeMem(lpEntryInfo); Exit end;
    Result:= lpEntryInfo^.lpszLocalFileName;
    FreeMem(lpEntryInfo);
    end;
end;

//---------------
const
  cUrlLen = 120; //260 is for full name, less is for url

function UrlStrip(const S: string; id, level: integer): string;
var
  Sep: string;
begin
  Result:= S;
  if fStripBr then Sep:= '\' else Sep:= ' ';
  if level = 2 then
    begin
    if Pos(fCookiePrefix, Result) = 1 then
      begin
      Result:= Format('[%.3d]%s%s', [id, Sep, Result]);
      end
    else
      begin
      Result:= SDeleteTo(Result, '://');
      Result:= SDeleteTo(Result, '/');
      Result:= Format('[%.3d]%s/%s', [id, Sep, Result]);
      if fStripParam then
        Result:= SDeleteFrom(Result, '?');
      end;
    end;
  if Length(Result) > cUrlLen then
    begin
    SetLength(Result, cUrlLen);
    Result:= Result + '...';
    end;
end;

//---------------
procedure UrlCopy(p: PAnsiChar; const url: string; id, level: integer);
begin
  if fMode then
    lstrcpynA(p, PAnsiChar(AnsiString(url)), MAX_PATH)
  else
    lstrcpynA(p, PAnsiChar(AnsiString(UrlStrip(url, id, level))), MAX_PATH);
end;

//---------------
function LangFN(const s: string): string;
begin
  Result:= ExtractFilePath(GetPluginFilename) + 'Lang\' + s + '.lng';
end;

//---------------
function GetMsg(id: integer): string;
var
  fn: string;
begin
  fn:= LangFN(fLanguage);
  Result:= GetIniKey('Lang', IntToStr(id), '', fn);
  SReplaceAll(Result, '\n', #13);
end;
