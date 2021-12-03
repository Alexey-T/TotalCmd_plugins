unit FakeAPI;

interface

uses Windows, WinInet;

function FindFirstUrlCacheEntry(pattern: PChar;
  var Entry: TInternetCacheEntryInfo;
  var BufSize: DWORD): THandle; stdcall;

function FindNextUrlCacheEntry(h: THandle;
  var Entry: TInternetCacheEntryInfo;
  var BufSize: DWORD): BOOL; stdcall;

function FindCloseUrlCache(h: THandle): BOOL; stdcall;

implementation

uses SProc, SysUtils;

var
  f: Text;
  s, url, size: string;

function FindFirstUrlCacheEntry(pattern: PChar;
  var Entry: TInternetCacheEntryInfo;
  var BufSize: DWORD): THandle; stdcall;
begin
  AssignFile(f, 'C:\DOWNLOAD\LIST1.txt');
  Reset(f);
  Readln(f, s);

  //'1: http://rb.zvuki.ru/cgi-bin -> zvuki.ru rb.zvuki.ru size:361'
  Readln(f, s);
  s:= SDeleteTo(s, ' '); url:= SDeleteFrom(s, ' ');
  s:= SDeleteTo(s, ' ');
  s:= SDeleteTo(s, ' ');
  s:= SDeleteTo(s, ' ');
  s:= SDeleteTo(s, ' '); size:= SDeleteTo(s, 'size:');
  Writeln('Url: ', url);
  Writeln('Size: ', size);

  if BufSize=0 then
    begin
    SetLastError(ERROR_INSUFFICIENT_BUFFER);
    BufSize:= SizeOf(TInternetCacheEntryInfo)+Length(url);
    Result:= 0;
    Exit
    end
  else
    begin
    FillChar(Entry, BufSize, 0);
    lstrcpy(PChar(@Entry.dwReserved), PChar(url));
    Entry.lpszSourceUrlName:= PChar(@Entry.dwReserved);
    Entry.dwSizeLow:= StrToIntDef(size, 0);
    Result:= 1;
    end;
end;

function FindNextUrlCacheEntry(h: THandle;
  var Entry: TInternetCacheEntryInfo;
  var BufSize: DWORD): BOOL; stdcall;
begin
  if Eof(f) then
    begin
    SetLastError(ERROR_NO_MORE_ITEMS);
    Result:= false;
    Exit
    end;

  if BufSize=0 then
    begin
    Readln(f, s);
    s:= SDeleteTo(s, ' '); url:= SDeleteFrom(s, ' ');
    s:= SDeleteTo(s, ' ');
    s:= SDeleteTo(s, ' ');
    s:= SDeleteTo(s, ' ');
    s:= SDeleteTo(s, ' '); size:= SDeleteTo(s, 'size:');
    SetLastError(ERROR_INSUFFICIENT_BUFFER);
    BufSize:= SizeOf(TInternetCacheEntryInfo)+Length(url);
    Result:= false;
    Exit
    end
  else
    begin
    FillChar(Entry, BufSize, 0);
    lstrcpy(PChar(@Entry.dwReserved), PChar(url));
    Entry.lpszSourceUrlName:= PChar(@Entry.dwReserved);
    Entry.dwSizeLow:= StrToIntDef(size, 0);
    Result:= true;
    end;
end;

function FindCloseUrlCache(h: THandle): BOOL; stdcall;
begin
  CloseFile(f);
  Result:= true;
end;

end.
