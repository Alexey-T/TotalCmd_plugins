//{_define debug}
unit CacheObj;

interface

uses Windows, WinInet;

var
  fExclMask: integer = COOKIE_CACHE_ENTRY or URLHISTORY_CACHE_ENTRY;
  fCookiePrefix: string = 'Cookie:';
  fSkipDomains: boolean = true;
  fSkipDomainsList: string = 'com,org,net,gov,edu,priv,co,or,ne,gv,ac';

type
  TCacheItem = record
    sUrl: string;////
    fSize: DWORD;
    fTime: TFileTime;
    end;
  TCacheItems = array[1..MaxInt div SizeOf(TCacheItem)] of TCacheItem;
  PCacheItems = ^TCacheItems;

type
  TReadProgressProc = procedure(n: integer);
  TCacheObj = object
    AllocatedNum: integer;
    ItemsNum: integer;
    Items: PCacheItems;
    Progress: TReadProgressProc;
    constructor Init;
    destructor Done;
    procedure Clear;
    procedure ReadCache;
    procedure AddEntry(lpEntryInfo: PInternetCacheEntryInfo);
    procedure WriteEntry(lpEntryInfo: PInternetCacheEntryInfo);
    procedure WriteAll(const f: Text);
    function TotalSize: integer;
    end;

var
  Cache: TCacheObj;

function GetCacheItemsNum: integer;
procedure SGetDomain(const url: string; var domain, site: string);
function SCharCount(const s: string; ch: char): integer;

implementation

uses SysUtils, RegProc;

const
  CCacheBufferSize = 90*1024; //Must be large enough to hold biggest cache entry

//----------------------------------------------
procedure TCacheObj.AddEntry(lpEntryInfo: PInternetCacheEntryInfo);
const
  CAllocateInc = 50000; //Increase to decrease memory fragmentation
begin
  if lpEntryInfo<>nil then
   with lpEntryInfo^ do
    if (CacheEntryType and fExclMask)=0 then
     begin
     if ItemsNum>=AllocatedNum then
       begin
       Inc(AllocatedNum, CAllocateInc);
       ReallocMem(Items, SizeOf(TCacheItem)*AllocatedNum);
       end;

     Inc(ItemsNum);
     FillChar(Items^[ItemsNum], SizeOf(TCacheItem), 0);

     with Items^[ItemsNum] do
       begin
       sUrl:= lpszSourceUrlName;
       fSize:= dwSizeLow;
       fTime.dwLowDateTime:= LastModifiedTime.dwLowDateTime;
       fTime.dwHighDateTime:= LastModifiedTime.dwHighDateTime;
       {$ifdef debug}
       //Writeln(ItemsNum, ': ', sUrl, ' size:', fSize);
       //Write(ItemsNum, ' ');
       {$endif}
       end;
     if Assigned(Progress) then
       Progress(ItemsNum);
     end;
end;

procedure TCacheObj.WriteEntry(lpEntryInfo: PInternetCacheEntryInfo);
var
  s1, s2: string;
begin
  if lpEntryInfo<>nil then
   with lpEntryInfo^ do
    if (CacheEntryType and fExclMask)=0 then
     begin
     s1:= lpszSourceUrlName;
     s2:= lpszLocalFileName;
     Writeln(s1, ' ', s2);
     end;
end;


procedure TCacheObj.ReadCache;
var
  h: THandle;
  buffer: array[0..CCacheBufferSize-1] of char;
  bufSize: DWORD;
  lpEntryInfo: PInternetCacheEntryInfo;
  dwLastError: DWORD;
begin
  Clear;
  lpEntryInfo:= @buffer;

  FillChar(buffer, SizeOf(buffer), 0);
  bufSize:= SizeOf(buffer);
  h:= FindFirstUrlCacheEntry(nil, lpEntryInfo^, bufSize);
  if h=0 then Exit;
  AddEntry(lpEntryInfo);

  repeat
    FillChar(buffer, SizeOf(buffer), 0);
    bufSize:= SizeOf(buffer);
    if FindNextUrlCacheEntry(h, lpEntryInfo^, bufSize) then
      begin
      AddEntry(lpEntryInfo);
      end
    else
      begin
      dwLastError:= GetLastError;
      if dwLastError=ERROR_NO_MORE_ITEMS then Break;
      if dwLastError=ERROR_INSUFFICIENT_BUFFER then
        begin
        MessageBox(0, PChar(
          'Too big cache entry: '+IntToStr(bufSize)+' bytes.'#13+
          'Please report this problem to developer!'),
          'Buffer overflow', MB_OK or MB_ICONERROR);
        Break
        end;
      end;
  until false;

  FindCloseUrlCache(h);
end;


function GetCacheItemsNum: integer;
var
  h: THandle;
  buffer: array[0..CCacheBufferSize-1] of char;
  bufSize: DWORD;
  lpEntryInfo: PInternetCacheEntryInfo;
  dwLastError: DWORD;
begin
  Result:= 0;
  lpEntryInfo:= @buffer;

  FillChar(buffer, SizeOf(buffer), 0);
  bufSize:= SizeOf(buffer);
  h:= FindFirstUrlCacheEntry(nil, lpEntryInfo^, bufSize);
  if h=0 then Exit;
  with lpEntryInfo^ do
    if (CacheEntryType and fExclMask)=0 then
      Inc(Result);

  repeat
    FillChar(buffer, SizeOf(buffer), 0);
    bufSize:= SizeOf(buffer);
    if FindNextUrlCacheEntry(h, lpEntryInfo^, bufSize) then
      begin
      with lpEntryInfo^ do
       if (CacheEntryType and fExclMask)=0 then
         Inc(Result);
      end
    else
      begin
      dwLastError:= GetLastError;
      if dwLastError=ERROR_NO_MORE_ITEMS then Break;
      if dwLastError=ERROR_INSUFFICIENT_BUFFER then
        begin
        MessageBox(0, PChar(
          'Too big cache entry: '+IntToStr(bufSize)+' bytes.'#13+
          'Please report this problem to developer!'),
          'Buffer overflow', MB_OK or MB_ICONERROR);
        Break
        end;
      end;
  until false;

  FindCloseUrlCache(h);
end;



constructor TCacheObj.Init;
begin
  AllocatedNum:= 0;
  ItemsNum:= 0;
  Items:= nil;
end;

destructor TCacheObj.Done;
begin
  Clear;
end;

procedure TCacheObj.Clear;
var
  n: integer;
begin
  for n:= ItemsNum downto 1 do
    with Items^[n] do
      begin
      sUrl:= '';
      fSize:= 0;
      fTime.dwLowDateTime:= 0;
      fTime.dwHighDateTime:= 0;
      end;
  ReallocMem(Items, 0);
  Items:= nil;
  ItemsNum:= 0;
  AllocatedNum:= 0;
end;

procedure TCacheObj.WriteAll(const f: Text);
var
  i: integer;
  domain, site: string;
begin
  for i:= 1 to ItemsNum do
    with Items^[i] do
      begin
      SGetDomain(sUrl, domain, site);
      Writeln(f, i, ': ', sUrl, ' -> ', domain, ' ', site, ' size:', fSize);
      Write('.');
      end;
end;

function TCacheObj.TotalSize: integer;
var
  i: integer;
begin
  Result:= AllocatedNum*SizeOf(TCacheItem);
  for i:= 1 to ItemsNum do
    with Items^[i] do
      begin
      Inc(Result, Length(sUrl)+1);
      end;
end;

// 'http://z.about.com.ru/f/' -> 'com.ru', 'z.about.com.ru'
// 'http://lenta.ru:8081/'    -> 'lenta.ru', 'lenta.ru:8081'
procedure SGetDomain(const url: string; var domain, site: string);
var
  s, s2: string;
  n, n1, n2: integer;
begin
  domain:= ''; site:= '';
  if url='' then Exit;

  s:= url;
  if Pos(fCookiePrefix, s)=1 then
    begin n:= Pos('@', s); if n>0 then Delete(s, 1, n); end;
  n:= Pos('://', s); if n>0 then Delete(s, 1, n+2);
  n:= Pos('/', s); if n>0 then Delete(s, n, MaxInt);
  site:= s;

  n:= Pos(':', s); if n>0 then Delete(s, n, MaxInt); //name:8081
  if s='' then Exit;
  if s[Length(s)] in ['0'..'9'] then
    begin domain:= s; Exit end;

  n2:= Length(s);
  while (n2>0) and (s[n2]<>'.') do Dec(n2);
  n1:= n2-1;
  while (n1>0) and (s[n1]<>'.') do Dec(n1);

  if fSkipDomains then
    begin
    s2:= Copy(s, n1+1, n2-n1-1);
    //Writeln('Domain2: ', s2);
    if Pos(','+s2+',', ','+fSkipDomainsList+',')>0 then
      begin Dec(n1); while (n1>0) and (s[n1]<>'.') do Dec(n1); end;
    end;

  Delete(s, 1, n1);
  domain:= s;
end;

//----------------------------------------------
function SCharCount(const s: string; ch: char): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to Length(s) do
    if s[i]=ch then Inc(Result);
end;

//----------------------------------------------
initialization
  Cache.Init;

  fCookiePrefix:= GetRegKeyStr(HKEY_LOCAL_MACHINE,
    'Software\Microsoft\Windows\CurrentVersion\Internet Settings\Cache\Special Paths\Cookies',
    'CachePrefix', fCookiePrefix);
  //Writeln('fCookiePrefix: ', fCookiePrefix);

  {
  MessageBox(0, 'Begin', 'Test', MB_OK);
  Cache.ReadCache;
  MessageBox(0, PChar('End: Size = '+IntToStr(Cache.TotalSize)), 'Test', MB_OK);
  }

finalization
  Cache.Done;

end.
