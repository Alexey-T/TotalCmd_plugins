unit FilesObj;

interface

uses Windows;

type
  TFileItem = record
    fName: AnsiString;
    fSize: DWORD;
    fTime: TFileTime;
    fNum: integer;
    end;
  TFileItems = array[1..MaxInt div SizeOf(TFileItem)] of TFileItem;
  PFileItems = ^TFileItems;

  TFileList = object
    AllocatedNum: integer;
    ItemsNum: integer;
    Items: PFileItems;
    constructor Init;
    destructor Done;
    procedure Clear;
    function IndexOf(const sfn: string): integer;
    procedure AddItem(const sfn: string; size: integer; const time: TFileTime; num: integer);
    procedure ReadCache(level: integer; const up: string);
    procedure WriteAll(const f: Text);
    end;
  PFileList = ^TFileList;

implementation

uses SysUtils, CacheObj;

constructor TFileList.Init;
begin
  AllocatedNum:= 0;
  ItemsNum:= 0;
  Items:= nil;
end;
  
destructor TFileList.Done;
begin
  Clear;
end;
  
procedure TFileList.Clear;
var
  n: integer;
begin
  for n:= ItemsNum downto 1 do
    with Items^[n] do
      begin
      fName:= '';
      fSize:= 0;
      fNum:= 0;
      fTime.dwLowDateTime:= 0;
      fTime.dwHighDateTime:= 0;
      end;
  ReallocMem(Items, 0);
  Items:= nil;
  ItemsNum:= 0;
  AllocatedNum:= 0;
end;

function TFileList.IndexOf(const sfn: string): integer;
var
  i: integer;
begin
  for i:= 1 to ItemsNum do
    with Items^[i] do
      if LowerCase(fName) = LowerCase(sfn) then
        begin Result:= i; Exit end;
  Result:= -1;
end;

procedure TFileList.AddItem(const sfn: string; size: integer; const time: TFileTime; num: integer);
const
  CAllocateInc = 10000;
begin
  if IndexOf(sfn) > 0 then Exit;
  if ItemsNum >= AllocatedNum then
    begin
    Inc(AllocatedNum, CAllocateInc);
    ReallocMem(Items, SizeOf(TFileItem)*AllocatedNum);
    end;

  Inc(ItemsNum);
  FillChar(Items^[ItemsNum], SizeOf(TFileItem), 0);
  with Items^[ItemsNum] do
    begin
    fName:= sfn;
    fSize:= size;
    fNum:= num;
    fTime.dwLowDateTime:= time.dwLowDateTime;
    fTime.dwHighDateTime:= time.dwHighDateTime;
    {$ifdef debug}
    //Writeln(ItemsNum, ': ', fName, ' size:', fSize);
    {$endif}
    end;
end;

//level: 0: domains, 1: sites in domain "up", 2: urls in site "up"
procedure TFileList.ReadCache(level: integer; const up: string);
var
  i: integer;
  domain, site: string;
  time: TFileTime;
begin
  Clear;
  time.dwLowDateTime:= 0;
  time.dwHighDateTime:= 0;
  for i:= 1 to Cache.ItemsNum do
    with Cache.Items^[i] do
      begin
      SGetDomain(sUrl, domain, site);
      if (domain='') or (site='') then Continue;
      case level of
        0:
          begin
          AddItem(domain, 0, fTime, i);
          end;
        1:
          begin
          if domain=up then
            AddItem(site, 0, fTime, i);
          end;
        2:
          begin
          if site=up then
            AddItem(sUrl, fSize, fTime, i);
          end;
      end;
      end;
end;

procedure TFileList.WriteAll(const f: Text);
var
  i: integer;
begin
  for i:= 1 to ItemsNum do
    with Items^[i] do
      begin
      Writeln(f, i, ': "', fName, '" ', IntToStr(fSize));
      end;
end;

end.
