unit ATxClipboard;

interface

function SCopyToClipboard(const S: AnsiString; IsOEM: Boolean = False): Boolean;
function SCopyToClipboardW(const S: WideString): Boolean;


implementation

uses
  Windows, SysUtils, SProc;

function SOpenAndClearClipboard: Boolean;
begin
  Result:= OpenClipboard(0);
  if Result then
    EmptyClipboard;
end;

//CF_UNICODETEXT format is supported only under NT
function SCopyToClipboardW_NT(const S: WideString; DoClear: Boolean): Boolean;
var
  DataSize, BufferSize: Integer;
  hData: HGLOBAL;
  gData: Pointer;
begin
  DataSize := Length(S) * 2;

  if DataSize > 0 then
  begin
    if DoClear then
    begin
      if not SOpenAndClearClipboard then
        begin Result := False; Exit end;
    end;

    BufferSize := DataSize + 2;
    hData := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, BufferSize);
    if hData <> 0 then
    begin
      gData := GlobalLock(hData);
      if gData <> nil then
      begin
        Move(S[1], gData^, BufferSize);
        GlobalUnlock(hData); 
        SetClipboardData(CF_UNICODETEXT, hData); 
      end;
    end;

    if DoClear then
      CloseClipboard;
  end;

  Result := True;
end;

function SCopyToClipboard(const S: AnsiString; IsOEM: Boolean = False): Boolean;
const
  Formats: array[Boolean] of Integer = (CF_TEXT, CF_OEMTEXT);
var
  DataSize, BufferSize: Integer;
  hData: HGLOBAL;
  gData: Pointer;
begin
  DataSize := Length(S);

  if DataSize > 0 then
  begin
    if not SOpenAndClearClipboard then
      begin Result := False; Exit end;

    BufferSize := DataSize + 1;
    hData := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, BufferSize);
    if hData <> 0 then
    begin
      gData := GlobalLock(hData);
      if gData <> nil then
      begin
        Move(S[1], gData^, BufferSize);
        GlobalUnlock(hData); 
        SetClipboardData(Formats[IsOEM], hData); 
      end;
    end;

    //Also copy in CF_UNICODETEXT format for compatability with Windows controls
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      if IsOEM then
        SCopyToClipboardW_NT(WideString(ToANSI(S)), False)
      else
        SCopyToClipboardW_NT(WideString(S), False);
    end;

    CloseClipboard; 
  end;

  Result := True;
end;


function SCopyToClipboardW(const S: WideString): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := SCopyToClipboardW_NT(S, True)
  else
    Result := SCopyToClipboard(AnsiString(S));;
end;


end.
