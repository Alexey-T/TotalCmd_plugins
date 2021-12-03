//Based on VersionInfo unit by Sortland Automasjon

unit VersionInfo;

interface

const
  vsCompanyName       = 'CompanyName';
  vsFileDescription   = 'FileDescription';
  vsFileVersion       = 'FileVersion';
  vsInternalName      = 'InternalName';
  vsOriginalFilename  = 'OriginalFilename';
  vsProductName       = 'ProductName';
  vsProductVersion    = 'ProductVersion';
  vsLegalCopyright    = 'LegalCopyright';
  vsLegalTrademarks   = 'LegalTrademarks';
  vsComments          = 'Comments';
  vsPrivateBuild      = 'PrivateBuild';
  vsSpecialBuild      = 'SpecialBuild';

function FileVersionInfo(const fn, vsKey: string): string;

implementation

uses Windows;

function Format2(const fmt: string; p1: integer; p2: PChar): string;
var
  buf: array[0..500] of char;
begin
  FillChar(buf, SizeOf(buf), #0);
  asm
    push p2;
    push p1;
  end;
  wsprintf(buf, PChar(fmt));
  Result:= buf;
end;

function SwapLong(L: Longint): Longint; assembler;
asm
  rol eax, 16;
end;

function FileVersionInfo(const fn, vsKey: string): string;
var
  buf: pointer;
  bufSize, n: DWORD;
  //pInfo: PVSFixedFileInfo;
  p: pointer;
begin
  Result:= '';
  bufSize:= GetFileVersionInfoSize(PChar(fn), n);
  if bufSize=0 then Exit;
  GetMem(buf, bufSize);

  if GetFileVersionInfo(PChar(fn), 0, bufSize, buf) then
    begin
    if VerQueryValue(buf, '\VarFileInfo\Translation', p, n) and
       //VerQueryValue(buf, PChar(Format(
       //  '\StringFileInfo\%.8x\%s', [SwapLong(Longint(p^)), vsKey])), p, n)
       VerQueryValue(buf, PChar(Format2(
         '\StringFileInfo\%.8x\%s', SwapLong(Longint(p^)), PChar(vsKey))), p, n)
         then Result:= PChar(p);
    end;

  FreeMem(buf, bufSize);
end;

end.
