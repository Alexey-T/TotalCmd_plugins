// Written 01/2004 by Alexey Torgashin
// Based on VersionInfo unit by Sortland Automasjon

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

uses
  Windows, SProc;

function SwapLong(L: Longint): Longint; assembler;
asm
  rol eax, 16;
end;

function FileVersionInfo(const fn, vsKey: string): string;
var
  buf: pointer;
  bufSize, n: DWORD;
  pInfo: PVSFixedFileInfo;
  p: pointer;
begin
  Result:= '';
  bufSize:= GetFileVersionInfoSize(PChar(fn), n);
  if bufSize=0 then Exit;
  GetMem(buf, bufSize);

  if GetFileVersionInfo(PChar(fn), 0, bufSize, buf) then
    if vsKey=''
      then
        begin
        if VerQueryValue(buf, '\', pointer(pInfo), n) then
          Result:= SFormat('%d.%d.%d.%d',
                     [HiWord(pInfo.dwFileVersionMS),
                      LoWord(pInfo.dwFileVersionMS),
                      HiWord(pInfo.dwFileVersionLS),
                      LoWord(pInfo.dwFileVersionLS)]);
        end
      else
        begin
        if VerQueryValue(buf, '\VarFileInfo\Translation', p, n) and
           VerQueryValue(buf, PChar(SFormat(
             '\StringFileInfo\%.8x\%s', [SwapLong(Longint(p^)), vsKey])), p, n) then
             Result:= string(PChar(p));
        end;

  FreeMem(buf, bufSize);
end;

end.
