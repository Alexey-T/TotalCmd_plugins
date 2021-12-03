unit SecProc;

interface

uses Windows;

function FFileGroup(const fn: WideString): string;
function FFileOwner(const fn: WideString): string;

implementation

//----------------------------------------------
function FFileOwner(const fn: WideString): string;
const
  BufSize = 1024;
var
  SecDescr: PSecurityDescriptor;
  SizeNeeded, SizeNeeded2: DWORD;
  OwnerSID: PSID;
  OwnerDefault: BOOL;
  OwnerName, DomainName: array[0..BufSize-1] of char;
  OwnerType: SID_NAME_USE;
  BufDescr: array[0..BufSize-1] of byte;
  BufSID: DWORD;
begin
  Result:= '';
  SecDescr:= @BufDescr;
  OwnerSID:= @BufSID;

  if not GetFileSecurityW(PWChar(fn), OWNER_SECURITY_INFORMATION,
    SecDescr, BufSize, SizeNeeded) then Exit;
  if not GetSecurityDescriptorOwner(SecDescr,
    OwnerSID, OwnerDefault) then Exit;
  SizeNeeded:= BufSize;
  SizeNeeded2:= BufSize;
  if not LookupAccountSid(nil, OwnerSID, OwnerName, SizeNeeded,
    DomainName, SizeNeeded2, OwnerType) then Exit;
  Result:= string(OwnerName)+'@'+string(DomainName);
end;

//----------------------------------------------
function FFileGroup(const fn: WideString): string;
const
  BufSize = 1024;
var
  SecDescr: PSecurityDescriptor;
  SizeNeeded, SizeNeeded2: DWORD;
  GroupSID: PSID;
  GroupDefault: BOOL;
  OwnerName, DomainName: array[0..BufSize-1] of char;
  OwnerType: SID_NAME_USE;
  BufDescr: array[0..BufSize-1] of byte;
  BufSID: DWORD;
begin
  Result:= '';
  SecDescr:= @BufDescr;
  GroupSID:= @BufSID;

  if not GetFileSecurityW(PWChar(fn), GROUP_SECURITY_INFORMATION,
    SecDescr, BufSize, SizeNeeded) then Exit;
  if not GetSecurityDescriptorGroup(SecDescr, GroupSID, GroupDefault) then Exit;

  SizeNeeded:= BufSize;
  SizeNeeded2:= BufSize;
  if not LookupAccountSid(nil, GroupSID, OwnerName, SizeNeeded,
    DomainName, SizeNeeded2, OwnerType) then Exit;
  Result:= string(OwnerName)+'@'+string(DomainName);
end;

//----------------------------------------------
//----------------------------------------------
//----------------------------------------------

end.
