unit FPermissions;

interface

function GetFilePermissions(const FileName: string; var s: string): Boolean;

implementation

uses Windows, SysUtils;

{$I FSid.inc}

type
  ACL_SIZE_INFORMATION = record
    AceCount: DWORD;
    AclBytesInUse: DWORD;
    AclBytesFree: DWORD;
  end;

  ACE_HEADER = record
    AceType: BYTE;
    AceFlags: BYTE;
    AceSize: WORD;
  end;

  ACCESS_ALLOWED_ACE = record
    Header: ACE_HEADER;
    Mask: ACCESS_MASK;
    SidStart: DWORD;
  end;

  PACCESS_ALLOWED_ACE = ^ACCESS_ALLOWED_ACE;

  TPermission = record
    Flag: boolean;
    Caption: string;
  end;

  TNTFSAccess = record
    FullAccess,
    Change,
    ReadExecute,
    Read,
    Write: boolean;
    Permissions: array [0..12] of TPermission;
  end;

const
  CRLF = #13#10;
  Delimiter = #9;
  FILE_READ_DATA        = $00000001;
  FILE_WRITE_DATA       = $00000002;
  FILE_APPEND_DATA      = $00000004;
  FILE_READ_EA          = $00000008;
  FILE_WRITE_EA         = $00000010;
  FILE_EXECUTE          = $00000020;
  FILE_READ_ATTRIBUTES  = $00000080;
  FILE_WRITE_ATTRIBUTES = $00000100;
  FILE_DELETE_CHILD     = $00000040;
  DELETE                = $00010000;

procedure InitializePermissionsCaption(var NTFSAccess: TNTFSAccess);
var
  i: byte;
begin
  with NTFSAccess do
    For i:= 0 to High(Permissions) do
      case i of
        0: Permissions[i].Caption:= 'Содержание папки/чтение данных';
        1: Permissions[i].Caption:= 'Создание файлов/запись данных';
        2: Permissions[i].Caption:= 'Создание папок/дозапись данных';
        3: Permissions[i].Caption:= 'Чтение дополнительных атрибутов';
        4: Permissions[i].Caption:= 'Запись дополнительных атрибутов';
        5: Permissions[i].Caption:= 'Обзор папок/выполнение файлов';
        6: Permissions[i].Caption:= 'Чтение атрибутов';
        7: Permissions[i].Caption:= 'Запись атрибутов';
        8: Permissions[i].Caption:= 'Удаление подпапок и файлов';
        9: Permissions[i].Caption:= 'Удаление';
        10: Permissions[i].Caption:= 'Чтение разрешений';
        11: Permissions[i].Caption:= 'Смена разрешений';
        12: Permissions[i].Caption:= 'Смена владельца';
      end;
end;

function GetFilePermissions(const FileName: string; var s: string): Boolean;
var
  SecDescr: PSecurityDescriptor;
  SizeNeeded1,SizeNeeded2: DWORD;
  AceName, DomainName: PChar;
  a: PACL;
  pAce: pointer;
  DaclPresent,DaclDefaulted: longbool;
  asi: ^ACL_SIZE_INFORMATION;
  i: integer;
  AceSID: PSID;
  OwnerType: SID_NAME_USE;
  ErrorCode: integer;
  Perm: TNTFSAccess;

  function ExtractPermisions(AceType: byte;mask: ACCESS_MASK): string;
  var
    AceTypeStr: string;
    j: byte;
  begin
    Result:= '';

    case AceType of
      0: AceTypeStr:= 'Разрешения';
      1: AceTypeStr:= 'Запрет';
      2: AceTypeStr:= 'System';
    else AceTypeStr:= 'Unknoun';
    end;

    Perm.Permissions[0].Flag:= (mask and FILE_READ_DATA)=FILE_READ_DATA;
    Perm.Permissions[1].Flag:= (mask and FILE_WRITE_DATA)=FILE_WRITE_DATA;
    Perm.Permissions[2].Flag:= (mask and FILE_APPEND_DATA)=FILE_APPEND_DATA;
    Perm.Permissions[3].Flag:= (mask and FILE_READ_EA)=FILE_READ_EA;
    Perm.Permissions[4].Flag:= (mask and FILE_WRITE_EA)=FILE_WRITE_EA;
    Perm.Permissions[5].Flag:= (mask and FILE_EXECUTE)=FILE_EXECUTE;
    Perm.Permissions[6].Flag:= (mask and FILE_READ_ATTRIBUTES)=FILE_READ_ATTRIBUTES;
    Perm.Permissions[7].Flag:= (mask and FILE_WRITE_ATTRIBUTES)=FILE_WRITE_ATTRIBUTES;
    Perm.Permissions[8].Flag:= (mask and FILE_DELETE_CHILD)=FILE_DELETE_CHILD;
    Perm.Permissions[9].Flag:= (mask and DELETE)=DELETE;
    Perm.Permissions[10].Flag:= (mask and READ_CONTROL)=READ_CONTROL;
    Perm.Permissions[11].Flag:= (mask and WRITE_DAC)=WRITE_DAC;
    Perm.Permissions[12].Flag:= (mask and WRITE_OWNER)=WRITE_OWNER;

    Result:= Delimiter+AceTypeStr+CRLF;
    For j:= 0 to High(Perm.Permissions) do
      if Perm.Permissions[j].Flag then
        Result:= Result+Delimiter+Delimiter+Perm.Permissions[j].Caption+CRLF;
  end;

begin
  Result:= False;
  s:= '';
  GetMem(SecDescr, 1024);
  GetMem(asi, sizeof(ACL_SIZE_INFORMATION));
  GetMem(AceName, 64);
  GetMem(DomainName, 64);
  try
    try
      InitializePermissionsCaption(Perm);

      if not GetFileSecurity(PChar(FileName),DACL_SECURITY_INFORMATION,SecDescr, 1024,SizeNeeded1)
      then
        Exit;

      if not GetSecurityDescriptorDacl(SecDescr,DaclPresent,a,DaclDefaulted)
      then
        Exit;

      if GetAclInformation(a^,asi,sizeof(ACL_SIZE_INFORMATION),AclSizeInformation) then
        For i:= 0 to asi.AceCount-1 do
        begin
          pAce:= nil;
          if GetAce(a^,i,pAce) then
          begin
            AceSid:= @PACCESS_ALLOWED_ACE(pAce).SidStart;

            if not LookupAccountSID(nil, AceSid, AceName,SizeNeeded1,
                                    DomainName, SizeNeeded2, OwnerType)
               and (GetLastError=ERROR_INSUFFICIENT_BUFFER)
            then
            begin
              ReAllocMem(AceName,SizeNeeded1);
              ReAllocMem(DomainName,SizeNeeded2);
              if not LookupAccountSID(nil, AceSid, AceName,SizeNeeded1,
                                      DomainName, SizeNeeded2, OwnerType)
              then begin
                ErrorCode:= GetLastError();
                s:= s+'ErrorCode: '+IntToStr(ErrorCode)+CRLF;
                Continue;
              end;
            end;
            s:= s+AceName;
            if StrPas(DomainName)<>'' then
              s:= s+' ('+DomainName+')'+CRLF
            else
              s:= s+CRLF;

            s:= s+Delimiter+SidToStr(AceSid)+CRLF;
            s:= s+ExtractPermisions(PACCESS_ALLOWED_ACE(pAce).Header.AceType,
                                   PACCESS_ALLOWED_ACE(pAce).Mask);
          end;
        end;
    except
    end;
  finally
    FreeMem(SecDescr);
    FreeMem(asi);
    FreeMem(AceName);
    FreeMem(DomainName);
  end;
  Result:= True;
end;
                                         
end.
