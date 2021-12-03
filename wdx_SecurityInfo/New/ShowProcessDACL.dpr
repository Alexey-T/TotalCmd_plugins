program ShowProcessDacl;
{$APPTYPE CONSOLE}
uses SysUtils, Windows, ACLApi, AccCtrl;

Var pHndl: THandle;

  Function toOEM(aStr: String): String;
  Begin
    Result := aStr;
    UniqueString(Result);
    ANSItoOEM(PChar(Result), PChar(Result));
  End;

//Эти декларации взяты из winnt.h, в библиотеках Delphi этого нет
Const
  ACCESS_ALLOWED_ACE_TYPE = 0;
  ACCESS_DENIED_ACE_TYPE  = 1;
Type
  _ACE_HEADER = Packed Record
    AceType, AceFlags: BYTE;
    AceSize: WORD;
  End;
  ACE_HEADER = _ACE_HEADER;
  PACE_HEADER = ^ACE_HEADER;

  _ACCESS_ALLOWED_ACE = Record
    Header: ACE_HEADER;
    Mask: ACCESS_MASK;
    SidStart: DWORD;
  End;
  PACCESS_ALLOWED_ACE = ^_ACCESS_ALLOWED_ACE;

  _SID = Record
    Revision: BYTE;
    SubAuthorityCount: BYTE;
    IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
    SubAuthority: DWORD;
  End;
  TSID = _SID;
  PSID = ^TSID;

  Function SidToStr(aSID: PSID): String;
  Var
    AuthID: PSidIdentifierAuthority;
    SubAuthCount: PUCHAR;
    SubAuth: PDWORD;
    J: Integer;
  begin
    Win32Check(IsValidSID(aSID));

    Result := 'S-' + IntToStr(aSID.Revision) + '-';

    AuthID := GetSidIdentifierAuthority(aSID);
    Result := Result + IntToStr(AuthID.Value[High(AuthID.Value)]) + '-';
    For J := High(AuthID.Value) - 1 DownTo Low(AuthID.Value) Do
      If AuthID.Value[J] <> 0 Then
        Result := Result + IntToStr(AuthID.Value[J]) + '-';

    SubAuthCount := GetSIDSubAuthorityCount(aSID);
    For J := 0 To SubAuthCount^ - 1 Do
    Begin
      SubAuth := GetSIDSubAuthority(ASID, J);
      Result := Result + IntToStr(SubAuth^) + '-';
    End;

    J := Length(Result);
    If J <> 0 Then
      SetLength(Result, J - 1);
  End;

  Function GetAccountBySID(ServerName: String; aSID: PSID): String;
  Var
    nSize, dSize, pUse: DWORD;
    aName, dName: PChar;
  Begin
    If Not IsValidSID(aSID) Then
    Begin
      Result := 'Erroneus SID!';
      Exit;
    End;
    nSize := 0; dSize := 0;
    If Not LookupAccountSid(PChar(ServerName), aSID, Nil, nSize, Nil, dSize, pUse) Then
      If GetLastError <> ERROR_INSUFFICIENT_BUFFER Then
        If GetLastError <> ERROR_NONE_MAPPED Then RaiseLastWin32Error
        Else Begin
          Result := SIDToStr(aSID);
          Exit;
        End;
    aName := StrAlloc(nSize);
    dName := StrAlloc(dSize);
    Try
      Win32Check(LookupAccountSid(PChar(ServerName), aSID, aName, nSize, dName, dSize, pUSE));
      Result := aName + ' (' + dName + ')';
    Finally
      StrDispose(aName);
      StrDispose(dName);
    End;
  End;

Var
  TempStr: String;
  SD: PPSECURITY_DESCRIPTOR; aDACL: PACL; anACE: PACCESS_ALLOWED_ACE;
  anError, tempMask: DWORD;
  J, I: Integer;

Const
  AccessMaskStringMap: Array[0..31] Of String = (
    'PROCESS_TERMINATE', 'PROCESS_CREATE_THREAD', 'PROCESS_SET_SESSIONID',
    'PROCESS_VM_OPERATION', 'PROCESS_VM_READ', 'PROCESS_VM_WRITE',
    'PROCESS_DUP_HANDLE', 'PROCESS_CREATE_PROCESS', 'PROCESS_SET_QUOTA',
    'PROCESS_SET_INFORMATION', 'PROCESS_QUERY_INFORMATION', 'PROCESS_SUSPEND_RESUME',
    '', '', '', '', 'DELETE', 'READ_DACL',
    'WRITE_DACL', 'WRITE_OWNER', 'SYNCHRONIZE', '', '', '', 'Access System ACL',
    '', '', '', '', '', '', ''
  );

  Procedure EnableDebugPrivilege;
  Const
    SE_DEBUG_NAME = 'SeDebugPrivilege';
  Var
    HToken: THandle;
    tkp: TOKEN_PRIVILEGES;
  Begin
    Win32Check(OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, hToken));

    Win32Check(LookupPrivilegeValue(Nil, SE_DEBUG_NAME, tkp.Privileges[0].Luid));
    tkp.PrivilegeCount := 1;
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    Win32Check(AdjustTokenPrivileges(HToken, False, tkp, 0, Nil, PDWORD(Nil)^));
  End;

begin
  Try
    If ParamCount < 1 Then
      Raise Exception.Create('Process ID must be first input parameter! Application terminated.');

    pHndl := OpenProcess(READ_CONTROL, False, StrToInt(ParamStr(1)));
    If pHndl = 0 Then
      If GetLastError = ERROR_ACCESS_DENIED Then
      Begin
        Write(Format('Access to process ID %s denied, try to enable debug privilege? (Y/N):', [ParamStr(1)]));
        ReadLn(TempStr);
        If (Length(TempStr) = 1) and (TempStr[1] in ['y', 'Y'])
        Then EnableDebugPrivilege Else Exit;

        pHndl := OpenProcess(READ_CONTROL, False, StrToInt(ParamStr(1)));
      End;

    If pHndl = 0 Then RaiseLastWin32Error;

    Try
      anError := GetSecurityInfo(pHndl, SE_KERNEL_OBJECT, DACL_SECURITY_INFORMATION, Nil, Nil, @aDACL, Nil, SD);
      If anError <> Error_Success Then
      Begin
        SetLastError(anError);
        RaiseLastWin32Error;
      End;
      Try
        //На этом этапе имеем Security Descriptor (в SD). Начинаем его "ковырять"...
        WriteLn('DACL information for Process ID ', ParamStr(1));
        WriteLn('ACEs count: ', aDACL.AceCount);
        If aDACL.AceCount = 0 Then
        Begin
          WriteLn('Nothing to show!');
          Exit;
        End;

        WriteLn('Start dumping...');
        WriteLn;

        //Вытаскиваем ACE's...
        For J := 0 To aDACL.AceCount - 1 Do
        Begin
          If Not GetACE(aDACL^, J, Pointer(anACE)) Then RaiseLastWin32Error;

          Case anACE.Header.AceType Of
            ACCESS_ALLOWED_ACE_TYPE: TempStr := 'Allowed';
            ACCESS_DENIED_ACE_TYPE: TempStr := 'Denied';
            else TempStr := 'Unknown';
          End;

          //Выводим пользователя в этом ACE...
          Writeln(
            'Access ', TempStr, ' Ace (at index ', J, ') for user "',
            toOEM(GetAccountBySID('', @anACE.SidStart)), '"'
          );

          WriteLn('Access mask:');
            //Выводим AccessRights для ACE:
          For I := 0 To 31 Do
          Begin //Проверяем каждый бит в 32-битной маске доступа...
            TempMask := 1 shl I;
            If (anACE.Mask And TempMask) = TempMask Then
              WriteLn('   ', AccessMaskStringMap[I]); //Выводим описание бита в маске доступа
          End;
          WriteLn;
        End;

        Writeln(aDACL.AceCount, ' ACE(s) printed successfully.');
      Finally
        LocalFree(DWORD(SD));
      End;
    Finally
      CloseHandle(pHndl);
    End;
      Writeln('This program written by HandleX. Enjoy it!');
      Write  ('Press ENTER to continue...');
      ReadLN;
  Except
    On E: Exception Do
    If Not (E is EAbort) Then
    Begin
      WriteLn('Exception Class ' + E.ClassName + ' occured.');
      WriteLn('Error: "' + toOEM(E.Message) + '"');
      WriteLn('Program terminated.');
    End;
  End;
end.


