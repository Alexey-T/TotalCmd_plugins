unit FSPlugin;    {Plugin definitions version 1.3}

interface

uses windows;

{ ids for FsGetFile }
const FS_FILE_OK=0;
      FS_FILE_EXISTS=1;
      FS_FILE_NOTFOUND=2;
      FS_FILE_READERROR=3;
      FS_FILE_WRITEERROR=4;
      FS_FILE_USERABORT=5;
      FS_FILE_NOTSUPPORTED=6;
      FS_FILE_EXISTSRESUMEALLOWED=7;

      FS_EXEC_OK=0;
      FS_EXEC_ERROR=1;
      FS_EXEC_YOURSELF=-1;
      FS_EXEC_SYMLINK=-2;

      FS_COPYFLAGS_OVERWRITE=1;
      FS_COPYFLAGS_RESUME=2;

      FS_COPYFLAGS_MOVE=4;
      FS_COPYFLAGS_EXISTS_SAMECASE=8;
      FS_COPYFLAGS_EXISTS_DIFFERENTCASE=16;
 
{ flags for tRequestProc }
const
  RT_Other=0;
  RT_UserName=1;
  RT_Password=2;
  RT_Account=3;
  RT_UserNameFirewall=4;
  RT_PasswordFirewall=5;
  RT_TargetDir=6;
  RT_URL=7;
  RT_MsgOK=8;
  RT_MsgYesNo=9;
  RT_MsgOKCancel=10;

{ flags for tLogProc }
const msgtype_connect=1;
      msgtype_disconnect=2;
      msgtype_details=3;
      msgtype_transfercomplete=4;
      msgtype_connectcomplete=5;
      msgtype_importanterror=6;
      msgtype_operationcomplete=7;

{ flags for FsStatusInfo }
const FS_STATUS_START=0;
      FS_STATUS_END=1;

      FS_STATUS_OP_LIST=1;
      FS_STATUS_OP_GET_SINGLE=2;
      FS_STATUS_OP_GET_MULTI=3;
      FS_STATUS_OP_PUT_SINGLE=4;
      FS_STATUS_OP_PUT_MULTI=5;
      FS_STATUS_OP_RENMOV_SINGLE=6;
      FS_STATUS_OP_RENMOV_MULTI=7;
      FS_STATUS_OP_DELETE=8;
      FS_STATUS_OP_ATTRIB=9;
      FS_STATUS_OP_MKDIR=10;
      FS_STATUS_OP_EXEC=11;
      FS_STATUS_OP_CALCSIZE=12;
      FS_STATUS_OP_SEARCH=13;
      FS_STATUS_OP_SEARCH_TEXT=14;
      FS_STATUS_OP_SYNC_SEARCH=15;
      FS_STATUS_OP_SYNC_GET=16;
      FS_STATUS_OP_SYNC_PUT=17;
      FS_STATUS_OP_SYNC_DELETE=18;

{Flags for FsExtractCustomIcon}
const FS_ICONFLAG_SMALL=1;
      FS_ICONFLAG_BACKGROUND=2;
      FS_ICON_USEDEFAULT=0;
      FS_ICON_EXTRACTED=1;
      FS_ICON_EXTRACTED_DESTROY=2;
      FS_ICON_DELAYED=3;


type
  tRemoteInfo=record
    SizeLow,SizeHigh:longint;
    LastWriteTime:TFileTime;
    Attr:longint;
  end;
  pRemoteInfo=^tRemoteInfo;

type
  tFsDefaultParamStruct=record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi:longint;
    DefaultIniName:array[0..MAX_PATH-1] of char;
  end;
  pFsDefaultParamStruct=^tFsDefaultParamStruct;

{ callback functions }
type
  TProgressProc=function(PluginNr:integer; SourceName,TargetName: pAnsiChar; PercentDone:integer): integer; stdcall;
  TLogProc=procedure(PluginNr,MsgType:integer; LogString:pAnsiChar); stdcall;
  TRequestProc=function(PluginNr,RequestType:integer; CustomTitle,CustomText,ReturnedText: pAnsiChar; maxlen:integer): bool; stdcall;


{ Function prototypes - the callback functions MUST be implemented exactly like this! }
{
function FsInit(PluginNr:integer;pProgressProc:tProgressProc;pLogProc:tLogProc;
                pRequestProc:tRequestProc):integer; stdcall;
function FsFindFirst(path :pchar;var FindData:tWIN32FINDDATA):thandle; stdcall;
function FsFindNext(Hdl:thandle;var FindData:tWIN32FINDDATA):bool; stdcall;
function FsFindClose(Hdl:thandle):integer; stdcall;
function FsMkDir(RemoteDir:pchar):bool; stdcall;
function FsExecuteFile(MainWin:thandle;RemoteName,Verb:pchar):integer; stdcall;
function FsRenMovFile(OldName,NewName:pchar;Move,OverWrite:bool;RemoteInfo:pRemoteInfo):integer; stdcall;
function FsGetFile(RemoteName,LocalName:pchar;CopyFlags:integer;RemoteInfo:pRemoteInfo):integer; stdcall;
function FsPutFile(LocalName,RemoteName:pchar;CopyFlags:integer):integer; stdcall;
function FsDeleteFile(RemoteName:pchar):bool; stdcall;
function FsRemoveDir(RemoteName:pchar):bool; stdcall;
function FsDisconnect(DisconnectRoot:pchar):bool; stdcall;
function FsSetAttr(RemoteName:pchar;NewAttr:integer):bool; stdcall;
function FsSetTime(RemoteName:pchar;CreationTime,LastAccessTime,LastWriteTime:PFileTime):bool; stdcall;
procedure FsStatusInfo(RemoteDir:pchar;InfoStartEnd,InfoOperation:integer); stdcall;
procedure FsGetDefRootName(DefRootName:pchar;maxlen:integer); stdcall;
function FsExtractCustomIcon(RemoteName:pchar;ExtractFlags:integer;var TheIcon:hicon):integer; stdcall;
procedure FsSetDefaultParams(dps:pFsDefaultParamStruct); stdcall;
}

implementation

end.

