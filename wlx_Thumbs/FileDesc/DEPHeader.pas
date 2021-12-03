//Disk Explorer Pro filter header
//Translated from filterplug.h by Alexey Torgashin <atorg@yandex.ru>

unit DEPHeader;

interface

uses Windows;

const
  DEFILTERPLUG_VER = $0101;

type
  TDEFILTERPLUG_FILEINFO = record
    Size: integer;
    FileName: array[0..MAX_PATH-1] of char;
  end;
  PDEFILTERPLUG_FILEINFO = ^TDEFILTERPLUG_FILEINFO;

const
  DEFILTERPLUG_DATATYPE_NONE = 0;
  DEFILTERPLUG_DATATYPE_TEXT = 1;
  DEFILTERPLUG_DATATYPE_IMAGE = 2;
  DEFILTERPLUG_DATATYPE_VIDEO = 4;
  DEFILTERPLUG_DATATYPE_FORCE_DWORD = $7FFFFFFF;

type
  TDEFILTERPLUG_DATATEXT = record
    TextSize: DWORD;
    Res: array[0..1] of DWORD;
  end;

const
  DEFPI_UPDOWN = $00000001;
  DEFPI_ADDSTR = $00000002; //when set, additional string will be appended added to the end of the filtered data
  DEFPI_ADDSTR2 = $00000004; //when set, additional string will be inserted to the beginning of the filtered data

type
  TDEFILTERPLUG_DATAIMAGE = record
    //thumbnail extents
    ImageSize: DWORD;
    Width: DWORD;
    Height: DWORD;
    BPP: DWORD;
    InfoStr: array[0..31] of char;
    //original image extents
    OrigWidth: DWORD;
    OrigHeight: DWORD;
    OrigBPP: DWORD;
    OrigPages: DWORD;
    OrigColors: DWORD;
    //DEP version 2.70+:
    //You can set DEFPI_* flag indicating that there is additional string at the end of the data
    Flags: DWORD;
  end;

  TDEFILTERPLUG_DATAVIDEO = record
    //thumbnail extents
    ImageSize: DWORD;
    Width: DWORD;
    Height: DWORD;
    BPP: DWORD;
    InfoStrV: array[0..10] of char; //video info string
    InfoStrA: array[0..20] of char; //audio info string
    //original video info
    VWidth: DWORD;
    VHeight: DWORD;
    VFps: Double;
    VFrames: DWORD;
    VTotalTime: Int64;
    //original audio info
    AChannels: DWORD;
    ASampleRate: DWORD;
    ABps: DWORD;
    ATotalSamples: DWORD;
    //DEP version 2.70+:
    //You can set DEFPI_* flag indicating that there is additional string at the end of the data
    Flags: DWORD;
  end;

  TDEFILTERPLUG_OUT = record
    Size: integer;
    DataType: DWORD;
    Data: pointer;
    DataSize: DWORD;
    PD: pointer;
  end;
  PDEFILTERPLUG_OUT = ^TDEFILTERPLUG_OUT;

  TDEProc = function: integer; cdecl;
  TDEWndProc = function(ParentWnd: HWND): integer; cdecl;
  TDEProcessFileProc = function(const pfi: PDEFILTERPLUG_FILEINFO;
    po: PDEFILTERPLUG_OUT): BOOL; cdecl;
  TDEFreeProcessMemProc = function(po: PDEFILTERPLUG_OUT): BOOL; cdecl;

  TDEFilterPlugin = record
    Size: integer;              //size of this struct (sizeof(DEFilterPlugIn))
    Version: integer;           //plugin interface version (DEFILTERPLUG_VER)
    GUID: TGUID;                //unique identificator
    SupportedFileTypes: PChar;  //supported filetypes specified by wildcards
    //operations
    Init: TDEProc;
    Config: TDEWndProc;
    About: TDEWndProc;
    Quit: TDEProc;
    ProcessFile: TDEProcessFileProc;
    FreeProcessMem: TDEFreeProcessMemProc;
    //the main application will fill this
    AppProcessID: DWORD;        //Application's process ID
    AppThreadID: DWORD;         //Application's thread ID
    AppHWnd: HWND;              //Application's window handle
    AppHInst: HINST;            //Application's instance
    AppMFC: pointer;            //MFC CWinApp
    AppMFCMainWnd: pointer;     //MFC CWnd
    AppRegPath: array[0..511] of char; //registry path
  end;
  PDEFilterPlugin = ^TDEFilterPlugin;

  TDEFilterPluginProc = function: PDEFilterPlugin; cdecl;
  //must be exported by plugin with name DEFilterPlug_Get

implementation

end.
