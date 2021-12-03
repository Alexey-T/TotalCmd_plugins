//Plugins header, compatible with WhereIsIt DescAPI interface V2.20.
//Note: not all DescAPI structures/types definitions are included.

unit WIHeader;

interface

uses Windows;

const
  MaxDescLength = 32768;
  MaxAliasLength = 255;
  MaxDiskNameLength = 255;

type
  TWIPluginType = (ptReserved, ptSameFile, ptParentItem, ptItemList,
    ptDiskInfo, ptCDDB, ptFormatting, ptThumbnails, ptCustomIcons,
    ptFileNameFormatting, ptFolder, ptRuntime);

const
  SWIPluginType: array[TWIPluginType] of string =
     ('ptReserved', 'ptSameFile', 'ptParentItem', 'ptItemList', 'ptDiskInfo', 'ptCDDB',
      'ptFormatting', 'ptThumbnails', 'ptCustomIcons', 'ptFileNameFormatting',
      'ptFolder', 'ptRuntime');

type
  TWIPluginDefaultState = (pdsUserDefined_EnabledFirstTime, 
    pdsUserDefined_DisabledFirstTime, pdsForceAlwaysEnabled, pdsForceAlwaysDisabled);

  TWIDiskInfoRecord = record
    lpszRootPath: PChar;
    lpszVirtualRoot: PChar;
    lpszVolumeLabel: PChar;
    lpszFSName: PChar;
    iMaxNameLength: Integer;
    dwSerialNumber: DWORD;
    wDiskNum: Word;
    wMediaType: Word;
    iBytesPerSector: Integer;
    iSectorsPerCluster: Integer;
    iTotalClusters: Integer;
    iFreeClusters: Integer;
    iNumberOfFiles: Integer;
    iNumberOfCompressedFiles: Integer;
    wNumberOfDirs: Word;
  end;

  TWIConfigPluginProc = procedure(HInstance: THandle; OwnerWnd: THandle;
    PluginID: Word); stdcall;

  TWIRegisterPlugin = function(PluginID: Word; PluginType: TWIPluginType; PluginName,
    FileMask: PChar; ConfigProc: TWIConfigPluginProc): Integer; stdcall;

  TWIRegisterDescPlugins = procedure(RegisterPlugin: TWIRegisterPlugin); stdcall;

  TWIInitDescImport = procedure; stdcall;

  TWIDoneDescImport = procedure; stdcall;

  TWIRequireFileProc = function(FileName: PChar): PChar; stdcall;


  TWIModuleInfo = procedure(ModuleName, Author, Version: PChar); stdcall;

  TWIImportDesc_SameFile = function(PluginID: Word; FileName,Desc: PChar;
    RequireFile: TWIRequireFileProc): Boolean; stdcall;


  TWIModuleInfoRec = record
    lpszModuleName: PChar;
    lpszAuthor: PChar;
    lpszVersion: PChar
  end;
  PWIModuleInfoRec = ^TWIModuleInfoRec;
  TWIModuleInfoEx = procedure(ModuleInfo: PWIModuleInfoRec); stdcall;

  TWIPluginDefaultStateProc = function(PluginID: Word): TWIPluginDefaultState; stdcall;

  TWIPluginCapabilitiesRec = record
    ImportingDesc: Boolean;
    ImportingAlias: Boolean;
  end;
  PWIPluginCapabilitiesRec = ^TWIPluginCapabilitiesRec;
  TWIPluginCapabilitiesProc = function(PluginID: Word; PluginCapabilities: PWIPluginCapabilitiesRec): Integer; stdcall;

  TWIImportSameFileRec = record
    FileName: PChar;
    Desc: PChar;
    AliasName: PChar;
    RequireFileProc: TWIRequireFileProc;
    MaxDescSize: Word;
    SilentMode: Boolean;
    OwnerWnd: THandle;
  end;
  PWIImportSameFileRec = ^TWIImportSameFileRec;
  TWIImportDesc_SameFileEx = function(PluginID: Word; ImportSameFile: PWIImportSameFileRec): Integer; stdcall;


  TWIThumbnailRec = record
    FileName: PChar;
    XSize,YSize: Word;
    BitmapHandle: HBitmap;
    RequireFileProc: TWIRequireFileProc;
    SilentMode: Boolean;
    OwnerWnd: THandle;
  end;
  PWIThumbnailRec = ^TWIThumbnailRec;
  TWIImportThumbnailProcEx = function(PluginID: Word; ThumbnailPtr: PWIThumbnailRec): Integer; stdcall;

implementation

end.
