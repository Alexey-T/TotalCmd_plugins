unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, PJMenuSpeedButtons, ImgList, Menus,
  tcProc, UFormInstallProgress;

type
  TColumnType = (colUnknown, colName, colDesc, colVersion, colState, colAssoc, colFilename);
  TColumnValues = array[TColumnType] of integer;

  
const
  cColUp = 5;
  cColDown = 6; //Imagelist indexes for sort order
  cColumnString: array[TColumnType] of string =
    ('ColDefault', 'ColName', 'ColDesc', 'ColVersion', 'ColState', 'ColAssoc', 'ColFilename');
  cColumnDefWidth: TColumnValues =
    (0, 200, 200, 80, 80, 150, 400);

type
  TFormMain = class(TForm)
    panelTC: TGroupBox;
    panelPlugins: TGroupBox;
    Image1: TImage;
    labTCVersionText: TLabel;
    labTCStateText: TLabel;
    labTCExe: TLabel;
    labTCVersion: TLabel;
    labTCState: TLabel;
    btnTCRun: TButton;
    btnBrowseExe: TButton;
    edTCExe: TEdit;
    OpenDialogExe: TOpenDialog;
    btnDisable: TPJMenuSpeedButton;
    btnRemove: TPJMenuSpeedButton;
    btnAdd: TPJLinkedSpeedButton;
    btnOptions: TPJLinkedSpeedButton;
    btnClose: TPJMenuSpeedButton;
    OpenDialogPlugins: TOpenDialog;
    btnRename: TPJMenuSpeedButton;
    Timer1: TTimer;
    labTCIni: TLabel;
    edTCIni: TEdit;
    btnBrowseIni: TButton;
    OpenDialogIni: TOpenDialog;
    btnTCRestart: TButton;
    btnConfigure: TPJMenuSpeedButton;
    ImageList1: TImageList;
    btnEnable: TPJMenuSpeedButton;
    menuMain: TPopupMenu;
    mnuDisable: TMenuItem;
    mnuEnable: TMenuItem;
    mnuUninstall: TMenuItem;
    mnuRename: TMenuItem;
    mnuConfigure: TMenuItem;
    N1: TMenuItem;
    mnuInstall: TMenuItem;
    mnuOrder: TMenuItem;
    mnuRefresh: TMenuItem;
    N2: TMenuItem;
    panelList: TPanel;
    ListView1: TListView;
    TabControl1: TTabControl;
    btnOrder: TPJLinkedSpeedButton;
    menuInstall: TPopupMenu;
    mnu2Install: TMenuItem;
    mnu2InstallZip: TMenuItem;
    OpenDialogZip: TOpenDialog;
    mnuInstallZip: TMenuItem;
    mnuBrowse: TMenuItem;
    StatusBar1: TStatusBar;
    btnTweak: TPJMenuSpeedButton;
    mnuTweak: TMenuItem;
    btnEditIni: TButton;
    mnuFilter: TMenuItem;
    btnAddMenu: TPJLinkedMenuSpeedButton;
    mnu2InstallFolder: TMenuItem;
    mnuInstallFolder: TMenuItem;
    btnOrderMenu: TPJLinkedMenuSpeedButton;
    menuOrder: TPopupMenu;
    mnuOrderLister: TMenuItem;
    mnuOrderPacker: TMenuItem;
    mnuOrderContent: TMenuItem;
    mnuProperties: TMenuItem;
    btnOptionsMenu: TPJLinkedMenuSpeedButton;
    menuOptions: TPopupMenu;
    mnuOptOptions: TMenuItem;
    N4: TMenuItem;
    mnuOptReport: TMenuItem;
    mnuOptAbout: TMenuItem;
    Panel1: TPanel;
    chkQS1: TRadioButton;
    chkQS2: TRadioButton;
    ListBox1: TListBox;
    OpenDialogDll: TOpenDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnTCRunClick(Sender: TObject);
    procedure btnBrowseExeClick(Sender: TObject);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnDisableClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnBrowseIniClick(Sender: TObject);
    procedure btnTCRestartClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure btnConfigureClick(Sender: TObject);
    procedure btnOrderClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TabControl1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuBrowseClick(Sender: TObject);
    procedure btnTweakClick(Sender: TObject);
    procedure mnuDescClick(Sender: TObject);
    procedure mnuInstallZipClick(Sender: TObject);
    procedure btnEditIniClick(Sender: TObject);
    procedure btnReportClick(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure mnuFilterClick(Sender: TObject);
    procedure mnuInstallFolderClick(Sender: TObject);
    procedure mnuOrderListerClick(Sender: TObject);
    procedure mnuPropertiesClick(Sender: TObject);
    procedure mnuOptAboutClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure chkQS1Click(Sender: TObject);
    procedure chkQS2Click(Sender: TObject);
  private
    { Private declarations }
    FLog: TLibLog;
    ColIdx: TColumnValues;
    FQS: string;
    function QSIni: string;
    function QSFile: string;
    function QSName: string;
    procedure LoadQS;
    procedure SaveQS;
    function IsFS: boolean;
    function IsQS: boolean;
    procedure LoadOptions2;
    procedure SaveOptions;
    procedure LoadColumns;
    procedure SaveColumns;
    procedure SetTCPath(const fn: string);
    procedure UpdateColumnIndexes;
    procedure UpdateTCState;
    procedure UpdateStates;
    procedure UpdateList(AUnloadPlugins: boolean);
    procedure UpdateColumnsImages;
    function LibFN: string;
    function LibTitle: string;
    function LibIni: string;
    procedure ButtonsAlign(AButtons: array of TSpeedButton);
    procedure ApplyColumns;
    procedure ShowOrderAndUpdateList(typ: TLibType);
    procedure SelectAll;
    function ColumnTypeToIndex(typ: TColumnType): integer;
    function ColumnIndexToType(idx: integer): TColumnType;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

function SettingsOK: boolean;
procedure RestartTC;
procedure InstallPlugin(
  const fn: string;
  CheckDups: boolean;
  const DefaultPackerExt: string;
  const Log: TLibLog);
procedure InstallPluginWithConfirmation(const fn: string);
function ShowOrderDialog(typ: TLibType): boolean;
procedure CheckCommandLine;


implementation

uses
  ShellAPI, VersionInfo, FileCtrl, ActiveX,
  FUnpack, IniProc, RegProc, SProc, SProc2, FProc, LogProc,
  tcPaths, tcReport, tcRegistration,
  Msg, MsgFile,
  UFormAbout, UFormOptions, UFormAdd, UFormOrder, UFormTweak,
  UFormAddExt, UFormReport, UFormInstallLog;

{$R *.DFM}

var
  Opt: record
    //Paths
    MainIni,
    TCExe,
    TCIni,
    TCIniFTP,
    PluginsSrc,
    PluginsSrcFolder,
    PluginsSrcZip,
    PluginsLister,
    PluginsPacker,
    PluginsFS,
    PluginsContent: string;

    EditorUse: boolean;
    EditorPath: string;

    //Fields
    Lang: string;
    SortCol: TColumnType;
    SortAsc,
    ShowTypes,
    ShowNumbers: boolean;

    ShowColDesc,
    ShowColVersion, 
    ShowColState,
    ShowColAssoc,
    ShowColFilename: boolean;

    {
    WidthColName,
    WidthColDesc,
    WidthColVersion,
    WidthColState,
    WidthColAssoc,
    WidthColFilename: integer;
    }

    Register: boolean;
    Timer: boolean;
    InstRestart: boolean;
    InstTweakBefore: boolean;
    InstTweakAfter: boolean;
    RestartCmd: integer;
    RestartTime: integer;
    RestartForeground: boolean;
    BrowseCmd: string;
    UnloadPlugins: boolean;
    FilterString: string;
  end;

const
  cTotalCmdClass = 'TTOTAL_CMD';

function GetIniKeyExpand(const fn: string; const section, key, default: string): string;
begin
  Result:= SExpandVars(GetIniKey(fn, section, key, default));
end;

function _DefPluginDir(typ: TLibType): string;
begin
  case typ of
    fLister:
      Result:= SExpandVars(Opt.PluginsLister);
    fPacker:
      Result:= SExpandVars(Opt.PluginsPacker);
    fFS:
      Result:= SExpandVars(Opt.PluginsFS);
    fContent:
      Result:= SExpandVars(Opt.PluginsContent);
    else
      Result:= FTempDirectory;
  end;
end;

procedure _CreatePluginDirs;
var
  typ: TLibType;
begin
  for typ:= Low(TLibType) to High(TLibType) do
    FCreateDirs(_DefPluginDir(typ));
end;

function UnloadPluginsProc(hWnd: THandle; Param: LPARAM): BOOL; stdcall;
const
  EM_DISPLAYBAND = WM_USER+51;
  cm_UnloadPlugins = 2913;
var
  buf: array[0..200] of char;
begin
  if (GetClassName(hWnd, buf, SizeOf(buf))>0) and (buf=cTotalCmdClass) then
    begin
    SendMessage(hWnd, EM_DISPLAYBAND, cm_UnloadPlugins, 0);
    Sleep(200);
    end;
  Result:= true;
end;

procedure UnloadPlugins;
begin
  EnumWindows(@UnloadPluginsProc, 0);
end;


{$I ProcMain.pas}
{$I ProcCommandLine.pas}


procedure TFormMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.btnOptionsClick(Sender: TObject);
begin
  with FormOptions do
    begin
    //ffShowTabs.Checked:= Opt.ShowTabs;
    ffShowTypes.Checked:= Opt.ShowTypes;
    ffShowNumbers.Checked:= Opt.ShowNumbers;

    ffColDesc.Checked:= Opt.ShowColDesc;
    ffColVersion.Checked:= Opt.ShowColVersion;
    ffColState.Checked:= Opt.ShowColState;
    ffColAssoc.Checked:= Opt.ShowColAssoc;
    ffColFilename.Checked:= Opt.ShowColFilename;

    ffTimer.Checked:= Opt.Timer;
    ffTimerSec.Text:= IntToStr(Timer1.Interval div 1000);
    ffRegister.Checked:= Opt.Register;
    ffUseTCVar.Checked:= OptTC.UseTCPath;
    ffInstRestart.Checked:= Opt.InstRestart;
    ffInstTweakBefore.Checked:= Opt.InstTweakBefore;
    ffInstTweakAfter.Checked:= Opt.InstTweakAfter;

    ffPluginsLister.Text:= Opt.PluginsLister;
    ffPluginsPacker.Text:= Opt.PluginsPacker;
    ffPluginsFS.Text:= Opt.PluginsFS;
    ffPluginsContent.Text:= Opt.PluginsContent;

    ffLang:= Opt.Lang;
    ffBarPath:= tcDefButtonBar;
    ffOptionsIni:= Opt.TCIni;

    ffFtpIni.Text:= Opt.TCIniFTP;
    ffRarPath.Text:= OptUnpack.RarPath;
    ffUseEditor.Checked:= Opt.EditorUse;
    ffEditor.Text:= Opt.EditorPath;

    ffRestart1.Checked:= Opt.RestartCmd=0;
    ffRestart2.Checked:= Opt.RestartCmd=1;
    ffRestartTime.Text:= IntToStr(Opt.RestartTime);
    ffRestartForeground.Checked:= Opt.RestartForeground;

    if ShowModal=mrOk then
      begin
      //Opt.ShowTabs:= ffShowTabs.Checked;
      Opt.ShowTypes:= ffShowTypes.Checked;
      Opt.ShowNumbers:= ffShowNumbers.Checked;

      SaveColumns;
      Opt.ShowColDesc:= ffColDesc.Checked;
      Opt.ShowColVersion:= ffColVersion.Checked;
      Opt.ShowColState:= ffColState.Checked;
      Opt.ShowColAssoc:= ffColAssoc.Checked;
      Opt.ShowColFilename:= ffColFilename.Checked;
      ApplyColumns;

      Opt.Register:= ffRegister.Checked;
      tcRegisterExtensions(Opt.Register, MsgCaption(0064), MsgCaption(0074));

      Opt.Timer:= ffTimer.Checked;
      Timer1.Enabled:= Opt.Timer;
      Timer1.Interval:= StrToIntDef(ffTimerSec.Text, 0)*1000;
      OptTC.UseTCPath:= ffUseTCVar.Checked;
      Opt.InstRestart:= ffInstRestart.Checked;
      Opt.InstTweakBefore:= ffInstTweakBefore.Checked;
      Opt.InstTweakAfter:= ffInstTweakAfter.Checked;

      Opt.PluginsLister:= ffPluginsLister.Text;
      Opt.PluginsPacker:= ffPluginsPacker.Text;
      Opt.PluginsFS:= ffPluginsFS.Text;
      Opt.PluginsContent:= ffPluginsContent.Text;
      _CreatePluginDirs;

      Opt.RestartCmd:= integer(ffRestart2.Checked);
      Opt.RestartTime:= StrToIntDef(ffRestartTime.Text, 1000);
      Opt.RestartForeground:= ffRestartForeground.Checked;

      Opt.TCIniFTP:= ffFtpIni.Text;
      OptUnpack.RarPath:= ffRarPath.Text;

      Opt.EditorUse:= ffUseEditor.Checked;
      Opt.EditorPath:= ffEditor.Text;

      UpdateList(false);
      end;

    if Opt.Lang<>ffLang then
      begin
      Opt.Lang:= ffLang;
      Self.FormShow(Self);
      end
    end;
end;


function SettingsOK: boolean;
var
  ok1, ok2: boolean;
  s: string;
begin
  s:= LowerCase(FileVersionInfo(Opt.TCExe, vsInternalName));
  ok1:= IsFileExist(Opt.TCIni);
  ok2:= IsFileExist(Opt.TCExe) {and ((s='totalcmd') or (s='totalcmd64'))};

  Result:= ok1 and ok2;

  //If files OK, emulate TC env variables
  if Result then
    begin
    SetEnvironmentVariable('COMMANDER_PATH', PChar(ExtractFileDir(Opt.TCExe)));
    SetEnvironmentVariable('COMMANDER_INI', PChar(Opt.TCIni));
    end;

  //If not, show error message
  if not ok2 then
    begin
    MsgError(Format(S0053, ['Totalcmd.exe']));
    Exit
    end;

  if not ok1 then
    begin
    MsgError(Format(S0053, ['wincmd.ini']));
    Exit
    end;
end;

//---------------------------
procedure TFormMain.SetTCPath(const fn: string);
var
  ok: boolean;
begin
  Opt.TCExe:= fn;
  edTCExe.Text:= fn;
  edTCIni.Text:= Opt.TCIni;
  labTCVersionText.Caption:= FileVersionInfo(Opt.TCExe, vsProductVersion);
  labTCStateText.Caption:= ssIdle;

  ok:= SettingsOK;
  ListView1.Items.Clear;
  ListView1.Enabled:= ok;
  Panel1.Enabled:= ok;
  TabControl1.Enabled:= ok;
  btnTCRun.Enabled:= ok;
  btnTCRestart.Enabled:= ok;
  btnAdd.Enabled:= ok;
  btnDisable.Enabled:= ok;
  btnEnable.Enabled:= ok;
  btnRemove.Enabled:= ok;
  btnRename.Enabled:= ok;
  btnTweak.Enabled:= ok;
  btnConfigure.Enabled:= ok;
  btnOrder.Enabled:= ok;
  btnOptions.Enabled:= ok;
  btnEditIni.Enabled:= ok;

  UpdateTCState;
  UpdateList(false);
end;

//-------------------------------
procedure TFormMain.UpdateTCState;
const
  clLoaded: array[boolean] of integer = (clWindowText, clBlue);
var
  loaded: boolean;
begin
  loaded:= IsFileExist(Opt.TCExe) and IsFileUsed(Opt.TCExe);
  if loaded
    then labTCStateText.Caption:= ssRunning
    else labTCStateText.Caption:= ssIdle;
  labTCStateText.Font.Color:= clLoaded[loaded];
  btnTCRestart.Enabled:= loaded;
end;

//-------------------------------
procedure TFormMain.UpdateStates;
var
  i, idx: integer;
begin
  idx:= ColumnTypeToIndex(colState);
  if idx>=0 then
    with ListView1 do
      if Enabled then
        begin
        Items.BeginUpdate;
        for i:= 0 to Items.Count-1 do
          Items[i].SubItems[idx-1]:= LibState(integer(Items[i].Data));
        Items.EndUpdate;
        end;
end;

//-------------------------------------------------
procedure TFormMain.ButtonsAlign(AButtons: array of TSpeedButton);
const
  MinWidth = 55;
  Space = 10;
var
  i, n: integer;
begin
  for i:= 0 to High(AButtons) do
    if AButtons[i]<>nil then
      begin
      n:= Canvas.TextWidth(AButtons[i].Caption)+2;
      if Pos('&', AButtons[i].Caption)=0 then Inc(n, 4);
      if n<MinWidth then n:= MinWidth;
      if not (AButtons[i] is TPJLinkedMenuSpeedButton) then
        AButtons[i].Width:= n;

      if (i>0) then
        if Assigned(AButtons[i-1])
          then AButtons[i].Left:= AButtons[i-1].Left+AButtons[i-1].Width
          else AButtons[i].Left:= AButtons[i-2].Left+AButtons[i-2].Width+Space;
      end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  {$I Init_UFormMain.pas}

  ButtonsAlign(
    [btnAdd, btnAddMenu, btnDisable, btnRemove, btnRename,
     btnConfigure, btnOrder, btnOrderMenu,
     nil, btnOptions, btnOptionsMenu, btnClose]);

  btnEnable.Left:= btnDisable.Left;
  btnEnable.Width:= btnDisable.Width;
  btnTweak.Left:= btnRename.Left;
  btnTweak.Width:= btnRename.Width;

  if not IsFileExist(Opt.TCExe) then
    btnBrowseExeClick(Self);
  if not IsFileExist(Opt.TCIni) then
    btnBrowseIniClick(Self);

  SetTCPath(Opt.TCExe);
  TabControl1Change(Self);
end;


//-------------------------------------------------
procedure TFormMain.ApplyColumns;
begin
  with ListView1.Columns do
    begin
    Clear;

    with Add do
      Caption:= MsgCaption(0354);

    if Opt.ShowColDesc then
      with Add do
        Caption:= MsgCaption(0355);

    if Opt.ShowColVersion then
      with Add do
        Caption:= MsgCaption(0356);

    if Opt.ShowColState then
      with Add do
        Caption:= MsgCaption(0357);

    if Opt.ShowColAssoc then
      with Add do
        Caption:= MsgCaption(0358);

    if Opt.ShowColFilename then
      with Add do
        Caption:= MsgCaption(0359);
    end;

  LoadColumns;
end;

//-------------------------------------------------
procedure TFormMain.FormCreate(Sender: TObject);
begin
  //Init fields
  InitLog(FLog);
  //Load additional options
  LoadOptions2;
end;

//-------------------------------------------------
procedure TFormMain.LoadOptions2;
begin
  //Read options
  Opt.ShowTypes:= GetIniKey(Opt.MainIni, 'Options', 'ShowTypes', true);
  Opt.ShowNumbers:= GetIniKey(Opt.MainIni, 'Options', 'ShowNumbers', true);
  OptTC.ShowDeletedAlways:= GetIniKey(Opt.MainIni, 'Options', 'ShowDeletedAlways', false);

  Opt.Timer:= GetIniKey(Opt.MainIni, 'Options', 'Refresh', false);
  Timer1.Enabled:= Opt.Timer;
  Timer1.Interval:= GetIniKey(Opt.MainIni, 'Options', 'RefreshInterval', 6000);

  Opt.SortCol:= TColumnType(GetIniKey(Opt.MainIni, 'Options', 'SortCol', integer(colName)));
  Opt.SortAsc:= GetIniKey(Opt.MainIni, 'Options', 'SortAsc', true);
  
  Opt.Register:= GetIniKey(Opt.MainIni, 'Options', 'RegisterExt', true);
  tcRegisterExtensions(Opt.Register, MsgCaption(0064), MsgCaption(0074));

  Left:= GetIniKey(Opt.MainIni, 'Window', 'Left', Left);
  Top:= GetIniKey(Opt.MainIni, 'Window', 'Top', Top);
  Width:= GetIniKey(Opt.MainIni, 'Window', 'Width', Width);
  Height:= GetIniKey(Opt.MainIni, 'Window', 'Height', Height);
  if GetIniKey(Opt.MainIni, 'Window', 'Max', false) then
    WindowState:= wsMaximized;
  TabControl1.TabIndex:= GetIniKey(Opt.MainIni, 'Window', 'TabIndex', 0);

  Opt.ShowColDesc:= GetIniKey(Opt.MainIni, 'Options', 'ShowColDesc', false);
  Opt.ShowColVersion:= GetIniKey(Opt.MainIni, 'Options', 'ShowColVersion', false);
  Opt.ShowColState:= GetIniKey(Opt.MainIni, 'Options', 'ShowColState', true);
  Opt.ShowColAssoc:= GetIniKey(Opt.MainIni, 'Options', 'ShowColAssoc', true);
  Opt.ShowColFilename:= GetIniKey(Opt.MainIni, 'Options', 'ShowColFilename', true);
  ApplyColumns;

  OptRpt.FN:= GetIniKey(Opt.MainIni, 'Report', 'FN', FTempDirectory+'\TC Plugman Report.txt');
  OptRpt.Names:= GetIniKey(Opt.MainIni, 'Report', 'Name', true);
  OptRpt.Vers:= GetIniKey(Opt.MainIni, 'Report', 'Version', true);
  OptRpt.States:= GetIniKey(Opt.MainIni, 'Report', 'State', false);
  OptRpt.Assocs:= GetIniKey(Opt.MainIni, 'Report', 'Assoc', false);
  OptRpt.Files:= GetIniKey(Opt.MainIni, 'Report', 'File', true);
end;


//-------------------------------------------------
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeLog(FLog);
  SaveOptions;
end;

//-------------------------------------------------
procedure TFormMain.SaveOptions;
begin
  SetIniKey(Opt.MainIni, 'Paths', 'FtpIni', Opt.TCIniFTP);
  SetIniKey(Opt.MainIni, 'Paths', 'RarPath', OptUnpack.RarPath);

  SetIniKey(Opt.MainIni, 'Paths', 'EditorUse', Opt.EditorUse);
  SetIniKey(Opt.MainIni, 'Paths', 'EditorPath', Opt.EditorPath);

  SetIniKey(Opt.MainIni, 'Paths', 'PluginsSrc', Opt.PluginsSrc);
  SetIniKey(Opt.MainIni, 'Paths', 'PluginsSrcFolder', Opt.PluginsSrcFolder);
  SetIniKey(Opt.MainIni, 'Paths', 'PluginsSrcZip', Opt.PluginsSrcZip);

  SetIniKey(Opt.MainIni, 'Paths', 'PluginsLister', Opt.PluginsLister);
  SetIniKey(Opt.MainIni, 'Paths', 'PluginsPacker', Opt.PluginsPacker);
  SetIniKey(Opt.MainIni, 'Paths', 'PluginsFS', Opt.PluginsFS);
  SetIniKey(Opt.MainIni, 'Paths', 'PluginsContent', Opt.PluginsContent);

  SetIniKey(Opt.MainIni, 'Options', 'Language', Opt.Lang);
  SetIniKey(Opt.MainIni, 'Options', 'ShowTypes', integer(Opt.ShowTypes));
  SetIniKey(Opt.MainIni, 'Options', 'ShowNumbers', integer(Opt.ShowNumbers));

  SetIniKey(Opt.MainIni, 'Options', 'ShowColDesc', integer(Opt.ShowColDesc));
  SetIniKey(Opt.MainIni, 'Options', 'ShowColVersion', integer(Opt.ShowColVersion));
  SetIniKey(Opt.MainIni, 'Options', 'ShowColState', integer(Opt.ShowColState));
  SetIniKey(Opt.MainIni, 'Options', 'ShowColAssoc', integer(Opt.ShowColAssoc));
  SetIniKey(Opt.MainIni, 'Options', 'ShowColFilename', integer(Opt.ShowColFilename));

  SetIniKey(Opt.MainIni, 'Options', 'Refresh', integer(Opt.Timer));
  SetIniKey(Opt.MainIni, 'Options', 'RefreshInterval', Timer1.Interval);
  SetIniKey(Opt.MainIni, 'Options', 'SortCol', integer(Opt.SortCol));
  SetIniKey(Opt.MainIni, 'Options', 'SortAsc', integer(Opt.SortAsc));

  SetIniKey(Opt.MainIni, 'Options', 'RegisterExt', integer(Opt.Register));
  SetIniKey(Opt.MainIni, 'Options', 'UseTCVar', integer(OptTC.UseTCPath));
  SetIniKey(Opt.MainIni, 'Options', 'InstRestart', integer(Opt.InstRestart));
  SetIniKey(Opt.MainIni, 'Options', 'InstTweakBefore', integer(Opt.InstTweakBefore));
  SetIniKey(Opt.MainIni, 'Options', 'InstTweakAfter', integer(Opt.InstTweakAfter));
  SetIniKey(Opt.MainIni, 'Options', 'BrowseCmd', Opt.BrowseCmd);

  SetIniKey(Opt.MainIni, 'Restart', 'RestartCmd', Opt.RestartCmd);
  SetIniKey(Opt.MainIni, 'Restart', 'RestartTime', Opt.RestartTime);
  SetIniKey(Opt.MainIni, 'Restart', 'RestartInForeground', integer(Opt.RestartForeground));

  if WindowState <> wsMaximized then
    begin
    SetIniKey(Opt.MainIni, 'Window', 'Left', Left);
    SetIniKey(Opt.MainIni, 'Window', 'Top', Top);
    SetIniKey(Opt.MainIni, 'Window', 'Width', Width);
    SetIniKey(Opt.MainIni, 'Window', 'Height', Height);
    end;
  SetIniKey(Opt.MainIni, 'Window', 'Max', WindowState = wsMaximized);
  SetIniKey(Opt.MainIni, 'Window', 'TabIndex', TabControl1.TabIndex);
  SaveColumns;

  SetIniKey(Opt.MainIni, 'Report', 'FN', OptRpt.FN);
  SetIniKey(Opt.MainIni, 'Report', 'Name', integer(OptRpt.Names));
  SetIniKey(Opt.MainIni, 'Report', 'Version', integer(OptRpt.Vers));
  SetIniKey(Opt.MainIni, 'Report', 'State', integer(OptRpt.States));
  SetIniKey(Opt.MainIni, 'Report', 'Assoc', integer(OptRpt.Assocs));
  SetIniKey(Opt.MainIni, 'Report', 'File', integer(OptRpt.Files));
end;


procedure TFormMain.UpdateColumnIndexes;
var
  i: TColumnType;
  n: integer;
begin
  for i:= Low(TColumnType) to High(TColumnType) do
    ColIdx[i]:= -1;

  n:= 0;
  ColIdx[colName]:= n;

  if Opt.ShowColDesc then
    begin
    Inc(n);
    ColIdx[colDesc]:= n;
    end;

  if Opt.ShowColVersion then
    begin
    Inc(n);
    ColIdx[colVersion]:= n;
    end;

  if Opt.ShowColState then
    begin
    Inc(n);
    ColIdx[colState]:= n;
    end;

  if Opt.ShowColAssoc then
    begin
    Inc(n);
    ColIdx[colAssoc]:= n;
    end;

  if Opt.ShowColFilename then
    begin
    Inc(n);
    ColIdx[colFilename]:= n;
    end;
end;

function TFormMain.ColumnTypeToIndex(typ: TColumnType): integer;
begin
  UpdateColumnIndexes;
  Result:= ColIdx[typ];
end;

function TFormMain.ColumnIndexToType(idx: integer): TColumnType;
var
  i: TColumnType;
begin
  Result:= colUnknown;

  UpdateColumnIndexes;

  for i:= Low(TColumnType) to High(TColumnType) do
    if ColIdx[i]=idx then
      begin Result:= i; Break end;
end;


procedure TFormMain.SaveColumns;
var
  i: TColumnType;
  n: integer;
begin
  for i:= Low(TColumnType) to High(TColumnType) do
    begin
    n:= ColumnTypeToIndex(i);
    if n>=0 then
      SetIniKey(Opt.MainIni, 'Window', cColumnString[i], ListView1.Columns[n].Width);
    end;
end;

procedure TFormMain.LoadColumns;
var
  i: TColumnType;
  n: integer;
begin
  for i:= Low(TColumnType) to High(TColumnType) do
    begin
    n:= ColumnTypeToIndex(i);
    if n>=0 then
      ListView1.Columns[n].Width:= GetIniKey(Opt.MainIni, 'Window', cColumnString[i], cColumnDefWidth[i]);
    end;
end;


procedure TFormMain.btnBrowseExeClick(Sender: TObject);
begin
  with OpenDialogExe do
    begin
    if IsFileExist(Opt.TCExe) then
      begin
      FileName:= Opt.TCExe;
      InitialDir:= ExtractFileDir(FileName);
      end
    else
      begin
      FileName:= 'Totalcmd.exe';
      InitialDir:= 'C:\';
      end;
    if Execute then
      begin
      SetTCPath(FileName);
      SetIniKey(Opt.MainIni, 'Paths', 'TCPath', Opt.TCExe); //Save TC path here
      end;
    end;
end;


procedure TFormMain.UpdateColumnsImages;
var
  i, idx: integer;
begin
  idx:= ColumnTypeToIndex(Opt.SortCol);
  with ListView1 do
    for i:= 0 to Columns.Count-1 do
      with Columns[i] do
        begin
        if i<>idx then ImageIndex:= -1 else
          if Opt.SortAsc
            then ImageIndex:= cColUp
            else ImageIndex:= cColDown;
        end;
end;


//--------------------------------------
function SMatch(const mask, s: string): boolean;
begin
  Result:= (mask='*') or (mask=s) or (Pos(UpperCase(mask), UpperCase(s))>0);
end;

//--------------------------------------
procedure TFormMain.UpdateList(AUnloadPlugins: boolean);

  function _Filtered(n: integer): boolean;
  begin
    with TabControl1 do
      Result:= (TabIndex=0) or (TabIndex=Ord(LibList[n].fType));
    if Opt.FilterString<>'' then
      Result:= Result and
        (SMatch(Opt.FilterString, LibList[n].Title) or
         SMatch(Opt.FilterString, ChangeFileExt(ExtractFileName(LibList[n].fn), '')) );
  end;

var
  i, n, n_disabled, cnt, top_num: integer;
  prefix,
  prefixType,
  prefixNumber: string;
begin
  //Unload plugins, if needed
  if AUnloadPlugins and Opt.UnloadPlugins then
    UnloadPlugins;

  //Reread ini file
  try
    Screen.Cursor:= crHourGlass;
    try
      tcSetPaths(Opt.TCExe, Opt.TCIni);
      tcSearchPlugins(true);
    finally
      Screen.Cursor:= crDefault;
    end;
  except
  end;

  //Fill listview
  with ListView1 do
    begin
    if Selected=nil
      then n:= 0
      else n:= Selected.Index;

    if TopItem=nil                  //Led
      then top_num:=0               //Led
      else top_num:= TopItem.Index; //Led
  
    Items.BeginUpdate;
    Items.Clear;
    n_disabled:= 0;
    for i:= 1 to LibNum do
      if _Filtered(i) then
        with Items.Add do
          begin
          Data:= pointer(i);

          if LibList[i].fDisabled then Inc(n_disabled);
          if (not LibList[i].fDisabled) and tcFileExists(LibList[i].fn)
            then ImageIndex:= Ord(LibList[i].fType)
            else ImageIndex:= 0;

          //Init prefixes
          prefixType:= cLibTypeNames[LibList[i].fType] + ':  ';
          prefixNumber:= '';
          cnt:= LibCount(LibList[i].fType);

          if (LibList[i].fType in [fLister, fContent]) then
            begin
            if cnt>100 then prefixNumber:= Format('[%3d] ', [LibList[i].fIndex]) else
             if cnt>10 then prefixNumber:= Format('[%2d] ', [LibList[i].fIndex]) else
                            prefixNumber:= Format('[%d] ', [LibList[i].fIndex]);
            if LibList[i].fDisabled then
              prefixNumber:= ' ' + prefixNumber;
            end;

          if (not Opt.ShowTypes) and (TabControl1.TabIndex > 0) then
            prefixType:= '';
          if (not Opt.ShowNumbers) then
            prefixNumber:= '';

          //End of prefixes

          Caption:= prefixType + prefixNumber + LibList[i].title;

          if Opt.ShowColDesc then
            begin
            prefix:= GetFileDiz(LibList[i].fn, 'descript.ion');
            if prefix='' then
              prefix:= FileVersionInfo(LibList[i].fn, vsFileDescription);
            SubItems.Add(prefix);
            end;

          if Opt.ShowColVersion then
            SubItems.Add(FileVersionInfo(LibList[i].fn, ''));

          if Opt.ShowColState then
            SubItems.Add(LibState(i));

          if Opt.ShowColAssoc then
            SubItems.Add(LibList[i].params);

          if Opt.ShowColFilename then
            SubItems.Add(LibList[i].fn);
          end;

    Items.EndUpdate;

    {Enabled:= Items.Count>0;
    if not Enabled
      then
        with Items.Add do
         begin
         Caption:= S0055;
         ImageIndex:= 4;
         end
      else}

    if n>=Items.Count then n:= 0;
    if n<Items.Count then
      begin
      Selected:= Items[n];
      ItemFocused:= Selected;
      end;

    if (Items.Count>VisibleRowCount) and (top_num<>0)  //Led
      then ListView1.Scroll(0, 5+17*top_num);          //Led
    end; //with ListView1 do

  //Update columns and status bar
  UpdateColumnsImages;

  with StatusBar1 do
    begin
    if ListView1.Enabled
      then n:= ListView1.Items.Count
      else n:= 0;
    Panels[0].Text:= Format(S0072, [n, n_disabled]);
    Panels[1].Text:= MsgCaption(0116)+': '+Opt.FilterString;
    end;
end;


function TFormMain.LibFN: string;
begin
  with ListView1 do
    if Selected<>nil then
      Result:= LibList[integer(Selected.Data)].fn;
end;

function TFormMain.LibTitle: string;
begin
  with ListView1 do
    if Selected<>nil then
      Result:= LibList[integer(Selected.Data)].title;
end;

function TFormMain.LibIni: string;
begin
  Result:= ChangeFileExt(libFN, '.ini');
end;


procedure TFormMain.btnTCRunClick(Sender: TObject);
begin
  RunTC;

  UpdateTCState;
  UpdateList(false);

  if not Opt.RestartForeground then
    Application.BringToFront;
end;


procedure TFormMain.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
var
  OldIdx, NewIdx: integer;
begin
  OldIdx:= ColumnTypeToIndex(Opt.SortCol);
  NewIdx:= Column.Index;

  if OldIdx=NewIdx then
    Opt.SortAsc:= not Opt.SortAsc
  else
    Opt.SortCol:= ColumnIndexToType(NewIdx);

  UpdateColumnsImages;
  ListView1.AlphaSort;
end;


function _ItemAssigned(Item: TListItem): boolean;
begin
  Result:= Assigned(Item) and Assigned(Item.SubItems);
end;

procedure TFormMain.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  idx: integer;
begin
  if not (_ItemAssigned(Item1) and _ItemAssigned(Item2)) then
    begin Compare:= 0; Exit end;

  case Opt.SortCol of
    colName:
      begin
      Compare:= StrIComp(
        PChar(Item1.Caption),
        PChar(Item2.Caption))
      end;

    else
      begin
      idx:= ColumnTypeToIndex(Opt.SortCol)-1;
      if idx>=0 then
        Compare:= StrIComp(
          PChar(Item1.SubItems[idx]),
          PChar(Item2.SubItems[idx]))
      else
        Compare:= StrIComp(
          PChar(Item1.Caption),
          PChar(Item2.Caption))
      end;
    end;

  if not Opt.SortAsc then
    Compare:= -Compare;
end;


procedure TFormMain.ListView1DblClick(Sender: TObject);
begin
  btnDisableClick(Self);
end;


procedure TFormMain.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  sel, en: boolean;
  n: integer;
begin
  with ListView1 do
    begin
    sel:= Selected<>nil;
    if sel then n:= integer(Selected.Data) else n:= 0;
    en:= sel and (LibList[n].fDisabled);

    btnEnable.Visible:= en;
    mnuEnable.Visible:= en;
    btnDisable.Visible:= not en;
    mnuDisable.Visible:= not en;

    mnu2InstallFolder.Enabled:= not IsQS;

    btnDisable.Enabled:= sel;
    btnRemove.Enabled:= sel or IsQS;
    btnRename.Enabled:= sel and (SelCount=1) and (LibList[n].fType in [fFS, fLister, fContent]);
    btnTweak.Enabled:= sel and (SelCount=1) and (LibList[n].fType=fPacker);
    btnTweak.Visible:= sel and (LibList[n].fType=fPacker);
    btnRename.Visible:= not btnTweak.Visible;
    btnConfigure.Enabled:= (sel and (SelCount=1)) or IsQS;
    btnOrder.Enabled:= (not IsFS) and (not IsQS);
    mnuDisable.Enabled:= sel;
    mnuUninstall.Enabled:= sel;
    mnuRename.Enabled:= btnRename.Enabled;
    mnuRename.Visible:= btnRename.Visible;
    mnuTweak.Enabled:= btnTweak.Enabled;
    mnuTweak.Visible:= btnTweak.Visible;
    mnuConfigure.Enabled:= btnConfigure.Enabled;
    mnuProperties.Enabled:= sel;
    mnuOrder.Enabled:= btnOrder.Enabled;
    mnuBrowse.Enabled:= sel;
    end;
end;

//-------------------------------------------------
procedure TFormMain.btnRemoveClick(Sender: TObject);
var
  i, n: integer;
  List: TStringList;
begin
  if IsQS then
    begin
    if chkQS1.Checked then
      begin MsgError(MsgString(0066)); Exit end;
    if not MsgConfirmed(Format(S0057, [QSName, ExtractFileName(FQS)])) then
      Exit;
    with Listbox1 do
     if ItemIndex >= 0 then
      begin
      n:= ItemIndex;
      Items.Delete(n);
      if (Items.Count > 0) then
        begin
        if (n > Pred(Items.Count)) then Dec(n);
        ItemIndex:= n;
        Listbox1Click(Self);
        end
      else
        begin
        chkQS1.Checked:= true;
        end;
      end;
    Exit;
    end;
    
  Timer1.Enabled:= false;
  with ListView1 do
    if Selected<>nil then
      begin
      n:= integer(Selected.Data);
      if ((SelCount=1) and (MsgConfirmed(Format(S0057, [LibList[n].title, ExtractFileName(LibList[n].fn)])))) or
         ((SelCount>1) and (MsgConfirmed(Format(S0062, [SelCount])))) then

        //------- Uninstall plugins
        begin
        List:= TStringList.Create;

        //1) Remember numbers to delete
        for i:= Selected.Index to Items.Count-1 do
          if Items[i].Selected then
            begin
            n:= integer(Items[i].Data);
            if LibList[n].fDisabled then
              begin
              if (SelCount=1) then
                MsgError(Format(S0056, [LibList[n].Title]));
              end
            else
              List.Add(IntToStr(n));
            end;

        //2) Delete them
        try
          ProgressShow(Self, progressPlugins);
          for i:= 0 to List.Count-1 do
            begin
            if ProgressActionCancelled(libList[StrToInt(List[i])].fn, i+1, List.Count) then
              Break;
            tcUninstallPlugin(StrToInt(List[i]), false);
            end;
        finally
          ProgressHide(Self);
          List.Free;
          UpdateList(true);
        end;
        end;
      end;

  Timer1.Enabled:= Opt.Timer;
end;

//-------------------------------------------------
procedure TFormMain.btnDisableClick(Sender: TObject);
var
  i, n: integer;
  Disabled: boolean;
  List: TStringList;
  _fn: string;
  _typ: TLibType;
begin
  Timer1.Enabled:= false;

  with ListView1 do
    if Selected<>nil then
      begin
      n:= integer(Selected.Data);
      Disabled:= LibList[n].fDisabled;

      //------ Enable plugins
      if Disabled then
        begin
        List:= TStringList.Create;

        //1) Remember filenames to enable
        for i:= 0 to Items.Count-1 do //for-to
          if Items[i].Selected then
            with LibList[integer(Items[i].Data)] do
              if fDisabled then
                List.Add(fn+';'+IntToStr(Ord(fType)));

        //2) Enable them
        try
          ProgressShow(Self, progressPlugins);
          for i:= 0 to List.Count-1 do
            begin
            if ProgressActionCancelled(List[i], i+1, List.Count) then
              Break;
            _fn:= SDeleteFrom(List[i], ';');
            _typ:= TLibType(StrToIntDef(SDeleteTo(List[i], ';'), 0));
            if _typ = fUnknownType then
              MsgError('Unknown type for disabled plugin:'#13+_fn)
            else
              tcEnablePlugin(_fn, _typ);
            end;
        finally
          ProgressHide(Self);
          List.Free;
          UpdateList(true);
        end;
        end

      else
      //------ Disable plugins
      if (SelCount>1) or (MsgConfirmed(Format(S0058, [LibList[n].title]))) then
        begin
        List:= TStringList.Create;

        //1) Remember numbers to disable
        for i:= Items.Count-1 downto 0 do //for-downto
          if Items[i].Selected then
            List.Add(IntToStr(integer(Items[i].Data)));

        //2) Disable them
        try
          ProgressShow(Self, progressPlugins);
          for i:= 0 to List.Count-1 do
            begin
            if ProgressActionCancelled(libList[StrToInt(List[i])].fn, i+1, List.Count) then
              Break;
            tcUninstallPlugin(StrToInt(List[i]), true);
            end;
        finally
          ProgressHide(Self);
          List.Free;
          UpdateList(true);
        end;
        end;
      end;

  Timer1.Enabled:= Opt.Timer;
end;


//-------------------------------------------------
procedure TFormMain.btnAddClick(Sender: TObject);
var
  i: integer;
begin
  if IsQS then
    with OpenDialogDll do
      begin
      FileName:= FQS;
      InitialDir:= ExtractFileDir(FileName);
      if Execute then
        with Listbox1 do
        if Items.IndexOf(FileName) < 0 then
          begin
          chkQS2.Checked:= true;
          Items.Add(FileName);
          ItemIndex:= Pred(Items.Count);
          ListBox1Click(Self);
          end;
      Exit;
      end;

  Timer1.Enabled:= false;
  with OpenDialogPlugins do
    begin
    InitialDir:= Opt.PluginsSrc;
    FileName:= '';
    Files.Clear;

    if Execute then
      begin
      Opt.PluginsSrc:= ExtractFileDir(FileName);
      ClearLog(FLog);
      try
        ProgressShow(Self, progressPlugins);
        for i:= 0 to Files.Count-1 do
          begin
          if ProgressActionCancelled(Files[i], i+1, Files.Count) then
            Break;
          InstallPlugin(Files[i], true, '', FLog);
          end;
      finally
        ProgressHide(Self);
        UpdateList(false);
      end;

      ShowLog(FLog);
      end;
    end;

  Timer1.Enabled:= Opt.Timer;
end;


procedure TFormMain.mnuInstallZipClick(Sender: TObject);
var
  i: integer;
begin
  with OpenDialogZip do
    if IsQS then
      Options:= Options - [ofAllowMultiSelect]
    else
      Options:= Options + [ofAllowMultiSelect];

  if IsQS then
    with OpenDialogZip do
      begin
      InitialDir:= Opt.PluginsSrcZip;
      FileName:= '';
      if Execute then
        begin
        Opt.PluginsSrcZip:= ExtractFileDir(FileName);
        InstallPluginMatchFromArc(FileName);
        end;
      Exit;
      end;

  Timer1.Enabled:= false;
  with OpenDialogZip do
    begin
    InitialDir:= Opt.PluginsSrcZip;
    FileName:= '';
    Files.Clear;

    if Execute then
      begin
      Opt.PluginsSrcZip:= ExtractFileDir(Files[0]);

      ClearLog(FLog);

      try
        ProgressShow(Self, progressPlugins);
        for i:= 0 to Files.Count-1 do
          begin
          if ProgressActionCancelled(Files[i], i+1, Files.Count) then
            Break;
          InstallPluginFromArchive(Files[i], FLog);
          end;
      finally
        ProgressHide(Self);
        UpdateList(false);
      end;

      //Show log
      ShowLog(FLog);
      end;
    end;

  Timer1.Enabled:= Opt.Timer;
end;


//-------------------------------------------------
procedure TFormMain.btnRefreshClick(Sender: TObject);
begin
  UpdateTCState;
  UpdateList(false);
end;

//-------------------------------------------------
procedure TFormMain.btnRenameClick(Sender: TObject);
var
  n: integer;
begin
  Timer1.Enabled:= false;
  with ListView1 do
   if Selected<>nil then
    begin
    n:= integer(Selected.Data);
    if LibList[n].fDisabled
      then
        MsgError(Format(S0056, [LibList[n].Title]))
      else
        with TFormAdd.Create(nil) do
          try
            sFile.Text:= LibList[n].fn;
            sExt.Text:= LibList[n].params;
            fLibType:= LibList[n].fType;
            fLibDeleted:= not tcFileExists(LibList[n].fn);
            if ShowModal=mrOk then
              begin
              tcRenamePlugin(n, sExt.Text);
              UpdateList(true);
              end;
          finally
            Release;
          end;
    end;
  Timer1.Enabled:= Opt.Timer;
end;

//----------------------------------------------
procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  UpdateTCState;
  if not IsQS then
    UpdateStates;
end;

//-------------------------------------------------
procedure TFormMain.btnBrowseIniClick(Sender: TObject);
begin
  with OpenDialogIni do
    begin
    if IsFileExist(Opt.TCIni) then
      begin
      FileName:= Opt.TCIni;
      InitialDir:= ExtractFileDir(FileName);
      end
    else
      begin
      FileName:= '';
      InitialDir:= 'C:\';
      end;
    if Execute then
      begin
      Opt.TCIni:= FileName;
      SetIniKey(Opt.MainIni, 'Paths', 'IniPath', Opt.TCIni); //Save TC ini path here
      SetTCPath(Opt.TCExe);
      end;
    end;
end;


//-------------------------------------------------
procedure LoadOptions;
begin
  Opt.MainIni:= ExtractFilePath(ParamStr(0))+'Plugman.ini';

  Opt.TCExe:= GetIniKeyExpand(Opt.MainIni, 'Paths', 'TCPath', tcDefExe);
  tcSetPaths(Opt.TCExe, '-');
  Opt.TCIni:= GetIniKeyExpand(Opt.MainIni, 'Paths', 'IniPath', tcDefIni);
  tcSetPaths(Opt.TCExe, Opt.TCIni);

  Opt.TCIniFTP:= GetIniKey(Opt.MainIni, 'Paths', 'FtpIni', tcDefIniFtp);
  OptUnpack.RarPath:= GetIniKey(Opt.MainIni, 'Paths', 'RarPath', SExpandVars('%ProgramFiles%\WinRAR\WinRAR.exe'));
  OptUnpack.RunMinimized:= GetIniKey(Opt.MainIni, 'Options', 'RunUnpackMinimized', true);

  Opt.EditorUse:= GetIniKey(Opt.MainIni, 'Paths', 'EditorUse', false);
  Opt.EditorPath:= GetIniKey(Opt.MainIni, 'Paths', 'EditorPath', '');

  OptLog.Filename:= ChangeFileExt(ParamStr(0), '.log');
  OptLog.Enable:= GetIniKey(Opt.MainIni, 'Options', 'Log', false);

  Opt.PluginsSrc:= GetIniKey(Opt.MainIni, 'Paths', 'PluginsSrc', ExtractFileDir(Opt.TCExe));
  Opt.PluginsSrcFolder:= GetIniKey(Opt.MainIni, 'Paths', 'PluginsSrcFolder', Opt.PluginsSrc);
  Opt.PluginsSrcZip:= GetIniKey(Opt.MainIni, 'Paths', 'PluginsSrcZip', 'C:\');

  Opt.PluginsLister:= GetIniKey(Opt.MainIni, 'Paths', 'PluginsLister', '%COMMANDER_PATH%\Plugins\WLX');
  Opt.PluginsPacker:= GetIniKey(Opt.MainIni, 'Paths', 'PluginsPacker', '%COMMANDER_PATH%\Plugins\WCX');
  Opt.PluginsFS:= GetIniKey(Opt.MainIni, 'Paths', 'PluginsFS', '%COMMANDER_PATH%\Plugins\WFX');
  Opt.PluginsContent:= GetIniKey(Opt.MainIni, 'Paths', 'PluginsContent', '%COMMANDER_PATH%\Plugins\WDX');

  OptTC.ShowNames:= libCmtFilename;
  OptTC.UseTCPath:= GetIniKey(Opt.MainIni, 'Options', 'UseTCVar', true);
  Opt.InstRestart:= GetIniKey(Opt.MainIni, 'Options', 'InstRestart', false);
  Opt.InstTweakBefore:= GetIniKey(Opt.MainIni, 'Options', 'InstTweakBefore', true);
  Opt.InstTweakAfter:= GetIniKey(Opt.MainIni, 'Options', 'InstTweakAfter', false);
  Opt.UnloadPlugins:= GetIniKey(Opt.MainIni, 'Options', 'UnloadPlugins', false);
  Opt.BrowseCmd:= GetIniKey(Opt.MainIni, 'Options', 'BrowseCmd', '%COMMANDER_PATH%\Totalcmd.exe /o /r="%s"');
  Opt.RestartCmd:= StrToIntDef(GetIniKey(Opt.MainIni, 'Restart', 'RestartCmd', '0'), 0);
  Opt.RestartTime:= GetIniKey(Opt.MainIni, 'Restart', 'RestartTime', 1500);
  Opt.RestartForeground:= GetIniKey(Opt.MainIni, 'Restart', 'RestartInForeground', false);

  Opt.Lang:= GetIniKey(Opt.MainIni, 'Options', 'Language', 'English');
  SetMsgLang(Opt.Lang);
end;


//-------------------------------------------------
procedure TFormMain.btnTCRestartClick(Sender: TObject);
begin
  RestartTC;
  UpdateTCState;
  UpdateList(false);
  if not Opt.RestartForeground then
    Application.BringToFront;
end;


//-------------------------------------------------
procedure TFormMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize:= (NewWidth>=570) and (NewHeight>=250);
end;

//-------------------------------
function TFormMain.QSName: string;
begin
  if FQS = '' then
    Result:= 'tcmatch'
  else
    Result:= ChangeFileExt(ExtractFileName(FQS), '');
end;

//------------------------------
function TFormMain.QSIni: string;
begin
  if FQS = '' then
    Result:= ExtractFilePath(Opt.TCExe) + 'tcmatch.ini'
  else
    Result:= ChangeFileExt(FQS, '.ini');
end;

//------------------------------
function TFormMain.QSFile: string;
begin
  if FQS = '' then
    Result:= ExtractFilePath(Opt.TCExe) + 'tcmatch.dll'
  else
    Result:= FQS;
end;

//-------------------------------------------------
procedure TFormMain.btnConfigureClick(Sender: TObject);
var
  MainHandle: THandle;
  n: integer;
begin
  if IsQS then
    begin
    if IsFileExist(QSIni)
      then RunEditor(QSIni)
      else MsgError(Format(S0070, [QSName]));
    Exit;  
    end;

  Timer1.Enabled:= false;
  with ListView1 do
   if (Selected<>nil) then
     begin
     MainHandle:= ListView1.Handle; //Should be Application's for Form's handle but left
                                    //LV.Handle for compatability with decClipboardFS.wfx

     n:= integer(Selected.Data);
     case LibList[n].fType of
       fPacker:
         if not tcPackerConfigure(LibFN, MainHandle) then
           MsgError(Format(S0069, [LibList[n].Title]));

       fFS:
         if not tcFSConfigure2(LibFN, ExtractFileDir(Opt.TCIni), MainHandle) then
           MsgError(Format(S0069, [LibList[n].Title]));

       fLister,
       fContent:
         if IsFileExist(LibIni)
           then RunEditor(LibIni)
           else MsgError(Format(S0070, [LibList[n].Title]));
      end;

    Application.BringToFront;
    end;

  Timer1.Enabled:= Opt.Timer;
end;


//-------------------------------------------------
procedure TFormMain.ShowOrderAndUpdateList(typ: TLibType);
begin
  Timer1.Enabled:= false;

  if ShowOrderDialog(typ) then
    UpdateList(true);

  Timer1.Enabled:= Opt.Timer;
end;

//-------------------------------------------------
procedure TFormMain.btnOrderClick(Sender: TObject);
var
  typ: TLibType;
begin
  with ListView1 do
    begin
    if (TabControl1.TabIndex=4) or //закладка Content
      ((TabControl1.TabIndex=0) and //либо закладка All + Content-плагин
       (Selected<>nil) and
       (LibList[integer(Selected.Data)].fType=fContent)) then typ:= fContent
    else
    if (TabControl1.TabIndex=2) or //закладка Packer
      ((TabControl1.TabIndex=0) and //либо закладка All + Packer-плагин
       (Selected<>nil) and
       (LibList[integer(Selected.Data)].fType=fPacker)) then typ:= fPacker
    else
      typ:= fLister;
    end;

  ShowOrderAndUpdateList(typ);
end;


//-------------------------------------------------
procedure TFormMain.mnuBrowseClick(Sender: TObject);
var
  cmd: string;
begin
  with ListView1 do
    if Selected<>nil then
      begin
      Opt.BrowseCmd:= GetIniKey(Opt.MainIni, 'Options', 'BrowseCmd', '%COMMANDER_PATH%\Totalcmd.exe /o /r="%s"');
      cmd:= SExpandVars(Opt.BrowseCmd);
      cmd:= Format(cmd, [ExtractFileDir(LibFN)]);
      FExecProcess(cmd, SW_SHOW, false);
      Sleep(Opt.RestartTime);
      UpdateTCState;
      end;
end;


//-------------------------------------------------
procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F1:
      if (Shift=[]) then
        begin
        ShowShortcutsHelp;
        Key:= 0;
        end;

    VK_DELETE,
    VK_F8:
      if (Shift=[]) then
        begin
        if btnRemove.Enabled then btnRemoveClick(Self);
        Key:= 0;
        end;

    VK_INSERT:
      begin
      if (Shift=[]) and btnAdd.Enabled then btnAddClick(Self) else
       if (Shift=[ssCtrl]) and mnu2InstallZip.Enabled then mnuInstallZipClick(Self) else
        if (Shift=[ssAlt]) and mnu2InstallFolder.Enabled then mnuInstallFolderClick(Self);
      Key:= 0;
      end;

    VK_F6:
      if (Shift=[]) then
        begin
        if btnRename.Visible and btnRename.Enabled then btnRenameClick(Self) else
         if btnTweak.Visible and btnTweak.Enabled then btnTweakClick(Self);
        Key:= 0;
        end;

    VK_F9:
      if (Shift=[]) then
        begin
        if btnConfigure.Enabled then btnConfigureClick(Self);
        Key:= 0;
        end;

    VK_RETURN:
      begin
      if (Shift=[ssAlt]) then
        begin
        if IsQS then FShowProperties(QSFile, Handle) else
         if mnuProperties.Enabled then mnuPropertiesClick(Self);
        Key:= 0;
        end
      else
      if (Shift=[]) then
        begin
        if btnDisable.Enabled then btnDisableClick(Self);
        Key:= 0;
        end;
      end;

    VK_F4:
      if (Shift=[]) then
        begin
        if mnuBrowse.Enabled then mnuBrowseClick(Self);
        Key:= 0;
        end;

    Ord('A'):
      if (Shift=[ssCtrl]) then
        begin
        SelectAll;
        Key:= 0;
        end;

    Ord('O'):
      if (Shift=[ssCtrl]) then
        begin
        if btnOrder.Enabled then btnOrderClick(Self);
        Key:= 0;
        end;

    Ord('R'):
      if (Shift=[ssCtrl]) then
        begin
        btnRefreshClick(Self);
        Key:= 0;
        end;

    Ord('Z'):
      if (Shift=[ssCtrl]) then
        begin
        if mnuUninstall.Enabled then mnuDescClick(Self);
        Key:= 0;
        end;

    Ord('F'):
      if (Shift=[ssCtrl]) then
        begin
        StatusBar1Click(Self);
        Key:= 0;
        end;

    Ord('1')..Ord('6'):
      begin
      TabControl1.TabIndex:= Key-Ord('1');
      TabControl1Change(Self);
      Key:= 0;
      end;
  end;
end;


procedure TFormMain.TabControl1Change(Sender: TObject);
begin
  UpdateList(false);
  ListView1Change(Self, nil, TItemChange(0)); //To update buttons
  ListView1.Visible:= not IsQS;
  StatusBar1.Visible:= not IsQS;
  Panel1.BoundsRect:= ListView1.BoundsRect;
  Panel1.Visible:= IsQS;
  if IsQS then LoadQS;
  if IsQS then
    begin if chkQS1.Checked then chkQS1.SetFocus else chkQS2.SetFocus; end
  else
    if ListView1.CanFocus then
      ListView1.SetFocus;
end;

procedure TFormMain.btnTweakClick(Sender: TObject);
var
  n: integer;
begin
  Timer1.Enabled:= false;
  with ListView1 do
   if Selected<>nil then
    begin
    n:= integer(Selected.Data);
    if LibList[n].fDisabled
      then
        MsgError(Format(S0056, [LibList[n].Title]))
      else
        with TFormTweak.Create(nil) do
          try
            SetFN(LibFN);
            if ShowModal=mrOk then
              UpdateList(true);
          finally
            Release;
          end;
    end;
  Timer1.Enabled:= Opt.Timer;
end;


procedure TFormMain.mnuDescClick(Sender: TObject);
begin
  with TFormAddExt.Create(nil) do
    try
      Caption:= Format(MsgCaption(0110), [LibTitle]);
      Label1.Caption:= MsgCaption(0111);
      ffExt.Text:= GetFileDiz(LibFN, 'Descript.ion');
      if ffExt.Text='' then ffExt.Text:= FileVersionInfo(LibFN, vsFileDescription);
      if ShowModal=mrOk then
        begin
        SetFileDiz(LibFN, 'Descript.ion', ffExt.Text);
        btnRefreshClick(Self);
        end;
    finally
      Release;
    end;
end;


procedure TFormMain.btnEditIniClick(Sender: TObject);
begin
  RunEditor(Opt.TCIni);
end;


procedure TFormMain.btnReportClick(Sender: TObject);
var
  BaseDir: string;
  OK: boolean;
begin
  with TFormReport.Create(nil) do
    try
      sFile.Text:= OptRpt.FN;
      fName.Checked:= OptRpt.Names;
      fVersion.Checked:= OptRpt.Vers;  
      fState.Checked:= OptRpt.States;
      fAssoc.Checked:= OptRpt.Assocs;
      fFilename.Checked:= OptRpt.Files; 

      if ShowModal=mrOk then
        begin
        OptRpt.FN:= sFile.Text;
        OptRpt.Names:= fName.Checked;
        OptRpt.Vers:= fVersion.Checked;
        OptRpt.States:= fState.Checked;
        OptRpt.Assocs:= fAssoc.Checked;
        OptRpt.Files:= fFilename.Checked;

        BaseDir:= tcOption('Configuration', 'PluginBaseDir');

        Screen.Cursor:= crHourGlass;
        try
          OK:= tcSaveReport(SVersion, Opt.TCExe, Opt.TCIni, BaseDir);
        finally
          Screen.Cursor:= crDefault;
        end;

        if OK then
          RunEditor(sFile.Text)
        else
          MsgError(Format(S0089, [sFile.Text]));
        end;
    finally
      Release;
    end;
end;

procedure TFormMain.StatusBar1Click(Sender: TObject);
begin
  with TFormAddExt.Create(nil) do
    try
      Caption:= MsgCaption(0116);
      Label1.Caption:= Caption+':';
      ffExt.Text:= Opt.FilterString;
      if ShowModal=mrOk then
        begin
        Opt.FilterString:= ffExt.Text;
        btnRefreshClick(Self);
        end;
    finally
      Release;
    end;
end;

procedure TFormMain.mnuFilterClick(Sender: TObject);
begin
  StatusBar1Click(Self);
end;


procedure TFormMain.mnuInstallFolderClick(Sender: TObject);
var
  Dir: string;
  List: TStringList;
  i: integer;
begin
  Dir:= Opt.PluginsSrcFolder;
  if SelectDirectory(MsgCaption(0096), '', Dir) then
    begin
    Opt.PluginsSrcFolder:= Dir;
    SDelLastSlash(Dir);

    ClearLog(FLog);
    List:= TStringList.Create;

    try
      ProgressShow(Self, progressScanDirs);

      if FindPluginsInDir(Dir, List) then
        begin
        if List.Count=0 then
          begin
          FLog.Errors.Add(MsgCaption(0427)+': '+Dir);
          end
        else
          begin
          ProgressShow(Self, progressPlugins);

          for i:= 0 to List.Count-1 do
            begin
            //Show progress
            if ProgressActionCancelled(List[i], i+1, List.Count) then
              begin
              FLog.Errors.Add(MsgCaption(0434));
              Break;
              end;

            //Install
            InstallPlugin(
              List[i], true, 
              ChangeFileExt(ExtractFileName(List[i]), ''), FLog);
            end;
          end;
        end
      else
        begin
        FLog.Errors.Add(MsgCaption(0434));
        end;

    finally
      ProgressHide(Self);
      List.Free;
      UpdateList(false);
    end;

    ShowLog(FLog);
    end;
end;


procedure TFormMain.mnuOrderListerClick(Sender: TObject);
var
  typ: TLibType;
begin
  if Sender=mnuOrderLister then typ:= fLister else
   if Sender=mnuOrderPacker then typ:= fPacker else
     if Sender=mnuOrderContent then typ:= fContent else
      typ:= fUnknownType;

  ShowOrderAndUpdateList(typ);
end;

procedure TFormMain.SelectAll;
var
  i: integer;
begin
  with ListView1 do
    for i:= 0 to Items.Count-1 do
      Items[i].Selected:= true;
end;

procedure TFormMain.mnuPropertiesClick(Sender: TObject);
var
  fn, fn2: string;
  is64: boolean;
begin
  fn:= LibFN;
  fn2:= fn+'64';

  is64:= (LowerCase(FileVersionInfo(Opt.TCExe, vsInternalName))='totalcmd64');
  if is64 and FileExists(fn2) then
    fn:= fn2;

  FShowProperties(fn, Handle);
end;


procedure TFormMain.mnuOptAboutClick(Sender: TObject);
begin
  with TFormAbout.Create(nil) do
    try
      ShowModal;
    finally
      Release;
    end
end;

function TFormMain.IsFS: boolean;
begin
  Result:= TabControl1.TabIndex = 3;
end;

function TFormMain.IsQS: boolean;
begin
  Result:= TabControl1.TabIndex = 5;
end;

procedure TFormMain.SaveQS;
var i: integer;
  s: string;
begin
  with Listbox1 do
    begin
    SetIniKey(Opt.MainIni, 'QS', 'Count', Items.Count);
    for i:= 0 to Items.Count-1 do
      SetIniKey(Opt.MainIni, 'QS', IntToStr(i), Items[i]);
    end;
  if FQS = '' then
    DelIniKey(Opt.TCIni, 'Configuration', 'tcmatch')
  else
    begin
    s:= FQS;
    SReplace(s, ExtractFileDir(Opt.TCExe), '%COMMANDER_PATH%');
    SetIniKey(Opt.TCIni, 'Configuration', 'tcmatch', s);
    end;
end;

procedure TFormMain.LoadQS;
var i: integer;
begin
  FQS:= GetIniKey(Opt.TCIni, 'Configuration', 'tcmatch', '');
  FQS:= SExpandVars(FQS);
  if FQS = '' then
    chkQS1.Checked:= true
  else
    chkQS2.Checked:= true;
  with Listbox1 do
    begin
    Items.Clear;
    for i:= 0 to Pred(GetIniKey(Opt.MainIni, 'QS', 'Count', 0)) do
      Items.Add(GetIniKey(Opt.MainIni, 'QS', IntToStr(i), ''));
    if FQS <> '' then
      begin
      i:= Items.IndexOf(FQS);
      if i >= 0 then //Dll saved in TCPM
        ItemIndex:= i
      else
        begin //Dll not saved in TCPM
        Items.Add(FQS);
        ItemIndex:= Pred(Items.Count);
        end;
      end;
    chkQS2.Enabled:= (Items.Count > 0);
    Listbox1.Enabled:= chkQS2.Checked;
    if Listbox1.Enabled then
      Listbox1.SetFocus;
    end;
end;

procedure TFormMain.ListBox1Click(Sender: TObject);
begin
  with Listbox1 do
  if ItemIndex >= 0 then
    begin
    FQS:= Items[ItemIndex];
    SaveQS;
    LoadQS;
    end;
end;

procedure TFormMain.chkQS1Click(Sender: TObject);
begin
  FQS:= '';
  SaveQS;
  LoadQS;
end;

procedure TFormMain.chkQS2Click(Sender: TObject);
begin
  with Listbox1 do
    begin
    Enabled:= Items.Count > 0;
    if Enabled then ItemIndex:= 0;
    Listbox1Click(Self);
    end;
end;

initialization
  //Options are loaded here because we need them set up even without
  //creating main form: when app is run with parameters.
  LoadOptions;
  CoInitialize(nil);

finalization
  CoUninitialize;

end.
