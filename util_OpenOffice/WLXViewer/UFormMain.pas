unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, SHDocVw,
  OleCtrls, IniFiles;

type
  TFormMain = class(TForm)
    Browser1: TWebBrowser;
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Browser1DocumentComplete(ASender: TObject; const pDisp: IDispatch;
      const URL: OleVariant);
  private
    TotCmdWin: HWND;
    ParentWin: HWND;
    QuickView: boolean;
    FileOut: string;
    DirTemp: string;
    procedure AppException(Sender: TObject; E: Exception);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor CreateParented(ParentWindow: HWND; const FileToView: string); reintroduce;
  end;

const
  ssCaption = 'OOoViewer';
  ssExceptionMsg = 'Exception:'#13'%s';
  ssExceptionDestroyMsg = 'Exception on close:'#13'%s';
  ssExceptionThumbnailMsg = 'Exception on reading thumbnail:'#13'"%s"';
  ssErrorCannotFindXsl = 'Cannot find XSL file:'#13'"%s"';
  ssErrorCannotFindDocxConv = 'Cannot find DOCX converter (OdfConverter.exe)';
  ssErrorCannotRunDocxConv = 'Cannot execute DOCX converter:'#13'"%s"';
  ssErrorCannotConvertDocx = 'Cannot convert DOCX document:'#13'"%s"';
  ssErrorCannotConvertXml = 'Cannot convert XML document:'#13'"%s"';
  ssErrorCannotConvertOdt = 'Cannot convert ODT document:'#13'"%s"';
  ssErrorCannotCopyToTemp = 'Cannot copy file to temporary folder:'#13'"%s"';
  ssErrorCannotUnzipFb2 = 'Cannot find FB2 in zip:'#13'"%s"';

function ShowDoc(ListerWin: HWND; const FileToLoad: string; ShowFlags: integer): HWND;
procedure HideDoc(PluginWin: HWND);

procedure MsgBox(h: HWND; const msg: string);
procedure MsgBoxErr(h: HWND; const msg: string);

type
  TPlugInfo = record
    PlugWinProc: Pointer;
    PlugForm: TFormMain;
  end;

var
  IniFile: TIniFile = nil;


implementation

uses
  SProc, FProc, WBProc, OOHtmlView, XmlProc,
  UnzipDll, UFormConverting;

{$R *.DFM}

//Special filenames - don't change
const
  fnXslOOo1 = 'sxw_html.xsl';
  fnXslOOo2 = 'odt_to_xhtml.xsl';
  fnXslFB2 = 'FB2_2_html.xsl';
  fnXslFB2b = 'FB2_2_html_basics.xsl';
  fnOdfConverter = 'OdfConverter.exe';

//Helper functions
procedure MsgBox(h: HWND; const msg: string);
begin
  MessageBox(h, PChar(msg), ssCaption, MB_OK or MB_ICONINFORMATION);
end;

procedure MsgBoxErr(h: HWND; const msg: string);
begin
  MessageBox(h, PChar(msg), ssCaption, MB_OK or MB_ICONERROR);
end;

//TFormMain

procedure TFormMain.AppException(Sender: TObject; E: Exception);
begin
  MsgBoxErr(Handle, Format(ssExceptionMsg, [E.Message]));
end;

procedure TFormMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style:= (WS_CHILD or WS_MAXIMIZE) and not WS_CAPTION and not WS_BORDER;
  Params.WindowClass.cbWndExtra:= SizeOf(Pointer); //4 bytes for address of form
end;

type
  TWinControlCracker = class(TWinControl);

constructor TFormMain.CreateParented(ParentWindow: HWND; const FileToView: string);
const
  WinCmdClassName = 'TTOTAL_CMD';
begin
  inherited CreateParented(ParentWindow);
  TotCmdWin:= FindWindow(WinCmdClassName, nil);
  ParentWin:= ParentWindow;
  QuickView:= GetParent(ParentWin) <> 0;
end;

function HookDestroy(PluginWin: HWND; Msg, wParam, lParam: LongInt): LongInt; stdcall;
var
  p: ^TPlugInfo;
begin
  p:= Pointer(GetWindowLong(PluginWin, GWL_USERDATA));
  if Msg <> WM_DESTROY
    then
      Result:= CallWindowProc(p^.PlugWinProc, PluginWin, Msg, wParam, lParam)
    else
      begin
      HideDoc(PluginWin);
      Result:= 0;
      end;
end;

procedure HideDoc(PluginWin: HWND);
var
  p: ^TPlugInfo;
begin
  p:= Pointer(GetWindowLong(PluginWin, GWL_USERDATA));
  with p^.PlugForm do
    try
      //remove form
      Application.RemoveComponent(p^.PlugForm);
      Application.Handle:= 0;
      //restore callback function
      SetWindowLong(Handle, GWL_WNDPROC, Integer(p^.PlugWinProc));
      Free;
    except
      on E: Exception do
        MsgBoxErr(Handle, Format(ssExceptionDestroyMsg, [E.Message]));
    end;
  Dispose(p);
end;

//For Novell ODF converter need to convert filenames to UTF8
function fnFix(const fnConv, fnToFix: string): string;
var
  fnDll: string;
begin
  Result:= fnToFix;
  fnDll:= FChangeFileName(fnConv, 'iconv.dll');
  if IsFileExist(fnDll) then
    Result:= UTF8Encode(Result);
end;

function ShowDoc(ListerWin: HWND; const FileToLoad: string; ShowFlags: integer): HWND;
var
  fnExtension, fnIn, fnOut, fnConv, fnXsl,
  fnTempDir, fnTemp, fnTemp1, fnTemp2: string;
  OOo2x: boolean;
  fmMain: TFormMain;
  p: ^TPlugInfo;
begin
  Result:= 0;

  fnExtension:= UpperCase(ExtractFileExt(FileToLoad));
  fnIn:= FileToLoad;
  fnOut:= '';
  fnConv:= '';
  fnXsl:= '';
  fnTemp:= '';
  fnTemp1:= '';
  fnTemp2:= '';

  if IniFile.ReadBool('Options', 'TempExtension', true) then
    //Folder with extension
    fnTempDir:= FGetTempPath(ExtractFileName(FileToLoad) + '.tmp')
  else
    //Folder without extension - preferred?
    fnTempDir:= FGetTempPath(ChangeFileExt(ExtractFileName(FileToLoad), ''));


  if (fnExtension = '.FB2Z') or
    (fnExtension = '.ZFB2') or
    (fnExtension = '.FBZ') then
    //-------------------------------- FB2 zip handling
    begin
    if not UnzipSingle(fnIn, fnTempDir, ['*.fb2']) then
      begin
      MsgBoxErr(ListerWin, Format(ssErrorCannotUnzipFb2, [FileToLoad]));
      Exit
      end;

    if not FSearchDir('*.fb2', fnTempDir, fnIn) then
      begin
      MsgBoxErr(ListerWin, Format(ssErrorCannotUnzipFb2, [FileToLoad]));
      Exit;
      end;

    fnExtension:= '.FB2';
    end;

  
  if (fnExtension = '.FB2') then
    //-------------------------------- 1) FB2 handling
    begin 
    fnTemp1:= fnTempDir + '\' + fnXslFB2;
    fnTemp2:= fnTempDir + '\' + fnXslFB2b;
    fnTemp:= fnTempDir + '\Converted.xml';

    CopyFile( PChar(FChangeFileName(FGetPluginFilename, fnXslFB2)), PChar(fnTemp1), false);
    CopyFile( PChar(FChangeFileName(FGetPluginFilename, fnXslFB2b)), PChar(fnTemp2), false);
    CopyFile( PChar(fnIn), PChar(fnTemp), false);

    if not ConvertXmlToHtml(fnTemp, fnTemp1) then
      begin
      FDeleteDir(fnTempDir);
      MsgBoxErr(ListerWin, Format(ssErrorCannotConvertXml, [FileToLoad]));
      Exit
      end;

    if IniFile.ReadBool('Options', 'ShowImages', true) then
      ExtractFB2Images(fnTemp, fnTempDir);

    fnOut:= fnTemp;
    end
  else
    //-------------------------------- 2) OOo handling
    begin
    //-------------------------------- 2a) DOCX/DOTX handling
    if (fnExtension = '.DOCX') or 
      (fnExtension = '.DOTX') then
      begin
      if not FSearchDir(fnOdfConverter, ExtractFileDir(FGetPluginFilename), fnConv) then
        begin
        MsgBoxErr(ListerWin, ssErrorCannotFindDocxConv);
        Exit;
        end;

      fnTemp:= fnTempDir + '\Converted.odt';

      with TFormConverting.CreateParented(ListerWin) do
        try
          edFilename.Text:= fnIn;
          edFilename.SelLength:= 0;
          Show;
          Update;
          if not FExecShell(
            fnConv,
            Format('/I "%s" /O "%s" /DOCX2ODT', [fnFix(fnConv, fnIn), fnFix(fnConv, fnTemp)]),
            SW_HIDE, true) then
            begin
            MsgBoxErr(ListerWin, Format(ssErrorCannotRunDocxConv, [fnConv]));
            Exit;
            end;
        finally
          Release;
        end;

      if not IsFileExist(fnTemp) then
        begin
        MsgBoxErr(ListerWin, Format(ssErrorCannotConvertDocx, [fnIn]));
        Exit;
        end;

      fnIn:= fnTemp;
      fnExtension:= '.ODT';
      end;

    //-------------------------------- 2b) ODF/SXW handling
    OOo2x:= Pos('.O', fnExtension) = 1;

    if OOo2x
      then fnXsl:= FChangeFileName(FGetPluginFilename, fnXslOOo2)
      else fnXsl:= FChangeFileName(FGetPluginFilename, fnXslOOo1);

    if not IsFileExist(fnXsl) then
      begin
      MsgBoxErr(ListerWin, Format(ssErrorCannotFindXsl, [fnXsl]));
      FDeleteDir(fnTempDir);
      Exit
      end;

    if not ConvertOdtToHtml(fnIn, fnXsl, fnTempDir, fnOut) then
      begin
      MsgBoxErr(ListerWin, Format(ssErrorCannotConvertOdt, [FileToLoad]));
      FDeleteDir(fnTempDir);
      Exit
      end;
    end;
    //------------------------------- End of OOo handling


  //Form creation
  try
    fmMain:= TFormMain.CreateParented(ListerWin, FileToLoad);
    fmMain.FileOut:= fnOut;
    fmMain.DirTemp:= fnTempDir;
    fmMain.Show;

    //synchronize our form and Lister
    Application.Handle:= ListerWin;
    Application.OnException:= fmMain.AppException;
    Application.InsertComponent(fmMain);

    //substitution callback function
    New(p);
    SetWindowLong(fmMain.Handle, GWL_USERDATA, Integer(p));
    p^.PlugForm:= fmMain;
    p^.PlugWinProc:= Pointer(SetWindowLong(fmMain.Handle, GWL_WNDPROC, Integer(@HookDestroy)));

    //set focus to our window
    if not fmMain.QuickView then
      PostMessage(fmMain.Handle, WM_SETFOCUS, 0, 0);
    Result:= fmMain.Handle;

  except
    on E: Exception do
      MsgBoxErr(ListerWin, Format(ssExceptionMsg, [E.Message]));
  end;
end;


procedure TFormMain.FormShow(Sender: TObject);
var
  Flags: OleVariant;
begin
  if FileOut <> ''
  then
    begin
    Flags:= navNoHistory + navNoReadFromCache + navNoWriteToCache;
    Browser1.Navigate('file:///' + FileOut)
    end
  else
    begin
    Browser1.Navigate('about:blank');
    end;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  WB_SetFocus(Browser1);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  //We need to wait, until webbrowser will be idle: this is needed because
  //it loads huge XML files in background thread:
  Browser1.Stop;
  repeat
    Application.ProcessMessages;
  until not Browser1.Busy;

  //Delete temporary folder:
  FDeleteDir(DirTemp);
end;

procedure TFormMain.Browser1DocumentComplete(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  if not QuickView then
    WB_SetFocus(Browser1);
end;

procedure TFormMain.WndProc(var Message: TMessage);
begin
  inherited;
  if (Message.Msg = WM_SETFOCUS) and
    (not (csDestroying in ComponentState)) then WB_SetFocus(Browser1);
end;


initialization
  IniFile:= TIniFile.Create(FChangeFileName(FGetPluginFilename, 'OOoViewer.ini'));

finalization
  IniFile.Free;

end.
