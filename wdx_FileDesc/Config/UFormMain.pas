unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ImgList;

type
  TFormMain = class(TForm)
    GroupBox1: TGroupBox;
    ListView1: TListView;
    LabelDetect: TLabel;
    ffDetect: TEdit;
    LabelVersion: TLabel;
    ffVersion: TEdit;
    LabelCopyright: TLabel;
    ffCopyright: TEdit;
    btnConfig: TButton;
    btnClose: TButton;
    ImageList1: TImageList;
    btnInstall: TButton;
    OpenDialog1: TOpenDialog;
    btnUninstall: TButton;
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnAboutClick(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure btnUninstallClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses IniFile, SProc, DescPlugin, UFormDetect;

{$R *.DFM}

var
  fnIni: string;
  fPluginID: array[1..100] of record
    fID: integer;
    fPluginNum: integer;
  end;

const
  _idMin = 0;
  _idMax = 100;

procedure InitDescPlugins(const fnIni: string);
var
  fn, sDetect, sFormat: string;
  i, j: integer;
begin
  FreeDescPlugins;
  j:= Pred(Low(fPluginID));
  for i:= _idMin to _idMax do
    begin
    fn:= GetIniKey('Plugins', IntToStr(i), '', fnIni);
    if fn<>'' then
      begin
      if Pos(':\', fn)=0 then //don't modify absolute path
        fn:= ExtractFilePath(fnIni)+'Plugins\'+fn;
      sDetect:= GetIniKey('Plugins', IntToStr(i)+'_detect', '', fnIni);
      sFormat:= GetIniKey('Plugins', IntToStr(i)+'_format', '', fnIni);
      if IsFileExist(fn) then
        begin
        InitDescPlugin(fn, sDetect, sFormat);
        if j<High(fPluginID) then
          begin
          Inc(j);
          with fPluginID[j] do
            begin fID:= i; fPluginNum:= DescPluginsNum end;
          DescPlugins[DescPluginsNum].fData:= j;
          end;
        end
      //else
      //  Application.MessageBox(PChar(Format('Cannot find plugin:'#13'%s', [fn])), PChar(fMainCaption), MB_OK or MB_ICONERROR);
      end;
    end;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  i, j: integer;
  fn1, fn2,
  fn1ex, fn2ex: string;
begin
  fMainCaption:= Caption;
  fn1:= ExtractFilePath(ParamStr(0))+'FileDesc.ini';
  fn2:= ExtractFilePath(ParamStr(0))+'Thumbs.ini';
  fn1ex:= ExtractFilePath(ParamStr(0))+'FileDesc.example.ini';
  fn2ex:= ExtractFilePath(ParamStr(0))+'Thumbs.example.ini';

  if IsFileExist(fn1ex) and not IsFileExist(fn1) then
    CopyFile(PChar(fn1ex), PChar(fn1), true);
  if IsFileExist(fn2ex) and not IsFileExist(fn2) then
    CopyFile(PChar(fn2ex), PChar(fn2), true);

  if IsFileExist(fn1) then fnIni:= fn1 else
    if IsFileExist(fn2) then fnIni:= fn2 else
      begin
      Application.MessageBox('Configuration file (*.ini) not found', PChar(Caption), MB_OK or MB_ICONERROR);
      Close;
      Exit
      end;

  InitDescPlugins(fnIni);
  ListView1.Items.Clear;
  ListView1.Items.BeginUpdate;
  for i:= 1 to DescPluginsNum do
   with DescPlugins[i] do
    with ListView1 do
      case fType of
        TypeWI:
          with fRecordWI do
            begin
            for j:= 1 to fWIPluginsNum do
              with fWIPlugins[j] do
                with Items.Add do
                  begin
                  Caption:= fName;
                  if fDetect<>''
                    then SubItems.Add(fDetect)
                    else SubItems.Add(fMask);
                  SubItems.Add(fFileName);
                  SubItems.Add(IntToStr(i));
                  SubItems.Add(IntToStr(j));
                  end;
            end;
        TypeDEP,
        TypeWDX:
          with Items.Add do
            begin
            Caption:= fName;
            SubItems.Add(fDetect);
            SubItems.Add(fFileName);
            SubItems.Add(IntToStr(i));
            SubItems.Add('0');
            end;
      end;
  ListView1.Items.EndUpdate;
  with ListView1 do
    if Items.Count>0 then
      Selected:= Items[0];
end;

procedure TFormMain.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  i: integer;
begin
  with ListView1 do
    if Selected<>nil then
      with Selected do
        begin
        ffDetect.Text:= SubItems[0];
        i:= StrToIntDef(SubItems[2], 0);
        ffVersion.Text:= DescPlugins[i].fVersion;
        ffCopyright.Text:= DescPlugins[i].fAuthor;
        end;
end;

procedure TFormMain.btnAboutClick(Sender: TObject);
var
  i: integer;
begin
  with ListView1 do
    if Selected<>nil then
      with Selected do
        begin
        i:= StrToIntDef(SubItems[2], 0);
        with DescPlugins[i] do
          if (fType<>TypeDEP) or not Assigned(fRecordDEP.fDEAbout)
            then Application.MessageBox('Plugin does not have About window', 'About', MB_OK or MB_ICONERROR)
            else fRecordDEP.fDEAbout(Handle);
        end;
end;

procedure TFormMain.btnConfigClick(Sender: TObject);
  procedure Error;
  begin
    Application.MessageBox('Plugin does not have Configure window', 'Configure', MB_OK or MB_ICONERROR);
  end;
var
  i, j: integer;
  sDetect, sFormat: string;
begin
  with ListView1 do
    if Selected<>nil then
      with Selected do
        begin
        i:= StrToIntDef(SubItems[2], 0);
        j:= StrToIntDef(SubItems[3], 0);

        with DescPlugins[i] do
          case fType of
            TypeWI:
              with fRecordWI do
                if (not Assigned(fWIPlugins[j].fConfigProc))
                  then Error
                  else fWIPlugins[j].fConfigProc(HInstance, Handle, fWIPlugins[j].fID);

            TypeDEP:
              with fRecordDEP do
                if not Assigned(fDEConfig)
                  then Error
                  else fDEConfig(Handle);

            TypeWDX:
              with TFormDetect.Create(nil) do
                try
                  Caption:= Format('%s configuration', [ExtractFileName(fFileName)]);
                  PluginNum:= i;
                  j:= DescPlugins[i].fData;
                  with fPluginID[j] do
                    begin
                    sDetect:= GetIniKey('Plugins', IntToStr(fID)+'_detect', '', fnIni);
                    sFormat:= GetIniKey('Plugins', IntToStr(fID)+'_format', '', fnIni);
                    end;
                  ffDetect.Text:= sDetect;
                  FormatStr:= sFormat;

                  if ShowModal=mrOk then
                    with fPluginID[j] do
                      begin
                      SetIniKey('Plugins', IntToStr(fID)+'_detect', ffDetect.Text, fnIni);
                      SetIniKey('Plugins', IntToStr(fID)+'_format', FormatStr, fnIni);
                      //Application.MessageBox(
                      //  PChar(Format('File saved: %s'#13'You need to restart TC to apply configuration.', [fIni])),
                      //  PChar(Caption), MB_OK or MB_ICONINFORMATION);
                      SubItems[0]:= ffDetect.Text;
                      ListView1Change(Self, ListView1.Selected, ctText);
                      end;
                finally
                  Free;
                end
          end;
        end;
end;

procedure TFormMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

function FreeIniID: integer;
var
  i: integer;
begin
  for i:= 40 to _idMax do
    if GetIniKey('Plugins', IntToStr(i), '', fnIni)='' then
      begin Result:= i; Exit end;
  Result:= -1;
end;

procedure TFormMain.btnInstallClick(Sender: TObject);
var
  n: integer;
  fn, dir: string;
begin
  dir:= ExtractFilePath(fnIni)+'Plugins';
  with OpenDialog1 do
    begin
    FileName:= '';
    InitialDir:= dir;
    if Execute then
      begin
      for n:= 1 to DescPluginsNum do
        if StrIComp(PChar(DescPlugins[n].fFileName), PChar(FileName))=0 then
          begin
          Application.MessageBox(PChar(Format('Plugin %s is already installed!', [ExtractFileName(FileName)])),
            'Install', MB_OK or MB_ICONERROR);
          Exit
          end;

      fn:= FileName;
      if Pos(UpperCase(dir), UpperCase(fn))=1 then
        Delete(fn, 1, Length(dir)+1);
      n:= FreeIniID;
      if n=-1
        then
          Application.MessageBox('No free space in ini-file!', 'Install', MB_OK or MB_ICONERROR)
        else
          begin
          SetIniKey('Plugins', IntToStr(n), fn, fnIni);
          SetIniKey('Plugins', IntToStr(n)+'_detect', '', fnIni);
          if UpperCase(ExtractFileExt(FileName))='.WDX' then
            SetIniKey('Plugins', IntToStr(n)+'_format', '', fnIni);
          FormShow(Self);
          end;
      end;
    end;

end;

procedure TFormMain.btnUninstallClick(Sender: TObject);
var
  i, j: integer;
begin
  with ListView1 do
    if Selected<>nil then
      with Selected do
        begin
        i:= StrToIntDef(SubItems[2], 0);
        j:= fPluginID[DescPlugins[i].fData].fID;
        if Application.MessageBox(
          PChar(Format('Do you want to uninstall plugin %s?', [ExtractFileName(DescPlugins[i].fFileName)])),
          'Uninstall', MB_YESNO or MB_ICONQUESTION)=IDYES then
          begin
          DelIniKey('Plugins', IntToStr(j), fnIni);
          DelIniKey('Plugins', IntToStr(j)+'_detect', fnIni);
          DelIniKey('Plugins', IntToStr(j)+'_format', fnIni);
          FormShow(Self);
          end;
        end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  GroupBox1.Width:= ClientWidth-15;
  GroupBox1.Height:= ClientHeight-38;
  btnConfig.Top:= GroupBox1.Height+6;
  btnInstall.Top:= btnConfig.Top;
  btnUninstall.Top:= btnConfig.Top;
  btnClose.Top:= btnConfig.Top;
  btnConfig.Left:= (GroupBox1.Width-4*(btnConfig.Width+6)) div 2 + 8;
  btnInstall.Left:= btnConfig.Left+btnConfig.Width+6;
  btnUninstall.Left:= btnInstall.Left+btnConfig.Width+6;
  btnClose.Left:= btnUninstall.Left+btnConfig.Width+6;
  ListView1.Width:= GroupBox1.Width-16;
  ListView1.Height:= GroupBox1.Height-97;
  LabelDetect.Top:= ListView1.Top+ListView1.Height+9;
  LabelVersion.Top:= LabelDetect.Top+24;
  LabelCopyright.Top:= LabelVersion.Top+24;
  ffDetect.Top:= LabelDetect.Top-2;
  ffVersion.Top:= LabelVersion.Top-2;
  ffCopyright.Top:= LabelCopyright.Top-2;
  ffDetect.Width:= ListView1.Width+ListView1.Left-ffDetect.Left;
end;

procedure TFormMain.FormPaint(Sender: TObject);
const
  Size = 17;
begin
  DrawFrameControl(GetDC(Handle),
    Rect(ClientWidth-Size, ClientHeight-Size, ClientWidth, ClientHeight),
    DFC_SCROLL,
    DFCS_SCROLLSIZEGRIP);
end;

procedure TFormMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  Resize:= (NewWidth>=500) and (NewHeight>=220);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  IniFilename:= ChangeFileName(ParamStr(0), 'Config.ini');
  Left:= IniFile.GetIniKey('Window', 'Left', Left);
  Top:= IniFile.GetIniKey('Window', 'Top', Top);
  Width:= IniFile.GetIniKey('Window', 'Width', Width);
  Height:= IniFile.GetIniKey('Window', 'Height', Height);
  with ListView1 do
    for i:= 0 to Columns.Count-1 do
      Columns[i].Width:= IniFile.GetIniKey('Window', Format('Column%dW', [i]), Columns[i].Width);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  IniFile.SetIniKey('Window', 'Left', Left);
  IniFile.SetIniKey('Window', 'Top', Top);
  IniFile.SetIniKey('Window', 'Width', Width);
  IniFile.SetIniKey('Window', 'Height', Height);
  with ListView1 do
    for i:= 0 to Columns.Count-1 do
      IniFile.SetIniKey('Window', Format('Column%dW', [i]), Columns[i].Width);
end;

end.
