unit unOpt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmOpt = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    btnOk: TButton;
    btnCancel: TButton;
    ffAbout: TLabel;
    ffPluginVer: TLabel;
    ffMsieVer: TLabel;
    ffCacheFolder: TLabel;
    ffCookies: TCheckBox;
    ffConfirmOpen: TCheckBox;
    ffSkipDomains: TCheckBox;
    ffSkipList: TEdit;
    ffMode: TCheckBox;
    ffStripPar: TCheckBox;
    ffStripBr: TCheckBox;
    ffOfflineOpen: TCheckBox;
    ffOfflineStart: TCheckBox;
    ffOnline: TCheckBox;
    ffLang: TLabel;
    ffLanguage: TComboBox;
    ffCachePath: TEdit;
    procedure ffModeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmOpt: TfmOpt;

implementation

{$R *.dfm}

procedure TfmOpt.ffModeClick(Sender: TObject);
var en:Boolean;
begin
  en:= not ffMode.Checked;
  ffStripPar.Enabled:= en;
  ffStripBr.Enabled:= en;
end;

procedure TfmOpt.FormShow(Sender: TObject);
begin
  ffModeClick(Self);
end;

end.
