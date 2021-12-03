unit UFormInstallProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMainProgressMode = (
    progressPlugins,
    progressScanDirs
    );

type
  TFormInstallProgress = class(TForm)
    labCaptionPlugins: TLabel;
    edProgress: TEdit;
    labCaptionDirs: TLabel;
    labNumbers: TLabel;
    btnCancel: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Cancelled: boolean;
    procedure SetMode(Mode: TMainProgressMode);
  end;

var
  FormInstallProgress: TFormInstallProgress;

procedure ProgressShow(MainForm: TForm; Mode: TMainProgressMode);
procedure ProgressHide(MainForm: TForm);
function ProgressActionCancelled(const Text: string; NCurrent, NTotal: integer): boolean;


implementation

uses
  MsgFile;

{$R *.DFM}

procedure TFormInstallProgress.FormShow(Sender: TObject);
begin
  {$I Init_UFormInstallProgress.pas}
  Cancelled:= false;
end;

procedure TFormInstallProgress.btnCancelClick(Sender: TObject);
begin
  Cancelled:= true;
end;

procedure TFormInstallProgress.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Cancelled:= true;
  CanClose:= false;
end;

procedure TFormInstallProgress.SetMode(Mode: TMainProgressMode);
var
  En: boolean;
begin
  En:= (Mode=progressPlugins);
  labCaptionPlugins.Visible:= En;
  labNumbers.Visible:= En;
  labCaptionDirs.Visible:= not En;
end;

//---------------------------------------------------------------
procedure ProgressShow(MainForm: TForm; Mode: TMainProgressMode);
begin
  if Assigned(MainForm) then
    MainForm.Enabled:= false;
  FormInstallProgress.Show;
  FormInstallProgress.SetMode(Mode);
end;

procedure ProgressHide(MainForm: TForm);
begin
  FormInstallProgress.Hide;
  if Assigned(MainForm) then
    MainForm.Enabled:= true;
end;

function ProgressActionCancelled(const Text: string; NCurrent, NTotal: integer): boolean;
begin
  FormInstallProgress.edProgress.Text:= Text;
  FormInstallProgress.labNumbers.Caption:= Format('%d / %d', [NCurrent, NTotal]);
  Application.ProcessMessages;
  Result:= FormInstallProgress.Cancelled;
end;


end.
