unit UFormReport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Dialogs;

type
  TFormReport = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    Panel1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    sFile: TEdit;
    Label3: TLabel;
    fName: TCheckBox;
    fVersion: TCheckBox;
    fState: TCheckBox;
    fAssoc: TCheckBox;
    fFileName: TCheckBox;
    btnBrowse: TButton;
    SaveDialog1: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormReport: TFormReport;

implementation

uses
  SProc, MsgFile;

{$R *.DFM}

procedure TFormReport.FormShow(Sender: TObject);
begin
  {$I Init_UFormReport.pas}
end;

procedure TFormReport.btnOkClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

procedure TFormReport.btnBrowseClick(Sender: TObject);
begin
  with SaveDialog1 do
    begin
    FileName:= sFile.Text;
    InitialDir:= ExtractFileDir(FileName);
    if Execute then
      sFile.Text:= FileName;
    end;  
end;

end.
