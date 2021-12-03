unit UFormArchive;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormArchive = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    boxPlugins: TGroupBox;
    boxFile: TGroupBox;
    chkArcAll: TRadioButton;
    chkArcSel: TRadioButton;
    chkArcType: TRadioButton;
    edType: TComboBox;
    chkZip: TRadioButton;
    chkRar: TRadioButton;
    labFormat: TLabel;
    labFilename: TLabel;
    edFilename: TEdit;
    btnBrowse: TButton;
    procedure chkArcAllClick(Sender: TObject);
    procedure chkZipClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormArchive: TFormArchive;

implementation

{$R *.DFM}

procedure TFormArchive.chkArcAllClick(Sender: TObject);
begin
  edType.Enabled:= chkArcType.Checked;
end;

procedure TFormArchive.chkZipClick(Sender: TObject);
begin
  if chkZip.Checked then
    edFilename.Text:= ChangeFileExt(edFilename.Text, '.zip')
  else
    edFilename.Text:= ChangeFileExt(edFilename.Text, '.rar');
end;

procedure TFormArchive.FormShow(Sender: TObject);
begin
  chkZipClick(Self);
  chkArcAllClick(Self);
end;

procedure TFormArchive.FormCreate(Sender: TObject);
begin
  edFilename.Text:= ExtractFilePath(ParamStr(0)) + 'TC Plugman Archive.zip';
end;

end.
