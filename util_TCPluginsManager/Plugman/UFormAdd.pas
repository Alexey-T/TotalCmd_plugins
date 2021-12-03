unit UFormAdd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, tcProc;

type
  TFormAdd = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    sFile: TEdit;
    sExt: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    btnDefault: TButton;
    btnHelp: TButton;
    procedure FormShow(Sender: TObject);
    procedure sExtChange(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fLibType: TLibType;
    fLibDeleted: boolean;
  end;

implementation

uses
  Msg, MsgFile;

{$R *.DFM}

procedure TFormAdd.FormShow(Sender: TObject);
begin
  {$I Init_UFormAdd.pas}
  sExtChange(Self);
  btnDefault.Enabled:= (fLibType in [fFS, fLister, fContent]) and (not fLibDeleted);
  SetForegroundWindow(Handle);
end;

procedure TFormAdd.sExtChange(Sender: TObject);
begin
  btnOk.Enabled:=
    (Trim(sExt.Text)<>'') or
    (fLibType in [fLister, fContent]);
end;

procedure TFormAdd.btnHelpClick(Sender: TObject);
begin
  MsgInfo(S0071);
end;

procedure TFormAdd.btnDefaultClick(Sender: TObject);
var
  fn, s: string;
begin
  s:= '';
  fn:= sFile.Text;
  try
    case fLibType of
      fFS:
        s:= tcFSRootName(fn);
      fLister:
        s:= tcLSDetectString(fn);
      fContent:
        s:= tcContentDetectString(fn);
    end;
  except
  end;
  sExt.Text:= s;
  sExtChange(Self);
  ActiveControl:= sExt;
end;

end.
