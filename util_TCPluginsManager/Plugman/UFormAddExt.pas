unit UFormAddExt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls;

type
  TFormAddExt = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    ffExt: TEdit;
    procedure ffExtChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ffDisableEmpty: boolean;
  end;

implementation

uses
  Msg, MsgFile;

{$R *.DFM}

procedure TFormAddExt.FormCreate(Sender: TObject);
begin
  {$I Init_UFormAddExt.pas}
  ffDisableEmpty:= false;
end;

procedure TFormAddExt.ffExtChange(Sender: TObject);
begin
  btnOk.Enabled:=
    ((not ffDisableEmpty) or (Trim(ffExt.Text) <> ''));
end;

procedure TFormAddExt.FormShow(Sender: TObject);
begin
  ffExtChange(Self);
end;

end.
