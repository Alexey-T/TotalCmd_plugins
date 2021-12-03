unit UFormConverting;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls;

type
  TFormConverting = class(TForm)
    Label1: TLabel;
    edFilename: TEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TFormConverting.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= false;
end;

end.
