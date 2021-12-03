unit unPro;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfmPro = class(TForm)
    Pro: TProgressBar;
    labPro: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmPro: TfmPro = nil;

var
  fReadCaption: string = 'Reading cache';
  fReadItemMsg: string = 'Reading item: %d / %d';

procedure ProgressN(N: integer);
procedure ProgressMax(n: integer);
procedure ProgressHide;

implementation

{$R *.dfm}

procedure TfmPro.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= false;
end;


procedure ProgressN(N: integer);
begin
  if fmPro=nil then
    fmPro:= TfmPro.Create(nil);

  if N=0 then
  begin
    fmPro.Show;
    if fmPro.CanFocus then
      fmPro.SetFocus;
  end;

  fmPro.Pro.Position:= n;
  fmPro.Caption:= fReadCaption;
  fmPro.labPro.Caption:= Format(fReadItemMsg, [n, fmPro.Pro.Max]);
  if fmPro.Visible then
    fmPro.BringToFront;
  Application.ProcessMessages;
end;

procedure ProgressMax(n: integer);
begin
  if Assigned(fmPro) then
    fmPro.Pro.Max:= n;
end;

procedure ProgressHide;
begin
  if Assigned(fmPro) then
    if fmPro.Visible then
      fmPro.Hide;
end;

initialization

finalization
  FreeAndNil(fmPro);

end.
