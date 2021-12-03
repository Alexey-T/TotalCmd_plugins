unit UFormAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Buttons;

type
  TFormAbout = class(TForm)
    LabelHomepage: TLabel;
    LabelEmail: TLabel;
    LabelTitle: TLabel;
    LabelCopyright: TLabel;
    LabelVersion: TLabel;
    btnOk: TButton;
    Label1: TLabel;
    Image1: TImage;
    procedure LabelHomepageClick(Sender: TObject);
    procedure LabelEmailClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  SVersion = '2.2.8';
  SVersionDate = '(aug 2013)';

implementation

uses
  FProc, Msg, MsgFile, SProc;

{$R *.DFM}

function STrim(const Msg: string): string;
begin
  Result:= Msg;
  SProc.STrimRight(Result, [':']);
end;

procedure TFormAbout.LabelEmailClick(Sender: TObject);
begin
  FOpenURL('mailto:support@uvviewsoft.com?Subject=TC%20Plugins%20Manager', Handle);
end;

procedure TFormAbout.LabelHomepageClick(Sender: TObject);
begin
  FOpenURL('http://www.totalcmd.net/plugring/tc_plugman.html', Handle);
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  {$I Init_UFormAbout.pas}
end;


end.
