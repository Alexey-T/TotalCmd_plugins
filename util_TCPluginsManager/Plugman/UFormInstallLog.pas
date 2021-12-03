unit UFormInstallLog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus;

type
  TFormInstallLog = class(TForm)
    PanelInfo: TPanel;
    PanelErr: TPanel;
    PanelButton: TPanel;
    btnOK: TButton;
    PanelInfoIcon: TPanel;
    PanelErrIcon: TPanel;
    PanelInfoMsg: TPanel;
    PanelErrMsg: TPanel;
    ImageInfo: TImage;
    ImageErr: TImage;
    boxInfo: TListView;
    labInfo: TLabel;
    boxErr: TListView;
    labErr: TLabel;
    labRestart: TLabel;
    mnuLogInfo: TPopupMenu;
    mnuLogInfoCopy: TMenuItem;
    mnuLogErr: TPopupMenu;
    mnuLogErrCopy: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure mnuLogInfoCopyClick(Sender: TObject);
    procedure mnuLogErrCopyClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  FormInstallLog: TFormInstallLog;

implementation

uses
  ATxClipboard, MsgFile;

{$R *.DFM}

procedure TFormInstallLog.FormShow(Sender: TObject);
begin
  {$I Init_UFormInstallLog.pas}

  ImageInfo.Picture.Icon.Handle:= LoadIcon(0, IDI_ASTERISK);
  ImageErr.Picture.Icon.Handle:= LoadIcon(0, IDI_EXCLAMATION);

  if boxErr.Items.Count=0 then
    begin
    PanelErr.Visible:= false;
    Height:= Height-PanelErr.Height;
    end;

  SetForegroundWindow(Handle);
end;

procedure CopyListToClip(List: TListView; Caption: string);
var
  i: integer;
  S: string;
begin
  S:= Caption + #13#10;
  for i:= 0 to List.Items.Count-1 do
    S:= S + List.Items[i].Caption + #13#10;
  SCopyToClipboard(S);
end;

procedure TFormInstallLog.mnuLogInfoCopyClick(Sender: TObject);
begin
  CopyListToClip(boxInfo, labInfo.Caption);
end;

procedure TFormInstallLog.mnuLogErrCopyClick(Sender: TObject);
begin
  CopyListToClip(boxErr, labErr.Caption);
end;

procedure TFormInstallLog.FormResize(Sender: TObject);
begin
  PanelErr.Height:= (ClientHeight-PanelButton.Height) div 2;
  btnOK.Left:= (ClientWidth-btnOK.Width) div 2;
  boxInfo.Columns[0].Width:= boxInfo.Width - 25;
  boxErr.Columns[0].Width:= boxErr.Width - 25;
end;

end.
