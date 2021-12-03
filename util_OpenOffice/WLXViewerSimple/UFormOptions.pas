unit UFormOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFormOptions = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    PanelOptions: TPanel;
    btnFont: TButton;
    ffShowMeta: TCheckBox;
    FontDialog1: TFontDialog;
    LabelFont: TLabel;
    btnColor: TButton;
    ColorDialog1: TColorDialog;
    procedure btnFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TFormOptions.FormShow(Sender: TObject);
begin
  with FontDialog1 do
    LabelFont.Caption:= Format('%s, %d', [Font.Name, Font.Size]);
end;

procedure TFormOptions.btnFontClick(Sender: TObject);
begin
  FontDialog1.Execute;
  FormShow(Self);
end;

procedure TFormOptions.btnColorClick(Sender: TObject);
begin
  ColorDialog1.Execute;
end;

end.
