program Guide;

uses
  Forms,
  UFormMain in 'UFormMain.pas' {Form1},
  UFormMemo in 'UFormMemo.pas' {FormMemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'WDX Guide';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormMemo, FormMemo);
  Application.Run;
end.
