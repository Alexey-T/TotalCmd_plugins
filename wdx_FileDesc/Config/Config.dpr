program Config;

uses
  Forms,
  UFormMain in 'UFormMain.pas' {FormMain},
  UFormDetect in 'UFormDetect.pas' {FormDetect};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Plugins Configuration';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
