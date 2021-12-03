program ProjectForm;

uses
  Forms,
  unOpt in 'unOpt.pas' {fmOpt},
  unPro in 'unPro.pas' {fmPro};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmOpt, fmOpt);
  Application.CreateForm(TfmPro, fmPro);
  Application.Run;
end.
