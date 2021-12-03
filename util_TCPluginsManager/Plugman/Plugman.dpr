{************************************************}
{                                                }
{  Project: TC Plugins Manager                   }
{  Copyright (C) Alexey Torgashin                }
{  http://atorg.net.ru                           }
{                                                }
{************************************************}

{$BOOLEVAL OFF}
{$WRITEABLECONST OFF}
{$R Res\Resources.res}

program Plugman;

uses
  Windows,
  SysUtils,
  Forms,
  tcProc,
  UFormMain in 'UFormMain.pas' {FormMain},
  UFormAbout in 'UFormAbout.pas' {FormAbout},
  UFormOptions in 'UFormOptions.pas' {FormOptions},
  UFormAdd in 'UFormAdd.pas' {FormAdd},
  UFormOrder in 'UFormOrder.pas' {FormOrder},
  UFormTweak in 'UFormTweak.pas' {FormTweak},
  UFormAddExt in 'UFormAddExt.pas' {FormAddExt},
  UFormReport in 'UFormReport.pas' {FormReport},
  UFormInstallLog in 'UFormInstallLog.pas' {FormInstallLog},
  UFormInstallProgress in 'UFormInstallProgress.pas' {FormInstallProgress},
  UFormArchive in 'UFormArchive.pas' {FormArchive};

begin
  CheckCommandLine;
  Application.Initialize;
  Application.Title := 'TC Plugins Manager';
  Application.HintPause:= 1000;
  Application.HintHidePause:= 3500;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormOptions, FormOptions);
  Application.CreateForm(TFormInstallProgress, FormInstallProgress);
  Application.Run;
end.
