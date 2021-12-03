object FormMain: TFormMain
  Left = 199
  Top = 207
  ActiveControl = Browser1
  Caption = 'Viewer'
  ClientHeight = 340
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Browser1: TWebBrowser
    Left = 0
    Top = 0
    Width = 558
    Height = 340
    Align = alClient
    TabOrder = 0
    OnDocumentComplete = Browser1DocumentComplete
    ControlData = {
      4C000000AC390000242300000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
