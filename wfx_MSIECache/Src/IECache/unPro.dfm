object fmPro: TfmPro
  Left = 426
  Top = 509
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Reading cache'
  ClientHeight = 69
  ClientWidth = 298
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object labPro: TLabel
    Left = 8
    Top = 12
    Width = 67
    Height = 13
    Caption = 'Reading items'
  end
  object Pro: TProgressBar
    Left = 8
    Top = 32
    Width = 281
    Height = 17
    TabOrder = 0
  end
end
