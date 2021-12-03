object FormProgress: TFormProgress
  Left = 358
  Top = 331
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Reading document'
  ClientHeight = 47
  ClientWidth = 268
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 6
    Width = 101
    Height = 13
    Caption = 'Reading document...'
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 24
    Width = 253
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 0
  end
end
