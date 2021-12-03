object FormConverting: TFormConverting
  Left = 321
  Top = 275
  ActiveControl = edFilename
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Converting document'
  ClientHeight = 58
  ClientWidth = 329
  Color = clBtnFace
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 168
    Height = 13
    Caption = 'Converting document, please wait:'
  end
  object edFilename: TEdit
    Left = 8
    Top = 24
    Width = 313
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
end
