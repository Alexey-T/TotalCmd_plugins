object FormMemo: TFormMemo
  Left = 167
  Top = 127
  ActiveControl = Memo1
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Field value'
  ClientHeight = 288
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnKeyDown = FormKeyDown
  DesignSize = (
    400
    288)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TTntMemo
    Left = 8
    Top = 8
    Width = 385
    Height = 247
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 307
    Top = 260
    Width = 86
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = Button1Click
  end
end
