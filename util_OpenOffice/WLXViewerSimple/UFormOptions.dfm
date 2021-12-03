object FormOptions: TFormOptions
  Left = 322
  Top = 257
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 142
  ClientWidth = 296
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 64
    Top = 112
    Width = 83
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 152
    Top = 112
    Width = 83
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PanelOptions: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 97
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object LabelFont: TLabel
      Left = 104
      Top = 36
      Width = 22
      Height = 13
      Caption = 'Font'
    end
    object btnFont: TButton
      Left = 8
      Top = 32
      Width = 89
      Height = 23
      Caption = '&Font...'
      TabOrder = 1
      OnClick = btnFontClick
    end
    object ffShowMeta: TCheckBox
      Left = 8
      Top = 8
      Width = 265
      Height = 17
      Caption = '&Show document meta info'
      TabOrder = 0
    end
    object btnColor: TButton
      Left = 8
      Top = 62
      Width = 89
      Height = 23
      Caption = '&Color...'
      TabOrder = 2
      OnClick = btnColorClick
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 256
    Top = 40
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Left = 256
    Top = 72
  end
end
