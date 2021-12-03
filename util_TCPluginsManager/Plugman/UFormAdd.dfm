object FormAdd: TFormAdd
  Left = 244
  Top = 211
  ActiveControl = sExt
  BorderStyle = bsDialog
  Caption = 'Plugin association'
  ClientHeight = 120
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 4
    Width = 32
    Height = 13
    Caption = 'Plugin:'
  end
  object Label2: TLabel
    Left = 8
    Top = 44
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object btnOk: TButton
    Left = 64
    Top = 88
    Width = 89
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 272
    Top = 88
    Width = 89
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object sFile: TEdit
    Left = 8
    Top = 20
    Width = 409
    Height = 21
    TabStop = False
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object sExt: TEdit
    Left = 8
    Top = 60
    Width = 369
    Height = 21
    TabOrder = 1
    OnChange = sExtChange
  end
  object btnHelp: TButton
    Left = 384
    Top = 60
    Width = 33
    Height = 21
    Caption = '?'
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object btnDefault: TButton
    Left = 160
    Top = 88
    Width = 105
    Height = 23
    Caption = 'Default'
    TabOrder = 4
    OnClick = btnDefaultClick
  end
end
