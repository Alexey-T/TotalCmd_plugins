object FormAddExt: TFormAddExt
  Left = 252
  Top = 353
  ActiveControl = ffExt
  BorderStyle = bsDialog
  Caption = 'Add new'
  ClientHeight = 88
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Extension:'
  end
  object btnOk: TButton
    Left = 100
    Top = 56
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 187
    Top = 56
    Width = 82
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ffExt: TEdit
    Left = 8
    Top = 26
    Width = 353
    Height = 21
    TabOrder = 0
    OnChange = ffExtChange
  end
end
