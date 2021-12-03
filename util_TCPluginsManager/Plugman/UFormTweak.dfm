object FormTweak: TFormTweak
  Left = 198
  Top = 147
  ActiveControl = ffExt
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Tweak Packer plugin'
  ClientHeight = 302
  ClientWidth = 458
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
  object Panel1: TGroupBox
    Left = 8
    Top = 4
    Width = 441
    Height = 261
    TabOrder = 0
    object LabelFlags: TLabel
      Left = 12
      Top = 76
      Width = 26
      Height = 13
      Caption = '(000)'
    end
    object Label2: TLabel
      Left = 12
      Top = 16
      Width = 32
      Height = 13
      Caption = 'Plugin:'
    end
    object Label3: TLabel
      Left = 12
      Top = 42
      Width = 51
      Height = 13
      Caption = 'Extension:'
    end
    object Label4: TLabel
      Left = 12
      Top = 64
      Width = 29
      Height = 13
      Caption = 'Flags:'
    end
    object ffFile: TEdit
      Left = 80
      Top = 12
      Width = 345
      Height = 21
      AutoSize = False
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object ffExt: TComboBox
      Left = 80
      Top = 38
      Width = 73
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = ffExtChange
    end
    object ffFlag0: TCheckBox
      Left = 80
      Top = 64
      Width = 353
      Height = 17
      Caption = 'Flag0'
      TabOrder = 6
      OnClick = ffFlag0Click
    end
    object ffFlag1: TCheckBox
      Left = 80
      Top = 80
      Width = 353
      Height = 17
      Caption = 'Flag1'
      TabOrder = 7
      OnClick = ffFlag0Click
    end
    object ffFlag2: TCheckBox
      Left = 80
      Top = 96
      Width = 353
      Height = 17
      Caption = 'Flag2'
      TabOrder = 8
      OnClick = ffFlag0Click
    end
    object ffFlag3: TCheckBox
      Left = 80
      Top = 112
      Width = 353
      Height = 17
      Caption = 'Flag3'
      TabOrder = 9
      OnClick = ffFlag0Click
    end
    object ffFlag4: TCheckBox
      Left = 80
      Top = 128
      Width = 353
      Height = 17
      Caption = 'Flag4'
      TabOrder = 10
      OnClick = ffFlag0Click
    end
    object ffFlag5: TCheckBox
      Left = 80
      Top = 144
      Width = 353
      Height = 17
      Caption = 'Flag5'
      TabOrder = 11
      OnClick = ffFlag0Click
    end
    object ffFlag6: TCheckBox
      Left = 80
      Top = 160
      Width = 353
      Height = 17
      Caption = 'Flag6'
      TabOrder = 12
      OnClick = ffFlag0Click
    end
    object ffFlag7: TCheckBox
      Left = 80
      Top = 176
      Width = 353
      Height = 17
      Caption = 'Flag7'
      TabOrder = 13
      OnClick = ffFlag0Click
    end
    object ffFlag8: TCheckBox
      Left = 80
      Top = 192
      Width = 353
      Height = 17
      Caption = 'Flag8'
      TabOrder = 14
      OnClick = ffFlag0Click
    end
    object btnAdd: TButton
      Left = 248
      Top = 38
      Width = 89
      Height = 21
      Caption = 'Add new'
      TabOrder = 3
      OnClick = btnAddClick
    end
    object btnRemove: TButton
      Left = 160
      Top = 38
      Width = 89
      Height = 21
      Caption = 'Remove'
      TabOrder = 2
      OnClick = btnRemoveClick
    end
    object btnDefault: TButton
      Left = 80
      Top = 230
      Width = 105
      Height = 21
      Caption = 'Default'
      TabOrder = 16
      OnClick = btnDefaultClick
    end
    object btnChange: TButton
      Left = 337
      Top = 38
      Width = 89
      Height = 21
      Caption = 'Change'
      TabOrder = 4
      OnClick = btnChangeClick
    end
    object ffFlag9: TCheckBox
      Left = 80
      Top = 208
      Width = 353
      Height = 17
      Caption = 'Flag9'
      TabOrder = 15
      OnClick = ffFlag0Click
    end
    object btnSet: TButton
      Left = 12
      Top = 92
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 5
      OnClick = btnSetClick
    end
  end
  object btnOk: TButton
    Left = 144
    Top = 272
    Width = 82
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 232
    Top = 272
    Width = 82
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
