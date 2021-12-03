object FormReport: TFormReport
  Left = 273
  Top = 249
  ActiveControl = sFile
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Report'
  ClientHeight = 189
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 103
    Top = 158
    Width = 77
    Height = 23
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 187
    Top = 158
    Width = 77
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Panel1: TGroupBox
    Left = 8
    Top = 4
    Width = 345
    Height = 145
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 220
      Height = 13
      Caption = 'You can save current plugins list to a text file.'
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 49
      Height = 13
      Caption = 'File name:'
    end
    object Label3: TLabel
      Left = 8
      Top = 54
      Width = 44
      Height = 13
      Caption = 'Columns:'
    end
    object sFile: TEdit
      Left = 72
      Top = 30
      Width = 225
      Height = 21
      TabOrder = 0
    end
    object fName: TCheckBox
      Left = 72
      Top = 54
      Width = 265
      Height = 17
      Caption = 'Name'
      TabOrder = 2
    end
    object fVersion: TCheckBox
      Left = 72
      Top = 70
      Width = 265
      Height = 17
      Caption = 'Version'
      TabOrder = 3
    end
    object fState: TCheckBox
      Left = 72
      Top = 86
      Width = 265
      Height = 17
      Caption = 'State'
      TabOrder = 4
    end
    object fAssoc: TCheckBox
      Left = 72
      Top = 102
      Width = 265
      Height = 17
      Caption = 'Registered for'
      TabOrder = 5
    end
    object fFileName: TCheckBox
      Left = 72
      Top = 118
      Width = 265
      Height = 17
      Caption = 'File name'
      TabOrder = 6
    end
    object btnBrowse: TButton
      Left = 300
      Top = 30
      Width = 33
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowseClick
    end
  end
  object SaveDialog1: TSaveDialog
    Filter = '*.txt|*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 300
    Top = 60
  end
end
