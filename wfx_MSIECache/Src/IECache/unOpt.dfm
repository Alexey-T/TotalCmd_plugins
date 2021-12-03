object fmOpt: TfmOpt
  Left = 389
  Top = 526
  ActiveControl = ffCookies
  BorderStyle = bsDialog
  Caption = 'MSIE Cache Browser'
  ClientHeight = 366
  ClientWidth = 401
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 385
    Height = 85
    TabOrder = 0
    object ffAbout: TLabel
      Left = 8
      Top = 12
      Width = 203
      Height = 13
      Caption = 'MSIE Cache Browser for Total Commander'
    end
    object ffPluginVer: TLabel
      Left = 8
      Top = 28
      Width = 66
      Height = 13
      Caption = 'Plugin version'
    end
    object ffMsieVer: TLabel
      Left = 8
      Top = 44
      Width = 62
      Height = 13
      Caption = 'MSIE version'
    end
    object ffCacheFolder: TLabel
      Left = 8
      Top = 60
      Width = 61
      Height = 13
      Caption = 'Cache folder'
    end
    object ffCachePath: TEdit
      Left = 112
      Top = 58
      Width = 249
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 92
    Width = 385
    Height = 165
    TabOrder = 1
    object ffLang: TLabel
      Left = 8
      Top = 16
      Width = 51
      Height = 13
      Caption = 'Language:'
    end
    object ffCookies: TCheckBox
      Left = 8
      Top = 36
      Width = 360
      Height = 17
      Caption = 'Show cookies'
      TabOrder = 1
    end
    object ffConfirmOpen: TCheckBox
      Left = 8
      Top = 52
      Width = 360
      Height = 17
      Caption = 'Confirmation of URL opening'
      TabOrder = 2
    end
    object ffSkipDomains: TCheckBox
      Left = 8
      Top = 68
      Width = 360
      Height = 17
      Caption = 'Skip 2nd-level domains:'
      TabOrder = 3
    end
    object ffSkipList: TEdit
      Left = 24
      Top = 84
      Width = 337
      Height = 21
      TabOrder = 4
    end
    object ffMode: TCheckBox
      Left = 8
      Top = 108
      Width = 360
      Height = 17
      Caption = 'Show plain URLs (note: incorrect work with long URLs)'
      TabOrder = 5
      OnClick = ffModeClick
    end
    object ffStripPar: TCheckBox
      Left = 24
      Top = 124
      Width = 350
      Height = 17
      Caption = 'Strip params after '#39'?'#39
      TabOrder = 6
    end
    object ffStripBr: TCheckBox
      Left = 24
      Top = 140
      Width = 350
      Height = 17
      Caption = 'Strip [numbers] (note: incorrect copying of URLs)'
      TabOrder = 7
    end
    object ffLanguage: TComboBox
      Left = 112
      Top = 12
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 260
    Width = 385
    Height = 69
    TabOrder = 2
    object ffOfflineOpen: TCheckBox
      Left = 8
      Top = 12
      Width = 370
      Height = 17
      Caption = 'Switch MSIE to offline mode on URL opening'
      TabOrder = 0
    end
    object ffOfflineStart: TCheckBox
      Left = 8
      Top = 28
      Width = 370
      Height = 17
      Caption = 'Switch MSIE to offline mode on plugin start'
      TabOrder = 1
    end
    object ffOnline: TCheckBox
      Left = 8
      Top = 44
      Width = 370
      Height = 17
      Caption = 'Switch MSIE on online mode on exit'
      TabOrder = 2
    end
  end
  object btnOk: TButton
    Left = 208
    Top = 336
    Width = 91
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 304
    Top = 336
    Width = 91
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
