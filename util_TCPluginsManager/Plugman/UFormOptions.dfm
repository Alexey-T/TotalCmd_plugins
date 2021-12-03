object FormOptions: TFormOptions
  Left = 277
  Top = 237
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 278
  ClientWidth = 392
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
  object btnOk: TButton
    Left = 216
    Top = 248
    Width = 82
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 304
    Top = 248
    Width = 82
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 6
    Top = 6
    Width = 381
    Height = 235
    ActivePage = TabSheet1
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'General'
      object LabelLang: TLabel
        Left = 8
        Top = 140
        Width = 51
        Height = 13
        Caption = 'Language:'
      end
      object ffRegister: TCheckBox
        Left = 8
        Top = 96
        Width = 361
        Height = 33
        Caption = 'Register extensions'
        TabOrder = 3
        WordWrap = True
      end
      object sLang: TComboBox
        Left = 80
        Top = 136
        Width = 129
        Height = 21
        Style = csDropDownList
        DropDownCount = 12
        ItemHeight = 13
        TabOrder = 4
        OnChange = sLangChange
      end
      object ffShowTypes: TCheckBox
        Left = 8
        Top = 4
        Width = 361
        Height = 17
        Caption = 'Show plugins types'
        TabOrder = 0
      end
      object boxColumns: TGroupBox
        Left = 8
        Top = 40
        Width = 353
        Height = 49
        Caption = 'Columns'
        TabOrder = 2
        object ffColDesc: TCheckBox
          Left = 8
          Top = 12
          Width = 97
          Height = 17
          Caption = 'Description'
          TabOrder = 0
        end
        object ffColAssoc: TCheckBox
          Left = 8
          Top = 28
          Width = 97
          Height = 17
          Caption = 'Association with'
          TabOrder = 3
        end
        object ffColVersion: TCheckBox
          Left = 128
          Top = 12
          Width = 97
          Height = 17
          Caption = 'Version'
          TabOrder = 1
        end
        object ffColFilename: TCheckBox
          Left = 128
          Top = 28
          Width = 97
          Height = 17
          Caption = 'Filename'
          TabOrder = 4
        end
        object ffColState: TCheckBox
          Left = 248
          Top = 12
          Width = 97
          Height = 17
          Caption = 'State'
          TabOrder = 2
        end
      end
      object ffShowNumbers: TCheckBox
        Left = 8
        Top = 20
        Width = 361
        Height = 17
        Caption = 'Show plugins order numbers'
        TabOrder = 1
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      object LabelSec: TLabel
        Left = 192
        Top = 140
        Width = 20
        Height = 13
        Caption = 'sec.'
      end
      object LabelInstalling: TLabel
        Left = 8
        Top = 4
        Width = 112
        Height = 13
        Caption = 'When installing plugins:'
      end
      object ffTimer: TCheckBox
        Left = 8
        Top = 140
        Width = 153
        Height = 17
        Caption = 'Refresh list every'
        TabOrder = 5
        OnClick = ffTimerClick
      end
      object ffTimerSec: TEdit
        Left = 160
        Top = 138
        Width = 25
        Height = 21
        TabOrder = 6
      end
      object ffUseTCVar: TCheckBox
        Left = 24
        Top = 20
        Width = 345
        Height = 17
        Caption = 'Use '#39'%COMMANDER_PATH%'#39' in paths'
        TabOrder = 0
      end
      object ffInstRestart: TCheckBox
        Left = 24
        Top = 68
        Width = 345
        Height = 17
        Caption = 'Restart TC after installing'
        TabOrder = 3
      end
      object ffInstTweakAfter: TCheckBox
        Left = 24
        Top = 52
        Width = 345
        Height = 17
        Caption = 'Tweak Packer plugins after installing'
        TabOrder = 2
      end
      object ffInstTweakBefore: TCheckBox
        Left = 24
        Top = 36
        Width = 345
        Height = 17
        Caption = 'Tweak Packer plugins before installing'
        TabOrder = 1
      end
      object btnBarAdd: TButton
        Left = 8
        Top = 102
        Width = 209
        Height = 22
        Caption = 'Button on TC toolbar...'
        TabOrder = 4
        OnClick = btnBarAddClick
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Paths'
      ImageIndex = 4
      object LabelFtp: TLabel
        Left = 8
        Top = 12
        Width = 51
        Height = 13
        Caption = 'TC FTP ini:'
      end
      object LabelRar: TLabel
        Left = 8
        Top = 52
        Width = 65
        Height = 13
        Caption = 'WinRAR.exe:'
      end
      object ffFtpIni: TEdit
        Left = 24
        Top = 28
        Width = 233
        Height = 21
        TabOrder = 0
      end
      object btnFtpBrowse: TButton
        Left = 260
        Top = 28
        Width = 33
        Height = 21
        Caption = '...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = btnFtpBrowseClick
      end
      object ffRarPath: TEdit
        Left = 24
        Top = 68
        Width = 233
        Height = 21
        TabOrder = 2
      end
      object btnRarBrowse: TButton
        Left = 260
        Top = 68
        Width = 33
        Height = 21
        Caption = '...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = btnRarBrowseClick
      end
      object ffUseEditor: TCheckBox
        Left = 8
        Top = 92
        Width = 289
        Height = 17
        Caption = 'Editor:'
        TabOrder = 4
      end
      object ffEditor: TEdit
        Left = 24
        Top = 108
        Width = 233
        Height = 21
        TabOrder = 5
      end
      object btnEditorBrowse: TButton
        Left = 260
        Top = 108
        Width = 33
        Height = 21
        Caption = '...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        OnClick = btnEditorBrowseClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TC restart'
      ImageIndex = 2
      object LabelRestartTime: TLabel
        Left = 8
        Top = 42
        Width = 63
        Height = 13
        Caption = 'Restart time:'
      end
      object ffRestart1: TRadioButton
        Left = 8
        Top = 8
        Width = 361
        Height = 17
        Caption = 'Restart last copy'
        TabOrder = 0
      end
      object ffRestart2: TRadioButton
        Left = 8
        Top = 24
        Width = 361
        Height = 17
        Caption = 'Restart all copies'
        TabOrder = 1
      end
      object ffRestartTime: TEdit
        Left = 24
        Top = 58
        Width = 37
        Height = 21
        TabOrder = 2
      end
      object ffRestartForeground: TCheckBox
        Left = 8
        Top = 82
        Width = 361
        Height = 17
        Caption = 'Start TC in foreground'
        TabOrder = 3
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Plugins folders'
      ImageIndex = 3
      object LabelLister: TLabel
        Left = 8
        Top = 25
        Width = 30
        Height = 13
        Caption = 'Lister:'
      end
      object LabelPacker: TLabel
        Left = 8
        Top = 49
        Width = 36
        Height = 13
        Caption = 'Packer:'
      end
      object LabelFS: TLabel
        Left = 8
        Top = 73
        Width = 16
        Height = 13
        Caption = 'FS:'
      end
      object LabelContent: TLabel
        Left = 8
        Top = 97
        Width = 43
        Height = 13
        Caption = 'Content:'
      end
      object LabelFolders: TLabel
        Left = 8
        Top = 4
        Width = 73
        Height = 13
        Caption = 'Plugins folders:'
      end
      object ffPluginsLister: TEdit
        Left = 108
        Top = 24
        Width = 253
        Height = 21
        TabOrder = 0
      end
      object ffPluginsPacker: TEdit
        Left = 108
        Top = 48
        Width = 253
        Height = 21
        TabOrder = 1
      end
      object ffPluginsFS: TEdit
        Left = 108
        Top = 72
        Width = 253
        Height = 21
        TabOrder = 2
      end
      object ffPluginsContent: TEdit
        Left = 108
        Top = 96
        Width = 253
        Height = 21
        TabOrder = 3
      end
    end
  end
  object OpenDialog2: TOpenDialog
    FileName = 'wcx_ftp.ini'
    Filter = '*.ini|*.ini'
    InitialDir = 'C:\'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 324
    Top = 20
  end
  object OpenDialog1: TOpenDialog
    FileName = 'WinRAR.exe'
    Filter = '*.exe|*.exe'
    InitialDir = 'C:\'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 300
    Top = 20
  end
  object OpenDialog3: TOpenDialog
    Filter = '*.exe|*.exe'
    InitialDir = 'C:\'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 348
    Top = 20
  end
end
