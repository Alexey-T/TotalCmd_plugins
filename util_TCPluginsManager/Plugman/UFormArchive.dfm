object FormArchive: TFormArchive
  Left = 282
  Top = 195
  AutoScroll = False
  Caption = 'Create plugins archive'
  ClientHeight = 276
  ClientWidth = 407
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
  object btnOK: TButton
    Left = 120
    Top = 248
    Width = 81
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 208
    Top = 248
    Width = 81
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object boxPlugins: TGroupBox
    Left = 8
    Top = 4
    Width = 393
    Height = 113
    Caption = 'What to archive'
    TabOrder = 2
    object chkArcAll: TRadioButton
      Left = 16
      Top = 20
      Width = 369
      Height = 17
      Caption = 'Archive all plugins'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = chkArcAllClick
    end
    object chkArcSel: TRadioButton
      Left = 16
      Top = 40
      Width = 369
      Height = 17
      Caption = 'Archive selected plugin(s) only'
      TabOrder = 1
      OnClick = chkArcAllClick
    end
    object chkArcType: TRadioButton
      Left = 16
      Top = 60
      Width = 369
      Height = 17
      Caption = 'Archive all plugins of type:'
      TabOrder = 2
      OnClick = chkArcAllClick
    end
    object edType: TComboBox
      Left = 32
      Top = 80
      Width = 153
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 3
    end
  end
  object boxFile: TGroupBox
    Left = 8
    Top = 120
    Width = 393
    Height = 121
    Caption = 'Archive file'
    TabOrder = 3
    object labFormat: TLabel
      Left = 16
      Top = 20
      Width = 75
      Height = 13
      Caption = 'Archive format:'
    end
    object labFilename: TLabel
      Left = 16
      Top = 72
      Width = 49
      Height = 13
      Caption = 'File name:'
    end
    object chkZip: TRadioButton
      Left = 32
      Top = 36
      Width = 57
      Height = 17
      Caption = 'ZIP'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = chkZipClick
    end
    object chkRar: TRadioButton
      Left = 32
      Top = 52
      Width = 65
      Height = 17
      Caption = 'RAR'
      TabOrder = 1
      OnClick = chkZipClick
    end
    object edFilename: TEdit
      Left = 32
      Top = 88
      Width = 289
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
    end
    object btnBrowse: TButton
      Left = 328
      Top = 88
      Width = 41
      Height = 21
      Caption = '...'
      TabOrder = 3
    end
  end
end
