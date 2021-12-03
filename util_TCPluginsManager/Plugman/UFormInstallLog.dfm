object FormInstallLog: TFormInstallLog
  Left = 298
  Top = 168
  Width = 494
  Height = 346
  ActiveControl = btnOK
  BorderIcons = [biSystemMenu]
  Caption = 'Installation log'
  Color = clBtnFace
  Constraints.MinHeight = 220
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelInfo: TPanel
    Left = 0
    Top = 0
    Width = 486
    Height = 126
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PanelInfoIcon: TPanel
      Left = 0
      Top = 0
      Width = 50
      Height = 126
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object ImageInfo: TImage
        Left = 8
        Top = 24
        Width = 32
        Height = 32
      end
    end
    object PanelInfoMsg: TPanel
      Left = 50
      Top = 0
      Width = 436
      Height = 126
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object labInfo: TLabel
        Left = 0
        Top = 8
        Width = 24
        Height = 13
        Caption = 'Info:'
      end
      object boxInfo: TListView
        Left = 0
        Top = 24
        Width = 425
        Height = 99
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Info'
            Width = 400
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = mnuLogInfo
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object PanelErr: TPanel
    Left = 0
    Top = 126
    Width = 486
    Height = 130
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object PanelErrIcon: TPanel
      Left = 0
      Top = 0
      Width = 50
      Height = 130
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object ImageErr: TImage
        Left = 8
        Top = 24
        Width = 32
        Height = 32
      end
    end
    object PanelErrMsg: TPanel
      Left = 50
      Top = 0
      Width = 436
      Height = 130
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object labErr: TLabel
        Left = 0
        Top = 8
        Width = 33
        Height = 13
        Caption = 'Errors:'
      end
      object boxErr: TListView
        Left = 0
        Top = 24
        Width = 425
        Height = 100
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Info'
            Width = 400
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = mnuLogErr
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object PanelButton: TPanel
    Left = 0
    Top = 256
    Width = 486
    Height = 56
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object labRestart: TLabel
      Left = 50
      Top = 4
      Width = 87
      Height = 13
      Caption = 'Info about restart'
    end
    object btnOK: TButton
      Left = 206
      Top = 24
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object mnuLogInfo: TPopupMenu
    Left = 18
    Top = 175
    object mnuLogInfoCopy: TMenuItem
      Caption = 'Copy log to Clipboard'
      OnClick = mnuLogInfoCopyClick
    end
  end
  object mnuLogErr: TPopupMenu
    Left = 18
    Top = 199
    object mnuLogErrCopy: TMenuItem
      Caption = 'Copy log to Clipboard'
      OnClick = mnuLogErrCopyClick
    end
  end
end
