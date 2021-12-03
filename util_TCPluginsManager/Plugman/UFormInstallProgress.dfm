object FormInstallProgress: TFormInstallProgress
  Left = 304
  Top = 254
  ActiveControl = btnCancel
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 87
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object labCaptionPlugins: TLabel
    Left = 8
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Installing:'
    Visible = False
  end
  object labCaptionDirs: TLabel
    Left = 8
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Installing:'
    Visible = False
  end
  object labNumbers: TLabel
    Left = 8
    Top = 48
    Width = 28
    Height = 13
    Caption = '1 / 10'
    Visible = False
  end
  object edProgress: TEdit
    Left = 8
    Top = 24
    Width = 385
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 163
    Top = 56
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
end
