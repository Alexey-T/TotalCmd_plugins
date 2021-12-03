object FormMain: TFormMain
  Left = 199
  Top = 207
  Caption = 'Viewer'
  ClientHeight = 340
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 558
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LabelTitle: TLabel
      Left = 8
      Top = 4
      Width = 24
      Height = 13
      Caption = 'Title:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LabelSubject: TLabel
      Left = 8
      Top = 18
      Width = 40
      Height = 13
      Caption = 'Subject:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LabelDesc: TLabel
      Left = 8
      Top = 32
      Width = 57
      Height = 13
      Caption = 'Description:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object Memo1: TRichEdit
    Left = 0
    Top = 48
    Width = 558
    Height = 292
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    HideSelection = False
    ParentFont = False
    PlainText = True
    PopupMenu = PopupMenu1
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    OnKeyDown = Memo1KeyDown
  end
  object PopupMenu1: TPopupMenu
    Left = 376
    Top = 8
    object mnuCopy: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = mnuCopyClick
    end
    object mnuSelectAll: TMenuItem
      Caption = '&Select all'
      ShortCut = 16449
      OnClick = mnuSelectAllClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuOptions: TMenuItem
      Caption = '&Options...'
      OnClick = mnuOptionsClick
    end
  end
end
