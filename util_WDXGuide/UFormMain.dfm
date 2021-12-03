object Form1: TForm1
  Left = 156
  Top = 129
  ActiveControl = ListView1
  AutoScroll = False
  Caption = 'WDX Guide - testing of Content plugins'
  ClientHeight = 389
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010002002020100000000000E80200002600000010101000000000002801
    00000E0300002800000020000000400000000100040000000000800200000000
    0000000000000000000000000000000000000000800000800000008080008000
    00008000800080800000C0C0C000808080000000FF0000FF000000FFFF00FF00
    0000FF00FF00FFFF0000FFFFFF00000000000000000000000000000000000000
    0000000000000888000000000000000000000000000008880000000000000888
    8888880008800878008800000000000000000007000088788000080000000666
    66660077708888888880888000000666666607777788FFFFF088778800000666
    66666077777F77777F007780000006666666660777777788887F080800000666
    66666077777770008887F880000006666666007F077008888088FF8000000666
    600007707770700088887F8800000666607777F0770F07F708087F8888800666
    607F77F0870F080F08087F8778800666607777708707088708077F8888800666
    600007F0877FF00070777F88000006666666607F0870FFFF0077F88000000666
    6666607008877000777778800000066666666607F08877777777780000000666
    66666077FF0088877077778000000666666607F777FF00000F78778800000666
    6000007770777777F77087800000066600667007000077777000080000000666
    0666770007760777006660080000066606667770777607770666660800000666
    0666777777760777066666080000066606667777777600000666660800000666
    0666777777766677766666080000066606667777777666777666600000000666
    0666777777777777766600000000000000007777777777777000000000000000
    0000000000000000000000000000FFFF07FFFFFF07FFFFEF07BF8000001F0000
    000F00000007000000070000000F0000000F0000000F00000007000000000000
    0000000000000000000000000000000000070000000F0000000F000000070000
    0003000000070000000F0000000F0000000F0000000F0000000F0000000F0000
    001F0000003F0000007FFFFFFFFF280000001000000020000000010004000000
    0000C00000000000000000000000000000000000000000000000000080000080
    000000808000800000008000800080800000C0C0C000808080000000FF0000FF
    000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000000000000000
    0000888000000000000877780000666660778887800066600778000878006660
    7780888087806660778078808780666077807780878066600778000878006666
    6077888770006666660777770000666777707770600066677770000060006667
    77766766600066677776676600006667777777600000FE0F0000FE0F0000FC03
    0000000300000000000000000000000000000000000000000000000300000003
    0000000700000007000000070000000F0000001F0000}
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    592
    389)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TGroupBox
    Left = 4
    Top = 4
    Width = 585
    Height = 61
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    DesignSize = (
      585
      61)
    object Label5: TLabel
      Left = 64
      Top = 38
      Width = 30
      Height = 13
      Alignment = taRightJustify
      Caption = 'TC ini:'
    end
    object Image1: TImage
      Left = 8
      Top = 12
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        055449636F6E0000010002002020100000000000E80200002600000010101000
        00000000280100000E0300002800000020000000400000000100040000000000
        8002000000000000000000000000000000000000000000000000800000800000
        00808000800000008000800080800000C0C0C000808080000000FF0000FF0000
        00FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000000000000000
        0000000000000000000000000888000000000000000000000000000008880000
        0000000008888888880008800878008800000000000000000007000088788000
        08000000066666660077708888888880888000000666666607777788FFFFF088
        77880000066666666077777F77777F007780000006666666660777777788887F
        08080000066666666077777770008887F880000006666666007F077008888088
        FF8000000666600007707770700088887F8800000666607777F0770F07F70808
        7F8888800666607F77F0870F080F08087F877880066660777770870708870807
        7F8888800666600007F0877FF00070777F88000006666666607F0870FFFF0077
        F880000006666666607008877000777778800000066666666607F08877777777
        78000000066666666077FF0088877077778000000666666607F777FF00000F78
        7788000006666000007770777777F77087800000066600667007000077777000
        0800000006660666770007760777006660080000066606667770777607770666
        6608000006660666777777760777066666080000066606667777777600000666
        6608000006660666777777766677766666080000066606667777777666777666
        6000000006660666777777777777766600000000000000007777777777777000
        0000000000000000000000000000000000000000FFFF07FFFFFF07FFFFEF07BF
        8000001F0000000F00000007000000070000000F0000000F0000000F00000007
        0000000000000000000000000000000000000000000000070000000F0000000F
        0000000700000003000000070000000F0000000F0000000F0000000F0000000F
        0000000F0000001F0000003F0000007FFFFFFFFF280000001000000020000000
        0100040000000000C00000000000000000000000000000000000000000000000
        000080000080000000808000800000008000800080800000C0C0C00080808000
        0000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000
        0000000000000000888000000000000877780000666660778887800066600778
        0008780066607780888087806660778078808780666077807780878066600778
        0008780066666077888770006666660777770000666777707770600066677770
        00006000666777766766600066677776676600006667777777600000FE0F0000
        FE0F0000FC030000000300000000000000000000000000000000000000000000
        0003000000030000000700000007000000070000000F0000001F0000}
    end
    object Label4: TLabel
      Left = 56
      Top = 14
      Width = 38
      Height = 13
      Alignment = taRightJustify
      Caption = 'TC exe:'
    end
    object sTcIni: TEdit
      Left = 100
      Top = 34
      Width = 389
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
    object btnBrowseIni: TButton
      Left = 494
      Top = 34
      Width = 37
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 3
      OnClick = btnBrowseIniClick
    end
    object sTCExe: TEdit
      Left = 100
      Top = 10
      Width = 389
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object btnBrowseExe: TButton
      Left = 494
      Top = 10
      Width = 37
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowseExeClick
    end
  end
  object Panel2: TGroupBox
    Left = 4
    Top = 72
    Width = 585
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    DesignSize = (
      585
      281)
    object Label2: TLabel
      Left = 52
      Top = 14
      Width = 42
      Height = 13
      Alignment = taRightJustify
      Caption = 'Test file:'
    end
    object Label3: TLabel
      Left = 104
      Top = 36
      Width = 170
      Height = 13
      Caption = 'Fields (double-click to see full text):'
    end
    object Label1: TLabel
      Left = 8
      Top = 36
      Width = 37
      Height = 13
      Caption = 'Plugins:'
    end
    object sFilename: TTntEdit
      Left = 100
      Top = 10
      Width = 389
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = 'C:\Config.sys'
    end
    object btnBrowse: TButton
      Left = 494
      Top = 10
      Width = 37
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowseClick
    end
    object Panel3: TPanel
      Left = 2
      Top = 56
      Width = 581
      Height = 223
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      Caption = 'Panel3'
      TabOrder = 2
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 581
        Height = 223
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel4'
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 97
          Top = 0
          Width = 8
          Height = 223
          Beveled = True
          ResizeStyle = rsUpdate
        end
        object ListView1: TTntListView
          Left = 105
          Top = 0
          Width = 476
          Height = 223
          Align = alClient
          Columns = <
            item
              Caption = 'Field name'
              Width = 120
            end
            item
              Caption = 'Unit'
              Width = 110
            end
            item
              Caption = 'Type'
              Width = 110
            end
            item
              Caption = 'Value'
              Width = 236
            end>
          HideSelection = False
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          PopupMenu = PopupMenu1
          TabOrder = 1
          ViewStyle = vsReport
          OnDblClick = ListView1DblClick
        end
        object ListBox1: TListBox
          Left = 0
          Top = 0
          Width = 97
          Height = 223
          Align = alLeft
          ItemHeight = 13
          TabOrder = 0
          OnClick = ListChange
        end
      end
    end
  end
  object btnClose: TButton
    Left = 504
    Top = 361
    Width = 83
    Height = 22
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object chkText: TCheckBox
    Left = 112
    Top = 360
    Width = 241
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Show fulltext fields'
    TabOrder = 2
    OnClick = chkTextClick
  end
  object OpenDialog1: TTntOpenDialog
    Filter = '*.*|*.*'
    InitialDir = 'C:\'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 496
    Top = 52
  end
  object OpenDialog2: TOpenDialog
    Filter = '*.ini|*.ini'
    InitialDir = 'C:\'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 524
    Top = 52
  end
  object OpenDialog3: TOpenDialog
    Filter = '*.exe|*.exe'
    InitialDir = 'C:\'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 552
    Top = 52
  end
  object XPManifest1: TXPManifest
    Left = 516
    Top = 96
  end
  object PopupMenu1: TPopupMenu
    Left = 492
    Top = 96
    object mnuCopy1: TMenuItem
      Caption = 'Copy field name'
      OnClick = mnuCopy1Click
    end
    object mnuCopy2: TMenuItem
      Caption = 'Copy field value'
      OnClick = mnuCopy2Click
    end
    object mnuCopy3: TMenuItem
      Caption = 'Copy field name/value'
      OnClick = mnuCopy3Click
    end
  end
end
