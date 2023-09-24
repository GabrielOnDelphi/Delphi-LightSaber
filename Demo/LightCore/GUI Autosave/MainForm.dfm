object frmTester: TfrmTester
  Left = 785
  Top = 327
  Anchors = []
  Caption = 'Cubic Core Functions'
  ClientHeight = 489
  ClientWidth = 347
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 90
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 4
  OnClick = FormClick
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 232
    Width = 341
    Height = 118
    Margins.Bottom = 9
    Align = alBottom
    Caption = 'Log'
    TabOrder = 0
    object Button4: TButton
      Left = 32
      Top = 31
      Width = 127
      Height = 25
      Margins.Right = 1
      Caption = 'Send error msg'
      TabOrder = 0
      OnClick = Button4Click
    end
    object Button3: TButton
      Left = 182
      Top = 31
      Width = 127
      Height = 25
      Caption = 'Send verbose msg'
      TabOrder = 1
      OnClick = Button3Click
    end
    object btnShowLog: TButton
      AlignWithMargins = True
      Left = 24
      Top = 82
      Width = 293
      Height = 31
      Margins.Left = 22
      Margins.Top = 9
      Margins.Right = 22
      Align = alBottom
      Caption = 'Show Log'
      TabOrder = 2
      OnClick = btnShowLogClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 359
    Width = 347
    Height = 130
    Align = alBottom
    Caption = 'INI file'
    TabOrder = 1
    object btnSave: TButton
      AlignWithMargins = True
      Left = 5
      Top = 63
      Width = 337
      Height = 29
      Margins.Bottom = 2
      Action = actSaveGUI
      Align = alBottom
      TabOrder = 0
    end
    object btnLoad: TButton
      AlignWithMargins = True
      Left = 5
      Top = 96
      Width = 337
      Height = 29
      Margins.Top = 2
      Action = actLoadGUI
      Align = alBottom
      TabOrder = 1
    end
    object CheckBox1: TCheckBox
      AlignWithMargins = True
      Left = 9
      Top = 35
      Width = 148
      Height = 17
      Action = actAutoSave
      State = cbChecked
      TabOrder = 2
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 347
    Height = 41
    Align = alTop
    TabOrder = 2
    DesignSize = (
      347
      41)
    object Button5: TButton
      Left = 41
      Top = 4
      Width = 96
      Height = 35
      Caption = 'Show second form'
      TabOrder = 0
      WordWrap = True
      OnClick = Button5Click
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 221
      Top = 4
      Width = 104
      Height = 35
      Anchors = [akTop, akRight]
      Caption = 'TEST FONT'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 106
    Top = 23
  end
  object MainMenu: TMainMenu
    Left = 176
    Top = 24
    object File1: TMenuItem
      Caption = 'File'
      object Autosave1: TMenuItem
        Action = actAutoSave
        AutoCheck = True
      end
    end
  end
  object ActionList: TActionList
    Left = 34
    Top = 23
    object actAutoSave: TAction
      AutoCheck = True
      Caption = 'Auto save GUI to INI'
      Checked = True
      ShortCut = 16449
      OnExecute = actAutoSaveExecute
    end
    object actSaveGUI: TAction
      Caption = 'Save GUI to INI'
      OnExecute = actSaveGUIExecute
    end
    object actLoadGUI: TAction
      Caption = 'Load GUI from INI'
      OnExecute = actLoadGUIExecute
    end
  end
end
