object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 439
  ClientWidth = 801
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 381
    Height = 439
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
    object RichLog: TRichLog
      Left = 1
      Top = 1
      Width = 379
      Height = 404
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'RichLog1')
      MaxLength = 2147483632
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      Zoom = 100
    end
    object Panel3: TPanel
      Left = 1
      Top = 405
      Width = 379
      Height = 33
      Align = alBottom
      TabOrder = 1
      object Button1: TButton
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 69
        Height = 25
        Align = alLeft
        Caption = 'Test'
        TabOrder = 0
        OnClick = Button1Click
      end
      object RichLogTrckbr1: TRichLogTrckbr
        AlignWithMargins = True
        Left = 79
        Top = 4
        Width = 296
        Height = 25
        Align = alClient
        BevelOuter = bvNone
        Caption = 'RichLogTrckbr1'
        ShowCaption = False
        TabOrder = 1
        TrackBar.Left = 146
        TrackBar.Top = 0
        TrackBar.Width = 150
        TrackBar.Height = 25
        TrackBar.Hint = 'Hide all messages below this level'
        TrackBar.Align = alRight
        TrackBar.Max = 5
        TrackBar.Position = 2
        TrackBar.TabOrder = 0
        Verbosity = lvInfos
        Log = RichLog
      end
    end
  end
  object Panel2: TPanel
    Left = 406
    Top = 0
    Width = 395
    Height = 439
    Align = alRight
    ShowCaption = False
    TabOrder = 1
    object Panel4: TPanel
      Left = 1
      Top = 405
      Width = 393
      Height = 33
      Align = alBottom
      TabOrder = 0
      object Button2: TButton
        AlignWithMargins = True
        Left = 98
        Top = 4
        Width = 58
        Height = 25
        Margins.Left = 1
        Margins.Right = 1
        Align = alLeft
        Caption = 'Test'
        TabOrder = 0
        OnClick = Button2Click
      end
      object LogVisTrckbr1: TLogVisTrckbr
        AlignWithMargins = True
        Left = 160
        Top = 4
        Width = 229
        Height = 25
        Align = alClient
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 1
        TrackBar.Left = 175
        TrackBar.Top = 0
        TrackBar.Width = 54
        TrackBar.Height = 25
        TrackBar.Hint = 'Hide all messages below this level'
        TrackBar.Align = alRight
        TrackBar.Anchors = [akLeft, akTop, akRight, akBottom]
        TrackBar.Max = 6
        TrackBar.Min = 1
        TrackBar.Position = 3
        TrackBar.TabOrder = 0
        Verbosity = lvInfos
      end
      object Button3: TButton
        AlignWithMargins = True
        Left = 50
        Top = 4
        Width = 46
        Height = 25
        Margins.Left = 1
        Margins.Right = 1
        Align = alLeft
        Caption = 'Save'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        AlignWithMargins = True
        Left = 2
        Top = 4
        Width = 46
        Height = 25
        Margins.Left = 1
        Margins.Right = 1
        Align = alLeft
        Caption = 'Load'
        TabOrder = 3
        OnClick = Button4Click
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 372
      Width = 393
      Height = 33
      Align = alBottom
      TabOrder = 1
      object CheckBox1: TCheckBox
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 73
        Height = 25
        Align = alLeft
        Caption = 'Show date'
        TabOrder = 0
        OnClick = CheckBox1Click
      end
      object CheckBox2: TCheckBox
        AlignWithMargins = True
        Left = 83
        Top = 4
        Width = 73
        Height = 25
        Align = alLeft
        Caption = 'Show time'
        TabOrder = 1
        OnClick = CheckBox2Click
      end
    end
    object VisLog: TVisLog
      Left = 1
      Top = 1
      Width = 393
      Height = 371
      Align = alClient
      BevelOuter = bvNone
      Caption = 'VisLog'
      ShowCaption = False
      TabOrder = 2
      Verbosity = lvVerbose
    end
  end
end
