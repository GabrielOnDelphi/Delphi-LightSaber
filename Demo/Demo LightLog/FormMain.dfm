object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'LightSaber Log Demo'
  ClientHeight = 437
  ClientWidth = 793
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  ShowHint = True
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 381
    Height = 431
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object RichLog: TRichLog
      Left = 0
      Top = 0
      Width = 381
      Height = 398
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'RichLog')
      MaxLength = 2147483632
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object Panel3: TPanel
      Left = 0
      Top = 398
      Width = 381
      Height = 33
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object Button1: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 69
        Height = 27
        Align = alLeft
        Caption = 'Test'
        TabOrder = 0
        OnClick = Button1Click
      end
      object RichLogTrckbr1: TRichLogTrckbr
        AlignWithMargins = True
        Left = 78
        Top = 3
        Width = 300
        Height = 27
        Align = alClient
        BevelOuter = bvNone
        Caption = 'RichLogTrckbr1'
        ShowCaption = False
        TabOrder = 1
        TrackBar.Left = 150
        TrackBar.Top = 0
        TrackBar.Width = 150
        TrackBar.Height = 27
        TrackBar.Hint = 'Hide all messages below this level'
        TrackBar.Align = alRight
        TrackBar.Max = 5
        TrackBar.Position = 2
        TrackBar.TabOrder = 0
        Verbosity = lvrInfos
        Log = RichLog
      end
    end
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 395
    Top = 3
    Width = 395
    Height = 431
    Align = alRight
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object Panel4: TPanel
      Left = 0
      Top = 398
      Width = 395
      Height = 33
      Align = alBottom
      TabOrder = 0
      object Button2: TButton
        AlignWithMargins = True
        Left = 2
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
      object LogVis: TLogVerbFilter
        AlignWithMargins = True
        Left = 160
        Top = 4
        Width = 231
        Height = 25
        Align = alClient
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 1
        TrackBar.Left = 111
        TrackBar.Top = 0
        TrackBar.Width = 120
        TrackBar.Height = 25
        TrackBar.Hint = 'Hide all messages below this level'
        TrackBar.Align = alRight
        TrackBar.Anchors = [akLeft, akTop, akRight, akBottom]
        TrackBar.Max = 6
        TrackBar.Min = 1
        TrackBar.Position = 3
        TrackBar.TabOrder = 0
        Verbosity = lvInfos
        Log = VisLog
      end
      object Button3: TButton
        AlignWithMargins = True
        Left = 62
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
        Left = 110
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
      Left = 0
      Top = 365
      Width = 395
      Height = 33
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object chkShowDate: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 81
        Height = 27
        Align = alLeft
        Caption = 'Show date'
        TabOrder = 0
        OnClick = chkShowDateClick
      end
      object chkShowTime: TCheckBox
        AlignWithMargins = True
        Left = 90
        Top = 3
        Width = 87
        Height = 27
        Align = alLeft
        Caption = 'Show time'
        TabOrder = 1
        OnClick = chkShowTimeClick
      end
    end
    object VisLog: TLogGrid
      Left = 0
      Top = 0
      Width = 395
      Height = 365
      Align = alClient
      BevelOuter = bvNone
      ColCount = 1
      DefaultRowHeight = 22
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goFixedRowDefAlign]
      TabOrder = 2
      Verbosity = lvVerbose
      ColWidths = (
        391)
    end
  end
end
