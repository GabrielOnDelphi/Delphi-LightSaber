object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'LightSaber Log Demo'
  ClientHeight = 451
  ClientWidth = 836
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
    Height = 445
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object RichLog: TRichLog
      Left = 0
      Top = 0
      Width = 381
      Height = 412
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
      Top = 412
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
    Left = 390
    Top = 3
    Width = 443
    Height = 445
    Align = alRight
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object Panel4: TPanel
      Left = 0
      Top = 412
      Width = 443
      Height = 33
      Align = alBottom
      TabOrder = 0
      object btnTest2: TButton
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
        OnClick = btnTest2Click
      end
      object LogVis: TLogVerbFilter
        AlignWithMargins = True
        Left = 245
        Top = 4
        Width = 194
        Height = 25
        Align = alClient
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 1
        TrackBar.Left = 51
        TrackBar.Top = 0
        TrackBar.Width = 143
        TrackBar.Height = 25
        TrackBar.Hint = 'Hide all messages below this level'
        TrackBar.Align = alRight
        TrackBar.Anchors = [akLeft, akTop, akRight, akBottom]
        TrackBar.Max = 6
        TrackBar.Min = 1
        TrackBar.Position = 3
        TrackBar.TabOrder = 0
        Verbosity = lvInfos
        Log = GridLog
      end
      object btnSave: TButton
        AlignWithMargins = True
        Left = 147
        Top = 4
        Width = 46
        Height = 25
        Margins.Left = 1
        Margins.Right = 1
        Align = alLeft
        Caption = 'Save'
        TabOrder = 2
        OnClick = btnSaveClick
      end
      object btnLoad: TButton
        AlignWithMargins = True
        Left = 195
        Top = 4
        Width = 46
        Height = 25
        Margins.Left = 1
        Margins.Right = 1
        Align = alLeft
        Caption = 'Load'
        TabOrder = 3
        OnClick = btnLoadClick
      end
      object btnLoopTest: TButton
        AlignWithMargins = True
        Left = 62
        Top = 4
        Width = 83
        Height = 25
        Margins.Left = 1
        Margins.Right = 1
        Align = alLeft
        Caption = 'Test 1000000'
        TabOrder = 4
        OnClick = btnLoopTestClick
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 379
      Width = 443
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
      object btnClear: TButton
        AlignWithMargins = True
        Left = 396
        Top = 3
        Width = 46
        Height = 27
        Margins.Left = 1
        Margins.Right = 1
        Align = alRight
        Caption = 'Clear'
        TabOrder = 2
        OnClick = btnClearClick
      end
    end
    object GridLog: TLogGrid
      Left = 0
      Top = 0
      Width = 443
      Height = 379
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      Verbosity = lvVerbose
    end
  end
end
