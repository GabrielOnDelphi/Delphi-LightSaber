object frmRichLog: TfrmRichLog
  Left = 549
  Top = 450
  AlphaBlend = True
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Log'
  ClientHeight = 512
  ClientWidth = 710
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnDestroy = FormDestroy
  TextHeight = 15
  object Container: TPanel
    Left = 0
    Top = 0
    Width = 710
    Height = 512
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object Log: TRichLog
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 704
      Height = 472
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HideSelection = False
      Lines.Strings = (
        'Log')
      MaxLength = 2147483632
      ParentColor = True
      ParentFont = False
      PlainText = True
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
      OnError = LogError
    end
    object pnlBottom: TPanel
      Left = 0
      Top = 478
      Width = 710
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object chkAutoOpen: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 156
        Height = 28
        Hint = 'Show this Log window when it displays error or warning messages.'
        Align = alLeft
        Caption = 'Show the log on error'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object btnClear: TButton
        AlignWithMargins = True
        Left = 639
        Top = 1
        Width = 68
        Height = 32
        Hint = 'Clear log'
        Margins.Top = 1
        Margins.Bottom = 1
        Align = alRight
        Caption = 'Clear'
        TabOrder = 1
        OnClick = btnClearClick
      end
      object trkLogVerb: TRichLogTrckbr
        AlignWithMargins = True
        Left = 373
        Top = 3
        Width = 260
        Height = 28
        Align = alRight
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 2
        TrackBar.Left = 110
        TrackBar.Top = 0
        TrackBar.Width = 150
        TrackBar.Height = 28
        TrackBar.Hint = 'Hide all messages below this level'
        TrackBar.Align = alRight
        TrackBar.Max = 5
        TrackBar.Position = 2
        TrackBar.TabOrder = 0
        Verbosity = lvrInfos
        Log = Log
      end
    end
  end
end
