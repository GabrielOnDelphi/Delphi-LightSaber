object frmRamLog: TfrmRamLog
  Left = 549
  Top = 450
  AlphaBlend = True
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Log'
  ClientHeight = 514
  ClientWidth = 718
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 17
  object Container: TPanel
    Left = 0
    Top = 0
    Width = 718
    Height = 514
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object pnlBottom: TPanel
      Left = 0
      Top = 483
      Width = 718
      Height = 31
      Align = alBottom
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object chkLogOnError: TCheckBox
        AlignWithMargins = True
        Left = 113
        Top = 3
        Width = 172
        Height = 25
        Hint = 'Show this Log window when it displays error or warning messages.'
        Align = alLeft
        Caption = 'Show the log on error'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkLogOnErrorClick
      end
      object btnClear: TButton
        AlignWithMargins = True
        Left = 647
        Top = 1
        Width = 68
        Height = 29
        Hint = 'Clear log'
        Margins.Top = 1
        Margins.Bottom = 1
        Align = alRight
        Caption = 'Clear'
        TabOrder = 1
        OnClick = btnClearClick
      end
      object trkLogVerb: TLogVisTrckbr
        AlignWithMargins = True
        Left = 291
        Top = 3
        Width = 350
        Height = 25
        Align = alClient
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 2
        TrackBar.Left = 200
        TrackBar.Top = 0
        TrackBar.Width = 150
        TrackBar.Height = 25
        TrackBar.Hint = 'Hide all messages below this level'
        TrackBar.Align = alRight
        TrackBar.Max = 6
        TrackBar.Min = 1
        TrackBar.Position = 3
        TrackBar.TabOrder = 0
        Verbosity = lvInfos
        Log = Log
      end
      object chkShowTime: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 104
        Height = 25
        Align = alLeft
        Caption = 'Show time'
        TabOrder = 3
        OnClick = chkShowTimeClick
      end
    end
    object Log: TLogGrid
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 712
      Height = 477
      Align = alClient
      BevelOuter = bvNone
      ColCount = 3
      DefaultRowHeight = 22
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goFixedRowDefAlign]
      ParentColor = True
      ParentFont = False
      TabOrder = 1
      Verbosity = lvInfos
      ColWidths = (
        691
        190
        64)
    end
  end
end
