object frmRamLog: TfrmRamLog
  Left = 549
  Top = 450
  AlphaBlend = True
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Log'
  ClientHeight = 507
  ClientWidth = 643
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
  OnDestroy = FormDestroy
  TextHeight = 17
  object Container: TPanel
    Left = 0
    Top = 0
    Width = 643
    Height = 507
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object pnlBottom: TPanel
      Left = 0
      Top = 476
      Width = 643
      Height = 31
      Align = alBottom
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object chkLogOnError: TCheckBox
        AlignWithMargins = True
        Left = 204
        Top = 3
        Width = 138
        Height = 25
        Hint = 'Show this Log window when it displays error or warning messages.'
        Align = alLeft
        Caption = 'Show log on error'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkLogOnErrorClick
      end
      object btnClear: TButton
        AlignWithMargins = True
        Left = 572
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
      object trkLogVerb: TLogVerbFilter
        AlignWithMargins = True
        Left = 348
        Top = 3
        Width = 218
        Height = 25
        Align = alClient
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 2
        TrackBar.Left = 98
        TrackBar.Top = 0
        TrackBar.Width = 120
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
        Left = 103
        Top = 3
        Width = 95
        Height = 25
        Align = alLeft
        Caption = 'Show time'
        TabOrder = 3
        OnClick = chkShowTimeClick
      end
      object chkShowDate: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 94
        Height = 25
        Align = alLeft
        Caption = 'Show date'
        TabOrder = 4
        OnClick = chkShowDateClick
      end
    end
    object Log: TLogGrid
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 637
      Height = 470
      Align = alClient
      BevelOuter = bvNone
      ColCount = 1
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
      PopupMenu = PopupMenu
      TabOrder = 1
      Verbosity = lvErrors
      ColWidths = (
        633)
    end
  end
  object PopupMenu: TPopupMenu
    Left = 460
    Top = 196
    object mnuCopy: TMenuItem
      Caption = 'Copy line'
      OnClick = mnuCopyClick
    end
    object mnuCopyFiltered: TMenuItem
      Caption = 'Copy all (filtered)'
      Enabled = False
    end
    object mnuCopyAll: TMenuItem
      Caption = 'Copy all (unfiltered)'
      Hint = 'Returns all lines, even if a filter is applied.'
      OnClick = mnuCopyAllClick
    end
  end
end
