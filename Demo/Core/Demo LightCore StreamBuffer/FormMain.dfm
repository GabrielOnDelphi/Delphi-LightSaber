object MainForm: TMainForm
  Left = 138
  Top = 275
  AlphaBlendValue = 249
  Caption = 'Enviroment'
  ClientHeight = 594
  ClientWidth = 717
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 600
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Calibri'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  SnapBuffer = 5
  Visible = True
  TextHeight = 15
  object InternetLabel: TInternetLabel
    Left = 610
    Top = 310
    Width = 28
    Height = 13
    Cursor = crHandPoint
    Caption = 'www'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Link = 'http://www.tahionic.com'
    LinkHint = False
    Visited = False
    VisitedColor = clPurple
    NotVisitedColor = clBlue
    OverColor = clRed
  end
  object lblTestSpeed: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 711
    Height = 20
    Align = alTop
    Alignment = taCenter
    Caption = 'Tests the speed of different stream classes'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitWidth = 305
  end
  object pgCtrl: TPageControl
    Left = 0
    Top = 26
    Width = 717
    Height = 568
    ActivePage = TabSheet1
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ExplicitTop = 0
    ExplicitHeight = 575
    object tabMain: TTabSheet
      Caption = 'TBufferedFileStream'
      DesignSize = (
        709
        538)
      object Label1: TLabel
        Left = 527
        Top = 103
        Width = 163
        Height = 15
        Anchors = [akTop]
        Caption = 'Max line length in File2: 2KB !'
        Color = 16744703
        ParentColor = False
        Transparent = False
      end
      object Label2: TLabel
        Left = 3
        Top = 19
        Width = 108
        Height = 75
        Caption = 
          'TODO:'#13#10'FastMM: Off'#13#10'Compiler optimization: On'#13#10'Compile for: 64 b' +
          'its'
        Visible = False
        WordWrap = True
      end
      object edtFile2: TCubicPathEdit
        Left = 130
        Top = 124
        Width = 449
        Height = 44
        Path = 'c:\TestFile'
        InputType = itFile
        Anchors = [akTop]
        Caption = 'File'
        TabOrder = 0
      end
      object spnCacheSize: TCubicSpinEdit
        Left = 135
        Top = 376
        Width = 70
        Height = 24
        Hint = 'Buffer size (KB)'
        MaxValue = 125
        MinValue = 0
        TabOrder = 1
        Value = 1000
      end
      object GroupBox2: TGroupBox
        Left = 130
        Top = 178
        Width = 449
        Height = 170
        Caption = 'System.Classes.TBufferedFileStream'
        TabOrder = 2
        object btnReadChar2: TButton
          AlignWithMargins = True
          Left = 5
          Top = 102
          Width = 439
          Height = 63
          Hint = 'Linear'#13#10'System.Classes.TBufferedFileStream'
          Align = alBottom
          Caption = 'Read Char'
          TabOrder = 0
          OnClick = btnReadChar2Click
          ExplicitTop = 140
          ExplicitWidth = 206
        end
        object btnNewDelphiStream: TButton
          AlignWithMargins = True
          Left = 5
          Top = 33
          Width = 439
          Height = 63
          Hint = 'System.Classes.TBufferedFileStream'
          Align = alBottom
          Caption = 'Read random'
          TabOrder = 1
          OnClick = btnNewDelphiStreamClick
          ExplicitTop = 71
          ExplicitWidth = 206
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'LightStream'
      ImageIndex = 2
      object GroupBox1: TGroupBox
        Left = 182
        Top = 136
        Width = 347
        Height = 177
        Caption = 'LightStream'
        TabOrder = 0
        object btnStreamWrite: TButton
          AlignWithMargins = True
          Left = 5
          Top = 40
          Width = 337
          Height = 63
          Align = alBottom
          Caption = 'StreamBuff Write'
          TabOrder = 0
          OnClick = btnStreamWriteClick
          ExplicitTop = 408
          ExplicitWidth = 206
        end
        object btnStreamRead: TButton
          AlignWithMargins = True
          Left = 5
          Top = 109
          Width = 337
          Height = 63
          Align = alBottom
          Caption = 'StreamBuff Read'
          TabOrder = 1
          OnClick = btnStreamReadClick
          ExplicitTop = 477
          ExplicitWidth = 206
        end
      end
    end
    object tabLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 4
      object pnlBottom: TPanel
        Left = 0
        Top = 493
        Width = 709
        Height = 45
        Align = alBottom
        TabOrder = 0
        ExplicitTop = 500
        DesignSize = (
          709
          45)
        object chkAutoOpen: TCubicCheckBox
          Left = 334
          Top = 17
          Width = 113
          Height = 13
          Hint = 'Show Log window whenever BioniX encounters errors'
          Anchors = [akLeft]
          Caption = 'Auto open on error'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 0
          AutoSize = True
        end
        object btnClear: TButton
          Left = 6
          Top = 10
          Width = 73
          Height = 27
          Hint = 'Clear log'
          Anchors = [akLeft]
          Caption = 'Clear'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btnClearClick
        end
        object RichLogTrckbr1: TRichLogTrckbr
          AlignWithMargins = True
          Left = 91
          Top = 11
          Width = 260
          Height = 27
          Anchors = [akLeft]
          BevelOuter = bvNone
          ShowCaption = False
          TabOrder = 2
          TrackBar.Left = 110
          TrackBar.Top = 0
          TrackBar.Width = 150
          TrackBar.Height = 27
          TrackBar.Hint = 'Hide all messages below this level'
          TrackBar.Align = alRight
          TrackBar.Max = 5
          TrackBar.TabOrder = 0
          Verbosity = lvrVerbose
          Log = Log
        end
      end
      object Log: TRichLog
        Left = 0
        Top = 0
        Width = 709
        Height = 493
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Lines.Strings = (
          'Log')
        MaxLength = 2147483632
        ParentFont = False
        ParentShowHint = False
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 1
        WordWrap = False
        Verbosity = lvrVerbose
        OnError = SwitchToLog
        OnWarn = SwitchToLog
        ExplicitHeight = 500
      end
    end
  end
end
