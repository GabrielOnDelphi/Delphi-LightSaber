object MainForm: TMainForm
  Left = 138
  Top = 275
  AlphaBlendValue = 249
  Caption = 'Enviroment'
  ClientHeight = 595
  ClientWidth = 721
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 600
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  SnapBuffer = 5
  Visible = True
  OnClose = FormClose
  TextHeight = 13
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
  object pgCtrl: TPageControl
    Left = 0
    Top = 0
    Width = 721
    Height = 576
    ActivePage = tabMain
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object tabMain: TTabSheet
      Caption = 'Main'
      DesignSize = (
        713
        548)
      object Label1: TLabel
        Left = 154
        Top = 143
        Width = 139
        Height = 13
        Anchors = [akTop]
        Caption = 'Max line length in File2: 2KB !'
        Color = 16744703
        ParentColor = False
        Transparent = False
      end
      object Label2: TLabel
        Left = 18
        Top = 43
        Width = 118
        Height = 52
        Caption = 
          'TODO:'#13#10'FastMM: Off'#13#10'Compiler optimization: On'#13#10'Compile for: 64 b' +
          'its'
        WordWrap = True
      end
      object lblTestSpeed: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 707
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
      end
      object edtFile2: TCubicPathEdit
        Left = 126
        Top = 98
        Width = 449
        Height = 41
        Path = 'c:\TestFile'
        InputType = itFile
        Anchors = [akTop]
        Caption = 'File'
        TabOrder = 0
      end
      object edtOutput: TCubicPathEdit
        Left = 126
        Top = 187
        Width = 449
        Height = 41
        Anchors = [akTop]
        Caption = 'Folder'
        TabOrder = 1
      end
      object btnSaveIni: TButton
        Left = 627
        Top = 51
        Width = 75
        Height = 25
        Hint = 'Save settings'
        Anchors = [akTop, akRight]
        Caption = 'Save settings'
        TabOrder = 2
        OnClick = btnSaveIniClick
      end
      object Panel1: TPanel
        Left = 154
        Top = 269
        Width = 392
        Height = 212
        Anchors = [akTop]
        BevelInner = bvLowered
        TabOrder = 3
        object spnCacheSize: TCubicSpinEdit
          Left = 324
          Top = 48
          Width = 53
          Height = 22
          MaxValue = 1000
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object GroupBox1: TGroupBox
          Left = 2
          Top = 2
          Width = 121
          Height = 208
          Align = alLeft
          Caption = 'ccStreamBuffHeff.pas'
          TabOrder = 1
          object btnLines: TButton
            Left = 12
            Top = 80
            Width = 75
            Height = 25
            Caption = 'Linear lines'
            TabOrder = 0
          end
          object btnRandom: TButton
            Left = 12
            Top = 126
            Width = 75
            Height = 25
            Caption = 'Random read'
            TabOrder = 1
          end
          object btnReadChar1: TButton
            Left = 12
            Top = 38
            Width = 75
            Height = 25
            Caption = 'Read Char'
            TabOrder = 2
          end
        end
        object GroupBox2: TGroupBox
          Left = 123
          Top = 2
          Width = 185
          Height = 208
          Align = alLeft
          Caption = 'System.Classes.TBufferedFileStream'
          TabOrder = 2
          object btnReadChar2: TButton
            Left = 25
            Top = 40
            Width = 87
            Height = 25
            Hint = 'Linear'#13#10'System.Classes.TBufferedFileStream'
            Caption = 'Read Char'
            TabOrder = 0
            OnClick = btnReadChar2Click
          end
          object btnNewDelphiStream: TButton
            Left = 25
            Top = 83
            Width = 87
            Height = 25
            Hint = 'System.Classes.TBufferedFileStream'
            Caption = 'Read random'
            TabOrder = 1
            OnClick = btnNewDelphiStreamClick
          end
        end
      end
    end
    object tabStreamBuff: TTabSheet
      Caption = 'ccStreamBuff'
      ImageIndex = 3
      DesignSize = (
        713
        548)
      object lblReadWrite: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 707
        Height = 20
        Align = alTop
        Alignment = taCenter
        Caption = 'Tests the Stream.Read / Stream.Write functions'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
      end
      object btnStreamRead: TButton
        Left = 266
        Top = 281
        Width = 173
        Height = 63
        Anchors = []
        Caption = 'StreamBuff Read'
        TabOrder = 0
        OnClick = btnStreamReadClick
      end
      object btnStreamWrite: TButton
        Left = 266
        Top = 204
        Width = 173
        Height = 63
        Anchors = []
        Caption = 'StreamBuff Write'
        TabOrder = 1
        OnClick = btnStreamWriteClick
      end
    end
    object tabLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 4
      object pnlBottom: TPanel
        Left = 0
        Top = 503
        Width = 713
        Height = 45
        Align = alBottom
        TabOrder = 0
        DesignSize = (
          713
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
          TrackBar.Position = 2
          TrackBar.TabOrder = 0
          Verbosity = lvInfos
          Log = Log
        end
      end
      object Log: TRichLog
        Left = 0
        Top = 0
        Width = 713
        Height = 503
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
        OnError = SwitchToLog
        OnWarn = SwitchToLog
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'About'
      ImageIndex = 3
      object lblVers: TLabel
        Left = 376
        Top = 268
        Width = 105
        Height = 13
        Alignment = taCenter
        Caption = 'Search key in 8GB file'
      end
    end
  end
  object StatBar: TStatusBar
    Left = 0
    Top = 576
    Width = 721
    Height = 19
    Panels = <>
    ParentShowHint = False
    ShowHint = True
    SimplePanel = True
  end
end
