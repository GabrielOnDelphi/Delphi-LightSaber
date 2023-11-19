object FrmUpdater: TFrmUpdater
  Left = 862
  Top = 277
  AlphaBlendValue = 245
  Anchors = []
  BorderIcons = [biSystemMenu]
  Caption = 'News & Updates'
  ClientHeight = 377
  ClientWidth = 682
  Color = clBtnFace
  Constraints.MinHeight = 335
  Constraints.MinWidth = 434
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Font.Quality = fqProof
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object lblConnectError: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 676
    Height = 19
    Align = alTop
    Alignment = taCenter
    BiDiMode = bdLeftToRight
    Caption = 
      'Cannot check for news! Make sure this program is not blocked by ' +
      'your firewall!'
    Color = 8421631
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Font.Quality = fqProof
    ParentBiDiMode = False
    ParentColor = False
    ParentFont = False
    Transparent = False
    Visible = False
    StyleElements = [seBorder]
  end
  object PageCtrl: TPageControl
    Left = 0
    Top = 25
    Width = 682
    Height = 352
    ActivePage = tabNews
    Align = alClient
    TabOrder = 0
    object tabNews: TTabSheet
      Caption = 'Updates'
      object lblStatus: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 668
        Height = 15
        Align = alTop
        Alignment = taCenter
        Caption = '@Status'
        Transparent = True
        Visible = False
        WordWrap = True
        StyleElements = [seBorder]
      end
      object pnlBottom: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 274
        Width = 668
        Height = 45
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object lblVersion: TLabel
          Tag = 128
          AlignWithMargins = True
          Left = 129
          Top = 3
          Width = 302
          Height = 39
          Align = alClient
          Alignment = taCenter
          AutoSize = False
          BiDiMode = bdLeftToRight
          Caption = 'You are running v0.0.0.00'
          Color = clWindow
          ParentBiDiMode = False
          ParentColor = False
          Transparent = True
          Layout = tlCenter
        end
        object btnSettings: TButton
          AlignWithMargins = True
          Left = 589
          Top = 6
          Width = 74
          Height = 33
          Margins.Left = 5
          Margins.Top = 6
          Margins.Right = 5
          Margins.Bottom = 6
          Align = alRight
          Caption = 'Settings'
          TabOrder = 1
          Visible = False
          OnClick = btnSettingsClick
        end
        object btnCheckManually: TButton
          Left = 0
          Top = 0
          Width = 126
          Height = 45
          Hint = 'Check our website for news and updates now.'
          Align = alLeft
          Caption = 'Check for news'
          TabOrder = 0
          OnClick = btnCheckManuallyClick
        end
        object Panel1: TPanel
          Left = 434
          Top = 0
          Width = 150
          Height = 45
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 2
          DesignSize = (
            150
            45)
          object inetWhatsNew: TInternetLabel
            AlignWithMargins = True
            Left = 10
            Top = 7
            Width = 86
            Height = 15
            Cursor = crHandPoint
            Hint = 
              'Release history'#13#10#13#10'Find out what are the new features added in t' +
              'his version.'
            Alignment = taCenter
            Anchors = [akLeft]
            Caption = 'Release history'
            Color = clBlue
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            Font.Quality = fqProof
            ParentColor = False
            ParentFont = False
            LinkHint = True
            Visited = False
            VisitedColor = clPurple
            NotVisitedColor = clBlue
            OverColor = clRed
          end
          object lblDownload: TInternetLabel
            AlignWithMargins = True
            Left = 10
            Top = 24
            Width = 117
            Height = 15
            Cursor = crHandPoint
            Alignment = taCenter
            Anchors = [akLeft]
            Caption = 'Download the update'
            Color = clBlue
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            Font.Quality = fqProof
            ParentColor = False
            ParentFont = False
            Link = 'http://'
            LinkHint = True
            Visited = False
            VisitedColor = clPurple
            NotVisitedColor = clBlue
            OverColor = clRed
          end
        end
      end
      object Log: TRichLog
        AlignWithMargins = True
        Left = 3
        Top = 24
        Width = 668
        Height = 211
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        Font.Quality = fqProof
        Lines.Strings = (
          'Log')
        MaxLength = 2147483632
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
        Zoom = 100
      end
      object LogVerb: TRichLogTrckbr
        AlignWithMargins = True
        Left = 3
        Top = 241
        Width = 668
        Height = 27
        Align = alBottom
        BevelOuter = bvNone
        Caption = 'LogVerb'
        ShowCaption = False
        TabOrder = 2
        Visible = False
        TrackBar.Left = 518
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
    object tabSettings: TTabSheet
      Caption = 'Settings'
    end
    object tabDemo: TTabSheet
      Caption = 'Demo'
      ImageIndex = 1
      DesignSize = (
        674
        322)
      object Panel3: TPanel
        AlignWithMargins = True
        Left = 232
        Top = 72
        Width = 205
        Height = 142
        Anchors = []
        AutoSize = True
        TabOrder = 0
        object btnIsTimeToCheck: TButton
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 197
          Height = 29
          Align = alTop
          Caption = 'IsTimeToCheckAgain'
          TabOrder = 0
          OnClick = btnIsTimeToCheckClick
        end
        object btnNewVersFound: TButton
          AlignWithMargins = True
          Left = 4
          Top = 109
          Width = 197
          Height = 29
          Align = alTop
          Caption = 'NewVersionFound'
          TabOrder = 1
          OnClick = btnNewVersFoundClick
        end
        object btnCheckDelay: TButton
          AlignWithMargins = True
          Left = 4
          Top = 74
          Width = 197
          Height = 29
          Align = alTop
          Caption = 'CheckForNewsDelay(3)'
          TabOrder = 2
          OnClick = btnCheckDelayClick
        end
        object btnCheckToday: TButton
          AlignWithMargins = True
          Left = 4
          Top = 39
          Width = 197
          Height = 29
          Align = alTop
          Caption = 'CheckForNewsToday'
          TabOrder = 3
          OnClick = btnCheckTodayClick
        end
      end
    end
    object tabRecEditor: TTabSheet
      Caption = 'Record editor'
      ImageIndex = 2
      DesignSize = (
        674
        322)
      object Panel2: TPanel
        AlignWithMargins = True
        Left = 212
        Top = 125
        Width = 246
        Height = 72
        Anchors = []
        TabOrder = 0
        object btnBinFile: TButton
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 238
          Height = 29
          Align = alTop
          Caption = 'Create BIN file'
          TabOrder = 0
          OnClick = btnBinFileClick
        end
        object Button1: TButton
          AlignWithMargins = True
          Left = 4
          Top = 39
          Width = 238
          Height = 29
          Align = alTop
          Caption = 'Show Updater Settings'
          TabOrder = 1
          OnClick = btnSettingsClick
        end
      end
    end
  end
end
