object MainForm: TMainForm
  Left = 891
  Top = 327
  AlphaBlendValue = 249
  Caption = 'Enviroment'
  ClientHeight = 371
  ClientWidth = 684
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 350
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  GlassFrame.Enabled = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 5
  TextHeight = 17
  object pgCtrl: TPageControl
    Left = 0
    Top = 0
    Width = 684
    Height = 371
    ActivePage = tabSecondary
    Align = alClient
    TabOrder = 0
    object tabMain: TTabSheet
      Caption = 'IP'
      object mmoIP: TMemo
        AlignWithMargins = True
        Left = 194
        Top = 3
        Width = 479
        Height = 333
        Align = alClient
        TabOrder = 1
      end
      object pnlRight: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 185
        Height = 333
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object btnIntConnection: TButton
          AlignWithMargins = True
          Left = 3
          Top = 43
          Width = 179
          Height = 34
          Align = alTop
          Caption = 'Test Internet Connection'
          TabOrder = 0
          OnClick = btnIntConnectionClick
        end
        object btnGetExtIp: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 179
          Height = 34
          Align = alTop
          Caption = 'GetExternalIP'
          TabOrder = 1
          OnClick = btnGetExtIpClick
        end
      end
    end
    object tabSecondary: TTabSheet
      Caption = 'Download (RTL)'
      ImageIndex = 1
      object mmoDown2: TMemo
        AlignWithMargins = True
        Left = 204
        Top = 3
        Width = 469
        Height = 333
        Align = alClient
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 201
        Height = 339
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        object btnDownloadFileRTL: TButton
          AlignWithMargins = True
          Left = 3
          Top = 49
          Width = 195
          Height = 40
          Align = alTop
          Caption = 'DownloadToFile'
          TabOrder = 0
          OnClick = btnDownloadFileRTLClick
        end
        object btnDownloadStreamRTL: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 195
          Height = 40
          Align = alTop
          Caption = 'DownloadToStream'
          TabOrder = 1
          OnClick = btnDownloadStreamRTLClick
        end
        object btnAsStringRTL: TButton
          AlignWithMargins = True
          Left = 3
          Top = 95
          Width = 195
          Height = 40
          Align = alTop
          Caption = 'DownloadAsString'
          TabOrder = 2
          OnClick = btnAsStringRTLClick
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Download (WinInet)'
      ImageIndex = 2
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 201
        Height = 339
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object Button1: TButton
          AlignWithMargins = True
          Left = 3
          Top = 95
          Width = 195
          Height = 40
          Align = alTop
          Caption = 'DownloadAsString'
          TabOrder = 0
          OnClick = Button1Click
        end
        object btnDownload2: TButton
          AlignWithMargins = True
          Left = 3
          Top = 49
          Width = 195
          Height = 40
          Align = alTop
          Caption = 'DownloadToFile'
          TabOrder = 1
          OnClick = btnDownload2Click
        end
        object btnDownload1: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 195
          Height = 40
          Align = alTop
          Caption = 'DownloadBytes'
          TabOrder = 2
          WordWrap = True
          OnClick = btnDownload1Click
        end
        object btnDownloadThreaded: TButton
          AlignWithMargins = True
          Left = 3
          Top = 296
          Width = 195
          Height = 40
          Align = alBottom
          Caption = 'ASync download'
          TabOrder = 3
          OnClick = btnDownloadThreadedClick
        end
      end
      object mmoDown3: TMemo
        AlignWithMargins = True
        Left = 204
        Top = 3
        Width = 469
        Height = 333
        Align = alClient
        TabOrder = 1
      end
    end
  end
end
