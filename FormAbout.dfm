object frmAboutApp: TfrmAboutApp
  Left = 1255
  Top = 92
  AlphaBlend = True
  AlphaBlendValue = 250
  Anchors = []
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'About'
  ClientHeight = 427
  ClientWidth = 660
  Color = clBtnFace
  CustomTitleBar.Height = 5
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Font.Quality = fqClearTypeNatural
  FormStyle = fsStayOnTop
  GlassFrame.Enabled = True
  GlassFrame.Left = 5
  GlassFrame.Top = 5
  GlassFrame.Right = 5
  GlassFrame.Bottom = 5
  GlassFrame.SheetOfGlass = True
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Container: TPanel
    AlignWithMargins = True
    Left = 15
    Top = 15
    Width = 630
    Height = 397
    Margins.Left = 15
    Margins.Top = 15
    Margins.Right = 15
    Margins.Bottom = 15
    Align = alClient
    TabOrder = 0
    DesignSize = (
      630
      397)
    object lblChildren: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 361
      Width = 622
      Height = 13
      Align = alBottom
      Alignment = taCenter
      Caption = 
        ' We have not inherited the earth from our parents, we'#39've borrowe' +
        'd it from our children'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = True
    end
    object lblVersion: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 43
      Width = 622
      Height = 13
      Align = alTop
      Alignment = taCenter
      BiDiMode = bdLeftToRight
      Caption = 'Version [Runtime]'
      ParentBiDiMode = False
      ParentShowHint = False
      ShowHint = True
      Transparent = True
    end
    object imgLogo: TImage
      AlignWithMargins = True
      Left = 4
      Top = 122
      Width = 622
      Height = 233
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
    end
    object inetHomePage: TInternetLabel
      AlignWithMargins = True
      Left = 4
      Top = 62
      Width = 622
      Height = 13
      Cursor = crHandPoint
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Home page'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Transparent = True
      LinkHint = False
      Visited = False
      VisitedColor = clPurple
      NotVisitedColor = clBlue
      OverColor = clRed
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 380
      Width = 622
      Height = 13
      Align = alBottom
      Alignment = taCenter
      Caption = 'By using this software you agree with our EULA'
    end
    object lblAppName: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 13
      Width = 622
      Height = 24
      Margins.Top = 12
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Application name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -24
      Font.Name = 'MS UI Gothic'
      Font.Style = [fsBold]
      Font.Quality = fqClearTypeNatural
      ParentFont = False
      Transparent = True
    end
    object pnlEnterKey: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 81
      Width = 622
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object btnEnterKey: TButton
        AlignWithMargins = True
        Left = 536
        Top = 3
        Width = 83
        Height = 29
        Align = alRight
        Caption = 'Enter key'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Font.Quality = fqProof
        ImageIndex = 81
        ParentFont = False
        TabOrder = 0
        OnClick = btnEnterKeyClick
      end
      object btnOrderNow: TButton
        AlignWithMargins = True
        Left = 444
        Top = 3
        Width = 86
        Height = 29
        Align = alRight
        Caption = 'Order now'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Font.Quality = fqProof
        ImageIndex = 82
        ParentFont = False
        TabOrder = 1
      end
    end
    object Panel1: TPanel
      Left = 357
      Top = 145
      Width = 267
      Height = 188
      Anchors = [akTop, akRight, akBottom]
      TabOrder = 1
      DesignSize = (
        267
        188)
      object lblExpire: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 32
        Width = 259
        Height = 13
        Margins.Top = 12
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Registration info'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Font.Quality = fqClearTypeNatural
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        Transparent = True
        Visible = False
      end
      object Label2: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 54
        Width = 259
        Height = 13
        Margins.Top = 6
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Freeware'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        Font.Quality = fqClearTypeNatural
        ParentFont = False
        Transparent = True
      end
      object lblCopyRight1: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 259
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'ctCopyright'
        ParentShowHint = False
        ShowHint = True
        Transparent = True
      end
      object inetEULA: TInternetLabel
        Left = 75
        Top = 178
        Width = 126
        Height = 14
        Cursor = crHandPoint
        Alignment = taCenter
        Anchors = [akTop]
        Caption = 'Online License Agreement'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        Transparent = True
        Visible = False
        Link = 'http://www.BionixWallpaper.com/contact/LicenseAgreement.txt'
        LinkHint = False
        Visited = False
        VisitedColor = clPurple
        NotVisitedColor = clBlue
        OverColor = clRed
      end
    end
  end
end
