object frmAboutApp: TfrmAboutApp
  Left = 1255
  Top = 92
  AlphaBlend = True
  AlphaBlendValue = 250
  Anchors = []
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'About'
  ClientHeight = 445
  ClientWidth = 535
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
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  TextHeight = 13
  object Container: TPanel
    AlignWithMargins = True
    Left = 15
    Top = 15
    Width = 505
    Height = 415
    Margins.Left = 15
    Margins.Top = 15
    Margins.Right = 15
    Margins.Bottom = 15
    Align = alClient
    TabOrder = 0
    DesignSize = (
      505
      415)
    object lblChildren: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 401
      Width = 497
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
      Top = 40
      Width = 497
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
      Top = 75
      Width = 497
      Height = 300
      Align = alClient
      Center = True
      Stretch = True
    end
    object lblCompany: TInternetLabel
      AlignWithMargins = True
      Left = 4
      Top = 56
      Width = 497
      Height = 13
      Cursor = crHandPoint
      Align = alTop
      Alignment = taCenter
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
    object lblAppName: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 13
      Width = 497
      Height = 24
      Margins.Top = 12
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = '@Application name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -24
      Font.Name = 'MS UI Gothic'
      Font.Style = [fsBold]
      Font.Quality = fqClearTypeNatural
      ParentFont = False
      Transparent = True
    end
    object lblExpire: TLabel
      AlignWithMargins = True
      Left = 82
      Top = 357
      Width = 363
      Height = 13
      Margins.Top = 12
      Alignment = taCenter
      Anchors = [akLeft, akRight, akBottom]
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
    object inetEULA: TInternetLabel
      AlignWithMargins = True
      Left = 4
      Top = 381
      Width = 497
      Height = 14
      Cursor = crHandPoint
      Align = alBottom
      Alignment = taCenter
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
    object btnEnterKey: TButton
      AlignWithMargins = True
      Left = 403
      Top = 323
      Width = 83
      Height = 29
      Anchors = [akRight, akBottom]
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
      Left = 403
      Top = 93
      Width = 86
      Height = 29
      Anchors = [akTop, akRight]
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
      OnClick = btnOrderNowClick
    end
  end
end
