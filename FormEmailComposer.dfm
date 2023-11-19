object frmComposer: TfrmComposer
  Left = 18
  Top = 400
  AlphaBlend = True
  AlphaBlendValue = 250
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSizeToolWin
  Caption = 'Email composer'
  ClientHeight = 384
  ClientWidth = 676
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object splitComposer: TCubicSplitter
    Left = 176
    Top = 0
    Height = 341
    ResizeStyle = rsUpdate
  end
  object pnlBottom: TPanel
    AlignWithMargins = True
    Left = 182
    Top = 3
    Width = 491
    Height = 335
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      491
      335)
    object lblWarn: TLabel
      Left = 0
      Top = 0
      Width = 491
      Height = 16
      Hint = 
        'Things to check:'#13#10'  * personal antivirus/firewall'#13#10'  * MS Window' +
        's firewall'#13#10'  * router settings (especially the email port)'#13#10'  *' +
        ' ISP email policy'#13#10'  * ISP firewall'#13#10'  * email server settings (' +
        'address, password, port)'
      Align = alTop
      Alignment = taCenter
      Caption = 'Please make sure the application is not blocked by your firewall'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
      StyleElements = [seClient, seBorder]
    end
    object lblInfoAttachment: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 272
      Width = 485
      Height = 13
      Align = alBottom
      Caption = 
        'Note: The program will automatically attach the wallpaper to the' +
        ' email.'
      Visible = False
    end
    object mmoEmailBody: TMemo
      AlignWithMargins = True
      Left = 9
      Top = 72
      Width = 473
      Height = 194
      Hint = 'Email body'
      Margins.Left = 9
      Margins.Top = 9
      Margins.Right = 9
      Align = alClient
      Color = 15988211
      DoubleBuffered = True
      Lines.Strings = (
        'Hello')
      ParentDoubleBuffered = False
      ParentShowHint = False
      ScrollBars = ssBoth
      ShowHint = True
      TabOrder = 1
    end
    object btnAttachment: TBitBtn
      Left = 447
      Top = 73
      Width = 32
      Height = 22
      Hint = 'Browse'
      Anchors = [akTop, akRight]
      Caption = '...'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Visible = False
    end
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 19
      Width = 485
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 2
      DesignSize = (
        485
        41)
      object edtSubject: TLabeledEdit
        Left = 6
        Top = 16
        Width = 473
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Color = 15988211
        DoubleBuffered = True
        EditLabel.Width = 36
        EditLabel.Height = 13
        EditLabel.Caption = 'Subject'
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
    object edtAttachment: TCubicPathEdit
      AlignWithMargins = True
      Left = 9
      Top = 291
      Width = 473
      Height = 41
      Margins.Left = 9
      Margins.Right = 9
      InputType = itFile
      ShowCreateBtn = False
      Align = alBottom
      Caption = 'File'
      TabOrder = 3
    end
  end
  object pnlBottomToolBar: TPanel
    AlignWithMargins = True
    Left = 2
    Top = 343
    Width = 672
    Height = 39
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 1
    object btnServSett: TButton
      AlignWithMargins = True
      Left = 560
      Top = 5
      Width = 105
      Height = 25
      Margins.Top = 5
      Margins.Bottom = 5
      Align = alRight
      Caption = 'Server settings'
      TabOrder = 0
      OnClick = btnServSettClick
    end
    object btnSaveIni: TButton
      AlignWithMargins = True
      Left = 501
      Top = 5
      Width = 53
      Height = 25
      Hint = 
        'Save current settings.'#13#10'The settings are saved anyway automatica' +
        'lly when you close the program.'
      Margins.Top = 5
      Margins.Bottom = 5
      Align = alRight
      Caption = 'Save'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnSaveIniClick
    end
    object btnSendMail: TBitBtn
      AlignWithMargins = True
      Left = 3
      Top = 1
      Width = 122
      Height = 33
      Margins.Top = 1
      Margins.Bottom = 1
      Align = alLeft
      Caption = 'Send email'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
        000037777777777777770FFFFFFFFFFFFFF07F3333FFF33333370FFFF777FFFF
        FFF07F333777333333370FFFFFFFFFFFFFF07F3333FFFFFF33370FFFF7777BBF
        FFF07F333777777F3FF70FFFFFFFB9BF1CC07F3FFF337F7377770F777FFFB99B
        C1C07F7773337F377F370FFFFFFFB9BBC1C07FFFFFFF7F337FF700000077B999
        B000777777777F33777733337377B9999B33333F733373F337FF3377377B99BB
        9BB33377F337F377377F3737377B9B79B9B737F73337F7F7F37F33733777BB7B
        BBB73373333377F37F3737333777BB777B9B3733333377F337F7333333777B77
        77BB3333333337333377333333333777337B3333333333333337}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnSendMailClick
    end
  end
  object pnlTop: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 170
    Height = 335
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      170
      335)
    object ledFrom: TLabeledEdit
      Left = 10
      Top = 28
      Width = 153
      Height = 21
      Hint = 'Your email address'
      Anchors = [akLeft, akTop, akRight]
      Color = 15726069
      DoubleBuffered = True
      EditLabel.Width = 24
      EditLabel.Height = 13
      EditLabel.Caption = 'From'
      ParentDoubleBuffered = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object edtTo: TLabeledEdit
      Left = 10
      Top = 76
      Width = 153
      Height = 21
      Hint = 
        'Your friend'#39's email.'#13#10'You can add more email addresses, comma se' +
        'parated.'
      Anchors = [akLeft, akTop, akRight]
      Color = 15726069
      DoubleBuffered = True
      EditLabel.Width = 12
      EditLabel.Height = 13
      EditLabel.Caption = 'To'
      ParentDoubleBuffered = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object edtBcc: TLabeledEdit
      Left = 10
      Top = 219
      Width = 153
      Height = 21
      Hint = 'You can add more friends here, comma separated'
      Anchors = [akLeft, akTop, akRight]
      Color = 15726069
      DoubleBuffered = True
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'BCC'
      ParentDoubleBuffered = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Visible = False
    end
    object chkSendAsHtml: TCubicCheckBox
      AlignWithMargins = True
      Left = 9
      Top = 315
      Width = 158
      Height = 17
      Hint = 
        'Check it if you want to send HTML formated emails (if you have H' +
        'TML code in the '#39'Email body'#39').'
      Margins.Left = 9
      Align = alBottom
      Caption = 'Send as HTML'
      TabOrder = 3
      AutoSize = True
    end
    object chkInternalSMTP: TCubicCheckBox
      AlignWithMargins = True
      Left = 9
      Top = 292
      Width = 158
      Height = 17
      Hint = 
        'We pewconfigured for you a Gmail account.'#13#10'However, GMail is fam' +
        'ous for blocking some messages.'#13#10'If it fails, use your own Gmail' +
        ' account or a different email server (see the '#39'Server settings'#39' ' +
        'button).'
      Margins.Left = 9
      Align = alBottom
      Caption = 'Use internal email server'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = chkInternalSMTPClick
      AutoSize = True
    end
  end
  object SMTP: TIdSMTP
    IOHandler = SSLIOHandler
    Host = 'smtp.gmail.com'
    Port = 587
    SASLMechanisms = <>
    UseTLS = utUseExplicitTLS
    Left = 374
    Top = 288
  end
  object SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL
    Destination = 'smtp.gmail.com:587'
    Host = 'smtp.gmail.com'
    MaxLineAction = maException
    Port = 587
    DefaultPort = 0
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    OnStatusInfo = SSLIOHandlerStatusInfo
    Left = 376
    Top = 228
  end
end
