object frmSmtpSettings: TfrmSmtpSettings
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 250
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'Email server settings'
  ClientHeight = 311
  ClientWidth = 557
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    557
    311)
  TextHeight = 13
  object grpSettings: TGroupBox
    AlignWithMargins = True
    Left = 7
    Top = 5
    Width = 544
    Height = 301
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      544
      301)
    object lblPort: TLabel
      Left = 41
      Top = 197
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object inetAllowLessSecure: TInternetLabel
      Left = 2
      Top = 286
      Width = 540
      Height = 13
      Cursor = crHandPoint
      Hint = 'https://stackoverflow.com/questions/20337040'
      Align = alBottom
      Alignment = taCenter
      Caption = 
        'If you use GMail you need to enable "Allow Less Secure Apps" int' +
        'o your google account'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Link = 'https://stackoverflow.com/questions/20337040'
      LinkHint = True
      Visited = True
      VisitedColor = clPurple
      NotVisitedColor = clBlue
      OverColor = clRed
    end
    object ledHost: TLabeledEdit
      Left = 41
      Top = 169
      Width = 138
      Height = 21
      Hint = 'Could be something as:  mail.YourDomain.com'
      EditLabel.Width = 93
      EditLabel.Height = 13
      EditLabel.Caption = 'Host (SMTP server)'
      EditLabel.Font.Charset = DEFAULT_CHARSET
      EditLabel.Font.Color = clWindowText
      EditLabel.Font.Height = -11
      EditLabel.Font.Name = 'MS Sans Serif'
      EditLabel.Font.Style = []
      EditLabel.ParentFont = False
      TabOrder = 0
      Text = ''
      TextHint = 'mail.YourServer.com'
    end
    object edtPsw: TLabeledEdit
      Left = 41
      Top = 97
      Width = 138
      Height = 21
      Hint = 
        'Note: The password is stored to disk encrypted but the encryptio' +
        'n is not very strong.'
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'Password'
      EditLabel.Font.Charset = DEFAULT_CHARSET
      EditLabel.Font.Color = clWindowText
      EditLabel.Font.Height = -11
      EditLabel.Font.Name = 'MS Sans Serif'
      EditLabel.Font.Style = []
      EditLabel.ParentFont = False
      PasswordChar = '*'
      TabOrder = 1
      Text = ''
      TextHint = '**********'
    end
    object spnPort: TSpinEdit
      Left = 41
      Top = 213
      Width = 65
      Height = 22
      Hint = 'Usualy is 25 or 110. '#13#10'GMail is 587.'
      MaxValue = 65535
      MinValue = 0
      TabOrder = 2
      Value = 110
    end
    object edtUserName: TLabeledEdit
      Left = 41
      Top = 52
      Width = 138
      Height = 21
      Hint = 'in many cases it also works with a blank password.'
      DoubleBuffered = True
      EditLabel.Width = 51
      EditLabel.Height = 13
      EditLabel.Caption = 'User name'
      EditLabel.Font.Charset = DEFAULT_CHARSET
      EditLabel.Font.Color = clWindowText
      EditLabel.Font.Height = -11
      EditLabel.Font.Name = 'MS Sans Serif'
      EditLabel.Font.Style = []
      EditLabel.ParentFont = False
      ParentDoubleBuffered = False
      TabOrder = 3
      Text = ''
      TextHint = 'JohnLennon@Gmail.com'
    end
    object grpTSL: TGroupBox
      Left = 308
      Top = 70
      Width = 196
      Height = 147
      Hint = 
        'Check your server documentation to see if it uses TSL or not.'#13#10'M' +
        'odern servers are using TSL now.'
      Anchors = [akTop, akRight]
      Caption = 'Authentification type'
      TabOrder = 4
      object radTslNone: TCubicRadioButton
        Left = 50
        Top = 37
        Width = 97
        Height = 17
        Hint = 'Default'
        Caption = 'No TSL support'
        TabOrder = 0
        AutoSize = True
      end
      object radTslImplic: TCubicRadioButton
        Left = 50
        Top = 60
        Width = 77
        Height = 17
        Caption = 'Implicit TSL'
        TabOrder = 1
        AutoSize = True
      end
      object radTslExpl: TCubicRadioButton
        Left = 50
        Top = 83
        Width = 77
        Height = 17
        Hint = 'unencrypted connection '#13#10'STARTTLS '
        Caption = 'Explicit TSL'
        TabOrder = 2
        AutoSize = True
      end
      object radTslRequire: TCubicRadioButton
        Left = 50
        Top = 106
        Width = 81
        Height = 17
        Hint = 'encrypted connection'
        Caption = 'Require TSL'
        TabOrder = 3
        AutoSize = True
      end
    end
    object Button1: TButton
      Left = 185
      Top = 96
      Width = 39
      Height = 22
      Hint = 'Show password'
      Caption = 'Show'
      TabOrder = 5
      OnClick = Button1Click
    end
    object btnGMailDef: TButton
      Left = 428
      Top = 6
      Width = 110
      Height = 25
      Hint = 
        'You don'#39't have your own email server? (Don'#39't worry; only compute' +
        'r geeks have one.)'#13#10'Use ap publick service like Google or Yahoo ' +
        'to send your emails.'#13#10'We will fill in geeky settings (port, SMTP' +
        ' server). '#13#10'You only fill your email address and email password.' +
        #13#10#13#10'Note:'#13#10'If you cannot send emails via GMail, you need to log ' +
        'in into your account and check the "Allow less secure apps".'#13#10'Fo' +
        'r details do a google search for: gmail manage access to less se' +
        'cure apps'
      Anchors = [akTop, akRight]
      Caption = 'Use GMail defaults'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = btnGMailDefClick
    end
  end
end
