object frmUpdaterSettings: TfrmUpdaterSettings
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 250
  Anchors = []
  BorderStyle = bsToolWindow
  Caption = 'Updater settings'
  ClientHeight = 364
  ClientWidth = 488
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 4
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 17
  object Container: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 482
    Height = 358
    Align = alClient
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      482
      358)
    object lblPrivacy: TLabel
      Left = 1
      Top = 277
      Width = 480
      Height = 34
      Align = alBottom
      Alignment = taCenter
      Caption = 
        'Privacy notice: The program only downloads data from our website' +
        '.'#13#10'No personal data is sent to our server!'
      Visible = False
    end
    object GroupBox1: TGroupBox
      Left = 6
      Top = 160
      Width = 428
      Height = 121
      Anchors = []
      Caption = 'Internet connection'
      TabOrder = 0
      DesignSize = (
        428
        121)
      object chkConnectFail: TCubicCheckBox
        Left = 63
        Top = 37
        Width = 324
        Height = 20
        Hint = 
          'Show message if the program encounters problems while trying to ' +
          'connect to our website.'#13#10'If other programs can access the intern' +
          'et but this program can'#39't it means that it is blocked by an anti' +
          'virus/firewall.'#13#10'Whitelist the program in your antivirus/firewal' +
          'l in order to get updates.'#13#10'It is important to stay up to date!'
        Anchors = [akTop]
        Caption = 'Warn when the program cannot connect to Internet'
        Checked = True
        State = cbChecked
        TabOrder = 0
        AutoSize = True
      end
      object btnTestInternet: TButton
        Left = 131
        Top = 71
        Width = 165
        Height = 34
        Hint = 
          'Test if this program can connect to the Internet or not.'#13#10#13#10'If o' +
          'ther programs can access the internet but this program can'#39't it ' +
          'means that it is blocked by an antivirus/firewall.'#13#10'Whitelist th' +
          'e program in your antivirus/firewall in order to get updates.'#13#10'I' +
          't is important to stay up to date!'
        Anchors = [akTop]
        Caption = 'Check Internet Connection'
        TabOrder = 1
        WordWrap = True
        OnClick = btnTestInternetClick
      end
    end
    object grpInterval: TGroupBox
      Left = 6
      Top = 4
      Width = 428
      Height = 161
      Anchors = []
      Caption = 'Updates'
      TabOrder = 1
      object lblHours: TLabel
        Left = 221
        Top = 96
        Width = 33
        Height = 17
        Alignment = taRightJustify
        Caption = 'hours'
      end
      object chkEveryStart: TLabel
        Left = 75
        Top = 34
        Width = 88
        Height = 17
        Alignment = taRightJustify
        Caption = 'Check for news'
      end
      object spnHours: TCubicSpinEdit
        Left = 168
        Top = 92
        Width = 45
        Height = 27
        Hint = 
          'How often to check the web site for news and updates.'#13#10'If you wa' +
          'nt to check every 3 days, enter 72h here.'
        MaxValue = 100
        MinValue = 1
        ParentColor = True
        TabOrder = 0
        Value = 24
      end
      object chkForceNewsFound: TCubicCheckBox
        Left = 76
        Top = 132
        Width = 183
        Height = 18
        Caption = 'Force ThereAreNewsOnline'
        TabOrder = 1
        Visible = False
        AutoSize = True
      end
      object cmbWhen: TComboBox
        Left = 108
        Top = 56
        Width = 178
        Height = 25
        Style = csDropDownList
        TabOrder = 2
        OnChange = cmbWhenChange
        Items.Strings = (
          'Never'
          'When the program starts'
          'Every...')
      end
    end
    object pnlBtm: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 314
      Width = 474
      Height = 40
      Align = alBottom
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
      object btnOK: TButton
        AlignWithMargins = True
        Left = 374
        Top = 3
        Width = 113
        Height = 34
        Hint = 
          'Save settings & close the window.'#13#10'Hint: You can also close the ' +
          'window with <Enter>'
        Align = alRight
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 0
      end
      object btnApply: TButton
        AlignWithMargins = True
        Left = 255
        Top = 3
        Width = 113
        Height = 34
        Hint = 
          'Save settings & close the window.'#13#10'Hint: You can also close the ' +
          'window with <Enter>'
        Align = alRight
        Caption = 'Apply'
        TabOrder = 1
        OnClick = btnApplyClick
      end
    end
  end
end
