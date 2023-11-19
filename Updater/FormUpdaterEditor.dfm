object frmUpdaterEditor: TfrmUpdaterEditor
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 250
  Anchors = []
  BorderStyle = bsToolWindow
  Caption = 'Updater settings'
  ClientHeight = 412
  ClientWidth = 600
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 4
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Container: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 594
    Height = 406
    Align = alClient
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      594
      406)
    object lblPrivacy: TLabel
      Left = 1
      Top = 1
      Width = 592
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = 
        'Privacy notice: The program only downloads data from our server.' +
        ' No data at all is sent to the server!'
      Visible = False
    end
    object GroupBox1: TGroupBox
      Left = 101
      Top = 214
      Width = 376
      Height = 121
      Anchors = []
      Caption = 'Internet connection'
      TabOrder = 0
      DesignSize = (
        376
        121)
      object chkConnectFail: TCubicCheckBox
        Left = 51
        Top = 37
        Width = 275
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
        Left = 105
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
      Left = 101
      Top = 49
      Width = 376
      Height = 152
      Anchors = []
      Caption = 'Updates'
      TabOrder = 1
      object lblDayHours: TLabel
        Left = 89
        Top = 94
        Width = 149
        Height = 13
        Alignment = taRightJustify
        Caption = 'The day has                     hours'
      end
      object chkEveryStart: TLabel
        Left = 89
        Top = 35
        Width = 74
        Height = 13
        Alignment = taRightJustify
        Caption = 'Check for news'
      end
      object spnInterval: TCubicSpinEdit
        Left = 156
        Top = 91
        Width = 45
        Height = 22
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
        Left = 89
        Top = 124
        Width = 156
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
        Height = 21
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 2
        Text = 'Once per day'
        OnChange = cmbWhenChange
        Items.Strings = (
          'Never'
          'Every time the program starts'
          'Once per day')
      end
    end
    object pnlBtm: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 366
      Width = 586
      Height = 36
      Align = alBottom
      ParentColor = True
      TabOrder = 2
      object btnOK: TButton
        AlignWithMargins = True
        Left = 396
        Top = 4
        Width = 90
        Height = 28
        Hint = 
          'Save settings & close the window.'#13#10'Hint: You can also close the ' +
          'window with <Enter>'
        Align = alRight
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        AlignWithMargins = True
        Left = 492
        Top = 4
        Width = 90
        Height = 28
        Hint = 
          'Close the window without saving.'#13#10'Hint: You can also close with ' +
          'Escape.'
        Align = alRight
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
        OnClick = btnCancelClick
      end
      object btnApply: TButton
        AlignWithMargins = True
        Left = 300
        Top = 4
        Width = 90
        Height = 28
        Hint = 
          'Save settings & close the window.'#13#10'Hint: You can also close the ' +
          'window with <Enter>'
        Align = alRight
        Caption = 'Apply'
        TabOrder = 2
        OnClick = btnApplyClick
      end
    end
  end
end
