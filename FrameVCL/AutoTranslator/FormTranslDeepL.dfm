object frmDeepLSettings: TfrmDeepLSettings
  Tag = 128
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'DeepL API Settings'
  ClientHeight = 284
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Calibri'
  Font.Style = []
  Position = poDesigned
  OnClose = FormClose
  TextHeight = 18
  object grpSettings: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 486
    Height = 237
    Align = alClient
    Caption = ' API Configuration '
    TabOrder = 0
    DesignSize = (
      486
      237)
    object lblApiKey: TLabel
      Left = 16
      Top = 28
      Width = 47
      Height = 18
      Caption = 'API Key'
    end
    object lblStatus: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 169
      Width = 476
      Height = 18
      Align = alBottom
      Caption = '.'
      WordWrap = True
    end
    object lblInfo: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 190
      Width = 476
      Height = 45
      Align = alBottom
      AutoSize = False
      Caption = 
        'Get your API key from www.deepl.com/en/your-account/keys'#13#10'Free a' +
        'ccount: 500K chars/month'#13#10'Pro account: $5.49/mo -> $25 per milli' +
        'on chars'
      Font.Charset = ANSI_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'Calibri'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object edtApiKey: TEdit
      Left = 16
      Top = 47
      Width = 447
      Height = 26
      Hint = 'Your DeepL API authentication key'
      Anchors = [akLeft, akTop, akRight]
      PasswordChar = '*'
      TabOrder = 0
    end
    object chkUseFreeAPI: TCheckBox
      Left = 16
      Top = 82
      Width = 447
      Height = 17
      Hint = 
        'Checked: Use free API (api-free.deepl.com)'#13#10'Unchecked: Use Pro A' +
        'PI (api.deepl.com)'
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use Free API (500K chars/month limit)'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object btnTest: TButton
      Left = 16
      Top = 114
      Width = 129
      Height = 27
      Hint = 'Test the connection with current settings'
      Caption = 'Test Connection'
      TabOrder = 2
      OnClick = btnTestClick
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 243
    Width = 492
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      AlignWithMargins = True
      Left = 333
      Top = 3
      Width = 75
      Height = 35
      Align = alRight
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 414
      Top = 3
      Width = 75
      Height = 35
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
