object frmDeepLSettings: TfrmDeepLSettings
  Tag = 128
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'DeepL API Settings'
  ClientHeight = 260
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  TextHeight = 13
  object grpSettings: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 414
    Height = 213
    Align = alClient
    Caption = ' API Configuration '
    TabOrder = 0
    object lblApiKey: TLabel
      Left = 16
      Top = 28
      Width = 38
      Height = 13
      Caption = 'API Key'
    end
    object lblStatus: TLabel
      Left = 16
      Top = 140
      Width = 382
      Height = 13
      AutoSize = False
      Caption = ''
    end
    object lblInfo: TLabel
      Left = 16
      Top = 160
      Width = 382
      Height = 45
      AutoSize = False
      Caption =
        'Get your API key from deepl.com/pro-api'#13#10'Free: 500K chars/month |' +
        ' Pro: $5.49/mo + $25/M chars'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object edtApiKey: TEdit
      Left = 16
      Top = 47
      Width = 382
      Height = 21
      Hint = 'Your DeepL API authentication key'
      PasswordChar = '*'
      TabOrder = 0
    end
    object chkUseFreeAPI: TCheckBox
      Left = 16
      Top = 80
      Width = 250
      Height = 17
      Hint =
        'Checked: Use free API (api-free.deepl.com)'#13#10'Unchecked: Use Pro A' +
        'PI (api.deepl.com)'
      Caption = 'Use Free API (500K chars/month limit)'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object btnTest: TButton
      Left = 16
      Top = 107
      Width = 100
      Height = 25
      Hint = 'Test the connection with current settings'
      Caption = 'Test Connection'
      TabOrder = 2
      OnClick = btnTestClick
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 219
    Width = 420
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      Left = 254
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 335
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
