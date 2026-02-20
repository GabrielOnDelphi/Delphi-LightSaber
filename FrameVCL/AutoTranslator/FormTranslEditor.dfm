object frmTranslEditor: TfrmTranslEditor
  Tag = 128
  Left = 549
  Top = 225
  AlphaBlend = True
  AlphaBlendValue = 250
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Translator'
  ClientHeight = 690
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Calibri'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnClose = FormClose
  TextHeight = 18
  object lblInfo: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 672
    Width = 358
    Height = 18
    Align = alBottom
    Alignment = taCenter
    Caption = '@Info'
    Visible = False
  end
  object grpLive: TCubicGroupBox
    Left = 0
    Top = 343
    Width = 364
    Height = 206
    Align = alClient
    Caption = 'Live forms'
    TabOrder = 0
    object lbxForms: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 26
      Width = 354
      Height = 150
      Hint = 'Live forms that available for translation.'
      Margins.Top = 6
      Align = alClient
      ItemHeight = 18
      TabOrder = 0
    end
    object chkTranslateTranslator: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 182
      Width = 354
      Height = 19
      Align = alBottom
      Caption = 'Also translate this form'
      TabOrder = 1
      OnClick = chkTranslateTranslatorClick
    end
  end
  object grpNew: TCubicGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 358
    Height = 210
    Align = alTop
    Caption = 'Create translation'
    TabOrder = 1
    object lblTargetLang: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 36
      Width = 97
      Height = 18
      Align = alBottom
      Caption = 'Target language'
    end
    object sbxTargetLang: TDropDownSearchBox
      AlignWithMargins = True
      Left = 5
      Top = 57
      Width = 348
      Height = 26
      Hint = 'Type or select the target language for translation'
      Margins.Bottom = 21
      Align = alBottom
      TabOrder = 0
      TextHint = 'Type language name...'
    end
    object edtAuthor: TLabeledEdit
      AlignWithMargins = True
      Left = 5
      Top = 107
      Width = 348
      Height = 26
      Hint = 
        '[Optional]'#13#10'The name of the person that created the translation ' +
        '(your name).'#13#10'If multiple authors, then put them in chronologic ' +
        'order, comma separated.'#13#10'You can enter also your website, facebo' +
        'ok, email, etc.'
      Align = alBottom
      EditLabel.Width = 81
      EditLabel.Height = 18
      EditLabel.Caption = 'Author name'
      TabOrder = 1
      Text = ''
      TextHint = 'GabrielMoraru'
    end
    object btnAutoTranslate: TButton
      AlignWithMargins = True
      Left = 5
      Top = 139
      Width = 348
      Height = 30
      Hint = 'Translate all texts using DeepL API'
      Align = alBottom
      Caption = 'Auto translate'
      TabOrder = 2
      OnClick = btnAutoTranslateClick
    end
    object btnManualTranslate: TButton
      AlignWithMargins = True
      Left = 5
      Top = 175
      Width = 348
      Height = 30
      Hint = 
        'Start a new translatio file and save all GUI strings to that fil' +
        'e.'#13#10'You can rename the file if you want, but keep the INI extens' +
        'ion.'
      Align = alBottom
      Caption = 'Extact from live forms'
      TabOrder = 3
      OnClick = btnManualTranslateClick
    end
  end
  object grpOptions: TGroupBox
    Left = 0
    Top = 216
    Width = 364
    Height = 127
    Align = alTop
    Caption = 'Save options'
    TabOrder = 2
    object btnLoadTranslation: TButton
      AlignWithMargins = True
      Left = 263
      Top = 47
      Width = 93
      Height = 50
      Hint = 'Load an existing translation file for editing'
      Caption = 'Load exiting translation...'
      TabOrder = 0
      WordWrap = True
      OnClick = btnLoadTranslationClick
    end
    object chkDontSaveEmpty: TCheckBox
      AlignWithMargins = True
      Left = 31
      Top = 63
      Width = 193
      Height = 19
      Hint = 
        'Don'#39't save properties (text, caption, hint) if their text is emp' +
        'ty'#13#10
      Caption = 'Don'#39't save empty'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkOverwrite: TCheckBox
      AlignWithMargins = True
      Left = 31
      Top = 90
      Width = 193
      Height = 19
      Hint = 
        'Checked: Overwrite existing file.'#13#10'Unchecked: The text is append' +
        'ed to the existing file.'
      Caption = 'Overwrite existing file'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkParseCtrlsAction: TCheckBox
      AlignWithMargins = True
      Left = 31
      Top = 36
      Width = 193
      Height = 19
      Hint = 
        'Parse controls that have an action assigned.'#13#10'Usually this shoul' +
        'd be set to False since we want to let the associated action tak' +
        'e precedence.'#13#10
      Caption = 'Parse controls with action'
      TabOrder = 3
    end
  end
  object grpHelp: TGroupBox
    Left = 0
    Top = 585
    Width = 364
    Height = 84
    Align = alBottom
    Caption = 'Help'
    TabOrder = 3
    object InternetLabel1: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 21
      Width = 83
      Height = 13
      Cursor = crHandPoint
      Align = alBottom
      Alignment = taCenter
      Caption = 'Google translator'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Transparent = True
      OnClick = InternetLabel1Click
    end
    object inetDeepL: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 37
      Width = 80
      Height = 13
      Cursor = crHandPoint
      Align = alBottom
      Alignment = taCenter
      Caption = 'DeepL translator'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Transparent = True
      OnClick = inetDeepLClick
    end
    object btnHelp: TButton
      AlignWithMargins = True
      Left = 5
      Top = 53
      Width = 354
      Height = 26
      Align = alBottom
      Caption = 'HELP'
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
  object btnDeepLSettings: TButton
    AlignWithMargins = True
    Left = 3
    Top = 552
    Width = 358
    Height = 30
    Hint = 'Configure DeepL API settings'
    Align = alBottom
    Caption = 'DeepL settings...'
    TabOrder = 4
    OnClick = btnDeepLSettingsClick
  end
end
