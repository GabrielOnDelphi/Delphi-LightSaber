object frmTranslEditor: TfrmTranslEditor
  Tag = 128
  Left = 549
  Top = 225
  AlphaBlend = True
  AlphaBlendValue = 250
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Translator'
  ClientHeight = 671
  ClientWidth = 366
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
    Top = 653
    Width = 360
    Height = 18
    Align = alBottom
    Alignment = taCenter
    Caption = '@Info'
    Visible = False
  end
  object grpLive: TCubicGroupBox
    Left = 0
    Top = 425
    Width = 366
    Height = 141
    Align = alClient
    Caption = 'Live forms'
    TabOrder = 0
    object lbxForms: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 26
      Width = 356
      Height = 93
      Hint = 'Live forms that available for translation.'
      Margins.Top = 6
      Align = alClient
      ItemHeight = 18
      TabOrder = 0
    end
    object chkTranslateTranslator: TCheckBox
      Left = 2
      Top = 122
      Width = 362
      Height = 17
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
    Width = 360
    Height = 202
    Align = alTop
    Caption = 'Create Translation'
    TabOrder = 1
    object lblTargetLang: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 28
      Width = 350
      Height = 18
      Align = alBottom
      Caption = 'Target language'
    end
    object sbxTargetLang: TDropDownSearchBox
      AlignWithMargins = True
      Left = 5
      Top = 49
      Width = 350
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
      Top = 99
      Width = 350
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
      Top = 131
      Width = 350
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
      Top = 167
      Width = 350
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
    Top = 208
    Width = 366
    Height = 217
    Align = alTop
    Caption = 'Options'
    TabOrder = 2
    object btnLoadTranslation: TButton
      AlignWithMargins = True
      Left = 5
      Top = 146
      Width = 356
      Height = 30
      Hint = 'Load an existing translation file for editing'
      Align = alBottom
      Caption = 'Load exiting translation...'
      TabOrder = 0
      OnClick = btnLoadTranslationClick
    end
    object btnDeepLSettings: TButton
      AlignWithMargins = True
      Left = 5
      Top = 182
      Width = 356
      Height = 30
      Hint = 'Configure DeepL API settings'
      Align = alBottom
      Caption = 'DeepL settings...'
      TabOrder = 1
      OnClick = btnDeepLSettingsClick
    end
    object GroupBox2: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 27
      Width = 356
      Height = 113
      Align = alBottom
      Caption = 'Save options'
      TabOrder = 2
      object chkParseCtrlsAction: TCheckBox
        AlignWithMargins = True
        Left = 29
        Top = 32
        Width = 179
        Height = 17
        Hint = 
          'Parse controls that have an action assigned.'#13#10'Usually this shoul' +
          'd be set to False since we want to let the associated action tak' +
          'e precedence.'#13#10
        Caption = 'Parse controls with action'
        TabOrder = 0
      end
      object chkDontSaveEmpty: TCheckBox
        AlignWithMargins = True
        Left = 29
        Top = 59
        Width = 179
        Height = 17
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
        Left = 29
        Top = 86
        Width = 179
        Height = 17
        Hint = 
          'Checked: Overwrite existing file.'#13#10'Unchecked: The text is append' +
          'ed to the existing file.'
        Caption = 'Overwrite existing file'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
  end
  object grpHelp: TGroupBox
    Left = 0
    Top = 566
    Width = 366
    Height = 84
    Align = alBottom
    Caption = 'Help'
    TabOrder = 3
    object InternetLabel1: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 21
      Width = 356
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
      Width = 356
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
      Width = 356
      Height = 26
      Align = alBottom
      Caption = 'HELP'
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
end
