object frmTranslator: TfrmTranslator
  Tag = 128
  Left = 549
  Top = 225
  AlphaBlend = True
  AlphaBlendValue = 250
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Translator'
  ClientHeight = 664
  ClientWidth = 836
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 648
    Width = 830
    Height = 13
    Align = alBottom
    Alignment = taCenter
    Caption = '@Info'
    Visible = False
  end
  object CubicGroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 239
    Height = 639
    Align = alLeft
    TabOrder = 0
    object lblLiveForms: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 316
      Width = 49
      Height = 13
      Hint = 
        'Live forms that available for translation.'#13#10'Only life (running) ' +
        'forms can be translated.'#13#10'Click to refresh.'
      Align = alBottom
      Caption = 'Live forms'
      OnClick = lblLiveFormsClick
    end
    object inetDeepL: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 570
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
    object InternetLabel1: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 589
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
    object btnCreateTransl: TButton
      Left = 9
      Top = 16
      Width = 114
      Height = 31
      Hint = 
        'Start a new translatio file and save all GUI strings to that fil' +
        'e.'#13#10'You can rename the file if you want, but keep the INI extens' +
        'ion.'
      Caption = 'New translation...'
      TabOrder = 0
      OnClick = btnCreateTranslClick
    end
    object btnHelp: TButton
      AlignWithMargins = True
      Left = 5
      Top = 608
      Width = 229
      Height = 26
      Align = alBottom
      Caption = 'HELP'
      TabOrder = 1
      OnClick = btnHelpClick
    end
    object lbxForms: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 335
      Width = 229
      Height = 229
      Hint = 'Live forms that available for translation'
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 2
    end
    object GroupBox2: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 177
      Width = 229
      Height = 133
      Align = alBottom
      Caption = 'Save options'
      TabOrder = 3
      object chkParseCtrlsAction: TCheckBox
        Left = 27
        Top = 34
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
        Left = 27
        Top = 61
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
        Left = 27
        Top = 88
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
    object btnLoad: TButton
      Left = 9
      Top = 53
      Width = 114
      Height = 31
      Hint = 'Load an existing translation file'
      Caption = 'Load translation...'
      TabOrder = 4
      OnClick = btnLoadClick
    end
  end
  object CubicGroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 248
    Top = 3
    Width = 585
    Height = 639
    Align = alClient
    Caption = 'Language editor'
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 343
      Top = 15
      Height = 622
      Align = alRight
    end
    object pnlRight: TPanel
      Left = 346
      Top = 15
      Width = 237
      Height = 622
      Align = alRight
      Caption = 'pnlRight'
      TabOrder = 0
      Visible = False
      object mmoValues: TMemo
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 229
        Height = 588
        Hint = 'Content of the currently loaded language file'
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel3: TPanel
        Left = 1
        Top = 595
        Width = 235
        Height = 26
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object btnCopyRight: TButton
          Left = 171
          Top = 0
          Width = 64
          Height = 26
          Hint = 
            'Copy right side (the one that needs to be translated) to the cli' +
            'pboard'
          Align = alRight
          Caption = 'Copy'
          TabOrder = 0
          OnClick = btnCopyRightClick
        end
      end
    end
    object Panel4: TPanel
      Left = 2
      Top = 15
      Width = 341
      Height = 622
      Align = alClient
      Caption = 'Panel4'
      TabOrder = 1
      object mmoLangEditor: TMemo
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 333
        Height = 580
        Hint = 'Content of the currently loaded language file'
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 1
        Top = 587
        Width = 339
        Height = 34
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object btnSaveEditor: TButton
          Left = 0
          Top = 0
          Width = 76
          Height = 34
          Hint = 
            'Save translation to disk.'#13#10'Current selected filename will be use' +
            'd.'
          Align = alLeft
          Caption = 'Save'
          TabOrder = 0
          OnClick = btnSaveEditorClick
        end
        object btnApplyEdits: TButton
          Left = 76
          Top = 0
          Width = 76
          Height = 34
          Hint = 'Save translation to file and apply translation'
          Align = alLeft
          Caption = 'Apply'
          Enabled = False
          TabOrder = 1
          OnClick = btnApplyEditsClick
        end
        object btnCopy: TButton
          Left = 206
          Top = 0
          Width = 58
          Height = 34
          Hint = 'Copy all text to clipboard'
          Align = alRight
          Caption = 'Copy'
          TabOrder = 2
          OnClick = btnCopyClick
        end
        object btnValues: TButton
          Left = 264
          Top = 0
          Width = 75
          Height = 34
          Hint = 'Show only the values (text that needs to be translated)'
          Align = alRight
          Caption = 'Values >>'
          TabOrder = 3
          OnClick = btnValuesClick
        end
      end
    end
  end
  object grpNewTransl: TGroupBox
    Left = 254
    Top = 96
    Width = 303
    Height = 162
    Caption = 'New translation'
    TabOrder = 2
    Visible = False
    object edtFileName: TLabeledEdit
      Left = 37
      Top = 48
      Width = 184
      Height = 21
      Hint = 
        'Example: Espanol.ini'#13#10#13#10'Special chanracters are not be allowed: ' +
        '? < > $ % : * '#13#10'If you want, you can rename this file manually, ' +
        'outside of the program.'#13#10#13#10'File extension must be .ini (added by' +
        ' the program automatically, if you forget).'
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'File Name'
      TabOrder = 0
      Text = 'NewLanguage'
    end
    object edtAuthor: TLabeledEdit
      Left = 37
      Top = 97
      Width = 184
      Height = 21
      Hint = 
        '[Optional]'#13#10'The name of the person that created the translation ' +
        '(your name).'#13#10'If multiple authors, then put them in chronologic ' +
        'order, comma separated.'#13#10'You can enter also your website, facebo' +
        'ok, email, etc.'
      EditLabel.Width = 62
      EditLabel.Height = 13
      EditLabel.Caption = 'Author name'
      TabOrder = 1
      TextHint = 'CubicDesign'
    end
    object btnOK: TButton
      Left = 233
      Top = 128
      Width = 60
      Height = 27
      Caption = 'OK'
      TabOrder = 2
      OnClick = btnOKClick
    end
  end
end
