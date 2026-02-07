object frmTranslSelector: TfrmTranslSelector
  AlignWithMargins = True
  Left = 549
  Top = 225
  AlphaBlend = True
  AlphaBlendValue = 250
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Language/Sprache/Langue/Limba/Idioma/'#35821#35328'/'#1071#1079#1099#1082
  ClientHeight = 336
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object grpChoose: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 345
    Height = 330
    Align = alClient
    Caption = 'Choose language'
    TabOrder = 0
    object lblAuthors: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 273
      Width = 335
      Height = 13
      Align = alBottom
      Caption = '@Authors'
      Visible = False
    end
    object ListBox: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 23
      Width = 335
      Height = 244
      Hint = 'Click to load it'
      Margins.Top = 8
      Align = alClient
      ItemHeight = 13
      ScrollWidth = 6
      TabOrder = 0
      OnClick = ListBoxDblClick
      OnDblClick = ListBoxDblClick
    end
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 289
      Width = 335
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnApplyLang: TButton
        AlignWithMargins = True
        Left = 246
        Top = 3
        Width = 86
        Height = 30
        Align = alRight
        Caption = 'OK'
        TabOrder = 0
        OnClick = btnApplyLangClick
      end
      object btnRefresh: TButton
        AlignWithMargins = True
        Left = 159
        Top = 4
        Width = 81
        Height = 28
        Hint = 'Reload the list of available languages'
        Margins.Top = 4
        Margins.Bottom = 4
        Align = alRight
        Caption = 'Refresh'
        TabOrder = 1
        OnClick = btnRefreshClick
      end
      object btnTranslate: TButton
        AlignWithMargins = True
        Left = 3
        Top = 4
        Width = 117
        Height = 28
        Hint = 
          'Create your own translation or edit an existing translation file' +
          '.'
        Margins.Top = 4
        Margins.Bottom = 4
        Align = alLeft
        Caption = 'New translation...'
        TabOrder = 2
        OnClick = btnTranslateClick
      end
    end
  end
end
