object frmTranslSelector: TfrmTranslSelector
  AlignWithMargins = True
  Left = 549
  Top = 225
  AlphaBlend = True
  AlphaBlendValue = 250
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Language/Sprache/Langue/Limba/Idioma/'#35821#35328'/'#1071#1079#1099#1082
  ClientHeight = 367
  ClientWidth = 357
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Calibri'
  Font.Style = []
  KeyPreview = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 18
  object grpChoose: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 351
    Height = 361
    Align = alClient
    Caption = 'Choose language'
    TabOrder = 0
    object lblAuthors: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 299
      Width = 341
      Height = 18
      Align = alBottom
      Caption = '@Authors'
      Visible = False
    end
    object ListBox: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 28
      Width = 341
      Height = 265
      Hint = 'Click to load it'
      Margins.Top = 8
      Align = alClient
      ItemHeight = 18
      ScrollWidth = 6
      TabOrder = 0
      OnClick = ListBoxDblClick
      OnDblClick = ListBoxDblClick
    end
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 320
      Width = 341
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnApplyLang: TButton
        AlignWithMargins = True
        Left = 249
        Top = 3
        Width = 89
        Height = 30
        Align = alRight
        Caption = 'OK'
        TabOrder = 0
        OnClick = btnApplyLangClick
      end
      object btnRefresh: TButton
        AlignWithMargins = True
        Left = 158
        Top = 4
        Width = 85
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
        Width = 121
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
