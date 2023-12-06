object frmLanguage: TfrmLanguage
  AlignWithMargins = True
  Left = 549
  Top = 225
  AlphaBlend = True
  AlphaBlendValue = 250
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Language/Sprache/Langue/Limba/Idioma/'#35821#35328'/'#1071#1079#1099#1082
  ClientHeight = 334
  ClientWidth = 317
  Color = clBtnFace
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
  SnapBuffer = 3
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object grpChoose: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 311
    Height = 328
    Align = alClient
    Caption = 'Choose language'
    TabOrder = 0
    object lblAuthors: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 253
      Width = 301
      Height = 13
      Align = alBottom
      Caption = '@Authors'
      Visible = False
    end
    object lblHint: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 278
      Width = 301
      Height = 13
      Margins.Top = 9
      Align = alBottom
      Caption = 'Hint: You can create your own translations. '
    end
    object ListBox: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 23
      Width = 301
      Height = 224
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
      Top = 297
      Width = 301
      Height = 26
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnApplyLang: TButton
        Left = 209
        Top = 0
        Width = 92
        Height = 26
        Align = alRight
        Caption = 'OK'
        TabOrder = 0
        OnClick = btnApplyLangClick
      end
      object btnRefresh: TButton
        Left = 86
        Top = 0
        Width = 78
        Height = 26
        Hint = 'Reload the list of available languages'
        Margins.Top = 4
        Margins.Bottom = 4
        Align = alLeft
        Caption = 'Refresh'
        TabOrder = 1
        OnClick = btnRefreshClick
      end
      object btnTranslate: TButton
        Left = 0
        Top = 0
        Width = 86
        Height = 26
        Hint = 'Create your own translation'
        Margins.Top = 4
        Margins.Bottom = 4
        Align = alLeft
        Caption = 'Translate'
        TabOrder = 2
        OnClick = btnTranslateClick
      end
    end
  end
end
