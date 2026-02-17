object frmTranslatorIniEditor: TfrmTranslatorIniEditor
  Left = 0
  Top = 0
  Caption = 'Translation Editor'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Calibri'
  Font.Style = []
  ShowHint = True
  OnClose = FormClose
  TextHeight = 18
  object Splitter: TSplitter
    Left = 384
    Top = 0
    Height = 441
    Align = alRight
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 441
    Align = alClient
    Caption = 'pnlLeft'
    TabOrder = 0
    object mmoLangEditor: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 376
      Height = 399
      Hint = 'Content of the currently loaded language file'
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 1
      Top = 406
      Width = 382
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnSaveEditor: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 76
        Height = 28
        Hint = 
          'Save translation to disk.'#13#10'Current selected filename will be use' +
          'd.'
        Align = alLeft
        Caption = 'Save'
        Enabled = False
        TabOrder = 0
        OnClick = btnSaveEditorClick
      end
      object btnApplyEdits: TButton
        AlignWithMargins = True
        Left = 85
        Top = 3
        Width = 76
        Height = 28
        Hint = 'Save translation to file and apply translation'
        Align = alLeft
        Caption = 'Apply'
        Enabled = False
        TabOrder = 1
        OnClick = btnApplyEditsClick
      end
      object btnCopy: TButton
        AlignWithMargins = True
        Left = 237
        Top = 3
        Width = 61
        Height = 28
        Hint = 'Copy all text to clipboard'
        Align = alRight
        Caption = 'Copy'
        TabOrder = 2
        OnClick = btnCopyClick
      end
      object btnValues: TButton
        AlignWithMargins = True
        Left = 304
        Top = 3
        Width = 75
        Height = 28
        Hint = 'Show only the values (text that needs to be translated)'
        Align = alRight
        Caption = 'Values >>'
        TabOrder = 3
        OnClick = btnValuesClick
      end
    end
  end
  object pnlRight: TPanel
    Left = 387
    Top = 0
    Width = 237
    Height = 441
    Align = alRight
    Caption = 'pnlRight'
    TabOrder = 1
    Visible = False
    object mmoValues: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 229
      Height = 399
      Hint = 'Content of the currently loaded language file'
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 1
      Top = 406
      Width = 235
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnCopyRight: TButton
        AlignWithMargins = True
        Left = 168
        Top = 3
        Width = 64
        Height = 28
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
end
