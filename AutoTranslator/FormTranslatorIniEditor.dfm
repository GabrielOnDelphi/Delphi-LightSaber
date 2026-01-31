object frmTranslatorIniEditor: TfrmTranslatorIniEditor
  Left = 0
  Top = 0
  Caption = 'Translation Editor'
  OnClose = FormClose
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object CubicGroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 618
    Height = 435
    Align = alClient
    Caption = 'Language editor'
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 376
      Top = 17
      Height = 416
      Align = alRight
    end
    object pnlRight: TPanel
      Left = 379
      Top = 17
      Width = 237
      Height = 416
      Align = alRight
      Caption = 'pnlRight'
      TabOrder = 0
      Visible = False
      object mmoValues: TMemo
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 229
        Height = 374
        Hint = 'Content of the currently loaded language file'
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 1
        Top = 381
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
    object Panel3: TPanel
      Left = 2
      Top = 17
      Width = 374
      Height = 416
      Align = alClient
      Caption = 'Panel3'
      TabOrder = 1
      object mmoLangEditor: TMemo
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 366
        Height = 374
        Hint = 'Content of the currently loaded language file'
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 1
        Top = 381
        Width = 372
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
          Left = 227
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
          Left = 294
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
  end
end
