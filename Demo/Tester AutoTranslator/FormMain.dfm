object MainForm: TMainForm
  Left = 450
  Top = 311
  Anchors = []
  Caption = 'Translator Demo - www.GabrielMoraru.com'
  ClientHeight = 283
  ClientWidth = 460
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 390
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Calibri'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 4
  TextHeight = 15
  object CheckBox1: TCheckBox
    Left = 12
    Top = 37
    Width = 123
    Height = 17
    Caption = 'Demo checkbox'
    TabOrder = 3
  end
  object pnlRight: TPanel
    Left = 256
    Top = 0
    Width = 204
    Height = 283
    Align = alRight
    TabOrder = 0
    object lblCurLang: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 94
      Height = 15
      Align = alTop
      Caption = 'Current language'
    end
    object btnSelector: TButton
      AlignWithMargins = True
      Left = 4
      Top = 177
      Width = 196
      Height = 48
      Align = alBottom
      Caption = 'Select language'
      TabOrder = 0
      OnClick = btnSelectorClick
    end
    object btnEditor: TButton
      AlignWithMargins = True
      Left = 4
      Top = 231
      Width = 196
      Height = 48
      Align = alBottom
      Caption = 'Edit current translation'
      TabOrder = 1
      OnClick = btnEditorClick
    end
  end
  object Memo: TMemo
    Left = 12
    Top = 148
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object LabeledEdit: TLabeledEdit
    Left = 12
    Top = 88
    Width = 185
    Height = 23
    EditLabel.Width = 73
    EditLabel.Height = 15
    EditLabel.Caption = 'Some editbox'
    TabOrder = 2
    Text = ''
  end
end
