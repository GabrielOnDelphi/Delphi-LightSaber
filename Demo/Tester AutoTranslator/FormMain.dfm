object frmTester: TfrmTester
  Left = 450
  Top = 311
  Anchors = []
  Caption = 'Tester'
  ClientHeight = 284
  ClientWidth = 464
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 390
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 4
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object CheckBox1: TCheckBox
    Left = 12
    Top = 26
    Width = 97
    Height = 17
    Caption = 'CheckBox'
    TabOrder = 3
  end
  object pnlRight: TPanel
    Left = 279
    Top = 0
    Width = 185
    Height = 284
    Align = alRight
    TabOrder = 0
    object btnShowTranslator: TButton
      Left = 1
      Top = 187
      Width = 183
      Height = 48
      Align = alBottom
      Caption = 'Select language'
      TabOrder = 0
      OnClick = btnShowTranslatorClick
    end
    object btnHelper: TButton
      Left = 1
      Top = 235
      Width = 183
      Height = 48
      Align = alBottom
      Caption = 'Translator helper'
      TabOrder = 1
      OnClick = btnHelperClick
    end
  end
  object Memo1: TMemo
    Left = 12
    Top = 124
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object LabeledEdit: TLabeledEdit
    Left = 12
    Top = 88
    Width = 121
    Height = 21
    EditLabel.Width = 33
    EditLabel.Height = 13
    EditLabel.Caption = 'English'
    TabOrder = 2
    Text = ''
  end
end
