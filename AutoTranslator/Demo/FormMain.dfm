object MainForm: TMainForm
  Left = 450
  Top = 311
  Anchors = []
  Caption = 'Translator Demo - www.GabrielMoraru.com'
  ClientHeight = 290
  ClientWidth = 466
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 390
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Calibri'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 4
  TextHeight = 18
  object pnlRight: TPanel
    AlignWithMargins = True
    Left = 259
    Top = 3
    Width = 204
    Height = 284
    Align = alRight
    TabOrder = 0
    object lblCurLang: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 196
      Height = 18
      Align = alTop
      Caption = 'Current language'
    end
    object btnSelector: TButton
      AlignWithMargins = True
      Left = 4
      Top = 70
      Width = 196
      Height = 48
      Align = alBottom
      Caption = 'Select language'
      TabOrder = 0
      WordWrap = True
      OnClick = btnSelectorClick
    end
    object btnEditManual: TButton
      AlignWithMargins = True
      Left = 4
      Top = 232
      Width = 196
      Height = 48
      Align = alBottom
      Caption = 'Edit current translation'#13#10'(manual)'
      TabOrder = 1
      WordWrap = True
      OnClick = btnEditManualClick
    end
    object btnEditAuto: TButton
      AlignWithMargins = True
      Left = 4
      Top = 178
      Width = 196
      Height = 48
      Align = alBottom
      Caption = 'Edit current translation'#13#10'(automatic)'
      TabOrder = 2
      WordWrap = True
      OnClick = btnEditAutoClick
    end
    object btnAddNewAuto: TButton
      AlignWithMargins = True
      Left = 4
      Top = 124
      Width = 196
      Height = 48
      Align = alBottom
      Caption = 'Add new translation'#13#10'(automatic)'
      TabOrder = 3
      WordWrap = True
      OnClick = btnAddNewAutoClick
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 250
    Height = 284
    Align = alClient
    Caption = 'Demo controls'
    TabOrder = 1
    object CheckBox1: TCheckBox
      AlignWithMargins = True
      Left = 8
      Top = 55
      Width = 123
      Height = 17
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Demo checkbox'
      TabOrder = 0
    end
    object LabeledEdit: TLabeledEdit
      AlignWithMargins = True
      Left = 8
      Top = 103
      Width = 234
      Height = 26
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alBottom
      EditLabel.Width = 86
      EditLabel.Height = 18
      EditLabel.Caption = 'Some editbox'
      TabOrder = 1
      Text = ''
    end
    object Memo: TMemo
      AlignWithMargins = True
      Left = 8
      Top = 141
      Width = 234
      Height = 135
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alBottom
      Lines.Strings = (
        'Memo1')
      TabOrder = 2
    end
  end
end
