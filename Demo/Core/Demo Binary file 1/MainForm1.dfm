object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Light Saber - Binary file demo'
  ClientHeight = 311
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    427
    311)
  TextHeight = 17
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 421
    Height = 34
    Align = alTop
    Caption = 
      'This demo demonstrates how to save some data to a binary file an' +
      'd read it back'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 26
    Top = 128
    Width = 20
    Height = 17
    Caption = 'Life'
  end
  object btnSave: TButton
    Left = 98
    Top = 266
    Width = 107
    Height = 37
    Anchors = [akBottom]
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object btnLoad: TButton
    Left = 211
    Top = 266
    Width = 107
    Height = 37
    Anchors = [akBottom]
    Caption = 'Load'
    TabOrder = 1
    OnClick = btnLoadClick
  end
  object spnLife: TSpinEdit
    Left = 52
    Top = 124
    Width = 55
    Height = 27
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object edtName: TLabeledEdit
    Left = 26
    Top = 80
    Width = 121
    Height = 25
    EditLabel.Width = 77
    EditLabel.Height = 17
    EditLabel.Caption = 'Soldier name'
    TabOrder = 3
    Text = ''
  end
end
