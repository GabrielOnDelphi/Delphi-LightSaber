object frmContainer: TfrmContainer
  Left = 0
  Top = 0
  Caption = 'Container'
  ClientHeight = 310
  ClientWidth = 385
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object grpContainer: TGroupBox
    Left = 60
    Top = 16
    Width = 245
    Height = 139
    Caption = 'Container on second form'
    TabOrder = 0
    object Label1: TLabel
      Left = 2
      Top = 111
      Width = 241
      Height = 26
      Align = alBottom
      Alignment = taCenter
      Caption = 
        'This will test if the controls on the second form are saved corr' +
        'ectly'
      WordWrap = True
    end
    object CheckBox1: TCheckBox
      Left = 40
      Top = 30
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 0
    end
    object CubicCheckBox1: TCubicCheckBox
      Left = 40
      Top = 53
      Width = 104
      Height = 17
      Caption = 'CubicCheckBox1'
      TabOrder = 1
      AutoSize = True
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 193
    Width = 379
    Height = 110
    Margins.Bottom = 7
    Align = alBottom
    Caption = 'Checkboxes'
    TabOrder = 1
    object CubicCheckBox2: TCubicCheckBox
      AlignWithMargins = True
      Left = 40
      Top = 31
      Width = 101
      Height = 17
      Caption = 'Cubic Checkbox'
      TabOrder = 0
      AutoSize = True
    end
    object RadioButton1: TRadioButton
      AlignWithMargins = True
      Left = 40
      Top = 50
      Width = 132
      Height = 17
      Caption = 'VCL RadioButton'
      TabOrder = 1
    end
    object CubicRadioButton1: TCubicRadioButton
      Left = 40
      Top = 70
      Width = 115
      Height = 17
      Caption = 'Cubic RadioButton '
      Checked = True
      TabOrder = 2
      TabStop = True
      AutoSize = True
    end
  end
end
