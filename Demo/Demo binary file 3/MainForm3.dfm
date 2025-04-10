object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Light Saber - Binary file demo'
  ClientHeight = 457
  ClientWidth = 488
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 17
  object lblInfo: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 482
    Height = 17
    Align = alTop
    Caption = 
      'This demo demonstrates how to save some data to a binary file an' +
      'd read it back'
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 23
    Width = 482
    Height = 93
    Align = alTop
    Caption = 'Soldier'
    TabOrder = 0
    object Label2: TLabel
      Left = 194
      Top = 57
      Width = 20
      Height = 17
      Caption = 'Life'
    end
    object edtName: TLabeledEdit
      Left = 18
      Top = 54
      Width = 121
      Height = 25
      EditLabel.Width = 77
      EditLabel.Height = 17
      EditLabel.Caption = 'Soldier name'
      TabOrder = 0
      Text = ''
    end
    object spnLife: TSpinEdit
      Left = 225
      Top = 52
      Width = 55
      Height = 27
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 317
    Width = 482
    Height = 97
    Align = alBottom
    Caption = 'New gun'
    TabOrder = 1
    Visible = False
    DesignSize = (
      482
      97)
    object Label3: TLabel
      Left = 181
      Top = 54
      Width = 66
      Height = 17
      Caption = 'Max ammo'
    end
    object spnAmmo: TSpinEdit
      Left = 258
      Top = 50
      Width = 55
      Height = 27
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object edtGunName: TLabeledEdit
      Left = 34
      Top = 52
      Width = 121
      Height = 25
      EditLabel.Width = 59
      EditLabel.Height = 17
      EditLabel.Caption = 'Gun name'
      TabOrder = 1
      Text = ''
    end
    object btnAddGun: TButton
      Left = 367
      Top = 50
      Width = 99
      Height = 34
      Anchors = [akBottom]
      Caption = 'Add gun'
      TabOrder = 2
      OnClick = btnAddGunClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 417
    Width = 488
    Height = 40
    Align = alBottom
    TabOrder = 2
    object btnSave: TButton
      AlignWithMargins = True
      Left = 264
      Top = 4
      Width = 107
      Height = 32
      Align = alRight
      Caption = 'Save'
      Enabled = False
      TabOrder = 0
      OnClick = btnSaveClick
    end
    object btnLoad: TButton
      AlignWithMargins = True
      Left = 377
      Top = 4
      Width = 107
      Height = 32
      Align = alRight
      Caption = 'Load'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnClear: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 69
      Height = 32
      Align = alLeft
      Caption = 'Clear'
      TabOrder = 2
      OnClick = btnClearClick
    end
  end
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 122
    Width = 482
    Height = 189
    Align = alClient
    Caption = 'Equipment'
    TabOrder = 3
    DesignSize = (
      482
      189)
    object ListBox1: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 22
      Width = 188
      Height = 162
      Align = alLeft
      ItemHeight = 17
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object btnSetActive: TButton
      Left = 367
      Top = 83
      Width = 99
      Height = 34
      Hint = 'Set selected gun as active gun'
      Anchors = [akBottom]
      Caption = 'Set active'
      TabOrder = 1
      OnClick = btnSetActiveClick
    end
    object btnNewGun: TButton
      Left = 367
      Top = 43
      Width = 99
      Height = 34
      Anchors = [akBottom]
      Caption = 'New gun'
      TabOrder = 2
      OnClick = btnNewGunClick
    end
  end
end
