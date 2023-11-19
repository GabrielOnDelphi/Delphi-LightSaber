object frmRecEditor: TfrmRecEditor
  Left = 862
  Top = 277
  AlphaBlendValue = 245
  Anchors = []
  Caption = 'News & Updates Editor'
  ClientHeight = 328
  ClientWidth = 653
  Color = clBtnFace
  Constraints.MinHeight = 335
  Constraints.MinWidth = 434
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Font.Quality = fqProof
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object pnlBtm: TPanel
    Left = 0
    Top = 292
    Width = 653
    Height = 36
    Align = alBottom
    TabOrder = 0
    object btnSave: TButton
      AlignWithMargins = True
      Left = 163
      Top = 4
      Width = 90
      Height = 28
      Hint = 
        'Close the window without saving.'#13#10'Hint: You can also close with ' +
        'Escape.'
      Align = alLeft
      Caption = 'Save'
      TabOrder = 0
      OnClick = btnSaveClick
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 67
      Top = 4
      Width = 90
      Height = 28
      Hint = 
        'Close the window without saving.'#13#10'Hint: You can also close with ' +
        'Escape.'
      Align = alLeft
      Caption = 'Load'
      TabOrder = 1
      OnClick = Button1Click
    end
    object btnCopy: TButton
      AlignWithMargins = True
      Left = 4
      Top = 6
      Width = 57
      Height = 24
      Hint = 'Copy file path to clipboard'
      Margins.Top = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Copy'
      TabOrder = 2
      OnClick = btnCopyClick
    end
    object btnClose: TButton
      AlignWithMargins = True
      Left = 559
      Top = 4
      Width = 90
      Height = 28
      Hint = 
        'Close the window without saving.'#13#10'Hint: You can also close with ' +
        'Escape.'
      Align = alRight
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 3
    end
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 645
    Height = 34
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 1
    object lblCounter: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 44
      Height = 15
      Align = alLeft
      Caption = 'NewsID'
      Layout = tlCenter
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 138
      Top = 4
      Width = 34
      Height = 15
      Align = alLeft
      Caption = 'Target'
      Layout = tlCenter
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 297
      Top = 4
      Width = 34
      Height = 15
      Align = alLeft
      Caption = 'Show '
      Layout = tlCenter
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 384
      Top = 4
      Width = 39
      Height = 15
      Align = alLeft
      Caption = 'time(s)'
      Layout = tlCenter
    end
    object spnCounter: TSpinEdit
      AlignWithMargins = True
      Left = 54
      Top = 4
      Width = 69
      Height = 26
      Hint = 
        ' Increment it each time we publish new news. This is how the pro' +
        'gram detects that a new announcement was published online.'
      Align = alLeft
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
    object cmbTarget: TComboBox
      AlignWithMargins = True
      Left = 178
      Top = 6
      Width = 104
      Height = 23
      Margins.Top = 5
      Align = alLeft
      TabOrder = 1
      Text = 'cmbTarget'
    end
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 288
      Top = 4
      Width = 3
      Height = 26
      Align = alLeft
      TabOrder = 2
    end
    object Panel4: TPanel
      AlignWithMargins = True
      Left = 129
      Top = 4
      Width = 3
      Height = 26
      Align = alLeft
      TabOrder = 3
    end
    object spnShowCntr: TSpinEdit
      AlignWithMargins = True
      Left = 337
      Top = 4
      Width = 41
      Height = 26
      Hint = 'How many times to show this to the user. Usually only one time.'
      Align = alLeft
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 4
      Value = 1
    end
  end
  object Panel5: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 46
    Width = 645
    Height = 34
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    TabOrder = 2
    object lblVers: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 44
      Height = 15
      Align = alLeft
      Caption = 'Version:'
      Layout = tlCenter
    end
    object Panel6: TPanel
      AlignWithMargins = True
      Left = 418
      Top = 4
      Width = 3
      Height = 26
      Align = alLeft
      TabOrder = 0
    end
    object Panel7: TPanel
      AlignWithMargins = True
      Left = 194
      Top = 4
      Width = 3
      Height = 26
      Align = alLeft
      TabOrder = 1
    end
    object Panel8: TPanel
      AlignWithMargins = True
      Left = 306
      Top = 4
      Width = 3
      Height = 26
      Align = alLeft
      TabOrder = 2
    end
    object edtOnlineVer: TEdit
      AlignWithMargins = True
      Left = 54
      Top = 6
      Width = 134
      Height = 23
      Hint = 'Online version, no build number.'#13#10'Ex: 14.4.0'#13#10
      Margins.Top = 5
      Margins.Bottom = 4
      Align = alLeft
      TabOrder = 3
      TextHint = 'Online version (14.4.0)'
    end
    object chkCriticalUpd: TCheckBox
      AlignWithMargins = True
      Left = 315
      Top = 4
      Width = 97
      Height = 26
      Align = alLeft
      Caption = 'Critical update'
      TabOrder = 4
    end
    object chkBetaVer: TCheckBox
      AlignWithMargins = True
      Left = 203
      Top = 4
      Width = 97
      Height = 26
      Align = alLeft
      Caption = 'Is beta version'
      TabOrder = 5
    end
    object edtComment: TEdit
      AlignWithMargins = True
      Left = 427
      Top = 6
      Width = 214
      Height = 23
      Hint = 'News headline'
      Margins.Top = 5
      Margins.Bottom = 4
      Align = alClient
      TabOrder = 6
      TextHint = 'Comments'
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 88
    Width = 645
    Height = 200
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Caption = 'News'
    TabOrder = 3
    object edtHeadline: TEdit
      AlignWithMargins = True
      Left = 5
      Top = 20
      Width = 635
      Height = 23
      Hint = 'News headline'
      Align = alTop
      TabOrder = 0
      TextHint = 'Text headline'
    end
    object Memo: TMemo
      AlignWithMargins = True
      Left = 8
      Top = 52
      Width = 629
      Height = 140
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      TabStop = False
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Font.Quality = fqProof
      HideSelection = False
      MaxLength = 2147483632
      ParentFont = False
      ScrollBars = ssHorizontal
      TabOrder = 1
      WordWrap = False
    end
  end
end
