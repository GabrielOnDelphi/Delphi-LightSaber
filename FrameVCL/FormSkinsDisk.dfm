object frmSkinDisk: TfrmSkinDisk
  Left = 1104
  Top = 300
  AlphaBlend = True
  AlphaBlendValue = 249
  Anchors = []
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Skin selector'
  ClientHeight = 464
  ClientWidth = 304
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 200
  DoubleBuffered = True
  ParentFont = True
  KeyPreview = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 5
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  TextHeight = 15
  object lblTop: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 6
    Width = 298
    Height = 13
    Margins.Top = 6
    Margins.Bottom = 1
    Align = alTop
    Caption = 'Click skin to load it'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Layout = tlBottom
    OnClick = lblTopClick
  end
  object lblMoreSkinsTrial: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 395
    Width = 298
    Height = 30
    Align = alBottom
    Alignment = taCenter
    Caption = 'Hint: Registered users can download more skins from our web site'
    Layout = tlCenter
    WordWrap = True
  end
  object lBox: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 23
    Width = 298
    Height = 362
    Align = alClient
    ItemHeight = 15
    ScrollWidth = 6
    TabOrder = 1
    OnClick = lBoxClick
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 388
    Width = 304
    Height = 4
    Align = alBottom
    TabOrder = 0
  end
  object pnlBtm: TPanel
    Left = 0
    Top = 428
    Width = 304
    Height = 36
    Align = alBottom
    TabOrder = 2
    object btnOK: TButton
      AlignWithMargins = True
      Left = 210
      Top = 4
      Width = 90
      Height = 28
      Hint = 
        'Save settings & close the window.'#13#10'Hint: You can also close the ' +
        'window with <Enter>'
      Align = alRight
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnSkinEditor: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 129
      Height = 28
      Hint = 
        'Do you feel creative?'#13#10'Use this skin editor to create your own s' +
        'kins. '#13#10'Feel free to share them with us. We will upload them on ' +
        'our website together with your name.'#13#10#13#10'Please see the Help menu' +
        ' in the skin editor for details.'
      Align = alLeft
      Caption = 'Create your own skins'
      TabOrder = 1
      OnClick = btnSkinEditorClick
    end
  end
end
