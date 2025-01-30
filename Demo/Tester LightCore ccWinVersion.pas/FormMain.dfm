object frmTester: TfrmTester
  Left = 450
  Top = 311
  AlphaBlend = True
  AlphaBlendValue = 250
  Anchors = []
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Show Windows version'
  ClientHeight = 543
  ClientWidth = 546
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 490
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
  OnCreate = FormCreate
  TextHeight = 15
  object pnlRight: TPanel
    Left = 428
    Top = 0
    Width = 118
    Height = 543
    Align = alRight
    TabOrder = 0
    object btnStart: TButton
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 110
      Height = 45
      Align = alTop
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = btnStartClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 428
    Height = 543
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
end
