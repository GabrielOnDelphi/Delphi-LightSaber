object frmTestIO: TfrmTestIO
  Left = 0
  Top = 0
  Caption = 'Test ccIO'
  ClientHeight = 334
  ClientWidth = 626
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  TextHeight = 13
  object Memo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 34
    Width = 620
    Height = 297
    Align = alClient
    Lines.Strings = (
      'Memo')
    TabOrder = 0
  end
  object Button1: TButton
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 620
    Height = 25
    Align = alTop
    Caption = 'TEST'
    TabOrder = 1
    OnClick = Button1Click
  end
end
