object frmTestIO: TfrmTestIO
  Left = 0
  Top = 0
  Caption = 'Test ccIO'
  ClientHeight = 335
  ClientWidth = 630
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
  OnCreate = FormCreate
  TextHeight = 13
  object Memo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 624
    Height = 329
    Align = alClient
    Lines.Strings = (
      'Memo')
    TabOrder = 0
  end
end
