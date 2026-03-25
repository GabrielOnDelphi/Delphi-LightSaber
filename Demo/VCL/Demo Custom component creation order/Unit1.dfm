object Form1: TForm1
  Left = 863
  Top = 437
  ActiveControl = Button1
  Caption = 'Demo program'
  ClientHeight = 199
  ClientWidth = 525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  DesignSize = (
    525
    199)
  TextHeight = 13
  object Button1: TButton
    Left = 381
    Top = 8
    Width = 136
    Height = 39
    Caption = 'TCreationOrderTest'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 381
    Top = 53
    Width = 136
    Height = 39
    Anchors = [akTop, akRight]
    Caption = 'CreateSpinEdit'
    TabOrder = 1
    OnClick = Button2Click
  end
end
