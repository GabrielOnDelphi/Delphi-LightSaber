object frmTestIO: TfrmTestIO
  Left = 0
  Top = 0
  Caption = 'Test ccIO'
  ClientHeight = 384
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
  object pnlStream: TPanel
    Left = 0
    Top = 0
    Width = 626
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnStreamWrite: TButton
      AlignWithMargins = True
      Left = 3
      Top = 8
      Width = 308
      Height = 25
      Caption = 'TLightStream: Write'
      TabOrder = 0
      OnClick = btnStreamWriteClick
    end
    object btnStreamRead: TButton
      AlignWithMargins = True
      Left = 317
      Top = 8
      Width = 306
      Height = 25
      Caption = 'TLightStream: Read'
      TabOrder = 1
      OnClick = btnStreamReadClick
    end
  end
  object Button1: TButton
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 620
    Height = 25
    Align = alTop
    Caption = 'TEST'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 75
    Width = 620
    Height = 306
    Align = alClient
    Lines.Strings = (
      'Memo')
    TabOrder = 2
  end
end
