object frmShowMsgAsync: TfrmShowMsgAsync
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Caption'
  ClientHeight = 112
  ClientWidth = 408
  Color = clBtnFace
  Constraints.MaxHeight = 300
  Constraints.MaxWidth = 700
  Constraints.MinHeight = 140
  Constraints.MinWidth = 150
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnClose = FormClose
  TextHeight = 13
  object lblMessage: TLabel
    AlignWithMargins = True
    Left = 9
    Top = 20
    Width = 390
    Height = 13
    Margins.Left = 9
    Margins.Top = 20
    Margins.Right = 9
    Align = alTop
    Caption = 'Msg'
    ParentShowHint = False
    ShowHint = True
    Layout = tlCenter
    WordWrap = True
    ExplicitWidth = 19
  end
  object Panel1: TPanel
    Left = 0
    Top = 71
    Width = 408
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      408
      41)
    object btnOK: TButton
      Left = 158
      Top = 5
      Width = 93
      Height = 31
      Anchors = [akBottom]
      Caption = 'Ok'
      TabOrder = 0
      OnClick = btnOKClick
    end
  end
end
