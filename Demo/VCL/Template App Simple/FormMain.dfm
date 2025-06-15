object MainForm: TMainForm
  Left = 891
  Top = 327
  AlphaBlendValue = 249
  Caption = 'Template App'
  ClientHeight = 370
  ClientWidth = 680
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 350
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  GlassFrame.Enabled = True
  Menu = MainMenu
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 5
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 17
  object pgCtrl: TPageControl
    Left = 0
    Top = 0
    Width = 680
    Height = 370
    ActivePage = tabMain
    Align = alClient
    TabOrder = 0
    object tabMain: TTabSheet
      Caption = 'Main tab'
      object mmo: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 475
        Height = 332
        Align = alClient
        TabOrder = 1
      end
      object pnlRight: TPanel
        AlignWithMargins = True
        Left = 484
        Top = 3
        Width = 185
        Height = 332
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object btnStart: TButton
          AlignWithMargins = True
          Left = 3
          Top = 276
          Width = 179
          Height = 53
          Align = alBottom
          Caption = 'START'
          TabOrder = 0
          OnClick = btnSTARTClick
        end
        object CheckBox1: TCheckBox
          Left = 8
          Top = 4
          Width = 97
          Height = 17
          Caption = 'Checkbox'
          TabOrder = 1
        end
      end
    end
    object tabSecondary: TTabSheet
      Caption = 'Another tab'
      ImageIndex = 1
    end
  end
  object MainMenu: TMainMenu
    Left = 63
    Top = 51
  end
  object Actions: TActionList
    Left = 130
    Top = 51
  end
end
