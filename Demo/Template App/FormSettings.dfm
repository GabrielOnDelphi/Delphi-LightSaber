object frmSettings: TfrmSettings
  Left = 891
  Top = 327
  AlphaBlendValue = 249
  BorderIcons = [biSystemMenu]
  Caption = 'Settings'
  ClientHeight = 514
  ClientWidth = 782
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
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 5
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 17
  object pgCtrl: TPageControl
    Left = 0
    Top = 0
    Width = 782
    Height = 514
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    object tabSystem: TTabSheet
      Caption = 'System'
      DesignSize = (
        774
        482)
      object grpSystem: TGroupBox
        Left = 196
        Top = 103
        Width = 381
        Height = 204
        Anchors = []
        Caption = 'System'
        TabOrder = 0
        DesignSize = (
          381
          204)
        object btnDesktopShortcut: TButton
          Left = 122
          Top = 158
          Width = 145
          Height = 33
          Hint = 'Creates a desktop shortcut'
          Anchors = [akBottom]
          Caption = 'Desktop shortcut'
          TabOrder = 1
          WordWrap = True
          OnClick = btnDesktopShortcutClick
        end
        object chkAutoStartUp: TCubicCheckBox
          Left = 60
          Top = 39
          Width = 173
          Height = 17
          Caption = 'Start at Windows startup '
          TabOrder = 0
          OnClick = chkAutoStartUpClick
          AutoSize = True
        end
        object chkTrayIcon: TCubicCheckBox
          Left = 60
          Top = 107
          Width = 119
          Height = 17
          Caption = 'Minimize to tray'
          TabOrder = 2
          AutoSize = True
        end
        object chkStartMinim: TCubicCheckBox
          Left = 60
          Top = 73
          Width = 115
          Height = 17
          Caption = 'Start minimized'
          TabOrder = 3
          AutoSize = True
        end
      end
      object Path: TCubicPathEdit
        Left = 196
        Top = 330
        Width = 381
        Height = 48
        Anchors = []
        Caption = 'Folder'
        TabOrder = 1
      end
      object btnCrash: TButton
        Left = 656
        Top = 12
        Width = 101
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Generate crash'
        TabOrder = 2
        Visible = False
        OnClick = btnCrashClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Help'
      ImageIndex = 1
      DesignSize = (
        774
        482)
      object GroupHelp: TGroupBox
        Left = 214
        Top = 130
        Width = 345
        Height = 221
        Anchors = []
        Caption = 'Help'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object lblHintHide: TLabel
          Left = 36
          Top = 173
          Width = 115
          Height = 17
          Caption = 'Hide hint after (ms):'
        end
        object radHintsOff: TCubicRadioButton
          Left = 36
          Top = 63
          Width = 225
          Height = 13
          Hint = 
            'Don'#39't use the embeded help system. No pop-up will appear and no ' +
            'info will be displayed in status bar.'
          Caption = 'Turn off the embeded help system'
          TabOrder = 0
          AutoSize = True
        end
        object radHintsTooltips: TCubicRadioButton
          Left = 36
          Top = 92
          Width = 154
          Height = 13
          Hint = 
            'Show a hint when you hold the mouse cursor over an item (button,' +
            ' checkbox, etc)'
          Caption = 'Show help as tool-tips'
          TabOrder = 1
          AutoSize = True
        end
        object radHintsStatBar: TCubicRadioButton
          Left = 36
          Top = 120
          Width = 188
          Height = 13
          Hint = 'Show information about an item in status bar.'
          Caption = 'Show help in status bar also'
          Checked = True
          TabOrder = 2
          TabStop = True
          AutoSize = True
        end
        object spnHideHint: TSpinEdit
          Left = 156
          Top = 170
          Width = 50
          Height = 27
          Hint = 'miliseconds'
          Increment = 100
          MaxValue = 20000
          MinValue = 800
          ParentColor = True
          TabOrder = 3
          Value = 4000
          OnChange = spnHideHintChange
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Interface'
      ImageIndex = 2
      DesignSize = (
        774
        482)
      object GroupBox1: TGroupBox
        Left = 242
        Top = 144
        Width = 290
        Height = 194
        Anchors = []
        Caption = 'Interface'
        TabOrder = 0
        DesignSize = (
          290
          194)
        object btnSkins: TButton
          Left = 86
          Top = 83
          Width = 124
          Height = 33
          Anchors = []
          Caption = 'Skins'
          TabOrder = 0
          OnClick = btnSkinsClick
        end
        object btnFont: TButton
          Left = 86
          Top = 126
          Width = 124
          Height = 33
          Anchors = []
          Caption = 'Font'
          TabOrder = 1
          OnClick = btnFontClick
        end
      end
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdApplyButton]
    OnApply = FontDialogApply
    Left = 566
    Top = 31
  end
end
