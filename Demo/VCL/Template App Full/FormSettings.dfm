object frmSettings: TfrmSettings
  Left = 891
  Top = 327
  AlphaBlendValue = 249
  BorderIcons = [biSystemMenu]
  Caption = 'Settings'
  ClientHeight = 418
  ClientWidth = 600
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
  TextHeight = 17
  object pgCtrl: TPageControl
    Left = 0
    Top = 0
    Width = 600
    Height = 418
    ActivePage = tabInterface
    Align = alClient
    TabOrder = 0
    object tabSystem: TTabSheet
      Caption = 'System'
      DesignSize = (
        592
        386)
      object grpSystem: TGroupBox
        Left = 88
        Top = 53
        Width = 381
        Height = 246
        Anchors = []
        Caption = 'System'
        TabOrder = 0
        DesignSize = (
          381
          246)
        object btnDesktopShortcut: TButton
          Left = 122
          Top = 200
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
          Height = 19
          Caption = 'Start at Windows startup '
          TabOrder = 0
          AutoSize = True
        end
        object chkTrayIcon: TCubicCheckBox
          Left = 60
          Top = 107
          Width = 119
          Height = 19
          Caption = 'Minimize to tray'
          TabOrder = 2
          AutoSize = True
        end
        object chkStartMinim: TCubicCheckBox
          Left = 60
          Top = 73
          Width = 115
          Height = 19
          Caption = 'Start minimized'
          TabOrder = 3
          AutoSize = True
        end
        object chkLogOnError: TCheckBox
          AlignWithMargins = True
          Left = 60
          Top = 140
          Width = 182
          Height = 19
          Hint = 'Show the Log window when it receives error or warning messages.'
          Caption = 'Show log on error'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
      end
      object Path: TCubicPathEdit
        Left = 88
        Top = 306
        Width = 381
        Height = 49
        Hint = 
          'User defined path where to save (large) files. Useful when the p' +
          'rogram needs to save large amounts of data that we don'#39't want to' +
          ' put on a SSD drive.'
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Data folder'
        TabOrder = 1
      end
      object btnCrash: TButton
        Left = 486
        Top = 3
        Width = 101
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Generate crash'
        TabOrder = 2
        Visible = False
        OnClick = btnCrashClick
      end
    end
    object tabHelp: TTabSheet
      Caption = 'Help'
      ImageIndex = 1
      DesignSize = (
        592
        386)
      object GroupHelp: TGroupBox
        Left = 106
        Top = 83
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
          Height = 17
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
          Height = 17
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
          Height = 17
          Hint = 'Show information about an item in status bar.'
          Caption = 'Show help in status bar also'
          Checked = True
          TabOrder = 2
          TabStop = True
          AutoSize = True
        end
        object spnHideHint: TSpinEdit
          Left = 159
          Top = 170
          Width = 77
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
    object tabInterface: TTabSheet
      Caption = 'Interface'
      ImageIndex = 2
      DesignSize = (
        592
        386)
      object GroupBox1: TGroupBox
        Left = 123
        Top = 80
        Width = 347
        Height = 226
        Anchors = []
        Caption = 'User interface'
        TabOrder = 0
        DesignSize = (
          347
          226)
        object btnSkins: TButton
          Left = 115
          Top = 117
          Width = 124
          Height = 33
          Anchors = []
          Caption = 'Skins'
          TabOrder = 0
          OnClick = btnSkinsClick
        end
        object btnFont: TButton
          Left = 115
          Top = 156
          Width = 124
          Height = 33
          Anchors = []
          Caption = 'Font'
          TabOrder = 1
          OnClick = btnFontClick
        end
        object spnOpacity: TCubicSpinEditSplit
          AlignWithMargins = True
          Left = 99
          Top = 64
          Width = 127
          Height = 25
          BevelOuter = bvNone
          ParentColor = True
          ShowCaption = False
          TabOrder = 2
          Spin.AlignWithMargins = True
          Spin.Left = 63
          Spin.Top = 0
          Spin.Width = 50
          Spin.Height = 25
          Spin.Hint = 'miliseconds'
          Spin.Margins.Top = 0
          Spin.Margins.Bottom = 0
          Spin.Align = alLeft
          Spin.MaxValue = 255
          Spin.MinValue = 50
          Spin.TabOrder = 0
          Spin.Value = 250
          Spin.OnChange = spnOpacity2Change
          Caption1 = 'Window opacity'
          Caption2 = '%'
          Value = 250
        end
      end
    end
    object tabUserDefined: TTabSheet
      Caption = 'User defined'
      ImageIndex = 3
      DesignSize = (
        592
        386)
      object grpUser: TGroupBox
        Left = 108
        Top = 83
        Width = 345
        Height = 221
        Anchors = []
        Caption = 'User defined'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object Label1: TLabel
          Left = 56
          Top = 137
          Width = 75
          Height = 17
          Caption = 'User defined'
        end
        object spnUser: TSpinEdit
          Left = 146
          Top = 134
          Width = 50
          Height = 27
          Hint = 'miliseconds'
          Increment = 10
          MaxValue = 0
          MinValue = 0
          ParentColor = True
          TabOrder = 0
          Value = 100
          OnChange = spnHideHintChange
        end
        object chkUser: TCheckBox
          Left = 56
          Top = 84
          Width = 122
          Height = 17
          Caption = 'User defined'
          TabOrder = 1
        end
      end
    end
  end
  object FontDialog: TFontDialog
    OnClose = FontDialogClose
    OnShow = FontDialogShow
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    Options = [fdEffects, fdApplyButton]
    OnApply = FontDialogApply
    Left = 526
    Top = 71
  end
end
