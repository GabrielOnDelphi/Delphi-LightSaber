object frmSettings: TfrmSettings
  Left = 891
  Top = 327
  AlphaBlendValue = 249
  BorderIcons = [biSystemMenu]
  Caption = 'Settings'
  ClientHeight = 511
  ClientWidth = 770
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
  OldCreateOrder = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 5
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 17
  object pgCtrl: TPageControl
    Left = 0
    Top = 0
    Width = 770
    Height = 511
    ActivePage = tabSystem
    Align = alClient
    TabOrder = 0
    object tabSystem: TTabSheet
      Caption = 'System'
      DesignSize = (
        762
        479)
      object grpSystem: TGroupBox
        Left = 180
        Top = 99
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
          Height = 17
          Caption = 'Start at Windows startup '
          TabOrder = 0
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
        object chkLogOnError: TCheckBox
          AlignWithMargins = True
          Left = 60
          Top = 140
          Width = 145
          Height = 17
          Hint = 'Show the Log window when it receives error or warning messages.'
          Caption = 'Show log on error'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
      end
      object Path: TCubicPathEdit
        Left = 82
        Top = 364
        Width = 579
        Height = 49
        Hint = 
          'User defined path where to save (large) files. Useful when the p' +
          'rogram needs to save large amounts of data that we don'#39't want to' +
          ' put on a SSD drive.'
        Anchors = [akLeft, akRight]
        Caption = 'Data folder'
        TabOrder = 1
      end
      object btnCrash: TButton
        Left = 624
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
    object tabHelp: TTabSheet
      Caption = 'Help'
      ImageIndex = 1
      DesignSize = (
        762
        479)
      object GroupHelp: TGroupBox
        Left = 198
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
          Height = 15
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
          Height = 15
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
          Height = 15
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
    object tabInterface: TTabSheet
      Caption = 'Interface'
      ImageIndex = 2
      DesignSize = (
        762
        479)
      object GroupBox1: TGroupBox
        Left = 226
        Top = 112
        Width = 290
        Height = 226
        Anchors = []
        Caption = 'Interface'
        TabOrder = 0
        DesignSize = (
          290
          226)
        object lblOpacity: TLabel
          Left = 84
          Top = 51
          Width = 93
          Height = 17
          Caption = 'Window opacity'
        end
        object btnSkins: TButton
          Left = 86
          Top = 117
          Width = 124
          Height = 33
          Anchors = []
          Caption = 'Skins'
          TabOrder = 0
          OnClick = btnSkinsClick
        end
        object btnFont: TButton
          Left = 86
          Top = 156
          Width = 124
          Height = 33
          Anchors = []
          Caption = 'Font'
          TabOrder = 1
          OnClick = btnFontClick
        end
        object spnOpacity: TSpinEdit
          Left = 165
          Top = 74
          Width = 50
          Height = 27
          Hint = 'miliseconds'
          Increment = 10
          MaxValue = 255
          MinValue = 50
          ParentColor = True
          TabOrder = 2
          Value = 250
          OnChange = spnOpacityChange
        end
      end
    end
    object tabUserDefined: TTabSheet
      Caption = 'User defined'
      ImageIndex = 3
      DesignSize = (
        762
        479)
      object grpUser: TGroupBox
        Left = 200
        Top = 130
        Width = 345
        Height = 221
        Anchors = []
        Caption = 'User defined'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object Label1: TLabel
          Left = 36
          Top = 137
          Width = 75
          Height = 17
          Caption = 'User defined'
        end
        object spnUser: TSpinEdit
          Left = 126
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
          Left = 36
          Top = 84
          Width = 97
          Height = 17
          Caption = 'User defined'
          TabOrder = 1
        end
      end
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    Options = [fdEffects, fdApplyButton]
    OnApply = FontDialogApply
    Left = 566
    Top = 31
  end
end
