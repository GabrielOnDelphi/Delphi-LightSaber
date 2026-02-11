object frmPower: TfrmPower
  Left = 503
  Top = 558
  Caption = 'Power'
  ClientHeight = 446
  ClientWidth = 488
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnDestroy = FormDestroy
  TextHeight = 15
  object Container: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 482
    Height = 440
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      482
      440)
    object grpPowerInfo: TCubicGroupBox
      Left = 14
      Top = 52
      Width = 454
      Height = 150
      Anchors = []
      Caption = 'CPU'
      TabOrder = 0
      DesignSize = (
        454
        150)
      object pnlCPU: TPanel
        AlignWithMargins = True
        Left = 117
        Top = 38
        Width = 219
        Height = 52
        Hint = 'CPU overload detection'
        Anchors = [akTop]
        BevelInner = bvSpace
        BevelOuter = bvLowered
        ParentBackground = False
        TabOrder = 0
        object lblCPU: TLabel
          Left = 2
          Top = 2
          Width = 215
          Height = 14
          Align = alTop
          Alignment = taCenter
          Caption = 'CPU utilization (all programs)'
          Font.Charset = EASTEUROPE_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object proCpu: TProgressBar
          AlignWithMargins = True
          Left = 5
          Top = 19
          Width = 209
          Height = 28
          Align = alClient
          TabOrder = 0
        end
      end
      object spnMaxCPU: TCubicSpinEditSplit
        AlignWithMargins = True
        Left = 60
        Top = 116
        Width = 359
        Height = 23
        Hint = 
          'Useful when playing games or other CPU-hungry applications.'#13#10'Set' +
          ' it to 101% to disable it.'
        Anchors = [akBottom]
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        ShowCaption = False
        TabOrder = 1
        Spin.AlignWithMargins = True
        Spin.Left = 291
        Spin.Top = 0
        Spin.Width = 50
        Spin.Height = 24
        Spin.Margins.Top = 0
        Spin.Margins.Bottom = 0
        Spin.Align = alLeft
        Spin.MaxValue = 101
        Spin.MinValue = 2
        Spin.TabOrder = 0
        Spin.Value = 95
        Caption1 = 'Don'#39't change the wallpaper if CPU utilization is above'
        Caption2 = '%'
        Value = 95
      end
    end
    object grpPowerOpt: TCubicGroupBox
      Left = 14
      Top = 229
      Width = 454
      Height = 158
      Anchors = []
      Caption = 'Battery'
      TabOrder = 1
      object lblBatProc: TLabel
        Left = 26
        Top = 56
        Width = 82
        Height = 15
        Caption = 'Battery status: ?'
      end
      object lblPwrType: TLabel
        Left = 26
        Top = 30
        Width = 78
        Height = 15
        Hint = 'Power type'
        Caption = 'Power status: ?'
        Layout = tlCenter
      end
      object chkOutOfJuice: TCubicCheckBox
        Left = 26
        Top = 85
        Width = 327
        Height = 17
        Hint = 
          'Print a message directly on the screen to inform you that your l' +
          'aptop has switched to batteries.'#13#10'Useful to prevent battery drai' +
          'n if the computer is accidentally disconnected from grid.'
        Caption = 'Announce when the laptop switches from AC to batteries'
        TabOrder = 0
        AutoSize = True
      end
      object chkBatteries: TCubicCheckBox
        Left = 26
        Top = 115
        Width = 315
        Height = 17
        Hint = 
          'Check this if battery life is very important to you. Alternative' +
          'ly, you can choose to change the wallpaper at a bigger interval ' +
          '(15 minutes or higher).'
        Caption = 'Don'#39't change the wallpaper when running on batteries.'
        TabOrder = 1
        AutoSize = True
      end
    end
  end
  object TimerPwr: TTimer
    Enabled = False
    OnTimer = TimerPwrTimer
    Left = 48
    Top = 14
  end
end
