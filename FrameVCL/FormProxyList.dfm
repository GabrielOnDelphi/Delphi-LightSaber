object frmProxyList: TfrmProxyList
  Left = 1171
  Top = 179
  Caption = 'Proxy settings'
  ClientHeight = 464
  ClientWidth = 624
  Color = clWindow
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ShowHint = True
  TextHeight = 13
  object grpInternetType: TGroupBox
    Left = 13
    Top = 128
    Width = 204
    Height = 190
    Caption = 'Internet access'
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 0
    object radGateway: TCubicRadioButton
      Left = 18
      Top = 86
      Width = 67
      Height = 17
      Hint = 'Gateway'
      Caption = 'Gateway'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
      OnClick = ConnectionTypeChanged
      AutoSize = True
    end
    object radDirect: TCubicRadioButton
      Left = 18
      Top = 56
      Width = 120
      Height = 17
      Hint = 'Direct Internet connection'
      Caption = 'Direct connection'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      OnClick = ConnectionTypeChanged
      AutoSize = True
    end
    object radProxyList: TCubicRadioButton
      Left = 18
      Top = 115
      Width = 125
      Height = 17
      Hint = 'Use the provided list of proxies.'
      Caption = 'Use list of proxies'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 2
      OnClick = ConnectionTypeChanged
      AutoSize = True
    end
    object edtGateway: TEdit
      Left = 87
      Top = 85
      Width = 99
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 4
      Text = '192.168.0.1:80'
    end
    object btnAutoDetect: TButton
      Left = 60
      Top = 154
      Width = 79
      Height = 28
      Caption = 'Auto detect'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 3
      Visible = False
      OnClick = btnAutoDetectClick
    end
  end
  object grpProxyList: TGroupBox
    AlignWithMargins = True
    Left = 227
    Top = 3
    Width = 394
    Height = 458
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Proxy list'
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 1
    Visible = False
    object mmoProxyList: TMemo
      Left = 2
      Top = -11
      Width = 390
      Height = 431
      Hint = 
        'The format accepted by this proxy list is:'#13#10' xxx.xxx.xxx.xxx:yy'#13 +
        #10#13#10'Example'#13#10' 66.122.234.1:80'
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      DoubleBuffered = True
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentDoubleBuffered = False
      ParentFont = False
      ParentShowHint = False
      ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 0
      WordWrap = False
    end
    object pnlBottom: TPanel
      Left = 2
      Top = 420
      Width = 390
      Height = 36
      Align = alBottom
      BevelInner = bvLowered
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      DesignSize = (
        390
        36)
      object btnSaveProxy: TButton
        Left = 11
        Top = 6
        Width = 75
        Height = 25
        Hint = 
          'Clean current proxy list and save it to disk.'#13#10'The list will be ' +
          'automatically loaded next time the program starts.'#13#10
        Anchors = [akLeft]
        Caption = 'Save'
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnSaveProxyClick
      end
      object btnLocate: TButton
        Left = 303
        Top = 6
        Width = 80
        Height = 25
        Hint = 'Open the folder that contains the proxy list'
        Anchors = [akRight]
        Caption = 'Open folder...'
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = btnLocateClick
      end
      object btnClean: TButton
        Left = 223
        Top = 6
        Width = 75
        Height = 25
        Anchors = [akRight]
        Caption = 'Clean'
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnCleanClick
      end
    end
  end
end
