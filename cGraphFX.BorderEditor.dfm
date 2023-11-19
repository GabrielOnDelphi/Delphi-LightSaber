object frmBorderEditor: TfrmBorderEditor
  Left = 829
  Top = 286
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Automatic background color settings'
  ClientHeight = 423
  ClientWidth = 550
  Color = clBtnFace
  Constraints.MinHeight = 380
  Constraints.MinWidth = 480
  DoubleBuffered = True
  ParentFont = True
  OldCreateOrder = False
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Container: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 544
    Height = 388
    Align = alClient
    Anchors = []
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      544
      388)
    object lblWarning: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 536
      Height = 13
      Hint = #39'Fade to color'#39' is CPU hungry'
      Align = alTop
      Alignment = taCenter
      Caption = 
        'Attention: This feature is CPU hungry. It may need few seconds t' +
        'o process.'
      Color = 11599867
      ParentColor = False
      Transparent = True
      Visible = False
    end
    object btnReset: TButton
      Left = 502
      Top = 4
      Width = 40
      Height = 19
      Anchors = [akTop, akRight]
      Caption = 'Reset'
      TabOrder = 4
      OnClick = btnResetClick
    end
    object grpFillFade: TGroupBox
      Left = 277
      Top = 45
      Width = 237
      Height = 298
      Anchors = []
      Caption = 'Fill/Fade'
      TabOrder = 2
      DesignSize = (
        237
        298)
      object radFade: TRadioButton
        Left = 19
        Top = 60
        Width = 169
        Height = 16
        Caption = 'Fade to color (color transition)'
        TabOrder = 0
        OnClick = SettingsChanged
        AutoSize = True
      end
      object radFill: TRadioButton
        Left = 21
        Top = 36
        Width = 143
        Height = 16
        Hint = 'This is fast'
        Caption = 'Solid color (no transition)'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = SettingsChanged
        AutoSize = True
      end
      object grpFadeParams: TGroupBox
        Left = 29
        Top = 97
        Width = 195
        Height = 178
        Anchors = [akRight]
        Caption = 'Fade parameters'
        TabOrder = 2
        Visible = False
        BoldCaption = False
        object lblFadeSpeed: TLabel
          Left = 92
          Top = 50
          Width = 56
          Height = 13
          Caption = 'Fade speed'
          Transparent = True
        end
        object lblEdgeSmear: TLabel
          Left = 92
          Top = 78
          Width = 56
          Height = 13
          Caption = 'Edge smear'
          Transparent = True
        end
        object lblNeighborWeight: TLabel
          Left = 92
          Top = 107
          Width = 84
          Height = 13
          Caption = 'Neighbour weight'
          Transparent = True
        end
        object lblFuzzy: TLabel
          Left = 92
          Top = 136
          Width = 50
          Height = 13
          Caption = 'Fuzzyness'
          Transparent = True
        end
        object spnNeighborWeight: TFloatSpinEdit
          Left = 32
          Top = 104
          Width = 53
          Height = 20
          Hint = 'Weight of the Neighbour pixels.'#13#10'Default: 1'
          MinValue = -2.000000000000000000
          MaxValue = 2.000000000000000000
          Increment = 0.005000000000000000
          Decimals = 20
          ShowHint = True
          Enabled = True
          OnChange = SettingsChanged
          Value = 1.000000000000000000
          TabOrder = 2
        end
        object spnFallSpeed: TFloatSpinEdit
          Left = 32
          Top = 46
          Width = 53
          Height = 21
          Hint = 
            'This value tells how slow the transition between the edges of th' +
            'e images and the background color should be. '#13#10'The higher the va' +
            'lue the faster the transition.'#13#10#13#10'NOTE! This feature will slow d' +
            'own the program A LOT!'#13#10'Default: 2'
          MinValue = -2147483647.000000000000000000
          MaxValue = 2147483647.000000000000000000
          Increment = 0.300000000000000000
          Decimals = 20
          ShowHint = True
          Enabled = True
          OnChange = SettingsChanged
          Value = 2.000000000000000000
          TabOrder = 0
        end
        object spnNeighborDistance: TSpinEdit
          Left = 32
          Top = 133
          Width = 55
          Height = 23
          Hint = 'Neighbour distance (fuzzyness).'#13#10'Default: 2'
          AutoSize = False
          MaxValue = 8
          MinValue = 0
          TabOrder = 3
          Value = 2
          OnChange = SettingsChanged
        end
        object spnEdgeSmear: TSpinEdit
          Left = 32
          Top = 75
          Width = 55
          Height = 23
          Hint = 
            'Makes the edge of the image to smear towards the borders of the ' +
            'screen. '#13#10'Default: 0 '
          AutoSize = False
          MaxLength = 3
          MaxValue = 100
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
      object Button1: TButton
        Left = 183
        Top = 82
        Width = 33
        Height = 18
        Hint = 'Advanced settings'
        Caption = '...'
        TabOrder = 3
        OnClick = Button1Click
      end
    end
    object grpShape: TGroupBox
      Left = 29
      Top = 220
      Width = 213
      Height = 124
      Anchors = []
      Caption = 'Background shape'
      TabOrder = 1
      object radShapeSolid: TRadioButton
        Left = 35
        Top = 41
        Width = 46
        Height = 16
        Hint = 
          'The color used will be the average image color (do not confuse w' +
          'ith '#39'dominant'#39' image color)'
        Caption = 'Solid'
        TabOrder = 0
        OnClick = SettingsChanged
        AutoSize = True
      end
      object radShapeRect: TRadioButton
        Left = 35
        Top = 64
        Width = 77
        Height = 16
        Caption = 'Rectangles'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = SettingsChanged
        AutoSize = True
      end
      object radShapeTraing: TRadioButton
        Left = 35
        Top = 87
        Width = 67
        Height = 16
        Caption = 'Triangles'
        TabOrder = 2
        OnClick = SettingsChanged
        AutoSize = True
      end
    end
    object grpBkgFill: TGroupBox
      Left = 29
      Top = 44
      Width = 213
      Height = 161
      Hint = 
        'This applies when the image is smaller than the desktop and the ' +
        'program has to fill the "empty" area with some color.'
      Anchors = []
      Caption = 'Background fill color'
      TabOrder = 0
      object lblPixTolerance: TLabel
        Left = 60
        Top = 118
        Width = 77
        Height = 13
        Caption = 'Color tolerance:'
        Transparent = True
      end
      object btnBackgroundClr: TButton
        Left = 145
        Top = 63
        Width = 33
        Height = 19
        Hint = 
          'Choose desktop background color.'#13#10'This color will also be used i' +
          'n the preview panel.'
        Caption = '...'
        TabOrder = 3
        OnClick = btnBackgroundClrClick
      end
      object radUserColor: TRadioButton
        Left = 20
        Top = 65
        Width = 111
        Height = 16
        Hint = 
          'The desktop background will have a single color, as defined by u' +
          'ser.'
        Caption = 'User defined color'
        TabOrder = 1
        OnClick = SettingsChanged
        AutoSize = True
      end
      object radAutoDetBorder: TRadioButton
        Left = 20
        Top = 90
        Width = 151
        Height = 16
        Hint = 
          'Autodetect dominant color of the borders. If the image has (for ' +
          'example) red color on the left border and blue on the right bord' +
          'er, then the desktop background color will be red on the left an' +
          'd blue on the right.'
        Caption = 'Autodetected border color'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = SettingsChanged
        AutoSize = True
      end
      object radImageAverage: TRadioButton
        Left = 20
        Top = 39
        Width = 123
        Height = 16
        Hint = 'The desktop background will have a single color.'
        Caption = 'Image average color'
        TabOrder = 0
        OnClick = SettingsChanged
        AutoSize = True
      end
      object spnPixTolerance: TSpinEdit
        Left = 152
        Top = 116
        Width = 38
        Height = 22
        Hint = 
          'Dominant color '#13#10#13#10'Considers two neighbour pixels identical if c' +
          'olor difference between them is smaller than this value.'#13#10'Increa' +
          'se this value if the background color is not determined correctl' +
          'y. '#13#10'Decrease this value if the background color could not be de' +
          'termined (at all).'#13#10' '#13#10'Range: 1-200'#13#10'Default value: 8'#13#10
        MaxValue = 200
        MinValue = 1
        TabOrder = 4
        Value = 8
        OnChange = SettingsChanged
      end
    end
    object pnlExplain: TPanel
      Left = 479
      Top = 162
      Width = 513
      Height = 143
      Hint = 'Click to close.'#13#10'(This message will only be shown once)'
      Anchors = []
      BevelInner = bvSpace
      BevelKind = bkSoft
      TabOrder = 3
      Visible = False
      object lblExplain: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 19
        Width = 499
        Height = 115
        Cursor = crHandPoint
        Margins.Top = 0
        Align = alClient
        Alignment = taCenter
        Caption = 
          'When your wallpaper is smaller than the desktop, BioniX can auto' +
          'matically fill the desktop with a color, instead of showing a bl' +
          'ank area. You can maually select a color or let BioniX auto dete' +
          'rmine the best color (based on the palette of the displayed wall' +
          'paper).'
        Color = 8454143
        ParentColor = False
        Transparent = False
        Layout = tlCenter
        WordWrap = True
        OnClick = lblExplainClick
      end
      object Label1: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 499
        Height = 14
        Cursor = crHandPoint
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Hint!'
        Color = 8454143
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = False
        Layout = tlCenter
        WordWrap = True
        OnClick = lblExplainClick
      end
    end
  end
  object pnlBottom: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 397
    Width = 544
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnApply: TButton
      Left = 472
      Top = 0
      Width = 72
      Height = 23
      Align = alRight
      Caption = 'Apply'
      TabOrder = 2
      OnClick = btnApplyClick
    end
    object btnOk: TButton
      Left = 328
      Top = 0
      Width = 72
      Height = 23
      Align = alRight
      Caption = 'Ok'
      Default = True
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 400
      Top = 0
      Width = 72
      Height = 23
      Align = alRight
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object ColorDialog: TColorDialog
    Color = 3810591
    CustomColors.Strings = (
      'ColorA=FFFFFFFF'
      'ColorB=FFFFFFFF'
      'ColorC=FFFFFFFF'
      'ColorD=FFFFFFFF'
      'ColorE=FFFFFFFF'
      'ColorF=FFFFFFFF'
      'ColorG=FFFFFFFF'
      'ColorH=FFFFFFFF'
      'ColorI=FFFFFFFF'
      'ColorJ=FFFFFFFF'
      'ColorK=FFFFFFFF'
      'ColorL=FFFFFFFF'
      'ColorM=FFFFFFFF'
      'ColorN=FFFFFFFF'
      'ColorO=FFFFFFFF'
      'ColorP=FFFFFFFF')
    Options = [cdFullOpen, cdAnyColor]
    Left = 239
    Top = 20
  end
end
