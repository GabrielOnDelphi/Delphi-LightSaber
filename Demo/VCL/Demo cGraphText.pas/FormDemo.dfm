object frmDemoShadow: TfrmDemoShadow
  Left = 646
  Top = 220
  ActiveControl = PageControl1
  Anchors = []
  Caption = 'Tester'
  ClientHeight = 711
  ClientWidth = 674
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 690
  DoubleBuffered = True
  ParentFont = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 4
  OnDestroy = FormDestroy
  OnResize = FormResize
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 674
    Height = 711
    ActivePage = tabShadow
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 796
    ExplicitHeight = 694
    object tabShadow: TTabSheet
      Caption = 'cGraphText'
      object imgShadow: TImage
        AlignWithMargins = True
        Left = 3
        Top = 18
        Width = 481
        Height = 645
        Align = alClient
        Center = True
        OnMouseDown = imgShadowMouseDown
        ExplicitLeft = 4
        ExplicitTop = 15
      end
      object lblCoord: TLabel
        Left = 0
        Top = 666
        Width = 666
        Height = 15
        Align = alBottom
        Caption = 'Click image to set text coordinates'
        Color = 8388863
        ParentColor = False
        Transparent = False
        ExplicitTop = 15
        ExplicitWidth = 182
      end
      object lblResizeWarn: TLabel
        Left = 0
        Top = 0
        Width = 666
        Height = 15
        Align = alTop
        Alignment = taCenter
        Caption = 'Form resized! You need to reload the original image!'
        Color = 33023
        ParentColor = False
        Transparent = False
        ExplicitWidth = 274
      end
      object CubicPanel1: TCubicPanel
        AlignWithMargins = True
        Left = 490
        Top = 18
        Width = 173
        Height = 645
        Align = alRight
        Alignment = taLeftJustify
        Caption = 'CubicPanel1'
        ShowCaption = False
        TabOrder = 0
        ExplicitLeft = 615
        ExplicitTop = 30
        ExplicitHeight = 634
        object Label2: TLabel
          Left = 1
          Top = 1
          Width = 171
          Height = 15
          Align = alTop
          Caption = 'Double click to load image'
          ExplicitLeft = -7
          ExplicitTop = -9
        end
        object btnShadowSimple: TButton
          AlignWithMargins = True
          Left = 4
          Top = 433
          Width = 165
          Height = 31
          Align = alTop
          Caption = 'DrawShadow3DSoft'
          TabOrder = 2
          OnClick = btnShadowSimpleClick
          ExplicitLeft = 1
          ExplicitTop = 152
          ExplicitWidth = 171
        end
        object btnShadow3D: TButton
          AlignWithMargins = True
          Left = 4
          Top = 470
          Width = 165
          Height = 31
          Align = alTop
          Caption = 'DrawShadow3DHard'
          TabOrder = 3
          OnClick = btnShadow3DClick
          ExplicitLeft = 1
          ExplicitTop = 183
          ExplicitWidth = 171
        end
        object btnOutline: TButton
          AlignWithMargins = True
          Left = 4
          Top = 533
          Width = 165
          Height = 21
          Align = alTop
          Caption = 'DrawTextOutline'
          TabOrder = 5
          OnClick = btnOutlineClick
          ExplicitLeft = 1
          ExplicitTop = 237
          ExplicitWidth = 171
        end
        object btnXor: TButton
          AlignWithMargins = True
          Left = 4
          Top = 507
          Width = 165
          Height = 20
          Align = alTop
          Caption = 'DrawTextXOR'
          TabOrder = 4
          OnClick = btnXorClick
          ExplicitLeft = 1
          ExplicitTop = 217
          ExplicitWidth = 171
        end
        object FileList: TCubicFileList
          Left = 1
          Top = 16
          Width = 171
          Height = 223
          Hint = 'Press the "Delete" key to delete the selected file from disk.'
          Align = alTop
          ItemHeight = 13
          Mask = '*.jpg;*.bmp;*.png;*.gif'
          TabOrder = 6
          OnDblClick = FileListDblClick
          ExplicitLeft = -7
          ExplicitTop = -40
        end
        object CubicPanel2: TCubicPanel
          AlignWithMargins = True
          Left = 1
          Top = 251
          Width = 171
          Height = 133
          Margins.Left = 0
          Margins.Top = 12
          Margins.Right = 0
          Margins.Bottom = 9
          Align = alTop
          Alignment = taLeftJustify
          ShowCaption = False
          TabOrder = 0
          ExplicitTop = 1
          object spnShadowOpac: TCubicSpinEdit
            Left = 10
            Top = 4
            Width = 47
            Height = 24
            Hint = 'ShadowOpacity'
            Increment = 10
            MaxValue = 255
            MinValue = 0
            TabOrder = 2
            Value = 10
            OnChange = ParametersChanged
          end
          object btnShadowBox: TButton
            AlignWithMargins = True
            Left = 4
            Top = 99
            Width = 163
            Height = 30
            Hint = 
              'Draws text in a semi-transparent rectangle with shadow text.'#13#10'Th' +
              'e shadow text is blended to the background and then blurred.'#13#10'Mi' +
              'ght be slow becuase of the alpha blending and because of the blu' +
              'r.'#13#10#13#10'  Parameters:'#13#10'     Opacity a value from 0-255. 0 => Shado' +
              'w is completelly transparent.'
            Align = alBottom
            Caption = 'Draw Shadow Box'
            TabOrder = 1
            OnClick = btnShadowBoxClick
            ExplicitLeft = 1
            ExplicitTop = 89
            ExplicitWidth = 169
          end
          object btnShadowBoxRect: TButton
            AlignWithMargins = True
            Left = 4
            Top = 63
            Width = 163
            Height = 30
            Hint = 
              'Draws text in a semi-transparent rectangle with shadow text.'#13#10'Th' +
              'e shadow text is blended to the background and then blurred.'#13#10'Mi' +
              'ght be slow becuase of the alpha blending and because of the blu' +
              'r.'#13#10#13#10'  Parameters:'#13#10'     Opacity a value from 0-255. 0 => Shado' +
              'w is completelly transparent.'
            Align = alBottom
            Caption = 'Draw Shadow Box Rect'
            TabOrder = 0
            OnClick = btnShadowBoxRectClick
            ExplicitLeft = 1
            ExplicitTop = 59
            ExplicitWidth = 169
          end
          object tglTopBtm: TToggleSwitch
            Left = 76
            Top = 5
            Width = 73
            Height = 20
            Hint = 'Draw text on top'
            State = tssOn
            TabOrder = 4
            OnClick = ParametersChanged
          end
          object spnBlur: TSpinEdit
            Left = 10
            Top = 31
            Width = 47
            Height = 24
            Hint = 'Blur'
            MaxValue = 20
            MinValue = 0
            TabOrder = 3
            Value = 2
            OnChange = ParametersChanged
          end
        end
        object btnAPI: TButton
          AlignWithMargins = True
          Left = 4
          Top = 396
          Width = 165
          Height = 31
          Hint = 
            'DrawShadowText API'#13#10'To use DrawShadowText, specify Comctl32.dll ' +
            'version 6 in the manifest. For more information on manifests, se' +
            'e Enabling Visual Styles.'
          Align = alTop
          Caption = 'DrawShadowText API'
          TabOrder = 1
          OnClick = btnAPIClick
          ExplicitLeft = 1
          ExplicitTop = 121
          ExplicitWidth = 171
        end
      end
    end
    object tabOthers: TTabSheet
      Caption = 'DrawStringEllipsis'
      ImageIndex = 2
      object Panel1: TPanel
        Left = 112
        Top = 232
        Width = 435
        Height = 114
        BevelInner = bvLowered
        TabOrder = 0
        object btnTest1: TButton
          Left = 24
          Top = 25
          Width = 113
          Height = 25
          Caption = 'Test Ellipsis text'
          TabOrder = 0
          OnClick = btnTest1Click
        end
        object StatusBar1: TStatusBar
          Left = 216
          Top = 28
          Width = 192
          Height = 20
          Align = alNone
          Panels = <>
          SimplePanel = True
        end
        object btnTest2: TButton
          Left = 24
          Top = 67
          Width = 113
          Height = 25
          Caption = 'Test Ellipsis canvas'
          TabOrder = 2
          OnClick = btnTest2Click
        end
        object StatusBar2: TStatusBar
          Left = -6
          Top = 126
          Width = 431
          Height = 20
          Align = alNone
          Panels = <>
          SimplePanel = True
        end
        object StatusBar3: TStatusBar
          Left = 216
          Top = 68
          Width = 192
          Height = 20
          Align = alNone
          Panels = <>
          SimplePanel = True
        end
      end
    end
  end
end
