object frmRainEditor: TfrmRainEditor
  Left = 0
  Top = 0
  Caption = 'Parameters'
  ClientHeight = 441
  ClientWidth = 590
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  TextHeight = 15
  object Container: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 584
    Height = 435
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      584
      435)
    object grpAdvanced: TGroupBox
      Left = 87
      Top = 126
      Width = 398
      Height = 256
      Anchors = [akTop]
      Caption = 'Advanced settings'
      TabOrder = 0
      Visible = False
      object Panel1: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 156
        Width = 388
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblDamping: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 49
          Height = 15
          Hint = 'Density of the liquid. How fast the waves are stopped.'
          Align = alLeft
          Caption = 'Damping'
          Layout = tlCenter
        end
        object trkDamping: TTrackBar
          AlignWithMargins = True
          Left = 90
          Top = 3
          Width = 295
          Height = 34
          Align = alRight
          Max = 99
          Min = 1
          Frequency = 5
          Position = 14
          SelEnd = 18
          SelStart = 11
          TabOrder = 0
          ThumbLength = 25
        end
      end
      object Panel2: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 20
        Width = 388
        Height = 38
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lblFPS: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 57
          Height = 15
          Hint = 
            'Desiered FPS. You might not reach this FPS number if your deskto' +
            'p resolution is high and your CPU is slow!'#13#10'Default: 18'
          Align = alLeft
          Caption = 'Target FPS:'
          Layout = tlCenter
        end
        object spnTargetFPS: TSpinEdit
          Left = 80
          Top = 9
          Width = 47
          Height = 24
          Hint = 
            'Animation speed.'#13#10'The higher the speed, the more CPU needed.'#13#10'Al' +
            'so, the CPU depends on the area in which it rains (greater the a' +
            're, higher the CPU).'
          Increment = 2
          MaxValue = 100
          MinValue = 1
          TabOrder = 0
          Value = 24
        end
      end
      object Panel3: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 64
        Width = 388
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object lblWaveAmp: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 75
          Height = 15
          Align = alLeft
          Caption = 'Wave aplitude'
          Layout = tlCenter
        end
        object trkWaveAmp: TTrackBar
          AlignWithMargins = True
          Left = 90
          Top = 3
          Width = 295
          Height = 34
          Align = alRight
          Max = 20
          Min = 1
          Frequency = 5
          Position = 3
          SelEnd = 4
          SelStart = 2
          TabOrder = 0
          ThumbLength = 25
        end
      end
      object Panel4: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 110
        Width = 388
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 3
        object Label1: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 83
          Height = 15
          Align = alLeft
          Caption = 'Wave travel dist'
          Layout = tlCenter
        end
        object trkTravelDist: TTrackBar
          AlignWithMargins = True
          Left = 90
          Top = 3
          Width = 295
          Height = 34
          Align = alRight
          Max = 2000
          Min = 10
          Frequency = 100
          Position = 500
          SelEnd = 800
          SelStart = 400
          TabOrder = 0
          ThumbLength = 25
        end
      end
      object Panel5: TPanel
        AlignWithMargins = True
        Left = 5
        Top = 202
        Width = 388
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 4
        object lblDropInterv: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 86
          Height = 15
          Hint = 
            '[in miliseconds]'#13#10'Unlike the others, this does not take effect w' +
            'hen you press Apply.'#13#10'You need to restart the animation in order' +
            ' to apply it.'#13#10
          Align = alLeft
          Caption = 'Droplet interval*'
          Layout = tlCenter
        end
        object trkDropInterv: TTrackBar
          AlignWithMargins = True
          Left = 90
          Top = 3
          Width = 295
          Height = 34
          Hint = 'miliseconds'
          Align = alRight
          Max = 1000
          Min = 1
          Frequency = 50
          Position = 90
          SelEnd = 150
          SelStart = 50
          TabOrder = 0
          ThumbLength = 25
        end
      end
    end
    object GroupBox2: TGroupBox
      Left = 119
      Top = 26
      Width = 334
      Height = 81
      Anchors = [akTop]
      Caption = 'Rain intensity '
      TabOrder = 1
      object trkRain: TTrackBar
        Left = 10
        Top = 30
        Width = 231
        Height = 42
        Hint = 
          'Predefined setting. '#13#10'Controlls all settings in "Advanced" at on' +
          'ce.'#13#10#13#10'Left side -> Slow rain.'#13#10'Right side -> Storm'
        Max = 4
        Position = 1
        SelEnd = 1
        SelStart = 1
        TabOrder = 0
        ThumbLength = 25
        TickMarks = tmBoth
        OnChange = trkRainChange
      end
      object btnAdvanced: TButton
        Left = 257
        Top = 40
        Width = 62
        Height = 25
        Caption = 'Advanced'
        TabOrder = 1
        OnClick = btnAdvancedClick
      end
    end
    object pnlBtm: TPanel
      Left = 0
      Top = 399
      Width = 584
      Height = 36
      Align = alBottom
      TabOrder = 2
      object btnOK: TButton
        AlignWithMargins = True
        Left = 406
        Top = 4
        Width = 90
        Height = 28
        Hint = 
          'Save settings & close the window.'#13#10'Hint: You can also close the ' +
          'window with <Enter>'
        Align = alRight
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        AlignWithMargins = True
        Left = 502
        Top = 4
        Width = 90
        Height = 28
        Hint = 
          'Close the window without saving.'#13#10'Hint: You can also close with ' +
          'Escape.'
        Align = alRight
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnApply: TButton
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 90
        Height = 28
        Align = alLeft
        Caption = 'Apply'
        TabOrder = 2
        OnClick = btnOKClick
      end
    end
  end
end
