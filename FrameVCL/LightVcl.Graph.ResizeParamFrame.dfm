object ResizeParameters: TResizeParameters
  Left = 0
  Top = 0
  Width = 371
  Height = 255
  Anchors = []
  TabOrder = 0
  object chkZoomMax: TCheckBox
    Left = 112
    Top = 15
    Width = 252
    Height = 16
    Hint = 
      'Very small images will look pixelated if they are enlarged too m' +
      'uch.'#13#10'This feature will prevent this from happening.'
    Caption = 'Enlarge images with no more than                  %'
    TabOrder = 0
    OnClick = GUIChanged
  end
  object numForceHeight: TNumberBox
    Left = 116
    Top = 220
    Width = 85
    Height = 21
    AutoSize = False
    LargeStep = 100.000000000000000000
    MinValue = 16.000000000000000000
    MaxValue = 20000.000000000000000000
    ParentColor = True
    SmallStep = 10.000000000000000000
    TabOrder = 1
    Value = 600.000000000000000000
    SpinButtonOptions.Placement = nbspInline
    OnChange = GUIChanged
  end
  object numForceWidth: TNumberBox
    Left = 116
    Top = 187
    Width = 85
    Height = 21
    AutoSize = False
    LargeStep = 100.000000000000000000
    MinValue = 16.000000000000000000
    MaxValue = 20000.000000000000000000
    ParentColor = True
    SmallStep = 10.000000000000000000
    TabOrder = 2
    Value = 800.000000000000000000
    SpinButtonOptions.Placement = nbspInline
    OnChange = GUIChanged
  end
  object radForceHeight: TRadioButton
    Left = 15
    Top = 223
    Width = 97
    Height = 16
    Caption = 'Force height to'
    TabOrder = 3
    OnClick = GUIChanged
  end
  object radForceWidth: TRadioButton
    Left = 15
    Top = 190
    Width = 93
    Height = 16
    Caption = 'Force width to'
    TabOrder = 4
    OnClick = GUIChanged
  end
  object radStretch: TRadioButton
    Left = 103
    Top = 152
    Width = 59
    Height = 16
    Caption = 'Stretch'
    TabOrder = 5
    OnClick = GUIChanged
  end
  object radZoomAuto: TRadioButton
    Left = 15
    Top = 15
    Width = 75
    Height = 16
    Hint = 
      'The program will automatically:'#13#10'* Choose the filling (fill/fit)' +
      ' based on wallpaper aspect ratio'#13#10'* Enlarge small images to fit ' +
      'the desktop'#13#10'* Reduce large images to fit the desktop'#13#10#13#10'[Strong' +
      'ly recommended to keep this option active]'
    Caption = 'Auto zoom'
    Checked = True
    TabOrder = 6
    TabStop = True
    OnClick = GUIChanged
  end
  object radZoomCustom: TRadioButton
    Left = 15
    Top = 48
    Width = 92
    Height = 16
    Hint = 'Manual zoom.'
    Caption = 'Custom zoom:'
    TabOrder = 7
    OnClick = GUIChanged
  end
  object radZoomFill: TRadioButton
    Left = 15
    Top = 115
    Width = 298
    Height = 16
    Caption = 'Fill (enlarge so empty spaces around image are removed)'
    TabOrder = 8
    OnClick = GUIChanged
  end
  object radZoomFit: TRadioButton
    Left = 15
    Top = 81
    Width = 321
    Height = 16
    Caption = 'Fit (enlarge until one border touches the edges of the screen)'
    TabOrder = 9
    OnClick = GUIChanged
  end
  object radZoomNone: TRadioButton
    Left = 15
    Top = 152
    Width = 65
    Height = 16
    Caption = 'No zoom'
    TabOrder = 10
    OnClick = GUIChanged
  end
  object spnZoomCustom: TNumberBox
    Left = 109
    Top = 45
    Width = 66
    Height = 21
    Hint = 
      'Increase/decrease the image size with the specified zoom.'#13#10#13#10' Ex' +
      'ample: '#13#10'Use 0.9 to decrease image to 90% of its original size. ' +
      #13#10'Use 1.1 to increase image with 10% (to 110).'#13#10'Use 2.0 to doubl' +
      'e the size of the image.'#13#10
    AutoSize = False
    Mode = nbmFloat
    MinValue = 0.050000000000000000
    MaxValue = 20.000000000000000000
    ParentColor = True
    SmallStep = 0.100000000000000000
    TabOrder = 11
    Value = 1.500000000000000000
    SpinButtonOptions.Placement = nbspInline
    NegativeValueColor = clRed
    OnChange = GUIChanged
  end
  object spnZoomMax: TSpinEdit
    Left = 315
    Top = 12
    Width = 40
    Height = 24
    Hint = 'Default value: 30%'
    AutoSize = False
    Increment = 2
    MaxLength = 3
    MaxValue = 95
    MinValue = 0
    ParentColor = True
    TabOrder = 12
    Value = 30
    OnChange = GUIChanged
  end
end
