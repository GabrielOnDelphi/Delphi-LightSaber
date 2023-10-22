object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Light Saber - Visual Controls'
  ClientHeight = 537
  ClientWidth = 899
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 893
    Height = 531
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Basics'
      object CMinimalLabel: TMinimalPathLabel
        Left = 33
        Top = 94
        Width = 112
        Height = 13
        Hint = 'c:\Users\trei\Downloads\'
        AutoSize = False
        Caption = 'c:\...\trei\Downloads\'
        ParentShowHint = False
        ShowHint = True
        CaptionMin = 'c:\Users\trei\Downloads\'
      end
      object CubicSplitter1: TCubicSplitter
        Left = 0
        Top = 0
        Height = 484
        ResizeStyle = rsUpdate
      end
      object CheckListBox: TCubicCheckListBox
        Left = 664
        Top = 214
        Width = 185
        Height = 95
        ItemHeight = 13
        Items.Strings = (
          'ListBox:'
          ' Item1'
          ' Item2'
          ' Item3')
        TabOrder = 0
      end
      object ComboBox: TCubicComboBox
        Left = 33
        Top = 237
        Width = 145
        Height = 22
        Style = csOwnerDrawFixed
        ItemIndex = 0
        TabOrder = 1
        Text = 'ComboBox'
        Items.Strings = (
          'ComboBox'
          ' Item1'
          ' Item2'
          ' Item3'
          ' Item4')
      end
      object Memo: TCubicMemo
        Left = 217
        Top = 220
        Width = 185
        Height = 89
        HideSelection = False
        Lines.Strings = (
          'Memo text')
        TabOrder = 2
        TypeMode = tmInsert
      end
      object Panel: TCubicPanel
        Left = 220
        Top = 94
        Width = 185
        Height = 109
        Alignment = taLeftJustify
        TabOrder = 3
        object CubicRadioButton1: TCubicRadioButton
          Left = 20
          Top = 30
          Width = 115
          Height = 17
          Caption = 'CubicRadioButton1'
          TabOrder = 0
          AutoSize = True
        end
        object CubicCheckBox1: TCubicCheckBox
          Left = 20
          Top = 58
          Width = 104
          Height = 17
          Caption = 'CubicCheckBox1'
          TabOrder = 1
          AutoSize = True
        end
      end
      object cubicStatusBar1: TcubicStatusBar
        Left = 0
        Top = 484
        Width = 885
        Height = 19
        Panels = <>
        SimplePanel = True
      end
      object GroupBox: TCubicGroupBox
        Left = 442
        Top = 94
        Width = 185
        Height = 215
        Caption = 'Spin editors'
        TabOrder = 5
        object FloatSpinEdit: TFloatSpinEdit
          Left = 29
          Top = 39
          Width = 75
          Height = 21
          Hint = ''
          MinValue = -10.000000000000000000
          MaxValue = 10.000000000000000000
          Increment = 0.100000000000000000
          Decimals = 2
          ShowHint = False
          Enabled = True
          TabOrder = 0
        end
        object SpinEditSplit: TCubicSpinEditSplit
          AlignWithMargins = True
          Left = 29
          Top = 158
          Width = 129
          Height = 22
          BevelOuter = bvNone
          Caption = 'SpinEditSplit'
          ShowCaption = False
          TabOrder = 1
          Spin.AlignWithMargins = True
          Spin.Left = 60
          Spin.Top = 0
          Spin.Width = 50
          Spin.Height = 22
          Spin.Margins.Top = 0
          Spin.Margins.Bottom = 0
          Spin.Align = alLeft
          Spin.MaxValue = 10
          Spin.MinValue = 0
          Spin.TabOrder = 0
          Spin.Value = 0
          Caption1 = 'Front label'
          Caption2 = '%'
          Value = 0
        end
        object SpinEdit: TCubicSpinEdit
          Left = 29
          Top = 77
          Width = 78
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object SpinEditD: TCubicSpinEditD
          Left = 29
          Top = 115
          Width = 78
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
        end
      end
      object LabelEdit: TCubicLabelEdit
        Left = 33
        Top = 187
        Width = 121
        Height = 21
        EditLabel.Width = 43
        EditLabel.Height = 13
        EditLabel.Caption = 'LabelEdit'
        TabOrder = 6
      end
      object CubicEdit: TCubicEdit
        Left = 33
        Top = 133
        Width = 121
        Height = 21
        TabOrder = 7
        Text = 'CubicEdit'
      end
      object ListBox: TCubicListBox
        Left = 664
        Top = 94
        Width = 185
        Height = 97
        ItemHeight = 13
        Items.Strings = (
          'ListBox:'
          ' Item1'
          ' Item2'
          ' Item3')
        ScrollWidth = 6
        TabOrder = 8
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Enhanced controls'
      ImageIndex = 2
      object ScrollBox: TCubicScrollBox
        Left = 282
        Top = 94
        Width = 341
        Height = 247
        TabOrder = 0
        object CMinimalLabel1: TMinimalPathLabel
          Left = 7
          Top = 3
          Width = 455
          Height = 13
          Hint = 
            'ScrollBox                                                       ' +
            '                                                                ' +
            '              .   '
          Caption = 
            'ScrollBox                                                       ' +
            '                                                                ' +
            '              .   '
          ParentShowHint = False
          ShowHint = True
          CaptionMin = 
            'ScrollBox                                                       ' +
            '                                                                ' +
            '              .   '
        end
        object RichEdit: TCubicRichEdit
          Left = 3
          Top = 30
          Width = 331
          Height = 193
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Lines.Strings = (
            'Cubic RichEdit')
          ParentFont = False
          TabOrder = 0
          Zoom = 100
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'String grids'
      ImageIndex = 3
      object BaseStrGrid1: TBaseStrGrid
        Left = 0
        Top = 126
        Width = 885
        Height = 175
        Align = alTop
        DefaultColWidth = 80
        DefaultRowHeight = 15
        DoubleBuffered = True
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowMoving, goRowSelect, goThumbTracking]
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        CursorTextColor = clYellow
        ColorCursor = clNavy
        CursorPosition = 1
        AutoRowHeight = False
        LargeColumn = 0
      end
      object EnhStrGrid1: TEnhStrGrid
        Left = 0
        Top = 0
        Width = 885
        Height = 126
        Align = alTop
        DefaultColWidth = 80
        DefaultRowHeight = 15
        DoubleBuffered = True
        DrawingStyle = gdsGradient
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowMoving, goRowSelect, goThumbTracking]
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        CursorTextColor = clYellow
        ColorCursor = clNavy
        CursorPosition = 1
        AutoRowHeight = False
        LargeColumn = 0
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Composite controls'
      ImageIndex = 4
      object ToolBox: TToolBox
        AlignWithMargins = True
        Left = 99
        Top = 9
        Width = 687
        Height = 120
        Margins.Left = 99
        Margins.Top = 9
        Margins.Right = 99
        Margins.Bottom = 9
        TopCaption = ' Tool box'
        CloseButton.Left = 660
        CloseButton.Top = 0
        CloseButton.Width = 22
        CloseButton.Height = 20
        CloseButton.Hint = 'Close'
        CloseButton.Anchors = [akTop, akRight]
        CloseButton.Caption = 'X'
        CloseButton.TabOrder = 0
        TopBar.Left = 1
        TopBar.Top = 1
        TopBar.Width = 681
        TopBar.Height = 18
        TopBar.Cursor = crHandPoint
        TopBar.Align = alTop
        TopBar.AutoSize = False
        TopBar.Caption = ' Tool box'
        TopBar.Color = clNavy
        TopBar.Font.Charset = DEFAULT_CHARSET
        TopBar.Font.Color = clWhite
        TopBar.Font.Height = -11
        TopBar.Font.Name = 'Tahoma'
        TopBar.Font.Style = [fsBold]
        TopBar.ParentColor = False
        TopBar.ParentFont = False
        TopBar.Transparent = False
        TopBar.Layout = tlCenter
        Align = alTop
        BevelKind = bkTile
        ParentColor = True
        ShowCaption = False
        TabOrder = 0
        DesignSize = (
          683
          116)
      end
      object ValueListEditor: TCubicValueListEditor
        AlignWithMargins = True
        Left = 99
        Top = 271
        Width = 687
        Height = 128
        Margins.Left = 99
        Margins.Top = 9
        Margins.Right = 99
        Margins.Bottom = 9
        Align = alTop
        TabOrder = 1
        ColWidths = (
          150
          531)
      end
      object MsgDispatcher: TMsgDispatcher
        AlignWithMargins = True
        Left = 99
        Top = 207
        Width = 687
        Height = 46
        Margins.Left = 99
        Margins.Top = 9
        Margins.Right = 99
        Margins.Bottom = 9
        Align = alTop
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvLowered
        Caption = 'MsgDispatcher'
        ShowCaption = False
        TabOrder = 2
      end
      object PathEdit: TCubicPathEdit
        AlignWithMargins = True
        Left = 99
        Top = 147
        Width = 687
        Height = 42
        Margins.Left = 99
        Margins.Top = 9
        Margins.Right = 99
        Margins.Bottom = 9
        Align = alTop
        Caption = 'PathEdit'
        TabOrder = 3
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Wizards'
      ImageIndex = 5
      object ProxyList1: TProxyList
        Left = 14
        Top = 18
        Width = 521
        Height = 503
        Caption = 'Proxy settings'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 0
      end
      object AssociateFileExt1: TAssociateFileExt
        Left = 546
        Top = 28
        Width = 174
        Height = 129
        Caption = 'Associate with...'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 1
        FileType = '.txt'
        AssocName = 'Demo Application'
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'I/O'
      ImageIndex = 6
      object CubicFilterBox1: TCubicFilterBox
        Left = 14
        Top = 204
        Width = 300
        Height = 21
        TabOrder = 0
      end
      object FreeDiskSpace1: TFreeDiskSpace
        Left = 14
        Top = 22
        Width = 300
        Height = 63
        TabOrder = 1
      end
      object CubicDirListBox1: TCubicDirListBox
        Left = 14
        Top = 94
        Width = 300
        Height = 97
        TabOrder = 2
      end
      object CubicFileList1: TCubicFileList
        Left = 14
        Top = 238
        Width = 300
        Height = 109
        Hint = 'Press the "Delete" key to delete the selected file from disk.'
        ItemHeight = 13
        TabOrder = 3
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Graphics'
      ImageIndex = 7
      object FastQChart: TFastQChart
        Left = 29
        Top = 284
        Width = 209
        Height = 129
      end
      object Label1: TLabel
        Left = 28
        Top = 10
        Width = 94
        Height = 13
        Caption = 'Cationed Thumbnail'
      end
      object Label3: TLabel
        Left = 28
        Top = 265
        Width = 56
        Height = 13
        Caption = 'FastQChart'
      end
      object CationedThumbnail: TCationedThumbnail
        AlignWithMargins = True
        Left = 28
        Top = 24
        Width = 209
        Height = 95
        Caption = 'CationedThumbnail'
        TabOrder = 0
      end
      object GradPanel1: TGradPanel
        Left = 28
        Top = 140
        Width = 209
        Height = 93
        Caption = 'Gradient Panel'
        TabOrder = 1
        object Label2: TLabel
          Left = 64
          Top = 44
          Width = 70
          Height = 13
          Caption = 'Gradient Panel'
        end
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Others'
      ImageIndex = 8
      object TimeLine: TTimeLine
        Left = 256
        Top = 103
        Width = 400
        Height = 35
        BevelOuter = bvNone
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 0
        MarkerPos = 0
        Progress.Left = 0
        Progress.Top = 0
        Progress.Width = 400
        Progress.Height = 17
        Progress.Align = alTop
        Progress.Max = 120
        Progress.Smooth = True
        Progress.MarqueeInterval = 100
        Progress.TabOrder = 0
      end
      object CreationOrder_Test: TCreationOrderTest
        Left = 256
        Top = 204
        Width = 379
        Height = 41
        Caption = 'SOMETHING'
        ShowCaption = False
        TabOrder = 1
        Button.Left = 1
        Button.Top = 1
        Button.Width = 75
        Button.Height = 39
        Button.Align = alLeft
        Button.Caption = 'SOMETHING'
        Button.Font.Charset = DEFAULT_CHARSET
        Button.Font.Color = clWindowText
        Button.Font.Height = -27
        Button.Font.Name = 'Tahoma'
        Button.Font.Style = []
        Button.ParentFont = False
        Button.TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 8
    end
  end
  object CountDown1: TCountDown
    Left = 636
    Top = 408
  end
  object CubicTimer1: TCubicTimer
    Left = 802
    Top = 407
  end
  object CubicTrayIcon1: TCubicTrayIcon
    Left = 719
    Top = 407
  end
end
