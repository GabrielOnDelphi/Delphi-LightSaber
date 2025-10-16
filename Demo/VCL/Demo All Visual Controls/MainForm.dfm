object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Light Saber - Visual Controls'
  ClientHeight = 536
  ClientWidth = 895
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Calibri'
  Font.Style = []
  Position = poDesigned
  OnShow = FormShow
  TextHeight = 15
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 889
    Height = 530
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Basics'
      object CheckListBox: TCubicCheckListBox
        Left = 577
        Top = 213
        Width = 185
        Height = 95
        ItemHeight = 17
        Items.Strings = (
          'ListBox:'
          ' Item1'
          ' Item2'
          ' Item3')
        TabOrder = 0
      end
      object ComboBox: TCubicComboBox
        Left = 133
        Top = 221
        Width = 185
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
      object CubicPanel: TCubicPanel
        Left = 133
        Top = 95
        Width = 185
        Height = 109
        Alignment = taLeftJustify
        TabOrder = 2
        object CubicRadioButton1: TCubicRadioButton
          Left = 20
          Top = 32
          Width = 123
          Height = 17
          Caption = 'CubicRadioButton'
          TabOrder = 0
          AutoSize = True
        end
        object CubicCheckBox1: TCubicCheckBox
          Left = 20
          Top = 60
          Width = 108
          Height = 17
          Caption = 'CubicCheckBox'
          TabOrder = 1
          AutoSize = True
        end
      end
      object StatusBar: TcubicStatusBar
        Left = 0
        Top = 481
        Width = 881
        Height = 19
        Panels = <>
        SimplePanel = True
        SimpleText = 'CubicStatusBar'
      end
      object GroupBox: TCubicGroupBox
        Left = 355
        Top = 87
        Width = 185
        Height = 221
        Caption = 'Spin editors'
        TabOrder = 4
        object FloatSpinEdit: TFloatSpinEdit
          Left = 29
          Top = 39
          Width = 78
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
          Width = 133
          Height = 25
          BevelOuter = bvNone
          Caption = 'SpinEditSplit'
          ParentBackground = False
          ParentColor = True
          ShowCaption = False
          TabOrder = 1
          Spin.AlignWithMargins = True
          Spin.Left = 65
          Spin.Top = 0
          Spin.Width = 50
          Spin.Height = 25
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
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object SpinEditD: TCubicSpinEditD
          Left = 29
          Top = 115
          Width = 78
          Height = 24
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
        end
      end
      object ListBox: TCubicListBox
        Left = 577
        Top = 95
        Width = 185
        Height = 97
        ItemHeight = 15
        Items.Strings = (
          'ListBox:'
          ' Item1'
          ' Item2'
          ' Item3')
        ScrollWidth = 6
        TabOrder = 5
      end
    end
    object tabText: TTabSheet
      Caption = 'Text'
      ImageIndex = 2
      object CMinimalLabel: TMinimalPathLabel
        Left = 22
        Top = 36
        Width = 112
        Height = 13
        Hint = 'c:\Users\trei\Downloads\'
        AutoSize = False
        Caption = 'c:\...\Downloads\'
        ParentShowHint = False
        ShowHint = True
        CaptionMin = 'c:\Users\trei\Downloads\'
      end
      object ScrollBox: TCubicScrollBox
        Left = 22
        Top = 121
        Width = 341
        Height = 247
        TabOrder = 0
        object RichEdit: TCubicRichEdit
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 331
          Height = 237
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Lines.Strings = (
            'Cubic RichEdit')
          ParentFont = False
          TabOrder = 0
        end
      end
      object pnlRichEditResize: TPanel
        Left = 379
        Top = 121
        Width = 193
        Height = 97
        BevelOuter = bvNone
        Caption = 'Autoresizable rich edit'
        ParentBackground = False
        ParentColor = True
        TabOrder = 1
        object RichEditResize1: TRichEditResize
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 187
          Height = 91
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Lines.Strings = (
            'TRichEditResize'
            'Expnads as you add lines to it')
          ParentFont = False
          TabOrder = 0
          MinHeight = 50
        end
      end
      object Memo: TCubicMemo
        Left = 593
        Top = 124
        Width = 185
        Height = 91
        HideSelection = False
        Lines.Strings = (
          'Memo text')
        TabOrder = 2
        TypeMode = tmInsert
      end
      object LabelEdit: TCubicLabelEdit
        Left = 190
        Top = 27
        Width = 121
        Height = 23
        EditLabel.Width = 50
        EditLabel.Height = 15
        EditLabel.Caption = 'LabelEdit'
        TabOrder = 3
        Text = ''
      end
      object CubicEdit: TCubicEdit
        Left = 190
        Top = 59
        Width = 121
        Height = 23
        TabOrder = 4
        Text = 'CubicEdit'
      end
      object Panel1: TPanel
        Left = 382
        Top = 224
        Width = 396
        Height = 101
        TabOrder = 5
        object DropDownSearchBox: TDropDownSearchBox
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 388
          Height = 23
          Align = alTop
          TabOrder = 0
          TextHint = 'Search...'
          OnEndSearch = DropDownSearchBoxEndSearch
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'String grids'
      ImageIndex = 3
      object CubicSplitter: TCubicSplitter
        Left = 0
        Top = 132
        Width = 881
        Height = 8
        Cursor = crVSplit
        Hint = 'CubicSplitter'
        Align = alTop
        ResizeStyle = rsUpdate
      end
      object BaseStrGrid1: TBaseStrGrid
        AlignWithMargins = True
        Left = 3
        Top = 143
        Width = 875
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
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 875
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
        Width = 683
        Height = 120
        Margins.Left = 99
        Margins.Top = 9
        Margins.Right = 99
        Margins.Bottom = 9
        TopCaption = ' Tool box'
        CloseButton.Left = 656
        CloseButton.Top = 0
        CloseButton.Width = 22
        CloseButton.Height = 20
        CloseButton.Hint = 'Close'
        CloseButton.Anchors = [akTop, akRight]
        CloseButton.Caption = 'X'
        CloseButton.TabOrder = 0
        TopBar.Left = 1
        TopBar.Top = 1
        TopBar.Width = 677
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
        ExplicitWidth = 300
        DesignSize = (
          679
          116)
      end
      object ValueListEditor: TCubicValueListEditor
        AlignWithMargins = True
        Left = 99
        Top = 211
        Width = 683
        Height = 116
        Margins.Left = 99
        Margins.Top = 9
        Margins.Right = 99
        Margins.Bottom = 9
        Align = alTop
        TabOrder = 1
        ExplicitTop = 210
        ColWidths = (
          150
          527)
      end
      object MsgDispatcher: TMsgDispatcher
        AlignWithMargins = True
        Left = 99
        Top = 147
        Width = 683
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
      object lblMinimalLabel: TMinimalPathLabel
        AlignWithMargins = True
        Left = 98
        Top = 135
        Width = 152
        Height = 15
        Hint = 
          'c:\Projects\LightSaber\Demo\Tester All Visual Controls\MinimalPa' +
          'thLabel.pas'
        Caption = 'c:\...\MinimalPathLabel.pas'
        Color = 16773601
        ParentColor = False
        ParentShowHint = False
        ShowHint = True
        Transparent = False
        CaptionMin = 
          'c:\Projects\LightSaber\Demo\Tester All Visual Controls\MinimalPa' +
          'thLabel.pas'
      end
      object Label4: TLabel
        Left = 99
        Top = 80
        Width = 648
        Height = 45
        Caption = 
          'When displaying a path, truncates the middle of the text if the ' +
          'entire path cannot fit into the visible area of the control.'#13#10'  ' +
          'It ONLY works with filenames and paths because of MinimizeName!'#13 +
          #10'  If ShowFullTextAsHint is true then the entire text will be sh' +
          'own into the Hint.'
      end
      object FreeDiskSpace1: TFreeDiskSpace
        Left = 318
        Top = 258
        Width = 267
        Height = 103
        TabOrder = 0
      end
      object PathEdit: TCubicPathEdit
        AlignWithMargins = True
        Left = 99
        Top = 9
        Width = 683
        Height = 42
        Margins.Left = 99
        Margins.Top = 9
        Margins.Right = 99
        Margins.Bottom = 9
        Align = alTop
        Caption = 'PathEdit'
        TabOrder = 1
      end
      object Panel2: TPanel
        Left = 14
        Top = 168
        Width = 185
        Height = 329
        Caption = 'Panel2'
        TabOrder = 2
        object CubicFilterBox1: TCubicFilterBox
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 177
          Height = 23
          Align = alTop
          TabOrder = 0
        end
        object CubicDirListBox1: TCubicDirListBox
          AlignWithMargins = True
          Left = 4
          Top = 33
          Width = 177
          Height = 120
          Align = alTop
          TabOrder = 1
        end
        object CubicFileList1: TCubicFileList
          AlignWithMargins = True
          Left = 4
          Top = 159
          Width = 177
          Height = 166
          Hint = 'Press the "Delete" key to delete the selected file from disk.'
          Align = alClient
          ItemHeight = 15
          TabOrder = 2
        end
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
        Width = 110
        Height = 15
        Caption = 'Cationed Thumbnail'
      end
      object Label3: TLabel
        Left = 28
        Top = 265
        Width = 62
        Height = 15
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
          Width = 82
          Height = 15
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
        Progress.Position = 22
        Progress.Smooth = True
        Progress.MarqueeInterval = 100
        Progress.TabOrder = 0
      end
      object CreationOrder_Test: TCreationOrderTest
        Left = 256
        Top = 204
        Width = 379
        Height = 45
        Caption = 'Create'
        ShowCaption = False
        TabOrder = 1
        Button.Left = 1
        Button.Top = 1
        Button.Width = 75
        Button.Height = 43
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
  end
  object CountDown: TCountDown
    Left = 636
    Top = 408
  end
  object CubicTimer: TCubicTimer
    Left = 802
    Top = 408
  end
  object CubicTrayIcon: TCubicTrayIcon
    Left = 719
    Top = 408
  end
end
