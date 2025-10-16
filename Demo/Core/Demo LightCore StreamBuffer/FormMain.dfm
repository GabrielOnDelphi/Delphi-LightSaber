object MainForm: TMainForm
  Left = 138
  Top = 275
  AlphaBlendValue = 249
  Caption = 'Enviroment'
  ClientHeight = 594
  ClientWidth = 717
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 600
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Calibri'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  SnapBuffer = 5
  Visible = True
  TextHeight = 15
  object InternetLabel: TInternetLabel
    Left = 610
    Top = 310
    Width = 28
    Height = 13
    Cursor = crHandPoint
    Caption = 'www'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Link = 'http://www.tahionic.com'
    LinkHint = False
    Visited = False
    VisitedColor = clPurple
    NotVisitedColor = clBlue
    OverColor = clRed
  end
  object pgCtrl: TPageControl
    Left = 0
    Top = 0
    Width = 717
    Height = 594
    ActivePage = tabMain
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object tabMain: TTabSheet
      Caption = 'TBufferedFileStream'
      DesignSize = (
        709
        564)
      object Label1: TLabel
        Left = 527
        Top = 103
        Width = 163
        Height = 15
        Anchors = [akTop]
        Caption = 'Max line length in File2: 2KB !'
        Color = 16744703
        ParentColor = False
        Transparent = False
      end
      object Label2: TLabel
        Left = 582
        Top = 443
        Width = 108
        Height = 75
        Caption = 
          'TODO:'#13#10'FastMM: Off'#13#10'Compiler optimization: On'#13#10'Compile for: 64 b' +
          'its'
        Visible = False
        WordWrap = True
      end
      object Label4: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 703
        Height = 15
        Align = alTop
        Caption = 'This tests the speed of TBufferedFileStream (RTL)'
        ExplicitWidth = 263
      end
      object edtFile2: TCubicPathEdit
        Left = 130
        Top = 124
        Width = 449
        Height = 44
        Path = 'c:\TestFile'
        InputType = itFile
        Anchors = [akTop]
        Caption = 'File'
        TabOrder = 0
      end
      object spnCacheSize: TCubicSpinEdit
        Left = 135
        Top = 376
        Width = 70
        Height = 24
        Hint = 'Buffer size (KB)'
        MaxValue = 125
        MinValue = 0
        TabOrder = 1
        Value = 125
      end
      object GroupBox2: TGroupBox
        Left = 130
        Top = 178
        Width = 449
        Height = 170
        Caption = 'System.Classes.TBufferedFileStream'
        TabOrder = 2
        object btnReadChar2: TButton
          AlignWithMargins = True
          Left = 5
          Top = 102
          Width = 439
          Height = 63
          Hint = 'Linear'#13#10'System.Classes.TBufferedFileStream'
          Align = alBottom
          Caption = 'Read Char'
          TabOrder = 0
          OnClick = btnReadChar2Click
        end
        object btnNewDelphiStream: TButton
          AlignWithMargins = True
          Left = 5
          Top = 33
          Width = 439
          Height = 63
          Hint = 'System.Classes.TBufferedFileStream'
          Align = alBottom
          Caption = 'Read random'
          TabOrder = 1
          OnClick = btnNewDelphiStreamClick
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'LightStream'
      ImageIndex = 2
      object Label3: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 703
        Height = 15
        Align = alTop
        Caption = 'This tests all the methods of TLightStream'
        ExplicitWidth = 227
      end
      object GroupBox1: TGroupBox
        Left = 182
        Top = 136
        Width = 347
        Height = 177
        Caption = 'LightStream'
        TabOrder = 0
        object btnStreamWrite: TButton
          AlignWithMargins = True
          Left = 5
          Top = 40
          Width = 337
          Height = 63
          Align = alBottom
          Caption = 'StreamBuff Write'
          TabOrder = 0
          OnClick = btnStreamWriteClick
        end
        object btnStreamRead: TButton
          AlignWithMargins = True
          Left = 5
          Top = 109
          Width = 337
          Height = 63
          Align = alBottom
          Caption = 'StreamBuff Read'
          TabOrder = 1
          OnClick = btnStreamReadClick
        end
      end
    end
  end
end
