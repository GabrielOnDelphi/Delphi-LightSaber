object frmReminder: TfrmReminder
  Left = 1116
  Top = 602
  AlphaBlendValue = 245
  Anchors = []
  BorderIcons = [biSystemMenu]
  Caption = 'Reminder'
  ClientHeight = 389
  ClientWidth = 413
  Color = clBtnFace
  DoubleBuffered = True
  ParentFont = True
  OldCreateOrder = True
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  SnapBuffer = 3
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlReminder: TCubicGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 407
    Height = 383
    Align = alClient
    Color = clBtnFace
    ParentColor = False
    TabOrder = 0
    DesignSize = (
      407
      383)
    object Label1: TLabel
      Left = 16
      Top = 126
      Width = 34
      Height = 13
      Caption = 'Action:'
      Transparent = True
    end
    object chkMakeNoise: TCubicCheckBox
      Left = 268
      Top = 149
      Width = 77
      Height = 14
      Hint = 'Beep when the time is up'
      Caption = 'Make beep'
      Checked = True
      State = cbChecked
      TabOrder = 6
      AutoSize = True
    end
    object grpExecute: TCubicGroupBox
      Left = 56
      Top = 222
      Width = 343
      Height = 106
      Enabled = False
      TabOrder = 4
      BoldCaption = False
      DesignSize = (
        343
        106)
      object btnRun: TButton
        Left = 255
        Top = 79
        Width = 72
        Height = 22
        Hint = 'Start the specified application now'
        Anchors = [akRight, akBottom]
        Caption = 'Execute now'
        ModalResult = 1
        TabOrder = 1
        OnClick = btnRunClick
      end
      object edtPath: TCubicPathEdit
        Left = 11
        Top = 22
        Width = 320
        Height = 39
        Path = 'c:\My music\Wake me up.mp3'
        InputType = itFile
        ShowCreateBtn = False
        Anchors = [akLeft, akTop, akRight]
        Caption = 'File'
        TabOrder = 0
      end
    end
    object grpTimer: TCubicGroupBox
      Left = 77
      Top = 29
      Width = 253
      Height = 90
      Anchors = [akTop]
      Caption = 'Timer'
      TabOrder = 5
      BoldCaption = False
      DesignSize = (
        253
        90)
      object lblInterval: TLabel
        Left = 43
        Top = 29
        Width = 168
        Height = 13
        Caption = 'Reminder in                         minutes'
        Transparent = True
      end
      object btnStart: TButton
        Left = 58
        Top = 62
        Width = 46
        Height = 24
        Anchors = [akBottom]
        Caption = 'Start'
        TabOrder = 0
        OnClick = btnStartClick
      end
      object btnStop: TButton
        Left = 104
        Top = 62
        Width = 47
        Height = 24
        Anchors = [akBottom]
        Caption = 'Stop'
        TabOrder = 1
        OnClick = btnStopClick
      end
      object btnReset: TButton
        Left = 151
        Top = 62
        Width = 46
        Height = 24
        Hint = 'Restart the timer'
        Anchors = [akBottom]
        Caption = 'Reset'
        TabOrder = 3
        OnClick = btnResetClick
      end
      object spnTime: TCubicSpinEdit
        Left = 107
        Top = 26
        Width = 53
        Height = 22
        Hint = 
          'Time interval (in seconds) to start the specified application. I' +
          'f this box is green, then the timer is running, if it is dark, t' +
          'hen the timer is stopped.'
        MaxValue = 60000
        MinValue = 1
        TabOrder = 2
        Value = 60
        OnChange = spnTimeChange
      end
    end
    object radRunFile: TCubicRadioButton
      Left = 36
      Top = 197
      Width = 276
      Height = 14
      Hint = 
        'When the time is up, run a file. '#13#10'This file can be: application' +
        ', image, sound file (mp3), movie, document, etc'#13#10#13#10'Example: '#13#10'  ' +
        'Write "buy milk" in a text file called "c:\ToDoList.txt".'#13#10'  Wri' +
        'te the path (c:\ToDoList.txt) of this file into the '#39'File'#39' edit ' +
        'box. '#13#10'  When the time is up, the program will open your ToDoLis' +
        't.txt file.'
      Caption = 'Execute a file (program, mp3, video, document, etc)'
      TabOrder = 3
      OnClick = radRunFileClick
      AutoSize = True
    end
    object radSleep: TCubicRadioButton
      Left = 36
      Top = 148
      Width = 129
      Height = 15
      Caption = 'Put computer to sleep'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = radSleepClick
      AutoSize = True
    end
    object radShutDown: TCubicRadioButton
      Left = 36
      Top = 172
      Width = 120
      Height = 15
      Caption = 'Shutdown computer'
      TabOrder = 2
      OnClick = radSleepClick
      AutoSize = True
    end
    object chkRunOnce: TCubicCheckBox
      Left = 21
      Top = 357
      Width = 93
      Height = 14
      Hint = 
        'When the time is up run the selected file and'#13#10#13#10'if this check b' +
        'ox is enabled:  stop the timer'#13#10'if this check box is disabled:  ' +
        'restart the timer (run the file multiple times).'
      Anchors = [akLeft, akBottom]
      Caption = 'Run only once'
      Checked = True
      State = cbChecked
      TabOrder = 0
      AutoSize = True
    end
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 334
    Top = 24
  end
end
