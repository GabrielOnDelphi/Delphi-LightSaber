object FrameWinExplorer: TFrameWinExplorer
  Left = 0
  Top = 0
  Width = 273
  Height = 585
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object lblTop: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 267
    Height = 13
    Hint = 'Refresh the filelist'
    Align = alTop
    Alignment = taCenter
    Caption = 'FILE EXPLORER'
    Transparent = True
    Layout = tlCenter
    WordWrap = True
    ExplicitWidth = 75
  end
  object Splitter: TSplitter
    Left = 0
    Top = 230
    Width = 273
    Height = 3
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Constraints.MaxHeight = 3
    Constraints.MinHeight = 3
    ResizeStyle = rsUpdate
    ExplicitTop = 232
  end
  object Directory: TDirectoryListBox
    AlignWithMargins = True
    Left = 3
    Top = 71
    Width = 267
    Height = 156
    Align = alTop
    FileList = FileList
    TabOrder = 0
    OnChange = DirectoryChange
    ExplicitTop = 73
  end
  object FileList: TCubicFileList
    AlignWithMargins = True
    Left = 3
    Top = 269
    Width = 267
    Height = 313
    Hint = 'Press the "Delete" key to delete the selected file from disk.'
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
    ExplicitTop = 271
    ExplicitHeight = 311
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 236
    Width = 267
    Height = 27
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    ExplicitTop = 238
    object Filter: TCubicFilterBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 227
      Height = 21
      Align = alClient
      FileList = FileList
      TabOrder = 0
    end
    object btnRefresh: TButton
      Left = 233
      Top = 0
      Width = 34
      Height = 27
      Align = alRight
      Caption = 'Ref'
      Default = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnRefreshClick
    end
  end
  object Path: TCubicPathEdit
    AlignWithMargins = True
    Left = 3
    Top = 22
    Width = 267
    Height = 43
    FileListBox = FileList
    Directory = Directory
    ShowCreateBtn = False
    Align = alTop
    Caption = 'Folder'
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    TabOrder = 3
    TabStop = True
    ExplicitTop = 24
  end
end
