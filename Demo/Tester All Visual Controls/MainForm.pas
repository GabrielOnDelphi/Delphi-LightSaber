UNIT MainForm;

INTERFACE

USES
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls, Vcl.FileCtrl, Vcl.ValEdit, Vcl.Grids, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Mask, Vcl.Samples.Spin, Vcl.CheckLst,
  cvListBox, cvStringGridBase, cvStringGrid,
  cvLabelEdit, cvPathEdit, cvSpinEditDelayed, cvSpinEdit, cvFileListBox,
  cvFloatSpinEdit, cvCheckBox, cvMsgDispatcher, cvRichEditResize, cvGradientPanel,
  cvActivityIndicator, cvCreationOrderTester, cvCaptionedThumb, cvTimeLine, cvTrayIcon, cvToolBox, cvTimer,
  cvStatusBar, cvSplitter, cvScrollBox, cvRichEdit, cvRadioButton, cvProxyList, cvPanel,
  cvMinimalPathLabel, cvMemo, cvLstEditor, cvGroupBox, cvGraphChart, cvFreeDiskSpace,
  cvFileFilter, cvEdit, cvDirectoryListBox, cvCountDown, cvComboBox, cvCheckListBox, cvAssociateExt, cvDropDownSearch,
  cbAppDataForm;

TYPE
  TfrmMain = class(TLightForm)
    AssociateFileExt1     : TAssociateFileExt;
    BaseStrGrid1          : TBaseStrGrid;
    CationedThumbnail     : TCationedThumbnail;
    CountDown             : TCountDown;
    CubicCheckBox1        : TCubicCheckBox;
    CheckListBox          : TCubicCheckListBox;
    ComboBox              : TCubicComboBox;
    CubicDirListBox1      : TCubicDirListBox;
    CubicFileList1        : TCubicFileList;
    CubicFilterBox1       : TCubicFilterBox;
    GroupBox              : TCubicGroupBox;
    ListBox               : TCubicListBox;
    CubicPanel            : TCubicPanel;
    CubicRadioButton1     : TCubicRadioButton;
    ScrollBox             : TCubicScrollBox;
    SpinEdit              : TCubicSpinEdit;
    SpinEditD             : TCubicSpinEditD;
    SpinEditSplit         : TCubicSpinEditSplit;
    StatusBar             : TcubicStatusBar;
    CubicTimer            : TCubicTimer;
    CubicTrayIcon         : TCubicTrayIcon;
    ValueListEditor       : TCubicValueListEditor;
    EnhStrGrid1           : TEnhStrGrid;
    FastQChart            : TFastQChart;
    FloatSpinEdit         : TFloatSpinEdit;
    FreeDiskSpace1        : TFreeDiskSpace;
    GradPanel1            : TGradPanel;
    MsgDispatcher         : TMsgDispatcher;
    PageControl           : TPageControl;
    ProxyList1            : TProxyList;
    TabSheet1             : TTabSheet;
    tabText               : TTabSheet;
    TabSheet4             : TTabSheet;
    TabSheet5             : TTabSheet;
    TabSheet6             : TTabSheet;
    TabSheet7             : TTabSheet;
    TabSheet8             : TTabSheet;
    TabSheet9             : TTabSheet;
    TimeLine              : TTimeLine;
    ToolBox               : TToolBox;
    CreationOrder_Test    : TCreationOrderTest;
    RichEdit              : TCubicRichEdit;
    Label1                : TLabel;
    Label2                : TLabel;
    Label3                : TLabel;
    pnlRichEditResize     : TPanel;
    RichEditResize1       : TRichEditResize;
    PathEdit              : TCubicPathEdit;
    lblMinimalLabel       : TMinimalPathLabel;
    Label4                : TLabel;
    Panel2                : TPanel;
    Memo                  : TCubicMemo;
    LabelEdit             : TCubicLabelEdit;
    CubicEdit             : TCubicEdit;
    CMinimalLabel         : TMinimalPathLabel;
    Panel1                : TPanel;
    DropDownSearchBox     : TDropDownSearchBox;
    CubicSplitter         : TCubicSplitter;
    procedure FormShow(Sender: TObject);
    procedure DropDownSearchBoxEndSearch(Sender, SelectedItem: TObject);
  private
  public
    procedure CreateSearchBox; // Dynamically created. del
  end;

VAR
  frmMain: TfrmMain;

IMPLEMENTATION  {$R *.dfm}


procedure TfrmMain.FormShow(Sender: TObject);
begin
  DropDownSearchBox.AddDemoStrings;
  {
  VAR RichResize:= TRichEditResize.Create(pnlRichEditResize);
  RichResize.Parent:= pnlRichEditResize;
  RichResize.ParentPanel:= pnlRichEditResize;  }
end;


procedure TfrmMain.DropDownSearchBoxEndSearch(Sender, SelectedItem: TObject);
begin
  Caption:= DropDownSearchBox.SelectedString;
end;





//del
procedure TfrmMain.CreateSearchBox; // Dynamically created
VAR
   SearchBox: TDropDownSearchBox;
begin
  SearchBox:= TDropDownSearchBox.Create(TabSheet5);
  SearchBox.Parent:= TabSheet5;
  SearchBox.OnEndSearch:= DropDownSearchBoxEndSearch;
  //SearchBox.SetHostParent(Self);
  SearchBox.AddDemoStrings;
end;


end.
