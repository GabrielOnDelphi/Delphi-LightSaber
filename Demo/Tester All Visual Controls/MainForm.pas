UNIT MainForm;

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cvListBox, Vcl.Grids, cvStringGridBase, cvStringGrid, Vcl.ExtCtrls,
  cvLabelEdit, cvPathEdit, cvSpinEditDelayed, cvSpinEdit, Vcl.Samples.Spin, Vcl.FileCtrl, cvFileListBox,
  cvFloatSpinEdit, Vcl.StdCtrls, cvCheckBox, cvMsgDispatcher, cvGradientPanel, Vcl.WinXCtrls,
  cvActivityIndicator, cvCreationOrderTester, cvCaptionedThumb, cvTimeLine, cvTrayIcon, cvToolBox, cvTimer,
  Vcl.ComCtrls, cvStatusBar, cvSplitter, cvScrollBox, cvRichEdit, cvRadioButton, cvProxyList, cvPanel,
  cvMinimalPathLabel, cvMemo, Vcl.ValEdit, cvLstEditor, cvGroupBox, cvGraphChart, cvFreeDiskSpace,
  cvFileFilter, cvEdit, cvDirectoryListBox, cvCountDown, cvComboBox, Vcl.CheckLst,
  cvCheckListBox, cvAssociateExt, cvDropDownSearch, Vcl.Mask;

TYPE
  TfrmMain = class(TForm)
    AssociateFileExt1     : TAssociateFileExt;
    BaseStrGrid1          : TBaseStrGrid;
    CationedThumbnail     : TCationedThumbnail;
    CMinimalLabel         : TMinimalPathLabel;
    CountDown             : TCountDown;
    CubicCheckBox1        : TCubicCheckBox;
    CheckListBox          : TCubicCheckListBox;
    ComboBox              : TCubicComboBox;
    CubicDirListBox1      : TCubicDirListBox;
    CubicEdit             : TCubicEdit;
    CubicFileList1        : TCubicFileList;
    CubicFilterBox1       : TCubicFilterBox;
    GroupBox              : TCubicGroupBox;
    LabelEdit             : TCubicLabelEdit;
    ListBox               : TCubicListBox;
    Memo                  : TCubicMemo;
    Panel                 : TCubicPanel;
    PathEdit              : TCubicPathEdit;
    CubicRadioButton1     : TCubicRadioButton;
    ScrollBox             : TCubicScrollBox;
    SpinEdit              : TCubicSpinEdit;
    SpinEditD             : TCubicSpinEditD;
    SpinEditSplit         : TCubicSpinEditSplit;
    cubicStatusBar1       : TcubicStatusBar;
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
    TabSheet3             : TTabSheet;
    TabSheet4             : TTabSheet;
    TabSheet5             : TTabSheet;
    TabSheet6             : TTabSheet;
    TabSheet7             : TTabSheet;
    TabSheet8             : TTabSheet;
    TabSheet9             : TTabSheet;
    TimeLine              : TTimeLine;
    ToolBox               : TToolBox;
    CreationOrder_Test    : TCreationOrderTest;
    CubicSplitter1        : TCubicSplitter;
    RichEdit              : TCubicRichEdit;
    CMinimalLabel1        : TMinimalPathLabel;
    Label1                : TLabel;
    Label2                : TLabel;
    Label3                : TLabel;
    TabSheet2             : TTabSheet;
    Panel1                : TPanel;
    DropDownSearchBox     : TDropDownSearchBox;
    procedure FormShow(Sender: TObject);
    procedure DropDownSearchBoxEndSearch(Sender, SelectedItem: TObject);
  private
    SearchBox: TDropDownSearchBox;
  public
  end;

VAR
  frmMain: TfrmMain;

IMPLEMENTATION  {$R *.dfm}



procedure TfrmMain.FormShow(Sender: TObject);
begin
  VAR TSL:= TStringList.Create;
  TSL.Add('Pink Floyd');
  TSL.Add('Deep Purple');
  TSL.Add('Fleetwood Mac');

  SearchBox:= TDropDownSearchBox.Create(TabSheet5);
  SearchBox.Parent:= TabSheet5;
  SearchBox.OnEndSearch:= DropDownSearchBoxEndSearch;
 // SearchBox.SetHostParent(Self); del
  SearchBox.Populate(TSL);

  FreeAndNil(TSL);
end;


procedure TfrmMain.DropDownSearchBoxEndSearch(Sender, SelectedItem: TObject);
begin
  Caption:= DropDownSearchBox.SelectedString;
end;


end.
