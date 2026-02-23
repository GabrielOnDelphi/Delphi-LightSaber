unit FormSecondary;

{-------------------------------------------------------------------------------------------------------------
  Secondary form showcasing various FMX controls to verify style rendering.
  Used alongside MainForm to test that style changes propagate across multiple forms.
-------------------------------------------------------------------------------------------------------------}

INTERFACE

USES
  System.SysUtils, System.Types, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  CoolTrayIcon, FMX.Objects, FMX.StdCtrls,
  FMX.Colors, FMX.TabControl, FMX.ScrollBox, FMX.Grid, FMX.ExtCtrls, FMX.ComboTrackBar, FMX.ComboEdit,
  FMX.SpinBox, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.DateTimeCtrls, FMX.Calendar,
  FMX.Controls.Presentation, FMX.ListView, FMX.TreeView, FMX.ListBox, FMX.Layouts, FMX.Menus, System.Rtti,
  FMX.Grid.Style, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

TYPE
  TfrmSecondary = class(TForm)
    AniIndicator1: TAniIndicator;
    ArcDial1: TArcDial;
    Calendar1: TCalendar;
    ComboBox1: TComboBox;
    ComboEdit1: TComboEdit;
    ComboTrackBar1: TComboTrackBar;
    CoolTrayIcon1: TCoolTrayIcon;
    CornerButton1: TCornerButton;
    DropTarget1: TDropTarget;
    Expander1: TExpander;
    Grid1: TGrid;
    HueTrackBar1: THueTrackBar;
    Layout1: TLayout;
    ListBox1: TListBox;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    NumberBox1: TNumberBox;
    Rectangle1: TRectangle;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpinBox1: TSpinBox;
    Splitter1: TSplitter;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    TimeEdit1: TTimeEdit;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
  public
  end;

var
  frmSecondary: TfrmSecondary;

IMPLEMENTATION
{$R *.fmx}


procedure TfrmSecondary.FormCreate(Sender: TObject);
begin
  // Set this in code so you don't have to rely on the IDE and to ensure it's always linked to the central source.
  // Self.StyleBook:= frmSimpleDemo.StyleBook;
end;

procedure TfrmSecondary.SpeedButton3Click(Sender: TObject);
begin
  Self.StyleBook:= NIL;
end;


end.
