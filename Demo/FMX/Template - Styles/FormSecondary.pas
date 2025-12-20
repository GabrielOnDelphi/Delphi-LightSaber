unit FormSecondary;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, System.Rtti, FMX.Grid.Style, CoolTrayIcon, FMX.Objects, FMX.StdCtrls,
  FMX.Colors, FMX.TabControl, FMX.ScrollBox, FMX.Grid, FMX.ExtCtrls, FMX.ComboTrackBar, FMX.ComboEdit,
  FMX.SpinBox, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.DateTimeCtrls, FMX.Calendar,
  FMX.Controls.Presentation, FMX.ListView, FMX.TreeView, FMX.ListBox, FMX.Layouts, FMX.Menus, MainForm;

type
  TfrmSecondary = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Expander1: TExpander;
    Splitter1: TSplitter;
    ListBox1: TListBox;
    ComboBox1: TComboBox;
    TreeView1: TTreeView;
    ListView1: TListView;
    SpeedButton1: TSpeedButton;
    CornerButton1: TCornerButton;
    ArcDial1: TArcDial;
    AniIndicator1: TAniIndicator;
    Calendar1: TCalendar;
    TimeEdit1: TTimeEdit;
    DropTarget1: TDropTarget;
    NumberBox1: TNumberBox;
    ComboEdit1: TComboEdit;
    PlotGrid1: TPlotGrid;
    Grid1: TGrid;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    HueTrackBar1: THueTrackBar;
    Rectangle1: TRectangle;
    ScrollBox1: TScrollBox;
    CoolTrayIcon1: TCoolTrayIcon;
    Layout1: TLayout;
    SpinBox1: TSpinBox;
    ComboTrackBar1: TComboTrackBar;
    TabItem4: TTabItem;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
  public
  end;

var
  frmSecondary: TfrmSecondary;

IMPLEMENTATION
{$R *.fmx}
USES DataModule;

procedure TfrmSecondary.FormCreate(Sender: TObject);
begin
  // Set this in code so you don't have to rely on the IDE and to ensure it's always linked to the central source.
//  Self.StyleBook:= frmSimpleDemo.StyleBook2;
end;

procedure TfrmSecondary.SpeedButton1Click(Sender: TObject);
begin
//  Self.StyleBook:= frmSimpleDemo.StyleBook1;
end;

procedure TfrmSecondary.SpeedButton2Click(Sender: TObject);
begin
//  Self.StyleBook:= frmSimpleDemo.StyleBook2;
end;

procedure TfrmSecondary.SpeedButton3Click(Sender: TObject);
begin
  Self.StyleBook:= NIL;
end;

end.
