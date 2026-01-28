UNIT Unit1;

{=============================================================================================================
   2026
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TDropDownSearchBox
   A search box with auto-suggest (VCL)
   When you type some text, a dropdown box similar to the Help Insight in Delphi IDE will appear.
   The list is filtered (gets smaller) as the user types in more characters into the searchbox.

   Can be closed with: click, double-click, escape, tab, and enter.
   The list can be navigated with arrow up/down.
   Use PopulateDictionary to add words to your list.
   Adapted from VCL to FMX, replacing TSearchBox with TEdit and TCubicListBox with TListBox.
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  LightFmx.Visual.DropDownSearch, LightFmx.Visual.SearchListBox, FMX.Layouts;

TYPE
  TForm1 = class(TForm)
    pnlDropDownSearchBox: TPanel;
    pnlSearchListBox: TPanel;
    LightSearchListBox1: TLightSearchListBox;
    procedure FormCreate(Sender: TObject);
  private
  public
    SearchBox: TDropDownSearchBox;
    ListBox: TLightSearchListBox;
  end;

var
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}
USES LightCore;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SearchBox:= TDropDownSearchBox.Create(pnlDropDownSearchBox);
  SearchBox.Parent    := pnlDropDownSearchBox;
  SearchBox.Visible   := TRUE;
  SearchBox.Position.X:= 10;
  SearchBox.Position.Y:= 10;
  SearchBox.AddDemoStrings;


  ListBox:= TLightSearchListBox.Create(pnlSearchListBox);
  ListBox.Parent    := pnlSearchListBox;
  ListBox.Visible   := TRUE;
  ListBox.Position.X:= 10;
  ListBox.Position.Y:= 10;
  //ListBox.align:= Client;

  for VAR i:= 1 to 30 do
    ListBox.AddItem(GetRandomPersonName);
end;

end.
