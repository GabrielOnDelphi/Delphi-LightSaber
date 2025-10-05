UNIT Unit1;

{=============================================================================================================
   2025.03
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------

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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, LightFmx.Visual.DropDownSearch,
  FMX.Controls.Presentation, FMX.StdCtrls;

TYPE
  TForm1 = class(TForm)
    Panel: TPanel;
    procedure FormCreate(Sender: TObject);
  private
  public
    SearchBox: TDropDownSearchBox;
  end;

var
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SearchBox:= TDropDownSearchBox.Create(Panel);
  SearchBox.Parent    := Panel;
  SearchBox.Visible   := TRUE;
  SearchBox.Position.X:= 10;
  SearchBox.Position.Y:= 10;
  SearchBox.AddDemoStrings;
end;

end.
