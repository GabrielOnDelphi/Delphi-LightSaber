unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, LightFMX.DropDownSearch,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
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
