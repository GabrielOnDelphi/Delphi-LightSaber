unit MainForm;

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  System.Generics.Collections, FormScreenCapture;

TYPE
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1Click(Sender);
end;


// Start screen capture demo
procedure TForm1.Button1Click(Sender: TObject);
begin
  ScreenCaptureNow(
    procedure(CapturedImages: TObjectList<FMX.Graphics.TBitmap>)
    begin
      // Display results
      if CapturedImages.Count > 0
      then Label1.Text:= 'Successfully captured ' + IntToStr(CapturedImages.Count) + ' screenshot(s)!' + #13#10 + 'First image size: ' + IntToStr(CapturedImages[0].Width) + ' x ' + IntToStr(CapturedImages[0].Height) + ' px'
      else Label1.Text:= 'No screenshots were captured.';
    end
  );
end;



end.
