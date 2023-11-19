program DemoLog;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {Form3},
  clVisLog in '..\clVisLog.pas',
  clLogUtils in '..\..\clLogUtils.pas',
  clRamLog in '..\..\clRamLog.pas',
  clRichLog in '..\..\clRichLog.pas',
  clRichLogTrack in '..\..\clRichLogTrack.pas';

{$R *.res}
{$WARN DUPLICATE_CTOR_DTOR OFF}    {Silence the: W1029 Duplicate constructor with identical parameters will be inacessible from C++. See: https://marc.durdin.net/2012/05/delphi-xe2s-hidden-hints-and-warnings-options/ }

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);y
  Application.Run;
end.
