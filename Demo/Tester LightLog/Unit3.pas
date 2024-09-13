unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, clVisLogTrack, cbLogRam, Vcl.Grids, clVisLog, Vcl.ExtCtrls,
  llRichLogTrack, Vcl.StdCtrls, Vcl.ComCtrls, FormLog, llRichLogUtils, llRichLog;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    RichLog: TRichLog;
    Panel3: TPanel;
    Button1: TButton;
    RichLogTrckbr1: TRichLogTrckbr;
    Panel4: TPanel;
    Button2: TButton;
    LogVisTrckbr1: TLogVisTrckbr;
    Button3: TButton;
    Button4: TButton;
    Panel5: TPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    VisLog: TLogGrid;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;

implementation {$R *.dfm}
Uses cbAppData, cbINIFile, ccIO, ccTextFile, cmIO, cmIO.Win, ccCore, csSystem, cbDialogs;


procedure TMainForm.FormCreate(Sender: TObject);
begin
 RichLog.Clear;
 //AppData.Initializing:= FALSE; moved to cbAppData
 //Button5Click(Sender);
end;


procedure TMainForm.Button1Click(Sender: TObject);
begin
 RichLog.Clear;

 RichLog.AddVerb ('AddVerb');
 RichLog.AddHint ('AddHint');
 RichLog.AddInfo ('AddInfo');
 RichLog.AddImpo ('AddImpo');
 RichLog.AddWarn ('AddWarn');
 RichLog.AddError('AddError');

 RichLog.AddEmptyRow;

 RichLog.AddBold  ('AddBold');
 RichLog.AddMsg   ('AddMsg');
 RichLog.AddMsgInt('AddMsgInt', 42);

 RichLog.AddEmptyRow;

 RichLog.AddInteger(42);
 RichLog.AddFromFile(AppData.CurFolder+ 'Test file.txt', lvImportant);
end;



procedure TMainForm.Button2Click(Sender: TObject);
begin
// VisLog.Clear;
 VisLog.RamLog.AddDebug('AddDebug');
 VisLog.RamLog.AddVerb ('AddVerb');
 VisLog.RamLog.AddHint ('AddHint');
 VisLog.RamLog.AddInfo ('AddInfo');
 VisLog.RamLog.AddImpo ('AddImpo');
 VisLog.RamLog.AddWarn ('AddWarn');
 VisLog.RamLog.AddError('AddError');

 VisLog.RamLog.AddEmptyRow;

 VisLog.RamLog.AddBold  ('AddBold');
 VisLog.RamLog.AddMsg   ('AddMsg');
 VisLog.RamLog.AddMsgInt('AddMsgInt', 42);

 VisLog.RamLog.AddEmptyRow;

 //VisLog.Grid.ScrollBars:= ssnone;
end;


procedure TMainForm.Button4Click(Sender: TObject);
begin
 VisLog.RamLog.LoadFromFile(AppData.CurFolder+ 'LogFile.log');
end;


procedure TMainForm.CheckBox1Click(Sender: TObject);
begin
 VisLog.ShowDate:= CheckBox1.Checked;
end;


procedure TMainForm.CheckBox2Click(Sender: TObject);
begin
 VisLog.ShowTime:= CheckBox2.Checked;
end;


procedure TMainForm.Button3Click(Sender: TObject);
begin
 VisLog.RamLog.SaveToFile(AppData.CurFolder+ 'LogFile.log');
end;



procedure TMainForm.Button5Click(Sender: TObject);
begin
 VAR VisLog2:= TLogGrid.Create(Self);
 VisLog2.Parent:= Self;
 //VisLog2.ScrollBars      := ssNone;   // Exception Notification - Project raised exception class EOSError with message 'A call to an OS function failed'.
end;


end.
