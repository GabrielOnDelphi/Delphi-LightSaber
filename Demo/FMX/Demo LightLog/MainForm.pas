unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.DialogService,
  FMX.Grid.Style, FMX.Presentation.Factory, FMX.Presentation.Style,
  LightFMX.lbAppData, LightFMX.lbAppData.Form,
  LightCore.INIFile,  LightCore.LogRam, FMX.ScrollBox, FMX.Grid,
  LightFmx.lbLogViewer, FMX.Layouts;

TYPE
  TForm1 = class(TLightForm)
    Layout1: TLayout;
    Button1: TButton;
    Layout2: TLayout;
    CheckBox1: TCheckBox;
    chkShowTime: TCheckBox;
    btnClear: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    LogViewer: TLogViewer;
    procedure Button1Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
  private
  public
   procedure FormPostInitialize; override;
  end;

VAR
  Form1: TForm1;

IMPLEMENTATION
{$R *.fmx}

USES LightCore.AppData;




procedure TForm1.FormPostInitialize;
begin
  AutoState:= asFull;  // Must set it before inherited!
  inherited;           // This will load the form's state from disk

 {
  LogViewer: TLogViewer;

  LogViewer:= TLogViewer.Create(Self);
  LogViewer.Parent:= Self;
  LogViewer.Align:= TAlignLayout.Client;
  LogViewer.Visible:= TRUE;}
end;





procedure TForm1.btnSaveClick(Sender: TObject);
begin
  LogViewer.RamLog.SaveToFile(AppData.ExeFolder+ 'LogFile.log');
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  LogViewer.RamLog.Clear;
  LogViewer.RamLog.LoadFromFile(AppData.ExeFolder+ 'LogFile.log');
end;



procedure TForm1.Button1Click(Sender: TObject);
begin
 //LogVis.TrackBar.Position:= 0;

 LogViewer.RamLog.AddImpo('This is TLogGrid based on TStringGrid');
 LogViewer.RamLog.AddEmptyRow;

 LogViewer.RamLog.AddDebug('AddDebug');
 LogViewer.RamLog.AddVerb ('AddVerb');
 LogViewer.RamLog.AddHint ('AddHint');
 LogViewer.RamLog.AddInfo ('AddInfo');
 LogViewer.RamLog.AddImpo ('AddImpo');
 LogViewer.RamLog.AddWarn ('AddWarn');
 LogViewer.RamLog.AddError('AddError');

 LogViewer.RamLog.AddEmptyRow;

 LogViewer.RamLog.AddBold  ('AddBold');
 LogViewer.RamLog.AddMsg   ('AddMsg');
 LogViewer.RamLog.AddMsgInt('AddMsgInt', 42);
end;

end.
