UNIT FormMain;

{
 Tester for LightCore.StreamBuffer.pas
 Tester for other streams

 Memory usage:
   File2 size:  20MBB ->  RAM 200MB
   File2 size:  700MB ->  RAM 1540MB. Finished OK
   File2 size: 1.11GB ->  RAM 1600MB, then it showed OutOfMem
}

INTERFACE

USES
  //WinApi.Messages,
  System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Forms, Vcl.Controls, Vcl.Samples.Spin, Vcl.ExtCtrls,
  LightVcl.Visual.INIFile, InternetLabel, LightVcl.Visual.PathEdit, LightVcl.Visual.SpinEdit,
  LightVcl.Visual.RichLogTrack, LightVcl.Visual.CheckBox, LightVcl.Visual.RichLog, LightCore.AppData, LightVcl.Common.AppData
, chHardID, LightVcl.Common.AppDataForm;

TYPE
  TMainForm = class(TLightForm)
    Log                : TRichLog;
    pgCtrl             : TPageControl;

    btnClear           : TButton;
    btnStreamRead      : TButton;
    btnStreamWrite     : TButton;
    chkAutoOpen        : TCubicCheckBox;
    edtFile2           : TCubicPathEdit;
    edtOutput          : TCubicPathEdit;
    InternetLabel      : TInternetLabel;
    Label1             : TLabel;
    Label2             : TLabel;
    lblVers            : TLabel;
    pnlBottom          : TPanel;
    RichLogTrckbr1     : TRichLogTrckbr;
    StatBar            : TStatusBar;
    tabLog             : TTabSheet;
    tabMain            : TTabSheet;
    TabSheet1          : TTabSheet;
    tabStreamBuff      : TTabSheet;
    Panel1: TPanel;
    spnCacheSize: TCubicSpinEdit;
    GroupBox1: TGroupBox;
    btnLines: TButton;
    btnRandom: TButton;
    btnReadChar1: TButton;
    GroupBox2: TGroupBox;
    btnReadChar2: TButton;
    btnNewDelphiStream: TButton;
    lblReadWrite: TLabel;
    lblTestSpeed: TLabel;
    btnSaveIni: TButton;
    procedure FormClose               (Sender: TObject; var Action: TCloseAction);
    procedure actExitExecute          (Sender: TObject);
    procedure btnClearClick           (Sender: TObject);
    procedure SwitchToLog             (Sender: TObject);
    procedure btnSaveIniClick         (Sender: TObject);
    procedure btnNewDelphiStreamClick (Sender: TObject);
    procedure btnReadChar2Click       (Sender: TObject);
    procedure btnStreamWriteClick     (Sender: TObject);
    procedure btnStreamReadClick      (Sender: TObject);
  private
  public
    procedure FormPostInitialize; override; // Called after the main form was fully created
 end;

VAR
   MainForm: TMainForm;

IMPLEMENTATION {$R *.dfm}

{.$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads

USES
   system.Math, LightCore, LightCore.StreamBuff2, LightVcl.Common.Debugger;





{--------------------------------------------------------------------------------------------------
   APP START/CLOSE
--------------------------------------------------------------------------------------------------}
procedure TMainForm.FormPostInitialize;
begin
 inherited FormPostInitialize;
 //SetCaption('');
 lblVers.Caption:= 'Version: '+ TAppData.GetVersionInfoV;
 Show;
end;


procedure TMainForm.actExitExecute(Sender: TObject);
begin
 Close;
end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
end;












{--------------------------------------------------------------------------------------------------
   LOG
--------------------------------------------------------------------------------------------------}
procedure TMainForm.btnClearClick(Sender: TObject);
begin
 Log.Clear;
end;


procedure TMainForm.SwitchToLog(Sender: TObject);
begin
 if chkAutoOpen.Checked
 then pgCtrl.ActivePage:= tabLog;
 Application.ProcessMessages;
end;








{--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------}

{ Delphi's TBufferedFileStream
  Read characters one by one

  Almost two times faster! }
procedure TMainForm.btnReadChar2Click(Sender: TObject);
VAR
   DelphiStream: TBufferedFileStream;
   Count: Cardinal;
   CacheSize: Integer;
   Char: AnsiChar;
begin
 SwitchToLog(Self);
 Log.AddVerb('  Loading...');
 Count:= 0;
 CacheSize:= spnCacheSize.Value;

 DelphiStream:= TBufferedFileStream.Create(edtFile2.Path, fmOpenRead, CacheSize * KB);
 DelphiStream.Position:= 0;                                      { Go to first line }

 TimerStart;
 WHILE DelphiStream.Read(Char, 1) > 0 DO
     if Char = #32
     then Inc(Count);

 Log.AddInfo('Cache size: ' + IntToStr(spnCacheSize.Value)+ 'KB.  Time: '+ TimerElapsedS+ '.  Count: '+ IntToStr(Count)+ '.  RAM: '+ ProcessPeakMem);
 log.SaveAsRtf(AppData.ExeFolder+ 'Log.rtf');

 FreeAndNil(DelphiStream);
end;



procedure TMainForm.btnNewDelphiStreamClick(Sender: TObject);
VAR
   DelphiStream: TBufferedFileStream;
   FileSize, LineCount2: Cardinal;
   Buff, i, CacheSize: Integer;
begin
 Randomize;
 SwitchToLog(Self);
 Log.AddVerb('  Loading...');
 LineCount2:= 0;
 CacheSize:= spnCacheSize.Value;

 DelphiStream:= TBufferedFileStream.Create(edtFile2.Path, fmOpenRead, CacheSize * KB);
 FileSize:= DelphiStream.Size;
 //DelphiStream.FirstLine;                                        { Go to first line }

 TimerStart;
 for i:= 1 to 100000 DO
  begin
   DelphiStream.Position:= Random(FileSize);
   DelphiStream.Read(Buff, 4);
   Inc(LineCount2)
  end;

 Log.AddInfo('Cache size: ' + IntToStr(spnCacheSize.Value)+ 'KB.  Time: '+ TimerElapsedS+ '.  Count: '+ IntToStr(LineCount2)+ '.  RAM: '+ ProcessPeakMem);
 log.SaveAsRtf(AppData.ExeFolder+ 'Log.rtf');

 FreeAndNil(DelphiStream);
end;






{--------------------------------------------------------------------------------------------------
   READ/WRITE TEST
--------------------------------------------------------------------------------------------------}
procedure TMainForm.btnStreamWriteClick(Sender: TObject);
VAR
   TSL: TStringList;
   Stream: TCubicBuffStream2;
   dDate: TDate;
begin
  Caption:= 'Writing...';
  dDate:= EncodeDate(2024, 08, 10);
  TSL:= TStringList.Create;
  TSL.Add('List0');
  TSL.Add('List1');

  Stream:= TCubicBuffStream2.CreateWrite('TCubicBuffStream.bin');
  TRY
   Stream.WriteHeader   ('MagicNo', 1);
   Stream.WriteBoolean  (TRUE);
   Stream.WriteDate     (dDate);
   Stream.WriteByte     (255);
   Stream.WriteCardinal (High(Cardinal));
   Stream.WriteInt64    (High(Int64));
   Stream.WriteInteger  (High(Integer));
   Stream.WriteShortInt (High(ShortInt));
   Stream.WriteSmallInt (High(Smallint));
   Stream.WriteUInt64   (High(UInt64));
   Stream.WriteWord     (65535);
   Stream.WriteStringA  ('abba');
   Stream.WriteString   ('abba');
   Stream.WriteChars    (AnsiString('x'));
   Stream.WriteChars    ('x');
   Stream.WriteEnter;
   Stream.WriteStrings(TSL);
   Stream.WriteStrings(TSL);

   Stream.WriteDouble   (3.14);
   Stream.WriteSingle   (3.14);

   Stream.WritePadding(30);
   Stream.WritePaddingDef;
   Stream.WritePadding(128);

   Stream.WriteCheckPoint('!');
  FINALLY
    FreeAndNil(Stream);
    FreeAndNil(TSL);
  END;

  Caption:= 'Write successful';
end;



procedure TMainForm.btnStreamReadClick(Sender: TObject);
VAR
   TSL: TStringList;
   Stream: TCubicBuffStream2;
   dDate: TDate;
begin
  Caption:= 'Reading...';
  dDate:= EncodeDate(2024, 08, 10);

  Stream:= TCubicBuffStream2.CreateRead('TCubicBuffStream.bin');
  TRY
   if NOT Stream.ReadHeader('MagicNo', 1)
   then RAISE Exception.Create('Cannot read header!');

   if NOT Stream.ReadBoolean                  then RAISE Exception.Create('Bool failure!');
   if Stream.ReadDate       <> dDate          then RAISE Exception.Create('Date failure!');
   if Stream.ReadByte       <> 255            then RAISE Exception.Create('ReadByte failure!');
   if Stream.ReadCardinal   <> High(Cardinal) then RAISE Exception.Create('ReadCardinal failure!');
   if Stream.ReadInt64      <> High(Int64)    then RAISE Exception.Create('ReadInt64 failure!');
   if Stream.ReadInteger    <> High(Integer)  then RAISE Exception.Create('ReadInteger failure!');
   if Stream.ReadShortInt   <> High(ShortInt) then RAISE Exception.Create('ReadShortInt failure!');
   if Stream.ReadSmallInt   <> High(Smallint) then RAISE Exception.Create('ReadSmallInt failure!');
   if Stream.ReadUInt64     <> High(UInt64)   then RAISE Exception.Create('ReadUInt64 failure!');
   if Stream.ReadWord       <> 65535          then RAISE Exception.Create('ReadWord failure!');
   if Stream.ReadStringA    <> 'abba'         then RAISE Exception.Create('ReadStringA failure!');
   if Stream.ReadString     <> 'abba'         then RAISE Exception.Create('ReadString failure!');
   if Stream.ReadCharsA(1)  <> 'x'            then RAISE Exception.Create('ReadCharsU failure!');
   if Stream.ReadChars(1)   <> 'x'            then RAISE Exception.Create('ReadCharsA failure!');
   if NOT Stream.ReadEnter                    then RAISE Exception.Create('ReadEnter failure!');
   TSL:= Stream.ReadStrings;
   if TSL[0] <> 'List0'                       then RAISE Exception.Create('ReadStrings failure!');
   if TSL[1] <> 'List1'                       then RAISE Exception.Create('ReadStrings failure!');
   TSL.Clear;
   Stream.ReadStrings(TSL);
   if TSL[0] <> 'List0'                       then RAISE Exception.Create('ReadStrings failure!');
   if TSL[1] <> 'List1'                       then RAISE Exception.Create('ReadStrings failure!');
   FreeAndNil(TSL);

   if NOT SameValue(Stream.ReadDouble, 3.14, 0.01) then RAISE Exception.Create('ReadDouble failure!');
   if NOT SameValue(Stream.ReadSingle, 3.14, 0.01) then RAISE Exception.Create('ReadSingle failure!');

   Stream.ReadPadding(30);
   Stream.ReadPaddingDef;
   Stream.ReadPaddingE(128);
   Stream.ReadCheckPointE('!');

   {ToDo: test also these:
     ReadByteChunk: TBytes;
     ReadStringAR (CONST Len: integer): AnsiString;
     ReadStringNoLen (CONST Len: Integer): string; }
  FINALLY
    FreeAndNil(Stream);
  END;

  Caption:= 'Read successful';
end;


procedure TMainForm.btnSaveIniClick(Sender: TObject);
begin
  SaveForm;
end;

end.




(*
procedure TMainForm.btnLinesClick(Sender: TObject);
VAR
   Stream2: TReadCachedStream;  //Hefferman
   InpLine: AnsiString;
   LineCount2: Cardinal;
   x, Buff, CacheSize: Integer;
begin
 SwitchToLog(Self);
 Log.AddVerb('  Loading...');
 LineCount2:= 0;
 CacheSize:= spnCacheSize.Value;

 Stream2:= TReadCachedStream.Create(edtFile2.Path, CacheSize * KB);
 Stream2.MaxLineLength:= 1* KB;
 Stream2.FirstLine;                                        { Go to first line }

 TimerStart;
 WHILE NOT Stream2.EOF DO
  begin
   InpLine := Stream2.ReadLine;
   Inc(LineCount2)
  end;

 {
 TimerStart;
 WHILE Stream2.Position < Stream2.Size DO
  begin
   //InpLine := DelphiStream.ReadLine;  <------------ must be implemented
   x := Stream2.Read(Buff, 4);
   Inc(LineCount2)
  end;    }

 Log.AddInfo('Cache size: ' + IntToStr(spnCacheSize.Value)+ 'KB.  Time: '+ TimerElapsedS+ '.  Count: '+ IntToStr(LineCount2)+ '.  RAM: '+ ProcessPeakMem);
 log.SaveAsRtf(AppData.ExeFolder+ 'Log.rtf');

 FreeAndNil(Stream2);
end;


procedure TMainForm.btnRandomClick(Sender: TObject);
VAR
   Stream2: TReadCachedStream;
   InpLine: AnsiString;
   FileSize, LineCount2: Cardinal;
   x, i, CacheSize: Integer;
begin
 Randomize;
 SwitchToLog(Self);
 Log.AddVerb('  Loading...');
 LineCount2:= 0;
 CacheSize:= spnCacheSize.Value;

 Stream2:= TReadCachedStream.Create(edtFile2.Path, CacheSize * KB);
 FileSize:= Stream2.Size;
 Stream2.FirstLine;                                        { Go to first line }

 TimerStart;
 for i:= 1 to 100000 DO
  begin
   Stream2.Position:= Random(FileSize);
   x := Stream2.ReadInteger;
   Inc(LineCount2)
  end;

 Log.AddInfo('Cache size: ' + IntToStr(spnCacheSize.Value)+ 'KB.  Time: '+ TimerElapsedS+ '.  Count: '+ IntToStr(LineCount2)+ '.  RAM: '+ ProcessPeakMem);
 log.SaveAsRtf(AppData.ExeFolder+ 'Log.rtf');

 FreeAndNil(Stream2);
end;




{  LightCore.StreamBuff
   Read characters one by one }
procedure TMainForm.btnReadChar1Click(Sender: TObject);
VAR
   Stream: TReadCachedStream;   //TReadOnlyCachedFileStream   TReadCachedStream
   Count: Cardinal;
   x, Buff, CacheSize: Integer;
   Char: AnsiChar;
begin
 SwitchToLog(Self);
 Log.AddVerb('  Loading...');
 Count:= 0;
 CacheSize:= spnCacheSize.Value;

 Stream:= TReadCachedStream.Create(edtFile2.Path, CacheSize * KB);
 Stream.MaxLineLength:= 1* KB;
 Stream.FirstLine;                                        { Go to first line }

 TimerStart;
 WHILE Stream.Read(Char, 1) > 0 DO
     if Char = #32
     then Inc(Count);

 Log.AddInfo('Cache size: ' + IntToStr(spnCacheSize.Value)+ 'KB.  Time: '+ TimerElapsedS+ tab+ '.  Count: '+ IntToStr(Count)+ '.  RAM: '+ ProcessPeakMem);
 log.SaveAsRtf(AppData.ExeFolder+ 'Log.rtf');

 FreeAndNil(Stream);
end; *)


