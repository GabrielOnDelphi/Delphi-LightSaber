UNIT llRichLog;

// OLD LOG based on RichEdit

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt 
--------------------------------------------------------------------------------------------------------------

   A simple but effective visual log.
     The programmer can send messages to the log that will be shown or not, depending on the chousen verbosity level of the log (see Verbosity property).
     There is a pre-defined form that holds the log. To show it, call CreateLogForm in FormLog.pas
     The purpose is to have one single log window per application that will receive messages from the entire application.

   Verbosity:
     Supports several verbosity levels (verbose, info, warnings, errors, etc)
     Receives only messages that are above the specified verbosity threshold.
     For example, if the log is set to show only warnings and errors and you send a messages marked as "verbose", then the messages will not be shown.

   Internals:
     It creates an internal TRamLog object.

   Future plans:
     TRamLog even though fully functional, it is a bit outdated. The plan is to be replaced with VisLogRam.

   Tester:
     c:\Myprojects\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.StdCtrls, Vcl.ComCtrls,
   llRichLogUtils;

TYPE
  TRichLog = class(TRichEdit)
   private
     FVerbosity  : TLogVerb;
     FAutoScroll : Boolean;
     FInsertDate : Boolean;
     FInsertTime : Boolean;
     FLogError   : TNotifyEvent;
     FLogWarn    : TNotifyEvent;
     function  getVerbosityI: Integer;
     procedure setVerbosityI(const Value: Integer);
     procedure Add(s: string);
     procedure AddFormated(s: string);
     procedure ScrollDown;
   protected
     Indent: Integer;    { Indent new added lines x spaces }
     procedure addColorMsg (CONST Mesaj: string; TextColor: TColor);
   public
     constructor Create(AOwner: TComponent); override;
     procedure RemoveLastEmptyRows;
     procedure CopyAll;
     { I/O }
     procedure SaveAsRtf   (CONST FullPath: string);
     procedure AppendToFile(CONST FullPath: string);
     function  LoadFromFile(CONST FullPath: string): Boolean;
     { Messages with standard verbosity }
     procedure AddVerb     (CONST Mesaj: string);
     procedure AddHint     (CONST Mesaj: string);
     procedure AddInfo     (CONST Mesaj: string);
     procedure AddImpo     (CONST Mesaj: string);
     procedure AddWarn     (CONST Mesaj: string);
     procedure AddError    (CONST Mesaj: string);
     { Messages without verbosity (forced messages) }
     procedure AddBold     (CONST Mesaj: string);
     procedure AddMsg      (CONST Mesaj: string);                        overload;
     procedure AddMsg      (CONST Mesaj: string; MsgType: TLogVerb);     overload;
     procedure AddMsgInt   (CONST Mesaj: string; i: Integer);
     procedure AddInteger  (CONST i: Integer);
     procedure AddFromFile (CONST FileName: string; MsgType: TLogVerb);
     procedure AddEmptyRow;
     procedure AddDateStamp;                                                                       { Adds the date/time, on a new line }
     {}
     property  VerbosityAsInt : Integer  read getVerbosityI  write setVerbosityI;                  { Indexed in 0 }
   published
     property InsertTime: Boolean       read FInsertTime    write FInsertTime    default FALSE;
     property InsertDate: Boolean       read FInsertDate    write FInsertDate    default FALSE;
     property AutoScroll: Boolean       read FAutoScroll    write FAutoScroll    default TRUE;                { Automatically scroll to show the last line }
     property Verbosity : TLogVerb      read FVerbosity     write FVerbosity     default DefaultVerbosity;

     property OnError: TNotifyEvent read FLogError   write FLogError;                                         { Can be used to inform the application to automatically switch to log when an error is listed }
     property OnWarn : TNotifyEvent read FLogWarn    write FLogWarn;
  end;


procedure Register;


IMPLEMENTATION

USES ccTextFile, ccCore;

const
  LogDefaultColor = clBlack;



constructor TRichLog.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);                                                   // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create        //if csCreating in ControlState then exit;

 Text        := 'Log';
 ScrollBars  := ssBoth;
 FVerbosity  := DefaultVerbosity;
 MaxLength   := $7FFFFFF0;                                                    { http://www.delphipages.com/forum/showthread.php?t=166695 }
 WordWrap    := FALSE;
 FAutoScroll := TRUE;
 Indent      := 0;                                                            { Intend new added lines with x spaces }
 FInsertTime := FALSE;
 FInsertDate := False;
end;







{-------------------------------------------------------------------------------------------------------------
   Messages with standard verbosity
-------------------------------------------------------------------------------------------------------------}
procedure TRichLog.AddVerb;
begin
 if (Verbosity<= lvrVerbose) AND (Mesaj> '')
 then addColorMsg(Mesaj, ctLogVerb);
end;

procedure TRichLog.AddInfo;
begin
 if (Verbosity<= lvrInfos) AND (Mesaj> '')
 then addColorMsg(Mesaj, ctLoginfo);
end;

procedure TRichLog.AddHint;
begin
 if (Verbosity<= lvrHints) AND (Mesaj> '')
 then addColorMsg(Mesaj, ctLogHint);
end;

procedure TRichLog.AddImpo;
begin
 if (Verbosity<= lvrImportant) AND (Mesaj> '')
 then addColorMsg(Mesaj, ctLogImprt);
end;

procedure TRichLog.AddWarn;
begin
 if (Verbosity<= lvrWarnings) AND (Mesaj> '')
 then addColorMsg(Mesaj, ctLogWarn);

 if Assigned(FLogWarn)
 then FLogWarn(Self);
end;

procedure TRichLog.AddError;
begin
 if (Verbosity<= lvrErrors) AND (Mesaj> '')
 then addColorMsg(Mesaj, ctLogError);

 if Assigned(FLogError)
 then FLogError(Self);
end;












{-------------------------------------------------------------------------------------------------------------
   Messages without verbosity
   These are forced messages: always shown regardless on which verbosity level the log is set)
-------------------------------------------------------------------------------------------------------------}
procedure TRichLog.addColorMsg(CONST Mesaj: string; TextColor: TColor);
VAR
   i: Integer;
   s: string;
   TSL: TStringList;
   Restore: TFontStyles;
begin
 Restore:= SelAttributes.Style;           { Font attributes are lost when we call sss (SelStart), so we need to memorize and restore them }

 { We split the text in multiple lines if it contains enters }
 TSL:= TStringList.Create;
 try
   TSL.Text:= ReplaceCharF(Mesaj, #0, ' '); { TSL is not split correctly in multiple lines if the #0 char is present + TrichEdit behave odd if the #0 char is present. Without this we get: "EOutOfResources-RichEdit line insertion error" }

   { We add the lines one by one }
   for i:= 0 to TSL.Count-1 DO
    begin
     s:= TSL[i];

     if InsertTime
     then s:= TimeToStr(Now)+ '  '+ s;
     if InsertDate
     then s:= DateToStr(Now)+ ' '+ s;

     s:= StringOfChar(' ', Indent)+ s;

     SelStart:= Length(Text);
     SelAttributes.Color:= TextColor;         { Other attributes: SelAttributes.Size := 8; SelAttributes.Name := 'MS Sans Serif' }

     Add(s);
    end;

   { Restore text attributes }
   SelAttributes.Style:= Restore;
   if AutoScroll
   then ScrollDown;

 finally
   FreeAndNil(TSL);
 end;
end;


procedure TRichLog.AddBold;
begin
 SelAttributes.Style := [fsBold];
 addColorMsg(Mesaj, LogDefaultColor);
 SelAttributes.Style := [];
end;


{ Always show this message, no matter the verbosity of the log. Equivalent to Log.AddError but the msg won't be shown in red. }
procedure TRichLog.AddMsg(CONST Mesaj: string);
begin
 addColorMsg(Mesaj, LogDefaultColor);
end;


procedure TRichLog.AddMsg(CONST Mesaj: string; MsgType: TLogVerb);
begin
 case MsgType of
   lvrVerbose  : AddVerb(Mesaj);
   lvrHints    : AddHint(Mesaj);
   lvrInfos    : AddInfo(Mesaj);
   lvrImportant: AddImpo(Mesaj);
   lvrWarnings : AddWarn(Mesaj);
   lvrErrors   : AddError(Mesaj);
 end;
end;



procedure TRichLog.AddInteger(CONST i: Integer);
begin
 AddInfo(IntToStr(i));
end;


{ Shows a text followed by an integer }
procedure TRichLog.AddMsgInt(CONST Mesaj: string; i: Integer);
begin
 AddInfo(Mesaj+ IntToStr(i));
end;


procedure TRichLog.AddEmptyRow;
VAR RestoreTime, RestoreDate: Boolean;
begin
 RestoreTime:= InsertTime;
 RestoreDate:= InsertDate;
 InsertTime:= FALSE;
 InsertDate:= FALSE;

 AddMsg(' ');     { CANNOT BE EMPTY }
 InsertTime:= RestoreTime;
 InsertDate:= RestoreDate;
end;


procedure TRichLog.AddDateStamp;
VAR RestoreTime, RestoreDate: Boolean;
begin
 RestoreTime:= InsertTime;
 RestoreDate:= InsertDate;
 InsertTime:= FALSE;
 InsertDate:= FALSE;

 addColorMsg(DateToStr(Now), LogDefaultColor);

 InsertTime:= RestoreTime;
 InsertDate:= RestoreDate;
end;


procedure TRichLog.Add(s: string);
begin
 s:= ReplaceEnters(s, ' ');
 AddFormated(s);
end;


{ For strings that contain enters }
procedure TRichLog.AddFormated(s: string);
begin
 { Fix bug in TRichEdit: RichEdit behaves erratically when you try to store the #0 character }
 { Details:
          http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_26783874.html
          http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Q_26812899.html#a34864849 }
 ReplaceChar(s, #0, ' ');

 {todo 3: fix this: TRichEdit does not support ENTER at the beginning of the line! }
 Lines.Add(s);
end;








{-----------------------------------------
   ADD MULTIPLE LINES
-----------------------------------------}

{ Reads lines from the specified file and adds them to the log using the specified color. }
procedure TRichLog.AddFromFile(const FileName: string; MsgType: TLogVerb);
VAR
   s: string;
   TSL: TStringList;
begin
 TSL:= StringFromFileTSL(FileName);
 TRY
  for s in TSL
    DO AddMsg(s, MsgType);
 FINALLY
  FreeAndNil(TSL);
 END;
end;









{-----------------------------------------
   LOAD/SAVE
-----------------------------------------}
procedure TRichLog.SaveAsRtf(CONST FullPath: string);
begin
 Lines.SaveToFile(FullPath);
end;


procedure TRichLog.AppendToFile(CONST FullPath: string);
begin
 StringToFile(FullPath, Lines.Text, woAppend);
end;


function TRichLog.LoadFromFile(const FullPath: string): Boolean;
begin
 Result:= FileExists(FullPath);
 if Result
 then Lines.LoadFromFile(FullPath);
end;






{-----------------------------------------
   UTILS
-----------------------------------------}

{ Remove last rows from the Log if they are empty. Unfortunatelly, for some weird reason, the RichEdit will always add an empty row at its end and we cannot access this last row }
procedure TRichLog.RemoveLastEmptyRows;
var
  i: Integer;
begin
 for i:= Lines.Count-1 downto 0 DO
  if Lines[i] = ''
  then Lines.Delete(i)
  else EXIT
end;



procedure TRichLog.CopyAll;
begin
 SelectAll;                                                                                        { This is mandatory else it won't copy nothing }
 CopyToClipboard;                                                                                  { Copy selection }
end;



procedure TRichLog.ScrollDown;                                                               { Scroll at the end of the text }
begin
 PostMessage(Handle, WM_VSCROLL, SB_BOTTOM, 0);                                                    { from here: http://www.delphi3000.com/articles/article_845.asp?SK= }
end;



function TRichLog.getVerbosityI: Integer;
begin
 Result:= Ord(Verbosity);
end;


procedure TRichLog.setVerbosityI(const Value: Integer);
begin
 Verbosity:= TLogVerb(Value);
end;










procedure Register;
begin
  RegisterComponents('LightSaber', [TRichLog]);
end;






end.
