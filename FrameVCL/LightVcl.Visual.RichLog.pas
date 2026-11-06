UNIT LightVcl.Visual.RichLog;

{=============================================================================================================
   2026.11.06
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   TRichLog
     A visual log based on TRichEdit.
     It shows the lines of a TRamLog, as one of the log's observers (ILogObserver), like TLogViewer does.
     Every line keeps the color of its level (the Verb2Color palette) and its bold flag.

   Which view to use
     TLogViewer (c:\Projects\LightSaber\FrameVCL\LightVcl.Common.LogViewer.pas) is a grid with columns: the view for very long logs.
     TRichLog is rich text: an entry may span several lines, and the user can select text with the mouse and copy it.

   The log behind the control
     A TRichLog creates its own TRamLog (property RamLog), so a control dropped on a form works at once.
     The own log never pops up a window (ShowOnError FALSE) and is single-threaded.
     AssignExternalRamLog shows another log instead, for example AppDataCore.RamLog.
     The own log is freed then.
     A TRichLog never frees an external log.

   An external log must outlive the TRichLog
     TRamLog is a TObject without free notification, so the destructor of TRichLog unregisters from the log it shows.
     If that log is already freed, the destructor writes to freed memory.
     The VCL order makes this easy to break: a form fires OnDestroy from TCustomForm.BeforeDestruction (c:\Delphi\Delphi 13\source\vcl\Vcl.Forms.pas:5498), and frees its controls later, in TComponent.Destroy -> DestroyComponents (c:\Delphi\Delphi 13\source\rtl\common\System.Classes.pas:17590).
     So a form must never free, in its OnDestroy, a log that one of its TRichLog controls observes, unless it frees that TRichLog first.
     Set FormDestroying to TRUE in OnDestroy: Populate and PopUpWindow then do nothing.

   Verbosity
     The slider levels (TLogVerb) filter the levels stored in the log (TLogVerbLvl).
     Verb2Level converts one into the other.
     TLogVerb has no Debug level: at lvrVerbose the debug lines show too, in the lvrVerbose color.
     Every line is filtered by its level, as in TLogViewer, so two views of one log show the same lines.
     That includes AddMsg with one parameter, AddBold, AddEmptyRow and AddDateStamp: they store an Info line, so a slider above lvrInfos hides them.
     The old TRichLog showed them at every slider position.
     A hidden line stays in the log: lowering the slider shows it again.

   Virtual scrolling
     The rich edit holds only the entries that fit on screen, to save RAM: the page is drawn again from the log when the view moves.
     The vertical scroll bar is a child TScrollBar whose range counts entries; the rich edit window has no vertical scroll bar of its own.
     The child bar changes how the VCL paints the control: WMPaint says why it passes WM_PAINT straight to the rich edit.
     The text never runs past the page (appendAtEnd trims first): an append past the bottom moved the child bar up by one line (measured).
     A horizontal scroll of the rich edit moved the child bar sideways (measured: Left 360 to 66 on WM_HSCROLL SB_RIGHT): WndProc puts the bar back.
     The horizontal scroll bar stays, always visible (ES_DISABLENOSCROLL), so the height of the page never changes.
     The page is counted in screen lines: an entry takes one line, plus one per line break inside it.
     A page may end in the middle of an entry, and an entry taller than the page is cut at the bottom of the page.
     Ctrl+C on a mouse selection copies only what is on screen.
     CopyAll, SaveAsRtf and AppendToFile give the whole log.
     Page Up/Down, Ctrl+Home/End, the Up and Down keys at the first and the last line, and the mouse wheel move the view by entries.
     WordWrap is forced off (CreateParams sets ES_AUTOHSCROLL, Loaded resets the property): the page math needs one screen line per paragraph.
     The WM_VSCROLL of the child bar goes to the bar itself: TWinControl.WMVScroll calls DoControlMsg first (c:\Delphi\Delphi 13\source\vcl\Vcl.Controls.pas:11741-11744).
     Under a VCL style the scroll messages still reach the control.
     TRichEditStyleHook is a TScrollingStyleHook (c:\Delphi\Delphi 13\source\vcl\Vcl.ComCtrls.pas:5546), whose WMVScroll and WMMouseWheel (c:\Delphi\Delphi 13\source\vcl\Vcl.Forms.pas:16392-16404) pass the message to the WindowProc of the control through CallDefaultProc (c:\Delphi\Delphi 13\source\vcl\Vcl.Themes.pas:7374), then paint.

   Events
     OnError and OnWarn fire once for every error or warning line that arrives in the log, whoever wrote it, also for a line the slider hides.
     They fire after the line is drawn.
     They do not fire on a redraw (a change of Verbosity, InsertTime or InsertDate), for the lines already in a log when it is attached, or for the lines present after a reset (Clear, LoadFromFile, an overflow save).
     The line that triggers an overflow save of the TRamLog gets no event: TRamLog moves it into the snapshot on disk before any Populate sees it.
     An event handler may write to the log: the nested Populate is deferred and the outer one loops, so the control never draws recursively.

   Time and date prefix
     InsertTime and InsertDate show the time stored with each entry, not the time of the draw, and apply to every drawn line.
     Switching them draws the page again.
     An empty entry gets no prefix.
     CopyAll, SaveAsRtf and AppendToFile write the messages without the prefix.

   Writes past the log are not supported
     ReadOnly is TRUE by default, so the user cannot type into the page.
     Text written straight into the rich edit (Text:= ..., Lines.Add, Lines.LoadFromFile) is not in the log, so the next draw removes it.
     Write through the Add methods or through RamLog.
     Clear empties the log the control shows, also an external log.
     Text that a DFM stores in Lines.Strings becomes the first entry of the own log, unless it is a placeholder (Log, Log:, RichLog).

   Files
     SaveAsRtf saves the whole log as RTF, in the Verb2Color palette.
     AppendToFile appends the whole log as plain text.
     LoadFromFile reads a text or RTF file and replaces the log with it: each text line becomes one Info entry.
     RemoveLastEmptyRows does nothing: an entry cannot be removed from a TRamLog.
     It stays because callers still use it (the Price calculator).

   Tester:
     c:\Projects\LightSaber\Demo\VCL\Demo LightLog\VCL_Demo_Log.dpr
=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Types,
   Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls,
   LightCore.LogTypes, LightCore.LogLinesAbstract, LightCore.LogRam, LightVcl.Visual.RichLogUtils;

TYPE
  TRichLog = class(TRichEdit, ILogObserver)
   private
     TYPE
       { One drawn entry }
       TDrawnEntry = record
         Chars: Integer;      { Characters of the entry in the rich edit, without the paragraph break in front of it }
         Lines: Integer;      { Screen lines it takes }
       end;
   private
     FRamLog         : TRamLog;
     FOwnRamLog      : Boolean;
     FFormDestroying : Boolean;
     FScrollBar      : TScrollBar;           { The vertical scroll bar. Its range counts filtered entries. }
     FUpdatingBar    : Boolean;              { TRUE while the code moves the bar, so its OnChange does not scroll again }
     { The page }
     FTopEntry       : Integer;              { Filtered index of the first drawn entry }
     FDrawn          : array of TDrawnEntry; { The drawn entries, top first. Only the first FDrawnCount are used. }
     FDrawnCount     : Integer;
     FDrawnLineTotal : Integer;              { Screen lines of the drawn entries }
     FDrawnCapacity  : Integer;              { The capacity the page was drawn for }
     FLastCut        : Boolean;              { The last drawn entry did not fit and was cut }
     FDrawnValid     : Boolean;              { FALSE = the rich edit does not hold the page described by the fields above }
     FLinePitch      : Integer;              { Height of one screen line, in pixels. 0 = not known yet. }
     FPitchMeasured  : Boolean;              { FLinePitch was measured on the drawn text }
     { What the control has read from the log }
     FSeenCount      : Integer;              { Raw lines of the log already read }
     FSeenReset      : Integer;              { Lines.ResetCount of the log at the last read }
     FFilteredCount  : Integer;              { Lines among the first FSeenCount that pass the slider }
     { Re-entrance guards }
     FDrawing        : Boolean;
     FDrawDark       : Boolean;
     FRedrawOff      : Boolean;
     FRedrawPending  : Boolean;
     FPopulating     : Boolean;
     FPendingPopulate: Boolean;
     FBatching       : Boolean;              { TRUE while the control writes many lines into its log: each line defers its Populate }
     FInCreateWnd    : Boolean;
     { Properties }
     FVerbosity      : TLogVerb;
     FAutoScroll     : Boolean;
     FInsertDate     : Boolean;
     FInsertTime     : Boolean;
     FLogError       : TNotifyEvent;
     FLogWarn        : TNotifyEvent;
     function  getVerbosityI: Integer;
     procedure setVerbosityI(const Value: Integer);
     procedure setVerbosity (Value: TLogVerb);
     procedure setInsertTime(Value: Boolean);
     procedure setInsertDate(Value: Boolean);
     procedure setTopEntry  (Value: Integer);
     function  IsDark: Boolean;
     { The log }
     procedure detachLog;
     procedure ensureHandle;
     procedure processLog;
     procedure resync;
     function  readNewEntries(out NewLines: TArray<RLogLine>): Boolean;
     function  readPage(First, MaxCount: Integer): TArray<RLogLine>;
     function  countFiltered(Limit: Integer): Integer;
     procedure fireEvents(CONST NewLines: TArray<RLogLine>);
     procedure flushPending;
     { The page }
     function  canDraw: Boolean;
     function  capacity: Integer;
     function  linePitch: Integer;
     function  pitchChanged: Boolean;
     function  isAtEnd: Boolean;
     function  bottomTop(Cap: Integer): Integer;
     procedure redraw;
     procedure requestRedraw;
     procedure drawBottom;
     procedure drawFrom(Top: Integer);
     procedure drawEntries(CONST Entries: TArray<RLogLine>; From, Top, Cap: Integer);
     procedure showNewEntries(CONST NewLines: TArray<RLogLine>; OldFiltered, NewVisible: Integer);
     procedure appendAtEnd(CONST NewLines: TArray<RLogLine>; Cap: Integer; KeepBottom: Boolean);
     procedure appendEntry(CONST Line: RLogLine; MaxLines: Integer);
     function  entryText(CONST Line: RLogLine; MaxLines: Integer; out LineCount: Integer; out Cut: Boolean): string;
     procedure trimTop;
     procedure beginDraw;
     procedure endDraw;
     { The scroll bar }
     procedure updateBar(BottomEntry: Integer);
     procedure applyFormatRect;
     procedure scrollBarChange(Sender: TObject);
     function  wheelStep: Integer;
     function  getDfmText: string;
     procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
     procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
   protected
     Indent: Integer;    { Indent each drawn line with this many spaces }
     procedure CreateParams(var Params: TCreateParams); override;
     procedure CreateWnd; override;
     procedure DestroyWnd; override;
     procedure Loaded; override;
     procedure Resize; override;
     procedure Change; override;
     procedure WndProc(var Message: TMessage); override;
     procedure KeyDown(var Key: Word; Shift: TShiftState); override;
     function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
     function  DoMouseWheelUp  (Shift: TShiftState; MousePos: TPoint): Boolean; override;
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure Clear; override;                                  { Empties the log the control shows, also an external one }

     { The log }
     procedure AssignExternalRamLog(ExternalLog: TRamLog);       { Shows ExternalLog instead of the own log. ExternalLog must outlive this control. }
     procedure Populate;                                         { ILogObserver: draws the lines that arrived in the log }
     procedure PopUpWindow;                                      { ILogObserver: shows the form of the control }
     property  RamLog: TRamLog read FRamLog;
     property  FormDestroying: Boolean read FFormDestroying write FFormDestroying;   { Set it TRUE in the OnDestroy of the form }
     property  TopEntry: Integer read FTopEntry write setTopEntry;                   { Filtered index of the first drawn entry. Setting it scrolls; it is clamped to the last page. }

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
     { Messages stored at the Info level }
     procedure AddBold     (CONST Mesaj: string);
     procedure AddMsg      (CONST Mesaj: string);                        overload;
     procedure AddMsg      (CONST Mesaj: string; MsgType: TLogVerb);     overload;
     procedure AddMsgInt   (CONST Mesaj: string; i: Integer);
     procedure AddInteger  (CONST i: Integer);
     procedure AddFromFile (CONST FileName: string; MsgType: TLogVerb);
     procedure AddEmptyRow;
     procedure AddDateStamp;                                                                       { Adds the date, on a new line }
     {}
     property  VerbosityAsInt : Integer  read getVerbosityI  write setVerbosityI;                  { Indexed in 0 }
   published
     property InsertTime: Boolean       read FInsertTime    write setInsertTime  default FALSE;
     property InsertDate: Boolean       read FInsertDate    write setInsertDate  default FALSE;
     property AutoScroll: Boolean       read FAutoScroll    write FAutoScroll    default TRUE;                { Stay at the last entry when a new one arrives }
     property Verbosity : TLogVerb      read FVerbosity     write setVerbosity   default DefaultVerbosity;
     property ReadOnly default TRUE;                                                                                  { The user cannot type into the page. The draw code still writes into it. }

     property OnError: TNotifyEvent read FLogError   write FLogError;                                         { Can be used to inform the application to automatically switch to log when an error is listed }
     property OnWarn : TNotifyEvent read FLogWarn    write FLogWarn;
  end;


procedure Register;


IMPLEMENTATION

USES
   Winapi.RichEdit, System.Math, Vcl.Forms,
   LightCore.AppData, LightCore.TextFile, LightCore.LogLinesS,
   LightVcl.Common.Clipboard, LightVcl.Common.LogViewer;



{-------------------------------------------------------------------------------------------------------------
   HELPERS
-------------------------------------------------------------------------------------------------------------}

{ A value copy of a line of the log, so the control never keeps a pointer into the log }
function copyOf(Line: PLogLine): RLogLine;
begin
 Result.Msg   := Line.Msg;
 Result.Level := Line.Level;
 Result.Indent:= Line.Indent;
 Result.Bold  := Line.Bold;
 Result.Time  := Line.Time;
end;


{ Screen lines the message takes: one, plus one per line break (as NormalizeLogBreaks counts them) }
function lineCountOf(CONST Msg: string): Integer;
VAR
   Body: string;
   i: Integer;
begin
 Body:= NormalizeLogBreaks(Msg);
 Result:= 1;
 for i:= 1 to Length(Body) do
   if Body[i] = #13
   then Inc(Result);
end;


{ How many entries at the end of Tail fit into Cap screen lines. At least 1 when Tail is not empty: an entry taller than the page is drawn cut. }
function fitFromEnd(CONST Tail: TArray<RLogLine>; Cap: Integer): Integer;
VAR
   i, Total, LineCount: Integer;
begin
 Result:= 0;
 Total:= 0;
 for i:= High(Tail) downto 0 do
   begin
     LineCount:= lineCountOf(Tail[i].Msg);
     if (Result > 0) AND (Total + LineCount > Cap) then BREAK;
     Inc(Total, LineCount);
     Inc(Result);
     if Total >= Cap then BREAK;
   end;
end;


{ The placeholder texts that a DFM stores for a TRichLog, which are not log entries }
function isRealDfmText(CONST s: string): Boolean;
VAR Trimmed: string;
begin
 Trimmed:= Trim(s);
 Result:= (Trimmed <> '')
      AND NOT SameText(Trimmed, 'Log')
      AND NOT SameText(Trimmed, 'Log:')
      AND NOT SameText(Trimmed, 'RichLog');
end;


{ Multi-threaded log: copies the lines from raw index Seen on, in one walk under the read lock, so the reset counter, the count and the copies agree }
function readNewLocked(LogLines: TAbstractLogLines; Seen: Integer; out Reset, Total: Integer): TArray<RLogLine>;
VAR
   WalkReset, WalkTotal, Kept: Integer;
   Copies: TArray<RLogLine>;
begin
 WalkReset:= LogLines.ResetCount;   { Used when the log is empty: no callback runs then }
 WalkTotal:= 0;
 Kept:= 0;
 LogLines.ForEachLocked(
   procedure(Line: PLogLine)
   begin
     WalkReset:= LogLines.ResetCount;
     if WalkTotal >= Seen then
       begin
         if Kept >= Length(Copies)
         then SetLength(Copies, Max(16, 2 * Length(Copies)));
         Copies[Kept]:= copyOf(Line);
         Inc(Kept);
       end;
     Inc(WalkTotal);
   end);

 SetLength(Copies, Kept);
 Reset := WalkReset;
 Total := WalkTotal;
 Result:= Copies;
end;


{ Multi-threaded log: the Want visible lines from filtered index First on, among the first Limit raw lines }
function readPageLocked(LogLines: TAbstractLogLines; First, Want, Limit: Integer; MinLevel: TLogVerbLvl): TArray<RLogLine>;
VAR
   Index, Visible, Kept: Integer;
   Page: TArray<RLogLine>;
begin
 SetLength(Page, Want);
 Index:= 0;
 Visible:= 0;
 Kept:= 0;
 LogLines.ForEachLocked(
   procedure(Line: PLogLine)
   begin
     if (Index < Limit) AND (Kept < Want) AND (Line.Level >= MinLevel) then
       begin
         if Visible >= First then
           begin
             Page[Kept]:= copyOf(Line);
             Inc(Kept);
           end;
         Inc(Visible);
       end;
     Inc(Index);
   end);

 SetLength(Page, Kept);
 Result:= Page;
end;


{ Multi-threaded log: the visible lines among the first Limit raw lines }
function countFilteredLocked(LogLines: TAbstractLogLines; Limit: Integer; MinLevel: TLogVerbLvl): Integer;
VAR
   Index, Counted: Integer;
begin
 Index:= 0;
 Counted:= 0;
 LogLines.ForEachLocked(
   procedure(Line: PLogLine)
   begin
     if (Index < Limit) AND (Line.Level >= MinLevel)
     then Inc(Counted);
     Inc(Index);
   end);
 Result:= Counted;
end;




{-------------------------------------------------------------------------------------------------------------
   CREATE / DESTROY
-------------------------------------------------------------------------------------------------------------}

constructor TRichLog.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);                                                   // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create

 Text        := 'Log';                                                       { The placeholder shows in the form designer. The first draw removes it. }
 ScrollBars  := ssBoth;
 ReadOnly    := TRUE;                                                        { Matches the 'default TRUE' of the published property }
 FVerbosity  := DefaultVerbosity;
 MaxLength   := $7FFFFFF0;                                                    { http://www.delphipages.com/forum/showthread.php?t=166695 }
 WordWrap    := FALSE;
 FAutoScroll := TRUE;
 Indent      := 0;
 FInsertTime := FALSE;
 FInsertDate := FALSE;

 FScrollBar:= TScrollBar.Create(Self);
 FScrollBar.Kind    := sbVertical;                                            { Before the width: TScrollBar.SetKind swaps Width and Height }
 FScrollBar.Width   := GetSystemMetrics(SM_CXVSCROLL);
 FScrollBar.Align   := alRight;
 FScrollBar.TabStop := FALSE;
 FScrollBar.Visible := FALSE;
 FScrollBar.OnChange:= scrollBarChange;
 FScrollBar.Parent  := Self;

 FOwnRamLog:= TRUE;
 FRamLog:= TRamLog.Create(FALSE, Self as ILogObserver);
 FSeenReset:= FRamLog.Lines.ResetCount;
end;


destructor TRichLog.Destroy;
begin
 detachLog;
 FreeAndNil(FScrollBar);
 inherited;
end;


{ Frees the own log, or unregisters from an external one (the observer lifetime contract of TRamLog) }
procedure TRichLog.detachLog;
begin
 if FRamLog = NIL then EXIT;

 if FOwnRamLog
 then FreeAndNil(FRamLog)
 else
   begin
     FRamLog.UnregisterLogObserver(Self as ILogObserver);
     FRamLog:= NIL;
   end;
end;


procedure TRichLog.AssignExternalRamLog(ExternalLog: TRamLog);
begin
 if ExternalLog = NIL
 then raise Exception.Create('TRichLog.AssignExternalRamLog: ExternalLog cannot be NIL');
 if ExternalLog = FRamLog then EXIT;

 detachLog;
 FOwnRamLog:= FALSE;
 FRamLog:= ExternalLog;
 FRamLog.RegisterLogObserver(Self as ILogObserver);
 resync;   { The lines already in the log are drawn, without events }
end;




{-------------------------------------------------------------------------------------------------------------
   WINDOW
-------------------------------------------------------------------------------------------------------------}

{ No vertical scroll bar of its own (FScrollBar replaces it).
  WS_CLIPCHILDREN: the rich edit must not paint over its child scroll bar.
  ES_AUTOHSCROLL: no word wrap, whatever WordWrap says (TCustomMemo.CreateParams removes it for WordWrap).
  ES_DISABLENOSCROLL: the horizontal scroll bar stays visible, so the height of the page never changes. }
procedure TRichLog.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);
 Params.Style:= (Params.Style AND NOT WS_VSCROLL) OR WS_CLIPCHILDREN OR ES_AUTOHSCROLL OR ES_DISABLENOSCROLL;
end;


{ CreateWnd also runs when the window is recreated: TCustomRichEdit.CreateWnd then loads the old text back, and the page is drawn again over it }
procedure TRichLog.CreateWnd;
begin
 FInCreateWnd:= TRUE;          { TCustomRichEdit.CreateWnd changes the font and loads text: no draw in between }
 TRY
   inherited CreateWnd;
 FINALLY
   FInCreateWnd:= FALSE;
 END;

 applyFormatRect;
 FLinePitch    := 0;
 FPitchMeasured:= FALSE;
 FDrawnValid   := FALSE;
 if ComponentState * [csLoading, csDesigning] = []
 then redraw;
end;


procedure TRichLog.DestroyWnd;
begin
 FDrawnValid:= FALSE;
 inherited DestroyWnd;
end;


procedure TRichLog.Loaded;
VAR DfmText: string;
begin
 inherited Loaded;
 if csDesigning in ComponentState then EXIT;

 DfmText:= getDfmText;         { Before the WordWrap reset below: that may destroy the window that holds the text }
 if WordWrap
 then WordWrap:= FALSE;

 if FOwnRamLog AND isRealDfmText(DfmText) then
   begin
     FBatching:= TRUE;
     TRY
       FRamLog.AddMsg(DfmText);
     FINALLY
       FBatching:= FALSE;
     END;
   end;

 FFilteredCount:= countFiltered(FSeenCount);   { The DFM may have changed Verbosity }
 FDrawnValid:= FALSE;
 Populate;
end;


{ The text the DFM loader wrote into Lines.Strings, lines joined with CRLF }
function TRichLog.getDfmText: string;
VAR i: Integer;
begin
 if NOT HandleAllocated then EXIT(Text);

 Result:= '';
 for i:= 0 to Lines.Count - 1 do
   if i = 0
   then Result:= Lines[i]
   else Result:= Result + sLineBreak + Lines[i];
end;


{ The DFM loader (Lines.Strings), the window recreation and the draw code empty the rich edit only.
  Everything else empties the log, and the redraw then empties the rich edit. }
procedure TRichLog.Clear;
begin
 if (FRamLog = NIL)
 OR FDrawing
 OR FInCreateWnd
 OR (ComponentState * [csLoading, csDesigning, csDestroying] <> [])
 OR (csRecreating in ControlState)
 then inherited Clear
 else FRamLog.Clear;
end;


{ Creates the window when the control can have one, so a line written before the form is shown is drawn at once.
  Never during the DFM load of the control or of its form. }
procedure TRichLog.ensureHandle;
VAR Top: TWinControl;
begin
 if HandleAllocated then EXIT;
 if ComponentState * [csLoading, csDesigning, csDestroying] <> [] then EXIT;

 Top:= Self;
 while Top.Parent <> NIL do
   Top:= Top.Parent;

 if NOT (Top is TCustomForm) AND (Top.ParentWindow = 0) then EXIT;   { No window to live in }
 if Top.ComponentState * [csLoading, csDestroying] <> [] then EXIT;

 HandleNeeded;
end;




{-------------------------------------------------------------------------------------------------------------
   OBSERVER
-------------------------------------------------------------------------------------------------------------}

{ Called by the log for every change. A call that arrives while the control is busy is deferred; the outer call loops until nothing is pending. }
procedure TRichLog.Populate;
begin
 if FFormDestroying OR (csDestroying in ComponentState) OR (FRamLog = NIL) then EXIT;

 if FBatching OR FPopulating OR FDrawing then
   begin
     FPendingPopulate:= TRUE;
     EXIT;
   end;

 FPopulating:= TRUE;
 TRY
   REPEAT
     FPendingPopulate:= FALSE;
     processLog;
   UNTIL NOT FPendingPopulate OR FFormDestroying OR (csDestroying in ComponentState) OR (FRamLog = NIL);
 FINALLY
   FPopulating:= FALSE;
 END;
end;


{ Runs a Populate that was deferred by a draw outside Populate (scrolling, resizing) }
procedure TRichLog.flushPending;
begin
 if FPendingPopulate AND NOT FPopulating AND NOT FBatching AND NOT FDrawing
 then Populate;
end;


procedure TRichLog.processLog;
VAR
   NewLines: TArray<RLogLine>;
   i, NewVisible, OldFiltered: Integer;
   MinLevel: TLogVerbLvl;
begin
 ensureHandle;

 if NOT readNewEntries(NewLines) then
   begin
     resync;                  { The log was reset: draw what it holds now, without events }
     EXIT;
   end;

 MinLevel:= Verb2Level(FVerbosity);
 NewVisible:= 0;
 for i:= 0 to High(NewLines) do
   if NewLines[i].Level >= MinLevel
   then Inc(NewVisible);

 OldFiltered:= FFilteredCount;
 Inc(FFilteredCount, NewVisible);
 showNewEntries(NewLines, OldFiltered, NewVisible);
 fireEvents(NewLines);
end;


{ Once per arriving line, also a hidden one }
procedure TRichLog.fireEvents(CONST NewLines: TArray<RLogLine>);
VAR i: Integer;
begin
 for i:= 0 to High(NewLines) do
   begin
     if FFormDestroying OR (csDestroying in ComponentState) then EXIT;

     case NewLines[i].Level of
       lvErrors  : if Assigned(FLogError) then FLogError(Self);
       lvWarnings: if Assigned(FLogWarn)  then FLogWarn(Self);
     end;
   end;
end;


{ Starts again from what the log holds now: after a reset, and when a log is attached }
procedure TRichLog.resync;
begin
 FSeenReset    := FRamLog.Lines.ResetCount;
 FSeenCount    := FRamLog.Lines.Count;
 FFilteredCount:= countFiltered(FSeenCount);
 FTopEntry     := 0;
 FDrawnValid   := FALSE;

 ensureHandle;               { May draw already, through CreateWnd }
 if NOT FDrawnValid
 then redraw;
end;


{ Copies the lines that arrived since the last call.
  FALSE when the log was reset since then (its reset counter changed, or it holds fewer lines than were read). }
function TRichLog.readNewEntries(out NewLines: TArray<RLogLine>): Boolean;
VAR
   LogLines: TAbstractLogLines;
   Reset, Total, i: Integer;
begin
 NewLines:= NIL;
 LogLines:= FRamLog.Lines;

 if LogLines is TLogLinesSingleThreaded then
   begin
     Total:= LogLines.Count;
     if (LogLines.ResetCount <> FSeenReset) OR (Total < FSeenCount) then EXIT(FALSE);

     SetLength(NewLines, Total - FSeenCount);
     for i:= FSeenCount to Total - 1 do
       NewLines[i - FSeenCount]:= copyOf(LogLines[i]);
     FSeenCount:= Total;
     EXIT(TRUE);
   end;

 NewLines:= readNewLocked(LogLines, FSeenCount, Reset, Total);
 if (Reset <> FSeenReset) OR (Total < FSeenCount) then
   begin
     NewLines:= NIL;
     EXIT(FALSE);
   end;

 FSeenCount:= Total;
 Result:= TRUE;
end;


{ The visible entries with filtered index in [First, First + MaxCount), among the lines already read }
function TRichLog.readPage(First, MaxCount: Integer): TArray<RLogLine>;
VAR
   LogLines: TAbstractLogLines;
   Want, Kept, Visible, Limit, i: Integer;
   MinLevel: TLogVerbLvl;
begin
 Result:= NIL;
 if First < 0 then First:= 0;
 Want:= Min(MaxCount, FFilteredCount - First);
 if Want <= 0 then EXIT;

 LogLines:= FRamLog.Lines;
 MinLevel:= Verb2Level(FVerbosity);

 if NOT (LogLines is TLogLinesSingleThreaded)
 then EXIT(readPageLocked(LogLines, First, Want, FSeenCount, MinLevel));

 Limit:= Min(FSeenCount, LogLines.Count);
 SetLength(Result, Want);
 Kept:= 0;

 if FFilteredCount = FSeenCount then
   begin
     { Every line is visible: the filtered index is the raw index }
     for i:= First to Min(First + Want, Limit) - 1 do
       begin
         Result[Kept]:= copyOf(LogLines[i]);
         Inc(Kept);
       end;
   end
 else
   if FFilteredCount - First <= First then
     begin
       { Nearer the end: walk backward from the last line }
       Visible:= FFilteredCount;
       for i:= Limit - 1 downto 0 do
         if LogLines[i].Level >= MinLevel then
           begin
             Dec(Visible);
             if Visible < First then BREAK;
             if Visible - First < Want then
               begin
                 Result[Visible - First]:= copyOf(LogLines[i]);
                 Inc(Kept);
               end;
           end;
       if Kept < Want
       then Result:= Copy(Result, Want - Kept, Kept);   { The filled slots are the last ones }
       EXIT;
     end
   else
     begin
       Visible:= 0;
       for i:= 0 to Limit - 1 do
         if LogLines[i].Level >= MinLevel then
           begin
             if Visible >= First then
               begin
                 Result[Kept]:= copyOf(LogLines[i]);
                 Inc(Kept);
                 if Kept >= Want then BREAK;
               end;
             Inc(Visible);
           end;
     end;

 SetLength(Result, Kept);
end;


{ The visible lines among the first Limit lines of the log }
function TRichLog.countFiltered(Limit: Integer): Integer;
VAR
   LogLines: TAbstractLogLines;
   MinLevel: TLogVerbLvl;
   i: Integer;
begin
 Result:= 0;
 if FRamLog = NIL then EXIT;

 LogLines:= FRamLog.Lines;
 MinLevel:= Verb2Level(FVerbosity);
 if LogLines is TLogLinesSingleThreaded
 then
   begin
     for i:= 0 to Min(Limit, LogLines.Count) - 1 do
       if LogLines[i].Level >= MinLevel
       then Inc(Result);
   end
 else
   Result:= countFilteredLocked(LogLines, Limit, MinLevel);
end;


procedure TRichLog.PopUpWindow;
VAR
   ParentForm: TCustomForm;
begin
 if FFormDestroying OR (csDestroying in ComponentState) then EXIT;

 ParentForm:= GetParentForm(Self);
 if Assigned(ParentForm) then
   begin
     { Show if hidden }
     if NOT ParentForm.Visible
     then ParentForm.Show;

     { Restore if minimized }
     if ParentForm.WindowState = TWindowState.wsMinimized
     then ParentForm.WindowState:= TWindowState.wsNormal;

     { Bring to front }
     ParentForm.BringToFront;
   end;
end;




{-------------------------------------------------------------------------------------------------------------
   THE PAGE
-------------------------------------------------------------------------------------------------------------}

function TRichLog.canDraw: Boolean;
begin
 Result:= HandleAllocated
      AND (FRamLog <> NIL)
      AND NOT FInCreateWnd
      AND (ComponentState * [csLoading, csDesigning, csDestroying] = []);
end;


{ Screen lines that fit into the format rectangle }
function TRichLog.capacity: Integer;
VAR R: TRect;
begin
 SendMessage(Handle, EM_GETRECT, 0, LPARAM(@R));
 Result:= Max(1, (Min(R.Bottom, ClientHeight) - R.Top) div linePitch);
end;


{ The height of a screen line: first the text metrics of the font; pitchChanged replaces it with the distance measured on the drawn text }
function TRichLog.linePitch: Integer;
VAR
   DC: HDC;
   OldFont: HGDIOBJ;
   Metrics: TTextMetric;
begin
 if FLinePitch > 0 then EXIT(FLinePitch);

 DC:= GetDC(Handle);
 TRY
   OldFont:= SelectObject(DC, Font.Handle);
   GetTextMetrics(DC, Metrics);
   SelectObject(DC, OldFont);
 FINALLY
   ReleaseDC(Handle, DC);
 END;

 FLinePitch:= Max(1, Metrics.tmHeight);
 Result:= FLinePitch;
end;


{ Measures the distance between the first two drawn lines, once per font.
  TRUE when it differs from the pitch the page was drawn with: the page must be drawn again. }
function TRichLog.pitchChanged: Boolean;
VAR
   P0, P1: TPoint;
   Index1, Measured: Integer;
begin
 Result:= FALSE;
 if FPitchMeasured OR (FDrawnLineTotal < 2) OR NOT HandleAllocated then EXIT;

 Index1:= SendMessage(Handle, EM_LINEINDEX, 1, 0);
 if Index1 < 0 then EXIT;
 SendMessage(Handle, EM_POSFROMCHAR, WPARAM(@P0), 0);
 SendMessage(Handle, EM_POSFROMCHAR, WPARAM(@P1), Index1);
 Measured:= P1.Y - P0.Y;
 if Measured <= 0 then EXIT;

 FPitchMeasured:= TRUE;
 if Measured <> FLinePitch then
   begin
     FLinePitch:= Measured;
     Result:= TRUE;
   end;
end;


{ The last entry of the log is drawn whole }
function TRichLog.isAtEnd: Boolean;
begin
 Result:= NOT FDrawnValid OR ((FTopEntry + FDrawnCount >= FFilteredCount) AND NOT FLastCut);
end;


{ The top entry of the last page }
function TRichLog.bottomTop(Cap: Integer): Integer;
begin
 Result:= FFilteredCount - fitFromEnd(readPage(FFilteredCount - Cap, Cap), Cap);
end;


{ Draws the page again: at the end with AutoScroll, else from TopEntry.
  Again (at most 3 passes) when the measured line pitch differs, or a resize arrived during the draw. }
procedure TRichLog.redraw;
VAR Pass: Integer;
begin
 Pass:= 0;
 REPEAT
   if NOT canDraw then
     begin
       FDrawnValid:= FALSE;
       EXIT;
     end;

   FRedrawPending:= FALSE;
   if FAutoScroll AND isAtEnd
   then drawBottom
   else drawFrom(FTopEntry);
   Inc(Pass);
 UNTIL (Pass >= 3) OR NOT (pitchChanged OR FRedrawPending);
end;


procedure TRichLog.requestRedraw;
begin
 if FDrawing
 then FRedrawPending:= TRUE
 else
   begin
     redraw;
     flushPending;
   end;
end;


{ The last page: as many entries as fit, ending with the last one }
procedure TRichLog.drawBottom;
VAR
   Cap, TailStart, Fit: Integer;
   Tail: TArray<RLogLine>;
begin
 Cap:= capacity;
 TailStart:= Max(0, FFilteredCount - Cap);
 Tail:= readPage(TailStart, Cap);
 Fit:= fitFromEnd(Tail, Cap);
 drawEntries(Tail, Length(Tail) - Fit, FFilteredCount - Fit, Cap);
 updateBar(FTopEntry);
end;


{ The page that starts with entry Top, clamped to the last page }
procedure TRichLog.drawFrom(Top: Integer);
VAR
   Cap, TailStart, BottomTop: Integer;
   Tail: TArray<RLogLine>;
begin
 Cap:= capacity;
 TailStart:= Max(0, FFilteredCount - Cap);
 Tail:= readPage(TailStart, Cap);
 BottomTop:= FFilteredCount - fitFromEnd(Tail, Cap);
 Top:= EnsureRange(Top, 0, BottomTop);

 if Top >= TailStart
 then drawEntries(Tail, Top - TailStart, Top, Cap)
 else drawEntries(readPage(Top, Cap), 0, Top, Cap);
 updateBar(BottomTop);
end;


{ Empties the rich edit and draws Entries[From..] until the page is full. Top is the filtered index of Entries[From]. }
procedure TRichLog.drawEntries(CONST Entries: TArray<RLogLine>; From, Top, Cap: Integer);
VAR i: Integer;
begin
 beginDraw;
 TRY
   FDrawnValid:= FALSE;
   inherited Clear;           { Never Clear or Lines.Clear here: they empty the log }
   FDrawnCount    := 0;
   FDrawnLineTotal:= 0;
   FLastCut       := FALSE;

   i:= Max(0, From);
   while (i <= High(Entries)) AND (FDrawnLineTotal < Cap) do
     begin
       appendEntry(Entries[i], Cap - FDrawnLineTotal);
       Inc(i);
     end;

   FTopEntry     := Top;
   FDrawnCapacity:= Cap;
   FDrawnValid   := TRUE;
 FINALLY
   endDraw;
 END;
end;


{ Shows the lines that just arrived. Appends them when the page shows the end of the log; else draws the last page (AutoScroll) or only moves the scroll bar. }
procedure TRichLog.showNewEntries(CONST NewLines: TArray<RLogLine>; OldFiltered, NewVisible: Integer);
VAR
   Cap: Integer;
   AtEnd: Boolean;
begin
 if NOT canDraw then
   begin
     FDrawnValid:= FALSE;
     EXIT;
   end;

 if NOT FDrawnValid then
   begin
     redraw;
     EXIT;
   end;

 if NewVisible = 0 then EXIT;

 AtEnd:= (FTopEntry + FDrawnCount = OldFiltered) AND NOT FLastCut;
 Cap:= capacity;
 if Cap <> FDrawnCapacity then
   begin
     { AutoScroll ends at the last entry, as the branch below does when the page height is unchanged.
       FFilteredCount already counts the new lines, so isAtEnd sees a page above the end.
       A page that is not valid counts as at the end: redraw then draws the last page. }
     if FAutoScroll
     then FDrawnValid:= FALSE;
     redraw;
     EXIT;
   end;

 if FAutoScroll then
   begin
     if AtEnd AND (NewVisible < Cap)
     then appendAtEnd(NewLines, Cap, TRUE)
     else
       begin
         drawBottom;
         if pitchChanged OR FRedrawPending
         then redraw;
       end;
   end
 else
   begin
     if AtEnd AND (FDrawnLineTotal < Cap)
     then appendAtEnd(NewLines, Cap, FALSE)
     else updateBar(bottomTop(Cap));   { The new lines are below the page: only the range of the bar grows }
   end;
end;


{ Appends the visible new lines to the page.
  KeepBottom (AutoScroll): the entries that scroll off the top are trimmed, so the page always ends with the last entry.
  Else the page is only filled up, and the rest stays below it. }
procedure TRichLog.appendAtEnd(CONST NewLines: TArray<RLogLine>; Cap: Integer; KeepBottom: Boolean);
VAR
   i, NewLineCount: Integer;
   MinLevel: TLogVerbLvl;
   TooTall: Boolean;
begin
 MinLevel:= Verb2Level(FVerbosity);
 TooTall:= FALSE;

 beginDraw;
 TRY
   FDrawnValid:= FALSE;
   for i:= 0 to High(NewLines) do
     begin
       if NewLines[i].Level < MinLevel then CONTINUE;

       if KeepBottom then
         begin
           NewLineCount:= lineCountOf(NewLines[i].Msg);
           if NewLineCount > Cap then
             begin
               TooTall:= TRUE;   { Cannot be appended whole: drawBottom cuts it }
               BREAK;
             end;
           { Trims before the append, so the text never runs past the page.
             Measured in the Demo LightLog: each append past the bottom of the page moved the child scroll bar up by one line pitch. }
           while (FDrawnCount > 0) AND (FDrawnLineTotal + NewLineCount > Cap) do
             trimTop;
           appendEntry(NewLines[i], Cap);
         end
       else
         begin
           if FDrawnLineTotal >= Cap then BREAK;
           appendEntry(NewLines[i], Cap - FDrawnLineTotal);
         end;
     end;
   FDrawnValid:= TRUE;
 FINALLY
   endDraw;
 END;

 if TooTall
 then drawBottom
 else
   if KeepBottom
   then updateBar(FTopEntry)
   else updateBar(bottomTop(Cap));

 if pitchChanged OR FRedrawPending
 then redraw;
end;


{ Appends one entry at the end of the rich edit, cut to MaxLines screen lines }
procedure TRichLog.appendEntry(CONST Line: RLogLine; MaxLines: Integer);
VAR
   Body: string;
   LineCount, Gain: Integer;
   Cut: Boolean;
begin
 Body:= entryText(Line, Max(1, MaxLines), LineCount, Cut);
 if FDrawnCount > 0
 then Body:= #13 + Body;      { The paragraph break that ends the entry before }

 Gain:= AppendRichText(Self, Body, Level2Color(Line.Level, FDrawDark), Line.Bold);
 if FDrawnCount > 0
 then Dec(Gain);              { The paragraph break belongs to no entry }

 if FDrawnCount >= Length(FDrawn)
 then SetLength(FDrawn, Max(16, 2 * Length(FDrawn)));
 FDrawn[FDrawnCount].Chars:= Gain;
 FDrawn[FDrawnCount].Lines:= LineCount;
 Inc(FDrawnCount);
 Inc(FDrawnLineTotal, LineCount);
 FLastCut:= Cut;
end;


{ The text of an entry as it is drawn: breaks normalized, cut after MaxLines lines, every line with the indent and the time/date prefix }
function TRichLog.entryText(CONST Line: RLogLine; MaxLines: Integer; out LineCount: Integer; out Cut: Boolean): string;
VAR
   Body, Prefix: string;
   i: Integer;
begin
 Body:= NormalizeLogBreaks(Line.Msg);
 Cut:= FALSE;
 LineCount:= 1;
 for i:= 1 to Length(Body) do
   if Body[i] = #13 then
     begin
       if LineCount >= MaxLines then
         begin
           SetLength(Body, i - 1);
           Cut:= TRUE;
           BREAK;
         end;
       Inc(LineCount);
     end;

 if Line.Msg = '' then EXIT(Body);   { An empty entry gets no prefix }

 Prefix:= StringOfChar(' ', Indent + Line.Indent);
 if FInsertDate then Prefix:= Prefix + DateToStr(Line.Time) + ' ';
 if FInsertTime then Prefix:= Prefix + TimeToStr(Line.Time) + '  ';

 if Prefix = ''
 then Result:= Body
 else Result:= Prefix + StringReplace(Body, #13, #13 + Prefix, [rfReplaceAll]);
end;


{ Deletes the first drawn entry and the paragraph break after it (one character) }
procedure TRichLog.trimTop;
VAR
   Range: TCharRange;
   i: Integer;
begin
 Range.cpMin:= 0;
 Range.cpMax:= FDrawn[0].Chars + 1;
 SendMessage(Handle, EM_EXSETSEL, 0, LPARAM(@Range));
 SendMessage(Handle, EM_REPLACESEL, 0, LPARAM(PChar('')));

 Dec(FDrawnLineTotal, FDrawn[0].Lines);
 for i:= 1 to FDrawnCount - 1 do
   FDrawn[i - 1]:= FDrawn[i];
 Dec(FDrawnCount);
 Inc(FTopEntry);
end;


{ WM_SETREDRAW only while the control is on screen: WM_SETREDRAW(TRUE) can make a hidden window visible }
procedure TRichLog.beginDraw;
begin
 FDrawing  := TRUE;
 FDrawDark := IsDark;
 FRedrawOff:= Showing;
 if FRedrawOff
 then SendMessage(Handle, WM_SETREDRAW, 0, 0);
end;


{ The caret goes to the top-left (an append leaves it at the end of the last line, which may scroll the text sideways), and the rich edit itself stays unscrolled }
procedure TRichLog.endDraw;
VAR
   Range: TCharRange;
   FirstLine: Integer;
begin
 TRY
   Range.cpMin:= 0;
   Range.cpMax:= 0;
   SendMessage(Handle, EM_EXSETSEL, 0, LPARAM(@Range));

   FirstLine:= SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
   if FirstLine > 0
   then SendMessage(Handle, EM_LINESCROLL, 0, -FirstLine);
 FINALLY
   if FRedrawOff then
     begin
       SendMessage(Handle, WM_SETREDRAW, 1, 0);
       Invalidate;
     end;
   FDrawing:= FALSE;
 END;
end;




{-------------------------------------------------------------------------------------------------------------
   SCROLL BAR, KEYS, WHEEL
-------------------------------------------------------------------------------------------------------------}

{ The range of the bar is [0, BottomEntry], the top entries a page can start with }
procedure TRichLog.updateBar(BottomEntry: Integer);
VAR NeedBar: Boolean;
begin
 NeedBar:= BottomEntry > 0;
 FUpdatingBar:= TRUE;
 TRY
   if NeedBar
   then FScrollBar.SetParams(EnsureRange(FTopEntry, 0, BottomEntry), 0, BottomEntry)
   else FScrollBar.SetParams(0, 0, 0);
   FScrollBar.LargeChange:= Min(Max(1, FDrawnCount - 1), 32767);   { TScrollBarInc = 1..32767 }
 FINALLY
   FUpdatingBar:= FALSE;
 END;

 if FScrollBar.Visible <> NeedBar then
   begin
     FScrollBar.Visible:= NeedBar;
     applyFormatRect;
   end;
end;


{ Keeps the text out from under the scroll bar. EM_SETRECT with lParam 0 restores the default rectangle first.
  The bar sits at the right edge of the align rectangle, which a right Padding moves inwards (see WndProc). }
procedure TRichLog.applyFormatRect;
VAR
   R, Got, AlignRect: TRect;
   Limit: Integer;
begin
 if NOT HandleAllocated then EXIT;

 SendMessage(Handle, EM_SETRECT, 0, 0);
 if NOT Assigned(FScrollBar) OR NOT FScrollBar.Visible then EXIT;

 AlignRect:= ClientRect;
 AdjustClientRect(AlignRect);
 Limit:= AlignRect.Right - FScrollBar.Width;
 SendMessage(Handle, EM_GETRECT, 0, LPARAM(@R));
 if R.Right <= Limit then EXIT;

 R.Right:= Limit;
 SendMessage(Handle, EM_SETRECT, 0, LPARAM(@R));

 { Reads the rectangle back. When the rich edit reports it wider than the one it was given, the difference is taken off once. }
 SendMessage(Handle, EM_GETRECT, 0, LPARAM(@Got));
 if Got.Right > Limit then
   begin
     R.Right:= Limit - (Got.Right - Limit);
     SendMessage(Handle, EM_SETRECT, 0, LPARAM(@R));
   end;
end;


{ TWinControl.WMPaint sends a control that has child controls (here the scroll bar) to PaintHandler (c:\Delphi\Delphi 13\source\vcl\Vcl.Controls.pas:11644-11647).
  PaintHandler calls BeginPaint (Vcl.Controls.pas:11426), then PaintWindow hands the DC to the rich edit in the wParam of a WM_PAINT (Vcl.Controls.pas:11463-11468).
  Measured in the Demo LightLog: on that path the rich edit painted nothing, not even after InvalidateRect and UpdateWindow.
  So a WM_PAINT without a DC goes straight to the rich edit, which paints itself; the scroll bar is a window and paints itself.
  This skips TCustomRichEdit.WMPaint (c:\Delphi\Delphi 13\source\vcl\Vcl.ComCtrls.pas:16374): its repaint of the 3 right-most pixels and its disabled colours under a VCL style. }
procedure TRichLog.WMPaint(var Message: TWMPaint);
begin
 if Message.DC = 0
 then DefaultHandler(Message)
 else inherited;
end;


{ Measured in the Demo LightLog: a horizontal scroll (WM_HSCROLL SB_RIGHT) moved the child scroll bar from Left 360 to Left 66, and each append past the bottom of the page moved it up by one line pitch.
  A scroll can come from the horizontal bar, a key (measured: End on a wide line scrolls the text sideways) or the code (EM_LINESCROLL in endDraw), so the check runs after every message.
  Realign puts the bar back where Align = alRight places it: at the right edge of the align rectangle, from top to bottom (ArrangeControl, c:\Delphi\Delphi 13\source\vcl\Vcl.Controls.pas:9673-9677).
  The align rectangle is the client rectangle less the Padding (AlignControls calls AdjustClientRect, Vcl.Controls.pas:10032), so the check compares with the same rectangle.
  With a Padding, a check against the raw client rectangle would start an align pass on every message (measured: TestScrollBar_Padding_NoAlignPerMessage). }
procedure TRichLog.WndProc(var Message: TMessage);
VAR AlignRect: TRect;
begin
 inherited WndProc(Message);

 if Assigned(FScrollBar) AND FScrollBar.Visible AND HandleAllocated AND NOT (csDestroying in ComponentState) then
   begin
     AlignRect:= ClientRect;
     AdjustClientRect(AlignRect);
     if (FScrollBar.Top <> AlignRect.Top) OR (FScrollBar.Left + FScrollBar.Width <> AlignRect.Right)
     then Realign;
   end;
end;


procedure TRichLog.scrollBarChange(Sender: TObject);
begin
 if FUpdatingBar OR FDrawing then EXIT;
 setTopEntry(FScrollBar.Position);
end;


procedure TRichLog.setTopEntry(Value: Integer);
begin
 if NOT canDraw OR FDrawing then
   begin
     FTopEntry:= EnsureRange(Value, 0, Max(0, FFilteredCount - 1));
     FDrawnValid:= FALSE;
     EXIT;
   end;

 drawFrom(Value);
 if pitchChanged OR FRedrawPending
 then redraw;
 flushPending;
end;


{ Page Up/Down, Ctrl+Home/End, and Up/Down at the first/last drawn line move the view by entries. Every other key goes to the rich edit. }
procedure TRichLog.KeyDown(var Key: Word; Shift: TShiftState);
VAR
   Step, CaretLine: Integer;
begin
 inherited KeyDown(Key, Shift);
 if (Key = 0) OR NOT canDraw then EXIT;

 Step:= Max(1, FDrawnCount - 1);
 case Key of
   VK_NEXT : TopEntry:= FTopEntry + Step;
   VK_PRIOR: TopEntry:= FTopEntry - Step;
   VK_HOME :
     if ssCtrl in Shift
     then TopEntry:= 0
     else EXIT;
   VK_END  :
     if ssCtrl in Shift
     then TopEntry:= MaxInt
     else EXIT;
   VK_UP   :
     begin
       CaretLine:= SendMessage(Handle, EM_EXLINEFROMCHAR, 0, SelStart);
       if (CaretLine > 0) OR (FTopEntry = 0) then EXIT;
       TopEntry:= FTopEntry - 1;
       SelStart:= 0;
     end;
   VK_DOWN :
     begin
       CaretLine:= SendMessage(Handle, EM_EXLINEFROMCHAR, 0, SelStart);
       if (CaretLine < FDrawnLineTotal - 1) OR isAtEnd then EXIT;
       TopEntry:= FTopEntry + 1;
       SelStart:= RichEditTextLength(Self);
     end;
 else
   EXIT;
 end;

 Key:= 0;
end;


{ The lines of one wheel notch (the Windows setting), or a page when the setting is "one screen" or more than the page }
function TRichLog.wheelStep: Integer;
begin
 Result:= Mouse.WheelScrollLines;
 if (Result <= 0) OR (Result >= FDrawnCount)
 then Result:= Max(1, FDrawnCount - 1);
end;


function TRichLog.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
 Result:= inherited DoMouseWheelDown(Shift, MousePos);
 if Result OR NOT canDraw then EXIT;

 TopEntry:= FTopEntry + wheelStep;
 Result:= TRUE;
end;


function TRichLog.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
 Result:= inherited DoMouseWheelUp(Shift, MousePos);
 if Result OR NOT canDraw then EXIT;

 TopEntry:= FTopEntry - wheelStep;
 Result:= TRUE;
end;


procedure TRichLog.Resize;
begin
 inherited Resize;
 if NOT HandleAllocated OR (ComponentState * [csLoading, csDesigning, csDestroying] <> []) then EXIT;

 applyFormatRect;
 if FDrawing then
   begin
     FRedrawPending:= TRUE;
     EXIT;
   end;

 if NOT FDrawnValid OR (capacity <> FDrawnCapacity)
 then redraw;
 flushPending;
end;


{ EN_CHANGE outside a draw: a program wrote into the rich edit (Text:=, Lines.Add, SelText), or the user typed or pasted into a page with ReadOnly FALSE.
  The page then no longer matches FDrawn, and trimTop would delete the wrong characters.
  So the next draw is a full one, from the log, and it removes the foreign text.
  The rich edit sends EN_CHANGE inside the SendMessage that changed the text, also with WM_SETREDRAW off, and never later (measured on RICHEDIT50W: EM_REPLACESEL, WM_SETTEXT).
  So a change made by the draw code arrives while FDrawing is TRUE, and an append stays an append.
  Lines.Text:= is not seen: TRichEditStrings.SetTextStr turns ENM_CHANGE off while it writes (c:\Delphi\Delphi 13\source\vcl\Vcl.ComCtrls.pas:15279-15286). }
procedure TRichLog.Change;
begin
 if NOT FDrawing
 then FDrawnValid:= FALSE;
 inherited Change;
end;


procedure TRichLog.CMFontChanged(var Message: TMessage);
begin
 inherited;
 FLinePitch    := 0;
 FPitchMeasured:= FALSE;
 if HandleAllocated
 then requestRedraw;
end;




{-------------------------------------------------------------------------------------------------------------
   PROPERTIES
-------------------------------------------------------------------------------------------------------------}

procedure TRichLog.setVerbosity(Value: TLogVerb);
begin
 if Value = FVerbosity then EXIT;
 FVerbosity:= Value;
 if (csLoading in ComponentState) OR (FRamLog = NIL) then EXIT;   { Loaded counts the lines }

 FFilteredCount:= countFiltered(FSeenCount);
 FTopEntry:= Min(FTopEntry, Max(0, FFilteredCount - 1));
 FDrawnValid:= FALSE;
 Populate;
end;


procedure TRichLog.setInsertTime(Value: Boolean);
begin
 if Value = FInsertTime then EXIT;
 FInsertTime:= Value;
 requestRedraw;
end;


procedure TRichLog.setInsertDate(Value: Boolean);
begin
 if Value = FInsertDate then EXIT;
 FInsertDate:= Value;
 requestRedraw;
end;


function TRichLog.getVerbosityI: Integer;
begin
 Result:= Ord(Verbosity);
end;


procedure TRichLog.setVerbosityI(const Value: Integer);
begin
 Verbosity:= TLogVerb(Value);
end;


function TRichLog.IsDark: Boolean;
begin
 Result:= NOT IsLightStyleColor(clWindow);
end;




{-------------------------------------------------------------------------------------------------------------
   Messages with standard verbosity
   They go into the log; Populate draws them. An empty message is ignored.
-------------------------------------------------------------------------------------------------------------}
procedure TRichLog.AddVerb(CONST Mesaj: string);
begin
 if Mesaj <> ''
 then FRamLog.AddVerb(Mesaj);
end;

procedure TRichLog.AddHint(CONST Mesaj: string);
begin
 if Mesaj <> ''
 then FRamLog.AddHint(Mesaj);
end;

procedure TRichLog.AddInfo(CONST Mesaj: string);
begin
 if Mesaj <> ''
 then FRamLog.AddInfo(Mesaj);
end;

procedure TRichLog.AddImpo(CONST Mesaj: string);
begin
 if Mesaj <> ''
 then FRamLog.AddImpo(Mesaj);
end;

procedure TRichLog.AddWarn(CONST Mesaj: string);
begin
 if Mesaj <> ''
 then FRamLog.AddWarn(Mesaj);
end;

procedure TRichLog.AddError(CONST Mesaj: string);
begin
 if Mesaj <> ''
 then FRamLog.AddError(Mesaj);
end;




{-------------------------------------------------------------------------------------------------------------
   Messages stored at the Info level
   The slider filters them like every other line.
-------------------------------------------------------------------------------------------------------------}
procedure TRichLog.AddBold(CONST Mesaj: string);
begin
 if Mesaj <> ''
 then FRamLog.AddBold(Mesaj);
end;


procedure TRichLog.AddMsg(CONST Mesaj: string);
begin
 if Mesaj <> ''
 then FRamLog.AddMsg(Mesaj);
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


{ An empty entry: an empty row on screen }
procedure TRichLog.AddEmptyRow;
begin
 FRamLog.AddEmptyRow;
end;


procedure TRichLog.AddDateStamp;
begin
 FRamLog.AddMsg(DateToStr(Now));
end;


{ Reads lines from the specified file and adds them to the log using the specified verbosity level.
  If the file does not exist, logs an error message. }
procedure TRichLog.AddFromFile(CONST FileName: string; MsgType: TLogVerb);
VAR
   s: string;
   TSL: TStringList;
begin
 if NOT FileExists(FileName) then
  begin
   AddError('File not found: ' + FileName);
   EXIT;
  end;

 TSL:= StringFromFileTSL(FileName);
 TRY
   FBatching:= TRUE;          { One draw for the whole file }
   TRY
     for s in TSL do
       AddMsg(s, MsgType);
   FINALLY
     FBatching:= FALSE;
   END;
 FINALLY
   FreeAndNil(TSL);
 END;

 Populate;
end;




{-------------------------------------------------------------------------------------------------------------
   LOAD/SAVE
-------------------------------------------------------------------------------------------------------------}

{ The whole log, also the lines the slider hides, without the time/date prefix }
procedure TRichLog.SaveAsRtf(CONST FullPath: string);
begin
 SaveLogAsRtf(FRamLog, FullPath, Level2Color, IsDark, Self);
end;


{ Appends the whole log, as plain text, to the file }
procedure TRichLog.AppendToFile(CONST FullPath: string);
begin
 if FRamLog.Lines.Count > 0
 then StringToFile(FullPath, FRamLog.GetAsText + sLineBreak, woAppend);
end;


{ Reads a text or RTF file (a temporary TRichEdit reads it, as the old TRichLog did) and replaces the log with it: each text line becomes one Info entry }
function TRichLog.LoadFromFile(CONST FullPath: string): Boolean;
VAR
   Reader: TRichEdit;
   i: Integer;
begin
 Result:= FileExists(FullPath);
 if NOT Result then EXIT;

 ensureHandle;
 Reader:= TRichEdit.Create(NIL);
 TRY
   Reader.Visible  := FALSE;
   Reader.MaxLength:= $7FFFFFF0;    { Set before the handle exists: TCustomEdit.CreateWnd passes it to the window }
   Reader.Parent   := Self;         { A rich edit needs a parent window to get a handle }
   Reader.Lines.LoadFromFile(FullPath);

   FBatching:= TRUE;
   TRY
     FRamLog.Clear;
     for i:= 0 to Reader.Lines.Count - 1 do
       FRamLog.AddMsg(Reader.Lines[i]);
   FINALLY
     FBatching:= FALSE;
   END;
 FINALLY
   FreeAndNil(Reader);
 END;

 Populate;
end;




{-------------------------------------------------------------------------------------------------------------
   UTILS
-------------------------------------------------------------------------------------------------------------}

{ Does nothing, and has no test: there is nothing to test.
  It stays because callers still use it: TMainForm.RecomputePrice in c:\Projects\Project Support\Tool - Price calculator\!Price calculator source\FormMain.pas.
  It cannot remove anything: a trailing empty row on screen is an empty entry of the TRamLog, and an entry cannot be removed from a TRamLog. }
procedure TRichLog.RemoveLastEmptyRows;
begin
end;


{ Copies the whole log (also the lines the slider hides) as plain text. A busy clipboard is logged, not raised. }
procedure TRichLog.CopyAll;
begin
 if NOT StringToClipboard(FRamLog.GetAsText)
 then TAppDataCore.LogWarn('TRichLog.CopyAll: the clipboard is busy. The log was not copied.');
end;




procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TRichLog]);
end;


end.
