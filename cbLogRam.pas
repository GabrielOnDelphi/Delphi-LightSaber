UNIT cbLogRam;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

   A simple but effective log (non-visual).
   Its data can be displayed by TLogGrid, but it can also work alone without being connected to a TLogGrid.

   Verbosity:
     Supports several verbosity levels (verbose, info, warnings, errors, etc)

   Future plans
     Replaces the TRamLog.

   Tester:
     c:\Myprojects\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Grids,
   cbLogLines, cbLogUtils, ccStreamBuff2;

TYPE
  ILogObserver = interface
    ['{A1B2C3D4-E5F6-4321-8765-9876543210AB}']
    procedure Populate;
    procedure PopUpWindow;
  end;


TYPE
  TRamLog = class(TObject)
   private
     FLogObserver: ILogObserver;
     const
      StreamSign  = 'TRamLog';
      StreamVer   = 3;
   protected
     function prepareString(CONST Msg: string): string;
   public
     Lines: TLogLines;
     ShowOnError: Boolean; // Automatically show the visual log form when warnings/errors are added to the log. This way the user is informed about the problems.

     constructor Create(aShowOnError: Boolean; Observer: ILogObserver);
     destructor Destroy;  override;
     procedure Clear;

     function Count(Filtered: Boolean; Filter: TLogVerbLvl): Integer;

     procedure AddBold    (const Msg: string);
     procedure AddMsg     (CONST Msg: string);                  overload;
     procedure AddMsg     (CONST Msg: string; Color: TColor);   overload;
     procedure AddMsgInt  (CONST Msg: string; i: Integer);
     procedure AddEmptyRow;

     procedure AddDebug   (CONST Msg: string);
     procedure AddVerb    (CONST Msg: string);
     procedure AddHint    (CONST Msg: string);
     procedure AddInfo    (CONST Msg: string);                             { Default level }
     procedure AddImpo    (CONST Msg: string);
     procedure AddWarn    (CONST Msg: string);
     procedure AddError   (CONST Msg: string);
     {}
     function  GetAsText: string;
     procedure SaveAsText  (CONST FullPath: string);
     {}
     function  LoadFromStream(Stream: TCubicBuffStream2): Boolean;
     procedure SaveToStream  (Stream: TCubicBuffStream2);

     procedure SaveToFile  (CONST FullPath: string);
     function  LoadFromFile(CONST FullPath: string): Boolean;

    // OBSERVER
    procedure RegisterLogObserver(Observer: ILogObserver);
    procedure UnregisterLogObserver;
    procedure NotifyLogObserver;
    procedure NotifyLogObserverAndShow;

    procedure PopUpWindow;
  end;


IMPLEMENTATION

USES
  ccCore, ccIO, ccTextFile, ccStreamBuff;


{-------------------------------------------------------------------------------------------------------------
   CTOR
-------------------------------------------------------------------------------------------------------------}
constructor TRamLog.Create(aShowOnError: Boolean; Observer: ILogObserver);
begin
 inherited Create;
 Lines:= TLogLines.Create;

 ShowOnError:= aShowOnError;
 if Observer <> NIL
 then RegisterLogObserver(Observer);
end;


destructor TRamLog.Destroy;
begin
 FreeAndNil(Lines);
 inherited;
end;


procedure TRamLog.Clear;
begin
 Lines.Clear;   { Call Clear to empty the Items array and set the Count to 0. Clear also frees the memory used to store the Items array and sets the Capacity to 0. }
 NotifyLogObserver;
end;




{-------------------------------------------------------------------------------------------------------------
   STUFF
-------------------------------------------------------------------------------------------------------------}
{ Filtered False: Returns the number of rows
  Filtered True: Returns the number of rows that we can extract from RamLog after we took into consideration the verbosity filter }
function TRamLog.Count(Filtered: Boolean; Filter: TLogVerbLvl): Integer;
begin
  Result := 0;
  if Filtered
  then
    for VAR i := 0 to Lines.Count - 1 do
      begin
       if Lines[i].Level >= Filter
       then Inc(Result);
      end
  else
    Result:= Lines.Count;
end;




procedure TRamLog.RegisterLogObserver(Observer: ILogObserver);
begin
  FLogObserver := NIL; // explicitly set to nil before assigning a new observer to prevent memory leaks or undefined behavior with multiple observers. Ensure proper lifecycle management of observers.
  FLogObserver := Observer;
end;


procedure TRamLog.UnregisterLogObserver;
begin
  FLogObserver := nil;
end;


procedure TRamLog.NotifyLogObserver;
begin
  if Assigned(FLogObserver)
  then FLogObserver.Populate;
end;


procedure TRamLog.NotifyLogObserverAndShow;
begin
  NotifyLogObserver;
  if ShowOnError
  then PopUpWindow;
end;


procedure TRamLog.PopUpWindow;
begin
  if Assigned(FLogObserver)
  then FLogObserver.PopUpWindow;
end;








{-------------------------------------------------------------------------------------------------------------
   ADD GENERIC MESSAGE
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.AddBold (CONST Msg: string);
begin
 Lines.AddNewLine( prepareString(Msg), lvInfos, TRUE );
 NotifyLogObserver;
end;


procedure TRamLog.AddMsg(CONST Msg: string);
begin
 Lines.AddNewLine( prepareString(Msg), lvInfos, FALSE );
 NotifyLogObserver;
end;


procedure TRamLog.AddMsg(CONST Msg: string; Color: TColor);
begin
 Lines.AddNewLine( prepareString(Msg), lvInfos, FALSE, Color);
 NotifyLogObserver;
end;


procedure TRamLog.AddMsgInt(const Msg: string; i: Integer);   { Adds a message text followed by an integer }
begin
 Lines.AddNewLine( prepareString(Msg)+ IntToStr(i), lvInfos, FALSE );
 NotifyLogObserver;
end;


procedure TRamLog.AddEmptyRow;
begin
 AddMsg('');
end;



{-------------------------------------------------------------------------------------------------------------
   ADD MESSAGE BY VERBOSITY
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.AddDebug(CONST Msg: string);                             { Relevance 0 }
begin
 Lines.AddNewLine(prepareString(Msg), lvDebug, FALSE );
 NotifyLogObserver;
end;


procedure TRamLog.AddVerb(CONST Msg: string);                               { Relevance 1 }
begin
 Lines.AddNewLine(prepareString(Msg), lvVerbose, FALSE );
 NotifyLogObserver;
end;


procedure TRamLog.AddHint(CONST Msg: string);                               { Relevance 2 }
begin
 Lines.AddNewLine( prepareString(Msg), lvHints, FALSE );
 NotifyLogObserver;
end;


procedure TRamLog.AddInfo(CONST Msg: string);                               { Relevance 3 }
begin
 Lines.AddNewLine( prepareString(Msg), lvInfos, FALSE );
 NotifyLogObserver;
end;


procedure TRamLog.AddImpo(CONST Msg: string);                               { Relevance 4 }
begin
 Lines.AddNewLine( prepareString(Msg), lvImportant, FALSE );
 NotifyLogObserver;
end;


procedure TRamLog.AddWarn(CONST Msg: string);                               { Relevance 5 }
begin
 Lines.AddNewLine( prepareString(Msg), lvWarnings, FALSE );
 NotifyLogObserverAndShow;
end;


procedure TRamLog.AddError(CONST Msg: string);                              { Relevance 6 }
begin
 Lines.AddNewLine( prepareString(Msg), lvErrors, FALSE );
 NotifyLogObserverAndShow;
end;








{-------------------------------------------------------------------------------------------------------------
   TEXT
-------------------------------------------------------------------------------------------------------------}
function TRamLog.GetAsText: string;
VAR i: Integer;
begin
 Result:= '';
 for i:= 0 to Lines.Count-1
   DO Result:= Result+ Lines[i].Msg+ CRLF;

 {Cut the last enter}
 Result:= ccCore.RemoveLastEnter(Result);
end;



function TRamLog.prepareString(CONST Msg: string): string;
begin
 Result:= ReplaceEnters (Msg , ' ');
 //Result:= StringOfChar(' ', Indent)+ Result;
end;









{-------------------------------------------------------------------------------------------------------------
    I/O
-------------------------------------------------------------------------------------------------------------}
procedure TRamLog.SaveAsText(CONST FullPath: string);
begin
  StringToFile(FullPath, GetAsText, woOverwrite, wpAuto);
end;



function TRamLog.LoadFromStream(Stream: TCubicBuffStream2): Boolean;
begin
  Result:= Stream.ReadHeader(StreamSign, StreamVer);
  if NOT Result then EXIT;
  Lines.ReadFromStream(Stream);
  Stream.ReadPaddingDef;
end;

procedure TRamLog.SaveToStream(Stream: TCubicBuffStream2);
begin
  Stream.WriteHeader(StreamSign, StreamVer);
  Lines.WriteToStream(Stream);
  Stream.WritePaddingdef;
end;




function TRamLog.LoadFromFile(const FullPath: string): Boolean;
begin
 VAR Stream:= TCubicBuffStream2.CreateRead(FullPath);
 TRY
   Result:= LoadFromStream(Stream);
 FINALLY
   FreeAndNil(Stream);
 END;

 if Result then NotifyLogObserver;
end;

procedure TRamLog.SaveToFile(const FullPath: string);
begin
 VAR Stream:= TCubicBuffStream2.CreateWrite(FullPath);
 TRY
   SaveToStream(Stream);
 FINALLY
   FreeAndNil(Stream);
 END;
end;



end.
