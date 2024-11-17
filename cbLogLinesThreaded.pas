UNIT cbLogLinesThreaded;

{=============================================================================================================
   Gabriel Moraru
   2024.10
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------
   Multi-Threaded Version of LogLines
   For the new log (the one based on TStringGrid)

   Tester:
     c:\Myprojects\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
   System.SysUtils, System.Classes, Vcl.Graphics, SyncObjs, // Added SyncObjs for locking mechanisms
   cbLogUtils, ccStreamBuff, cbLogLinesAbstract;

TYPE
  TLogLinesMultiThreaded = class(TAbstractLogLines)
  private
    FLock: TMultiReadExclusiveWriteSynchronizer;
  protected
    function getItem(Index: Integer): PLogLine; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
    function Count: Integer; override;
    function Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer; override;

    function AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Color: TColor = -1): PLogLine; override;
    function Add(Value: PLogLine): Integer; override;

    procedure ReadFromStream(Stream: TCubicBuffStream); override;
    procedure WriteToStream (Stream: TCubicBuffStream); override;
  end;



IMPLEMENTATION

{-------------------------------------------------------------------------------------------------------------
  CTOR
-------------------------------------------------------------------------------------------------------------}
constructor TLogLinesMultiThreaded.Create;
begin
  inherited Create;
  FList := TList.Create;
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TLogLinesMultiThreaded.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  FreeAndNil(FLock);
  inherited;
end;

function TLogLinesMultiThreaded.getItem(Index: Integer): PLogLine;
begin
  FLock.BeginRead;
  try
    Result := PLogLine(FList[Index]);
  finally
    FLock.EndRead;
  end;
end;

procedure TLogLinesMultiThreaded.Clear;
var
  i: Integer;
begin
  FLock.BeginWrite;
  try
    for i := 0 to FList.Count - 1 do
      Dispose(PLogLine(FList[i]));
    FList.Clear;
  finally
    FLock.EndWrite;
  end;
end;

function TLogLinesMultiThreaded.Count: Integer;
begin
  FLock.BeginRead;
  try
    Result := FList.Count;
  finally
    FLock.EndRead;
  end;
end;





function TLogLinesMultiThreaded.Add(Value: PLogLine): Integer;
begin
  FLock.BeginWrite;
  try
    Result := FList.Add(Value);
  finally
    FLock.EndWrite;
  end;
end;

function TLogLinesMultiThreaded.AddNewLine(Msg: string; Level: TLogVerbLvl; Bold: Boolean = FALSE; Color: TColor = -1): PLogLine;
begin
  New(Result);
  Result.Msg   := Msg;
  Result.Level := Level;
  Result.Bold  := Bold;
  Result.Time  := Now;
  Result.Color := Color;
  Result.Indent:= 0;

  FLock.BeginWrite;
  try
    Add(Result);
  finally
    FLock.EndWrite;
  end;
end;

function TLogLinesMultiThreaded.Row2FilteredRow(Row: Integer; Verbosity: TLogVerbLvl): Integer;
var
  i, Total: Integer;
begin
  FLock.BeginRead;
  try
    Result := -1;
    Total := -1;
    for i := 0 to FList.Count - 1 do
    begin
      if PLogLine(FList[i]).Level >= Verbosity then
        begin
          Inc(Total);
          Result := i;
        end;
      if Total = Row then
        Exit;
    end;
    if Total < Row
    then Result := -1;
  finally
    FLock.EndRead;
  end;
end;








procedure TLogLinesMultiThreaded.ReadFromStream(Stream: TCubicBuffStream);
begin
  FLock.BeginWrite;
  try
    inherited ReadFromStream(Stream);
  finally
    FLock.EndWrite;
  end;
end;

procedure TLogLinesMultiThreaded.WriteToStream(Stream: TCubicBuffStream);
begin
  FLock.BeginRead;
  try
    inherited WriteToStream(Stream);
  finally
    FLock.EndRead;
  end;
end;



end.
