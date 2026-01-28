unit LightCore.SearchResult;

{=============================================================================================================
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   Search result classes for IDE integration.

   TIDEPosition: Stores a single position in a source file where an issue was found.
   TSearchResult: Collection of positions within a single file.
   TSearchResults: Collection of TSearchResult objects (multiple files).

   Used for reporting code analysis results, warnings, and suggested fixes.
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Generics.Collections;

TYPE
  { Represents a single position in a source file where an issue was found }
  TIDEPosition = record
    LinePos: Integer;      { Line number (1-based) }
    ColumnPos: Integer;    { Column number (1-based) }
    CodeLine: string;      { The line of code where the problem was found }
    WarningMsg: string;    { Warning/fix message to show to the user }
    Offender: string;      { The piece of code that caused the problem }
  end;

  { Collection of positions within a single source file }
  TSearchResult = class(TObject)
  private
    FFileName: string;
    FPositions: TList<TIDEPosition>;
  public
    constructor Create(CONST aFileName: string);
    destructor Destroy; override;

    procedure AddNewPos(LinePos, ColumnPos: Integer; CONST CodeLine: string); overload;
    procedure AddNewPos(LinePos, ColumnPos: Integer; CONST CodeLine, Offender, WarningMsg: string); overload;
    procedure AddNewPos(CONST WarningMsg: string); overload;  { For file-wide issues }

    function AsString: string;           { Full report with positions and messages }
    function PositionsAsString: string;  { Comma-separated list of line numbers }
    function Found: Boolean;             { True if any positions recorded }
    function Count: Integer;
    procedure Clear;

    property FileName: string read FFileName;
    property Positions: TList<TIDEPosition> read FPositions;
  end;

  { Collection of search results from multiple files }
  TSearchResults = class(TObjectList<TSearchResult>)
    function Last: TSearchResult;
  end;


IMPLEMENTATION

USES LightCore;



constructor TSearchResult.Create(const aFileName: string);
begin
  inherited Create;
  FFileName:= aFileName;
  FPositions:= TList<TIDEPosition>.Create;
end;


destructor TSearchResult.Destroy;
begin
  FreeAndNil(FPositions);
  inherited Destroy;
end;


procedure TSearchResult.Clear;
begin
  FPositions.Clear;
end;


{ CodeLine is the line of code where the problem was found.
  Offender is the piece of code that caused the problem.
  WarningMsg is the warning message to show to the user. It usually explains what was wrong at that line and how to fix it. }
procedure TSearchResult.AddNewPos(LinePos, ColumnPos: Integer; const CodeLine, Offender, WarningMsg: string);
var IDEPosition: TIDEPosition;
begin
  IDEPosition.LinePos   := LinePos;
  IDEPosition.ColumnPos := ColumnPos;
  IDEPosition.CodeLine  := CodeLine;
  IDEPosition.WarningMsg:= WarningMsg;
  IDEPosition.Offender  := Offender;
  FPositions.Add(IDEPosition);
end;


procedure TSearchResult.AddNewPos(LinePos, ColumnPos: Integer; const CodeLine: string);
var IDEPosition: TIDEPosition;
begin
  IDEPosition.LinePos   := LinePos;
  IDEPosition.ColumnPos := ColumnPos;
  IDEPosition.CodeLine  := CodeLine;
  IDEPosition.WarningMsg:= '';
  IDEPosition.Offender  := '';
  FPositions.Add(IDEPosition);
end;


{ A generic issue that does not apply to a certain line number but to the whole file }
procedure TSearchResult.AddNewPos(CONST WarningMsg: string);
var IDEPosition: TIDEPosition;
begin
  IDEPosition.LinePos   := 1;
  IDEPosition.ColumnPos := 1;
  IDEPosition.CodeLine  := '';
  IDEPosition.WarningMsg:= WarningMsg;
  IDEPosition.Offender  := '';
  FPositions.Add(IDEPosition);
end;


function TSearchResult.Found: Boolean;
begin
  Result:= FPositions.Count > 0;
end;


function TSearchResult.Count: Integer;
begin
  Result:= FPositions.Count;
end;


{ Comma-separated list of positions where issues were found }
function TSearchResult.PositionsAsString: string;
begin
  Result:= '';

  for var IDEPosition in FPositions do
    Result:= Result + IntToStr(IDEPosition.LinePos) + ', ';

  Result:= RemoveLastChar(Result);
  Result:= RemoveLastChar(Result);
end;


{ List of positions where issues were found AND the warning message (or the fix) }
function TSearchResult.AsString: string;
begin
  Result:= '';

  for var IDEPosition in FPositions do
    Result:= Result + '   Line ' + IntToStr(IDEPosition.LinePos) + ':'
           + CRLF + '   ' + Trim(IDEPosition.CodeLine)
           + CRLF + '   ' + IDEPosition.Offender + ' ' + IDEPosition.WarningMsg + CRLF;

  Result:= RemoveLastEnter(Result);
end;






{ TSearchResults }

function TSearchResults.Last: TSearchResult;
begin
  Assert(Count > 0, 'TSearchResults - Count is 0!');
  Result:= Self[Count-1];
end;

end.
