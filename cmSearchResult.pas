unit cmSearchResult;
{ This units implements a list of positions in a PAS file. Each position stores the line and column number where an issue was found on that line of code.
  It also stores the issue and the solution. }

INTERFACE

USES
  System.SysUtils, System.Generics.Collections;

TYPE
  TIDEPosition = record
    LinePos, ColumnPos: Integer;           // The position in the IDE where to put the cursor
    CodeLine: string;                      // The line of code where the problem was found.
    WarningMsg: string;                    // The warning message to show to the user. It usually explained what was wrong at that line and how to fix it.
    Offender: string;                      // The piece of code that caused the problem.
    //FileName: string;                      // Which file is being processed
  end;

  TSearchResult= class(TObject)
     FileName: string;
     Positions: TArray<TIDEPosition>;      // The line(s) where the text was found
     procedure AddNewPos(LinePos, ColumnPos: Integer; const CodeLine: string);                       overload;
     procedure AddNewPos(LinePos, ColumnPos: Integer; const CodeLine, Offender, WarningMsg: string); overload;
     procedure AddNewPos(WarningMsg: string);                                                        overload;
     function AsString: string;            // List of positions where issues were found AND the fix
     function PositionsAsString: string;   // Comma sepparated list of positions where the issue was found
     function Found: Boolean;
     function Count: Integer;
     procedure Clear;
     constructor Create(const aFileName: string);
  end;

  TSearchResults= class(TObjectList<TSearchResult>)
     function Last: TSearchResult;
  end;


IMPLEMENTATION

USES ccCore;



constructor TSearchResult.Create(const aFileName: string);
begin
  inherited Create;
  FileName:= aFileName;
end;


procedure TSearchResult.Clear;
begin
 // Add it to the list
 SetLength(Positions, 0);
end;


{ CodeLine is the line of code where the problem was found.
  Offender is the piece of code that caused the problem.
  WarningMsg is the warning message to show to the user. It usually explained what was wrong at that line and how to fix it. }
procedure TSearchResult.AddNewPos(LinePos, ColumnPos: Integer; const CodeLine, Offender, WarningMsg: string);
var IDEPosition: TIDEPosition;
begin
 // New postion
 IDEPosition.LinePos   := LinePos;
 IDEPosition.ColumnPos := ColumnPos;
 IDEPosition.CodeLine  := CodeLine;
 IDEPosition.WarningMsg:= WarningMsg;
 IDEPosition.Offender  := Offender;
 //IDEPosition.FileName  := FileName;

 // Add it to the list
 SetLength(Positions, Length(Positions)+1);
 Positions[High(Positions)]:= IDEPosition;
end;


procedure TSearchResult.AddNewPos(LinePos, ColumnPos: Integer; const CodeLine: string);
var IDEPosition: TIDEPosition;
begin
 // New postion
 IDEPosition.LinePos   := LinePos;
 IDEPosition.ColumnPos := ColumnPos;
 IDEPosition.CodeLine  := CodeLine;
 IDEPosition.WarningMsg:= '';
 IDEPosition.Offender  := '';
 //IDEPosition.FileName  := FileName;

 // Add it to the list
 SetLength(Positions, Length(Positions)+1);
 Positions[High(Positions)]:= IDEPosition;
end;


{ A generic issue that does not apply to a certain line number but to the whole file  }
procedure TSearchResult.AddNewPos(WarningMsg: string);
var IDEPosition: TIDEPosition;
begin
 // New postion
 IDEPosition.LinePos   := 1;
 IDEPosition.ColumnPos := 1;
 //IDEPosition.FileName  := FileName;
 IDEPosition.CodeLine  := '';
 IDEPosition.WarningMsg:= WarningMsg;

 // Add it to the list
 SetLength(Positions, Length(Positions)+1);
 Positions[High(Positions)]:= IDEPosition;
end;


function TSearchResult.Found: Boolean;
begin
 Result:= Length(Positions) > 0;
end;


function TSearchResult.Count: Integer;
begin
 Result:= Length(Positions);
end;


{ Comma sepparated list of positions where issues were found }
function TSearchResult.PositionsAsString: string;
begin
 Result:= '';

 for var IDEPosition in Positions
   do Result:= Result+ IntToStr(IDEPosition.LinePos)+ ', ';

 Result:= RemoveLastChar(Result);
 Result:= RemoveLastChar(Result);
end;


{ List of positions where issues were found AND the warning msg (or the fix) }
function TSearchResult.AsString: string;
begin
 Result:= '';

 for var IDEPosition in Positions
   do Result:= Result+ 'Line: '+ IntToStr(IDEPosition.LinePos)
                + CRLF + IDEPosition.Offender+ ' ' + IDEPosition.WarningMsg
                + CRLF + Trim(IDEPosition.CodeLine)+ CRLF;

 Result:= RemoveLastEnter(Result);
end;








{ TSearchResults }

function TSearchResults.Last: TSearchResult;
begin
  Assert(Count > 0);
  Result:= Self[Count-1];
end;

end.
