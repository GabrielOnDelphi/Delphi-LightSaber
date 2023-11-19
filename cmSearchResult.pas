unit cmSearchResult;

INTERFACE

USES
  System.SysUtils;

TYPE
  TIDEPosition = record
    Line, Column: Integer;
  end;

  TSearchResult= class(TObject)
     FileName: string;
     Positions: TArray<TIDEPosition>;   // The line(s) where the text was found
     procedure AddNewPos(Line, Column: Integer);
     function Found: Boolean;
     function PositionsAsString: string;
     function Count: Integer;
  end;



IMPLEMENTATION

USES ccCore;


procedure TSearchResult.AddNewPos(Line, Column: Integer);
var IDEPosition: TIDEPosition;
begin
 IDEPosition.Line:= Line;
 IDEPosition.Column:= Column;

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


function TSearchResult.PositionsAsString: string;
begin
 Result:= '';

 for var IDEPosition in Positions
   do Result:= Result+ IntToStr(IDEPosition.Line)+ ', ';

 Result:= RemoveLastChar(Result);
 Result:= RemoveLastChar(Result);
end;


end.
