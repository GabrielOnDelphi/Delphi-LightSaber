unit cmSearchResult;

INTERFACE

USES
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms;

TYPE
  TSearchResult= class(TObject)
     FileName: string;
     Positions: TArray<integer>;
     procedure AddNewPos(Pos: Integer);
     function Found: Boolean;
     function PositionsAsString: string;
     function Count: Integer;
  end;



IMPLEMENTATION

USES cmPascal, ccCore;


procedure TSearchResult.AddNewPos(Pos: Integer);
begin
 SetLength(Positions, Length(Positions)+1);
 Positions[High(Positions)]:= Pos;
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
 for var i in Positions
   do Result:= Result+ IntToStr(i)+ ',';
 Result:= RemoveLastChar(Result);
end;


end.
