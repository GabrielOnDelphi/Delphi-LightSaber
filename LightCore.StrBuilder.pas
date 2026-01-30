UNIT LightCore.StrBuilder;

{=============================================================================================================
   2026
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
==============================================================================================================

  Replacement for TStringBuilder which can be slow.
  360 times faster than Result:= Result + s[i].

  ------------------------

  Speed test:
     500x loop;
     test file: TesterForm.pas 2.7K;
     wrap after 20 chars;

     This                    ->   31ms
     SysUtils.WrapText       ->  484ms
     Result:= Result+ s[i]   -> 5788ms

  If you want to make it even faster, integrate it directly in your code. Example in LightCore.RemoveNumbers
  ------------------------

  Also exists:
    System.SysUtils.WrapText
    It will nicely wrap the text by words, like in a Word document.
    This means that some lines will be shorter than MaxRowLenght intentionally in order to keep the words unbreaked.
    BUT it wraps the text also when it encounters a separator char (like space).
    If the separator is not encountered, the text is never wrapped !!
    So, it is great for splitting actual "text" (like in books) hence its name, and not "strings".

=============================================================================================================}

INTERFACE

USES LightCore;

TYPE
 TCStringBuilder = class(TObject)
  private
    FBuffer: string;
    FAllocatedLen: Integer;  { Currently allocated buffer length }
    FPosition: Integer;      { Current write position (1-based) }
    FBufferGrowth: Integer;  { Amount to grow buffer when full }
  public
    constructor Create(InitialBuffSize: Integer= 10000);
    procedure AddChar(Ch: Char);
    procedure AddEnter;

    function  AsText: string;
    procedure Clear;

    property BufferGrowth: Integer read FBufferGrowth write FBufferGrowth;
 end;



IMPLEMENTATION


constructor TCStringBuilder.Create(InitialBuffSize: Integer= 10000);
begin
  inherited Create;
  FBufferGrowth:= InitialBuffSize;
  Clear;
end;


procedure TCStringBuilder.Clear;
begin
  FPosition:= 1;
  FAllocatedLen:= 0;
  FBuffer:= '';
end;






{ Returns the accumulated string without modifying internal state.
  Can be called multiple times safely. }
function TCStringBuilder.AsText: string;
begin
  Result:= Copy(FBuffer, 1, FPosition - 1);
end;


{ Appends a single character to the buffer, growing if needed. }
procedure TCStringBuilder.AddChar(Ch: Char);
begin
  if FPosition > FAllocatedLen then
  begin
    SetLength(FBuffer, FAllocatedLen + FBufferGrowth);
    FAllocatedLen:= Length(FBuffer);
  end;

  FBuffer[FPosition]:= Ch;
  Inc(FPosition);
end;


{ Appends a line break (CRLF) to the buffer. }
procedure TCStringBuilder.AddEnter;
begin
  { +1 because we write two characters }
  if FPosition + 1 > FAllocatedLen then
  begin
    SetLength(FBuffer, FAllocatedLen + FBufferGrowth);
    FAllocatedLen:= Length(FBuffer);
  end;

  FBuffer[FPosition]:= CR;
  FBuffer[FPosition + 1]:= LF;
  Inc(FPosition, 2);
end;


end.
