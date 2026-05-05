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
    procedure AddString(const S: string);   { Bulk append. Uses doubling growth (vs. AddChar's linear growth) so it scales to huge buffers without quadratic SetLength cost. }

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


{ Bulk-appends an entire string in one Move.

  Why this exists separately from AddChar:
    AddChar grows the buffer by FBufferGrowth (linear). That's fine for small
    documents but degrades to O(N^2 / FBufferGrowth) on big ones because every
    growth step SetLength-copies the existing content.
    AddString uses doubling growth — amortized O(N) for arbitrarily large output.

  Why Move (not a loop with AddChar):
    One memmove per call vs. one method call per character. For a 1M-char append
    that is the difference between sub-millisecond and visibly slow.

  Allocation rule (in priority order):
    new size = max(FAllocatedLen * 2, Required, FBufferGrowth)
    - FAllocatedLen * 2 gives amortized O(N) growth on subsequent calls.
    - Required guarantees the string fits even when doubling is not enough.
    - FBufferGrowth honors the caller's pre-sizing hint from Create(InitialBuffSize),
      so the first call can jump straight to the user-provided estimate instead of
      allocating only Length(S) and re-growing on the next call. }
procedure TCStringBuilder.AddString(const S: string);
VAR
  SLen, Required, NewLen: Integer;
begin
  SLen:= Length(S);
  if SLen = 0 then EXIT;

  Required:= FPosition - 1 + SLen;   { FPosition is 1-based; chars already written = FPosition - 1 }
  if Required > FAllocatedLen then
    begin
      NewLen:= FAllocatedLen * 2;
      if NewLen < Required      then NewLen:= Required;
      if NewLen < FBufferGrowth then NewLen:= FBufferGrowth;   { Honor caller's pre-size hint on first growth }
      SetLength(FBuffer, NewLen);
      FAllocatedLen:= Length(FBuffer);
    end;

  { Move SLen Char-elements (i.e. SLen * SizeOf(Char) bytes) from S[1] to FBuffer[FPosition]. }
  Move(Pointer(S)^, FBuffer[FPosition], SLen * SizeOf(Char));
  Inc(FPosition, SLen);
end;


end.
