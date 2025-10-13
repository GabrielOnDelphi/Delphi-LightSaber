UNIT LightVcl.Common.EllipsisText;

{=============================================================================================================
   2025.10
   www.GabrielMoraru.com
==============================================================================================================

   Shortens a text so it can fix in the specified canvas.
   Example: 'This is a long text that will be truncated' could be shortened to something like "This is a...runcated'.

   Also exists:
        FileCtrl.MinimizeName (if you require pixels)
        cGraphics.DrawStringEllipsis
        LightCore.ShortenString
        LightCore.IO.ShortenFileName

   TESTER:
       LightSaber\Demo\VCL\Demo cGraphText.pas\VCL_Demo_cGraphText.dpr

=============================================================================================================}

INTERFACE

USES
   Winapi.Windows, System.Types, System.SysUtils, System.AnsiStrings, Vcl.Graphics;

function GetAverageCharSize(Canvas: TCanvas): TPoint;

{ Pure text routines }
function  ShortenString      (CONST s: String; MaxLength: Integer): String;  { Replace long text with ellipsis. Also exists: FileCtrl.MinimizeName }

{ Graphics routines }
function  DrawStringEllipsis (CONST s: string; Canvas: TCanvas; ARect: TRect): integer;  overload;
function  DrawStringEllipsis (CONST s: string; Canvas: TCanvas): integer; overload;

function  GetEllipsisText    (CONST s: String; Canvas: TCanvas; MaxWidth, MaxHeight: Integer; Font: TFont = nil; PathEllipsis: Boolean = False; TextFormat: TTextFormat = []): String; overload;
function  GetEllipsisText    (CONST s: String; Handle: HDC;     MaxWidth, MaxHeight: Integer;                    PathEllipsis: Boolean = False; TextFormat: LongWord = 0): string; overload;
function  GetEllipsisText    (CONST s: string; Canvas: TCanvas; MaxWidth: Integer): string; overload;


IMPLEMENTATION


{--------------------------------------------------------------------------------------------------
   GET STRING BASED ON CANVAS SIZE
--------------------------------------------------------------------------------------------------}

// Source: www.delphipraxis.net/164139-text-kuerzen-ellipsis.html
function GetEllipsisText(CONST S: String; Canvas: TCanvas; MaxWidth, MaxHeight: Integer; Font: TFont = nil; PathEllipsis: Boolean = False; TextFormat: TTextFormat = []): String;
var
  R: TRect;
  TempCanvas: TCanvas;
begin
  TempCanvas:= nil; //not necessary

  Result := S;
  UniqueString(Result);

  // Use our own canvas if "Canvas" was nil
  if Assigned(Canvas)
  then TempCanvas := Canvas
  else TempCanvas := TCanvas.Create;
  try
    if Assigned(Font)
    then TempCanvas.Font.Assign(Font);

    // Text format
    TextFormat := TextFormat + [tfCalcRect, tfModifyString];
    if PathEllipsis
    then TextFormat := TextFormat + [tfPathEllipsis];
    if TextFormat * [tfEndEllipsis, tfPathEllipsis] = []
    then TextFormat := TextFormat + [tfEndEllipsis];

    // Draw
    R := Rect(1, 1, MaxWidth, MaxHeight);
    TempCanvas.TextRect(R, Result, TextFormat);
  finally
    if Canvas = NIL
    then FreeAndNil(TempCanvas);  // New canvas object ws created ONLY if aCanvas was nil
  end;
end;



{ DOESN'T WORK }
function GetEllipsisText(CONST S: String; Handle: HDC; MaxWidth, MaxHeight: Integer; PathEllipsis: Boolean = False; TextFormat: LongWord = 0): String;
var
  R: TRect;
begin
  Result := S;
  UniqueString(Result);

  TextFormat := TextFormat or DT_CALCRECT or DT_MODIFYSTRING;
  if PathEllipsis
  then TextFormat := TextFormat or DT_PATH_ELLIPSIS;
  if TextFormat and (DT_END_ELLIPSIS or DT_PATH_ELLIPSIS) = 0
  then TextFormat := TextFormat or DT_END_ELLIPSIS;

  R := Rect(1, 1, MaxWidth, MaxHeight);
  DrawTextEx(Handle, PChar(Result), Length(Result), R, TextFormat, nil);
  SetLength(Result, StrLen(PChar(Result)));
end;



//ToDo: Return value of function 'GetEllipsisText' might be undefined
{ Takes a long string and truncates it in the middle. Example: '123...789' }
function GetEllipsisText(CONST s: string; Canvas: TCanvas; MaxWidth: Integer): string;
var
  NewStr, LastStr: string;
  TextSize: TSize;
  EllipsisSize: Integer;
begin
  Result:= '';
  NewStr := '...';
  EllipsisSize:= Canvas.TextWidth(NewStr);

  GetTextExtentPoint32(Canvas.Handle, s, Length(s), TextSize);
  if TextSize.cX > MaxWidth
  then
     //Start with the smallest possible truncated-and-ellipsis-modified string, and expand until we have the biggest one that can fit
     for VAR i:= 1 to Length(s) div 2 do
      begin
         LastStr := NewStr;
         NewStr := Copy(s, 1, I) + '...' + Copy(s, Length(s) - I + 1, I);   // Get the first I chars, then the ellipsis, then the last I chars
         GetTextExtentPoint32(Canvas.Handle, NewStr, Length(NewStr), TextSize);
         if TextSize.cx > (MaxWidth - EllipsisSize)
         then Exit(LastStr);
      end
  else
     Result:= s;   //The string will fit in the width of the given rect, don't mess with it
end;









function DrawStringEllipsis(CONST s: string; Canvas: TCanvas; aRect: TRect): integer;
var
  Buf: array of Char;
  BufLen: Integer;
begin
  // DrawText with DT_MODIFYSTRING modifies the buffer in-place.
  // We must supply a writable buffer large enough to hold modifications.
  BufLen := Length(s) + 16; // allocate a little extra for ellipsis/changes
  if BufLen < 32
  then BufLen := 32; // ensure a minimum buffer size

  SetLength(Buf, BufLen + 1); // +1 for null terminator
  // copy original text into the buffer (StrPLCopy writes a terminating #0)
  StrPLCopy(PChar(Buf), s, BufLen);

  // Pass buffer and buffer length (not the original string length) to DrawText.
  Result:= DrawText(Canvas.Handle, PChar(Buf), BufLen, aRect,
                    DT_LEFT or DT_END_ELLIPSIS or DT_MODIFYSTRING {or DT_CALCRECT});
end;


function DrawStringEllipsis(CONST s: string; Canvas: TCanvas): integer;
begin
  Result:= DrawStringEllipsis(s, Canvas, Canvas.ClipRect);
end;








{--------------------------------------------------------------------------------------------------
   PURE TEXT ROUTINES
--------------------------------------------------------------------------------------------------}

{ Only show the start and the end of the text with ellipses in-between.
  Note: For all Unicode lovers, you can keep two more original characters by using the ellipsis character (U+2026: HORIZONTAL ELLIPSIS)
  Works with UNC paths }
function ShortenString(CONST s: String; MaxLength: Integer): string;
VAR TotalLength, FLength: Integer;
begin
  TotalLength:= Length(s);
  if TotalLength > MaxLength
  then
   begin
    FLength:= (MaxLength Div 2) - 2;
    Result := system.COPY(s, 0, fLength)
              + '..'
              + system.COPY(s, TotalLength-fLength, TotalLength);
    end
  else Result:= s;  { Input is too short anyway }
end;











{ Average text-size on specified canvas. Function copied from Vcl.Dialogs.PAS because it is not exposed there in "Interface" }
function GetAverageCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;


end.
