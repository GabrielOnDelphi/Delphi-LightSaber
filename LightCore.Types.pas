UNIT LightCore.Types;

{=============================================================================================================
   www.GabrielMoraru.com
   2025.07
   See Copyright file
=============================================================================================================}

INTERFACE

USES
   System.AnsiStrings, System.Character, System.SysUtils, System.Math, System.IOUtils, System.StrUtils,
   System.Classes, System.Types, System.TimeSpan, System.DateUtils;

{ Extra VK constants that are missing from Delphi's Win dows API interface (Windows.pas unit)
  More virtual keys here: http://delphi.about.com/od/objectpascalide/l/blvkc.htm }
CONST
   VK_NULL         = 0;
   VK_PRIOR        = $21;       { PAGE UP }
   VK_NEXT         = $22;       { PAGE DOWN }
   VK_pgUp         = $21;       { PAGE UP }
   VK_pgDwn        = $22;       { PAGE DOWN }
   VK_COPYRIGHT    = 169;       { Type ALT 0169 to get © }
   VK_REGISTERED   = 174;       { Type ALT 0174 to get ® }
   VK_SEMICOLON    = 186;
   VK_EQUAL        = 187;
   VK_COMMA        = 188;
   VK_MINUS        = 189;
   VK_PERIOD       = 190;
   VK_SLASH        = 191;
   VK_BACKQUOTE    = 192;
   VK_LEFTBRACKET  = 219;
   VK_BBACKSLASH   = 220;
   VK_RIGHTBRACKET = 221;
   VK_QUOTE        = 222;
  {VK_ENTER        = Winapi.Windows.VK_RETURN; { #13 }

CONST
   Numbers         = ['0'..'9'];
   LettersLowCase  = ['a'..'z'];
   LettersUpCase   = ['A'..'Z'];
   LettersSigns    = [' ', '~', '`', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '+', '=', '[', ']', '{', '}', ';', ':', '''', '"', '<', '>', ',', '.', '/', '?', '\', '|'];
   Alphabet        = ['a'..'z', 'A'..'Z'];
   AlphabetNo      = ['a'..'z', 'A'..'Z', '0'..'9'];
   Vowels          = ['a', 'e', 'i','o', 'u', 'y', 'A', 'E', 'I', 'O', 'U', 'Y'];
   LettersSpecial  = [#10, #13, #9]; { CR, LF, TAB }

{ Indexes }
CONST
   IndexedIn1      = 1;
   IndexedIn0      = 0;
   IndexDiff       = 1;
   HeaderOverhead  = 1;
   HeaderRow       = 0;
   UnInitialized   = -7777777;

{ Mine }
CONST
   SHOW            = TRUE;
   HIDE            = FALSE;
   OFF             = FALSE;
   &ON             = TRUE;


{ Units }
CONST
   KB              = 1024;
   MB              = 1048576;
   GB              = 1073741824;
   TB: Int64       = 1099511627776;

{ Range limits on integer type vars }
{ Int64 max value is 9223372036854775807 = 9.2 quintillion }
CONST
   MinINT          = Low(Integer);                  { -2147483648 }

   MaxSmallInt     = High(SmallInt);
   MinSmallInt     = Low (SmallInt);

CONST
   HexNumbers = ['0'..'9', 'a'..'f', 'A'..'F'];

   {
   MINBYTE         = Low(byte);
   MAXLONGWORD     = High(longword);
   MINLONGWORD     = Low(longword);
   MAXSTRING       = MaxInt;  }

TYPE
  TStringArray     = array of string;
  TSingleArray     = array of Single;
  TBytesArray      = System.SysUtils.TBytes;
  TNotifyMsgEvent  = procedure(Sender: TObject; Msg: string) of object;    { For general use }
  WebURL           = string;


  TDoubleArray     = array of Double;
  TDoubleArrayHelper= record helper for TDoubleArray
  public
    procedure Add(const Element: Double);
  end;


  TIntegerArray     = array of Integer;
  TIntegerArrayHelper= record helper for TIntegerArray
  public
    function Average: Single;
    procedure Add(const Element: Integer);
  end;


{=============================================================================================================
   TIME
=============================================================================================================}

TYPE
    { Returns the next day in the week }
    TWeekDays = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);   // Days enumeration
    TWeekDaysHelper = record helper for TWeekDays
       function NextDay: TWeekDays;
       function ToString: string;
    end;

CONST
   NullDate     = -700000;                    { https://stackoverflow.com/questions/14985037/delphi-how-to-determine-and-empty-tdatetime-value }
   Second       = 1000;                       { Miliseconds per sec. Already exists: MSecsPerSec }
   Minute       = 60;                         { Miliseconds per min. Already exists System.SysUtils.SecsPerMin }
   Hour         = 3600;                       { Miliseconds per hour }
   Day          = 86400;
   MSecsPerMin  = 60000;
   MinutesPerDay= 24*60;


IMPLEMENTATION



{ TIntegerArrayHelper }

procedure TIntegerArrayHelper.Add(const Element: Integer);
var Len: Integer;
begin
  Len:= Length(Self);
  SetLength(Self, Len + 1);
  Self[len]:= Element;
end;


function TIntegerArrayHelper.Average: Single;
VAR i, Summ: Integer;
begin
  if Length(Self) = 0 then EXIT(0);

  Summ:= 0;
  for i in Self DO
    Summ:= Summ+ i;

  Result:= Summ / Length(Self);
end;



{ Returns the next day in the week }
function TWeekDaysHelper.NextDay: TWeekDays;
var
    Next: Integer;
begin
    Next := Ord(Self) + 1;

    if Next <= Ord(High(TWeekDays))
    then Result := TWeekDays(Next)
    else Result := TWeekDays.Monday;
end;

function TWeekDaysHelper.ToString: string;
begin
  case Self of
     Monday    : Result := 'Monday';
     Tuesday   : Result := 'Tuesday';
     Wednesday : Result := 'Wednesday';
     Thursday  : Result := 'Thursday';
     Friday    : Result := 'Friday';
     Saturday  : Result := 'Saturday';
     Sunday    : Result := 'Sunday';
  end;
end;



{ TDoubleArrayHelper }

procedure TDoubleArrayHelper.Add(const Element: Double);
var Len: Integer;
begin
  Len:= Length(Self);
  SetLength(Self, Len + 1);
  Self[len]:= Element;
end;


end.

