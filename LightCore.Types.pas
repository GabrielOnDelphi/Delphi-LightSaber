UNIT LightCore.Types;

{=============================================================================================================
   2026.01.30
   www.GabrielMoraru.com
=============================================================================================================}

INTERFACE

USES
   System.SysUtils,
   System.Classes,
   System.Generics.Collections;

{ Extra VK constants that are missing from Delphi's Windows API interface (Windows.pas unit)
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
   VK_BACKSLASH    = 220;
   VK_RIGHTBRACKET = 221;
   VK_QUOTE        = 222;
   { Note: VK_ENTER = VK_RETURN ($0D / 13) is defined in Winapi.Windows }

CONST
   Numbers         = ['0'..'9'];
   LettersLowCase  = ['a'..'z'];
   LettersUpCase   = ['A'..'Z'];
   LettersSigns    = [' ', '~', '`', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '+', '=', '[', ']', '{', '}', ';', ':', '''', '"', '<', '>', ',', '.', '/', '?', '\', '|'];
   Alphabet        = ['a'..'z', 'A'..'Z'];
   AlphabetNo      = ['a'..'z', 'A'..'Z', '0'..'9'];
   Vowels          = ['a', 'e', 'i','o', 'u', 'y', 'A', 'E', 'I', 'O', 'U', 'Y'];
   LettersSpecial  = [#10, #13, #9]; { LF, CR, TAB }

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
   OwnObjects      = TRUE;

{ Units }
CONST
   KB              = 1024;
   MB              = 1048576;
   GB              = 1073741824;
   TB              = 1099511627776;

CONST
   HexNumbers = ['0'..'9', 'a'..'f', 'A'..'F'];

CONST
   // Use 'Segoe UI Symbol' or 'Segoe UI' for monochrome symbol glyphs.
   // Use 'Segoe UI Emoji'                for color emoji.
   IconRecycleBin    = #$D83D + #$DDD1;     // Recycle basket
   IconRecycleBinBW  = #9851;               // BLACK UNIVERSAL RECYCLING SYMBOL
   IconCancel        = '❌';                // #10006  Red
   IconCancelPurple  = '✖';                // Purple
   IconCheckMark     = '✔';
   IconBackArrow     = #$2B05;
   IconUndo          = #$21BA;              // ANTICLOCKWISE OPEN CIRCLE ARROW— good for "reverse direction / undo order"
   IconSettingsGear  = '⚙';                // U+2699
   IconSettingsKey   = '🔧';                // U+1F527 (WRENCH)

   IconPlay        = '▶';    // U+25B6
   IconPause       = '⏸';   // U+23F8
   IconStop        = '⏹';   // U+23F9
   IconRecord      = '⏺';   // U+23FA
   IconFastForward = '⏩';   // U+23E9
   IconRewind      = '⏪';   // U+23EA
   IconSkipNext    = '⏭';   // U+23ED
   IconSkipPrev    = '⏮';   // U+23EE
   IconStopwatch   = '⏱';   // U+23F1: Stopwatch
   IconClock       = '⏲';   // U+23F2: Timer Clock
   IconAlarm       = '⏰';   // U+23F0: Alarm Clock
   IconHourglass   = '⏳';   // U+23F3: Hourglass with Flowing Sand
   IconWatch       = '⌚';   // U+231A: Watch





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
    procedure Sort;
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
   Second       = 1000;                       { Milliseconds per second. See also: MSecsPerSec }
   Minute       = 60;                         { Seconds per minute. See also: System.SysUtils.SecsPerMin }
   Hour         = 3600;                       { Seconds per hour }
   Day          = 86400;                      { Seconds per day }
   MSecsPerMin  = 60000;                      { Milliseconds per minute }
   MinutesPerDay= 24 * 60;                    { Minutes per day (1440) }

   NanosPerMicroSec= 1000;
   NanosPerMileSec = 1000000;
   NanosPerSecond  = 1000000000;
   NanosPerMinute  = Int64(60) * NanosPerSecond;

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
VAR i, Sum: Integer;
begin
  if Length(Self) = 0 then EXIT(0);

  Sum:= 0;
  for i in Self do
    Sum:= Sum + i;

  Result:= Sum / Length(Self);
end;



{ Returns the next day in the week }
function TWeekDaysHelper.NextDay: TWeekDays;
var Next: Integer;
begin
    Next := Ord(Self) + 1;

    if Next <= Ord(High(TWeekDays))
    then Result := TWeekDays(Next)
    else Result := TWeekDays.Monday;
end;

function TWeekDaysHelper.ToString: string;
begin
  case Self of
    Monday    : Result:= 'Monday';
    Tuesday   : Result:= 'Tuesday';
    Wednesday : Result:= 'Wednesday';
    Thursday  : Result:= 'Thursday';
    Friday    : Result:= 'Friday';
    Saturday  : Result:= 'Saturday';
    Sunday    : Result:= 'Sunday';
  else
    RAISE Exception.Create('TWeekDaysHelper - Unknown type');
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


procedure TDoubleArrayHelper.Sort;
begin
  TArray.Sort<Double>(Self);
end;


end.
