UNIT LightCore.Time;

{=============================================================================================================
   www.GabrielMoraru.com
   2025.09.14
   See Copyright file
--------------------------------------------------------------------------------------------------------------
   DateTime utilities

   For I/O (disk, files) related functions see the "Time" section in LightCore.IO:
      function  ExtractTimeFromFileName
      function  DateToStr_IO
      function  TimeToStr_IO
      function  DateTimeToStr_IO
      function  DateTimeToStr_IO

=============================================================================================================}

INTERFACE

USES
   System.AnsiStrings, System.Character, System.SysUtils, System.Math, System.IOUtils, System.StrUtils, System.Classes, System.Types, System.TimeSpan, System.DateUtils,
   LightCore.Types;

 // CURRENT DATE-TIME
 function TodayIs: string;                                         { Returns today as date based on Locale. Example: Montag }
 function CurrentDateToString{(ShowSeconds: Boolean)}: string;     { Returns today as date & time. Example: 31.12.2021 - 16:50 }
 function CurrentTimeToString(ShowSeconds: Boolean): string;       { Returns time in short format (no seconds). Example: 16:50 }
 function TimeToString(CONST T: TDateTime; ShowSeconds: Boolean): string;

 function CurrentYear: Word;
 function CurrentMonth: Word;
 function CurrentDay: Word;
 function CurrentHour: Word;

 // DECODE
 function DecodeHour             (Time: TTime): Word;
 function DecodeMinute           (Time: TTime): Word;
 function DecodeSecond           (Time: TTime): Word;

 // FORMAT
 function GetUniversalDateFormat: TFormatSettings;

 // COMPARISON
 function SameDateEx             (Time1, Time2: TDateTime): boolean;  deprecated 'Use System.DateUtils.SameDate instead';
 function EarlierThan            (Date1, Date2: TDateTime): boolean;                           { Returns true if Date1 is smaller (earlier) than Date2 }
 function DaysBetweenEx          (CONST MilestoneDate, CurrentDate: TDateTime): Double; deprecated 'Use System.DateUtils.DaySpan instead';
 function NewDaysBetween         (CONST MilestoneDate, CurrentDate: TdateTime): Double;
 function SecondsBetweenEx       (CONST MilestoneDate, CurrentDate: TDateTime): Int64;         { Returns the number of seconds between two specified TDateTime values.  The difference between this function and the Embarcadero one is that it returns zero if CurrentDate >= MilestoneDate }
 function DateIsToday            (Year, Month, Day: word): Boolean;                            { Returns true if the specified date is today }

 // STRING CONVERSIONS
 function  StringToSeconds       (CONST s: String): Integer;                                   { Converts a string formated like 'hh:mm:ss' to seconds.  }
 function  StringIsDate          (CONST s: string): Boolean;
 function  StringIsTime          (CONST s: string): Boolean;
 function  DateToStrUS           (CONST Time: TDateTime): string;                              { converts date to string, using the US (YYY.MM.DD) format }

 // CONVERSIONS
 procedure SecondsToTime         (Seconds : Cardinal; VAR D, H, M, S: Cardinal);
 function  MiliSecToTimeAuto     (MSeconds: Cardinal): string;
 function  mSecondsToTime        (mSeconds: Cardinal): string;

 function  ShowTimeNice          (Seconds: Cardinal): string;  overload;
 function  ShowTimeNice          (aTime: TDateTime): string;   overload;

 function  DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
 function  DateToTime_           (aDate : TDateTime): string;

 // CONVERSIONS
 function  Date2Cardinal         (xDate: TDate): Cardinal;
 function  Cardinal2Date         (xCardinal: Cardinal): TDate;



IMPLEMENTATION
USES LightCore;


function mSecondsToTime(mSeconds: Cardinal): string;
begin
  Result:= ShowTimeNice(mSeconds DIV 1000);
end;



function DateToTime_(aDate: TDateTime): string;
begin
 Result:= FormatDateTime('d:hh:nn:ss', aDate);
end;


procedure SecondsToTime (Seconds: Cardinal; VAR D, H, M, S: Cardinal);
begin
 D := (Seconds DIV SecsPerDay);
 H := (Seconds DIV SecsPerHour) - (D* 24);
 M := (Seconds DIV SecsPerMin)  - (D* 24* SecsPerMin) - (H* 60);
 S :=  Seconds                  - (D* SecsPerday)     - (H* 3600)- (M*60);
end;


{ Common function that does the actual formatting }
function ShowTimeNiceCommon(D, H, M, S: Cardinal): string;
begin
  Result := '';

  { Show days }
  if D > 0 then Result := IntToStr(D) + ' days ';

  { Show hours }
  if H > 0 then Result := Result + IntToStr(H) + 'h ';

  { Show minutes but ONLY if we have less than 7 days }
  if D <= 7 then
  begin
    Result := Result + IntToStr(M) + 'm ';

    { Show seconds ONLY if the time is short (under 1h) }
    if (D = 0) AND (H = 0)
    then Result := Result + IntToStr(S) + 's';
  end;
end;


{ Converts seconds to time, but showing the shortest string possible.
  For example: it will convert 59 to '59s' and 61 to '1m 01s' }
function ShowTimeNice(Seconds: Cardinal): string; // Old name: SecondsToTime_FormatAuto
var D, H, M, S: Cardinal;
begin
  D := (Seconds DIV SecsPerDay);
  H := (Seconds DIV SecsPerHour)  - (D * 24);
  M := (Seconds DIV SecsPerMin)   - (D * 24 * SecsPerMin) - (H * 60);
  S := Seconds - (D * SecsPerDay) - (H * 3600) - (M * 60);

  Result := ShowTimeNiceCommon(D, H, M, S);
end;


{ Example 5 days, 23h:59m:59s }
function ShowTimeNice(aTime: TDateTime): string;
var D: Integer;
    H, M, S: Word;
begin
  D := Trunc(aTime);
  H := HourOf(aTime);
  M := MinuteOf(aTime);
  S := SecondOf(aTime);

  Result:= ShowTimeNiceCommon(D, H, M, S);
end;


function MiliSecToTimeAuto(MSeconds: Cardinal): string;     //old name: mSecondsToTime
begin
  if MSeconds< 1000                                                                                 { under 1 sec }
  then Result:= IntToStr(MSeconds)+ 'ms'
  else
    if MSeconds< 60000                                                                              { under 1 minute }
    then Result:= Real2Str(MSeconds / 1000, 3)+ 'sec'
    else Result:= ShowTimeNice(MSeconds DIV 1000);
end;



function DateToStrUS(CONST Time: TDateTime): string; { converts date to string, using the US (YYY.MM.DD) format }
VAR aYear, aMonth, aDay : Word;
begin
  DecodeDate(Time, aYear, aMonth, aDay);

  Result:= IntToStr(aYear)+ '.';
  if aMonth < 10
  then Result:= Result+ '0'+IntToStr(aMonth)+ '.'
  else Result:= Result+ IntToStr(aMonth)+ '.';

  if aDay < 10
  then Result:= Result+ '0'+IntToStr(aDay)
  else Result:= Result+ IntToStr(aDay);
end;




function SameDateEx(Time1, Time2: TDateTime): boolean;     { We cannot compare two TDateTimes in Delphi because of Real precision. CompareDateTime also won't work. See: http://stackoverflow.com/questions/38705011/odd-behavior-when-comparing-two-tdatetime-vars }
VAR
   Year1, Month1, Day1, Hour1, Min1, Sec1, Msec1: Word;
   Year2, Month2, Day2, Hour2, Min2, Sec2, Msec2: Word;
begin
 DecodeDateTime(Time1, Year1, Month1, Day1, Hour1, Min1, Sec1, Msec1);
 DecodeDateTime(Time2, Year2, Month2, Day2, Hour2, Min2, Sec2, Msec2);
 Result:= (Year1 = Year2) AND (Month1 = Month2) AND (Day1 = Day2) AND (Hour1 = Hour2) AND (Min1 = Min2) AND (Sec1 = Sec2) AND (Msec1 = Msec2);
end;


function EarlierThan(Date1, Date2: TDateTime): boolean;      { Returns true if Date1 is smaller (earlier) than Date2 }
begin
 Result:= System.DateUtils.CompareDateTime(Date1, Date2) = -1;
end;





function DecodeHour(Time: TTime): Word;
VAR wMin, wSec, wMsec: word;
begin
 DecodeTime(Time, Result, wMin, wSec, wMsec);
end;


function DecodeMinute(Time: TTime): Word;
VAR wHor, wSec, wMsec: word;
begin
 DecodeTime(Time, wHor, Result, wSec, wMsec);
end;


function DecodeSecond(Time: TTime): Word;
VAR wHor, wMin, wMsec: word;
begin
 DecodeTime(Time, wHor, wMin, Result, wMsec);
end;




{ Was used to save our data in INI files. Now we save it as floats. }
function GetUniversalDateFormat: TFormatSettings;  //Unused
begin
  Result:= TFormatSettings.Create;
  Result.DateSeparator:= '-';
  Result.TimeSeparator:= ':';
  Result.ShortDateFormat:= 'YYYY-MM-DD';
end;




{ Converts a string formatted like 'hh:mm:ss' or 'mm:ss' to seconds.
  Returns -1 is the string does not contain a valid time.

    StringToSeconds('00:01:30')     // returns 90     (sec)
    StringToSeconds('01:30')        // returns 5400   (sec)
    StringToSeconds('10')           // returns 864000 (sec)
    StringToSeconds('1.30')         // returns -1
    StringToSeconds('x')            // returns -1 }
function StringToSeconds(CONST s: String): integer;
VAR
  TimeSpan: TTimeSpan;
begin
  TRY
   TimeSpan:= System.TimeSpan.TTimeSpan.Parse(s);
   Result  := Round(TimeSpan.TotalSeconds);
  EXCEPT
   Result:= -1;
  end;
end;


{...check if a string is a valid date or time?}
function StringIsDate(CONST s: string): Boolean;
begin
  Result:= True;
  TRY
    StrToDate(s);
  EXCEPT
    Result:= False;
  END;
end;

{...check if a string is a valid date or time?}
function StringIsTime(CONST s: string): Boolean;
begin
  Result:= True;
  TRY
    StrToTime(s);
  EXCEPT
    Result:= False;
  end;
end;







function CurrentYear: Word;
VAR aMonth, aDay : Word;
begin
  DecodeDate(Now, Result, aMonth, aDay);
end;


function CurrentMonth: Word;
VAR aYear, aDay : Word;
begin
  DecodeDate(Now, aYear, Result, aDay);
end;


function CurrentDay: Word;
VAR aMonth, aYear : Word;
begin
  DecodeDate(Now, aYear, aMonth, Result);
end;


function CurrentHour: Word;
VAR aMin, aSec, mSec: Word;
begin
  DecodeTime(Now, Result, aMin, aSec, mSec);
end;


{ Returns today as date (string) based on Locale. Example: Montag }
function TodayIs: string; // This is cross-platform
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := TFormatSettings.Create;  // Use the current locale settings
  Result := FormatDateTime('dddd', Now, FormatSettings);
end;


{ Returns today as date AND time. Example: 31.12.2021 - 16:50 }
function CurrentDateToString: string;
VAR
  Present: TDateTime;
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  Present:= Now;
  DecodeDate(Present, Year, Month, Day);
  DecodeTime(Present, Hour, Min, Sec, MSec);
  Result:= IntToStr(Day)+'.'+IntToStr(Month)+'.'+IntToStr(Year)+' - '+ TimeToString(Present, FALSE);
end;


{ Returns time in short format (no seconds). Example: 16:50 }
function CurrentTimeToString(ShowSeconds: Boolean): string;
begin
  Result:= TimeToString(Now, ShowSeconds);
end;


{ Returns time in short format. Example: 16:50 }
function TimeToString(CONST T: TDateTime; ShowSeconds: Boolean): string;
VAR
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(T, Hour, Min, Sec, MSec);

  if Hour < 10
  then Result:= '0'+ IntToStr(Hour)
  else Result:= IntToStr(Hour);

  if Min < 10
  then Result:= Result+ ':'+ '0'+IntToStr(Min)
  else Result:= Result+ ':'+ IntToStr(Min);

  if ShowSeconds then
    if Sec < 10
    then Result:= Result+ ':'+ '0'+IntToStr(Sec)
    else Result:= Result+ ':'+ IntToStr(Sec);
end;


function DateIsToday(Year, Month, Day: word): Boolean;   { Returns true if the specified date is today }
VAR NewDate: TDateTime;
begin
  NewDate:= EncodeDate(Year, Month, Day);
  Result:= IsToday(NewDate);
end;


function Date2Cardinal(xDate: TDate): Cardinal;
begin
  Result:= DaysBetween(xDate, 0);                        { Calculate how many days are between xDate and 01/01/1899 }
end;


function Cardinal2Date(xCardinal: Cardinal): TDate;      { Date returned is in US format }
begin
  Result:= IncDay(0, xCardinal);
end;




{ Copied from DateUtils.PAS where it is marked as 'Internal'.
  These functions in DateUtils.pas are not accurate. See this of details: http://stackoverflow.com/questions/17109814/why-datetimetomilliseconds-in-dateutils-pas-is-marked-as-internal }
function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
var
  LTimeStamp: TTimeStamp;
begin
  LTimeStamp := DateTimeToTimeStamp(ADateTime);
  Result := LTimeStamp.Date;
  Result := (Result * MSecsPerDay) + LTimeStamp.Time;
end;


{ Returns the number of seconds between two specified TDateTime values.
  The difference between this function and Embarcadero.DateUtils.SecondsBetween is that it returns zero if CurrentDate >= MilestoneDate.
  These functions in DateUtils.pas are not accurate. See this of details: http://stackoverflow.com/questions/17109814/why-datetimetomilliseconds-in-dateutils-pas-is-marked-as-internal }
function SecondsBetweenEx(CONST MilestoneDate, CurrentDate: TDateTime): Int64;
begin
  if CurrentDate < MilestoneDate
  then Result := (DateTimeToMilliseconds(MilestoneDate) - DateTimeToMilliseconds(CurrentDate)) DIV (MSecsPerSec)
  else Result := 0;
end;



{ Comparing '31 Dec 1999 23:59' and '1 Jan 2000 00:01' (2 minutes difference):
     DaySpan        returns 0.03  (or something like this)
     DaysBetween    returns 0
     NewDaysBetween retunrs 1,03 }
function DaysBetweenEx(CONST MilestoneDate, CurrentDate: TdateTime): Double;
begin
 RAISE exception.Create('DaysBetweenEx is deprecated ');
end;


function NewDaysBetween(CONST MilestoneDate, CurrentDate: TdateTime): Double;
begin
  if SameDate(MilestoneDate, CurrentDate)
  then Result := DaySpan(MilestoneDate, CurrentDate)
  else Result := DaySpan(MilestoneDate, CurrentDate) + 1;
end;

{ Exaclty the same as DaysBetweenEx but uses a different approach to calculate it
function DaySpanEx(CONST MilestoneDate, CurrentDate: TDateTime): Double;
begin
  Result := Abs(DateTimeToMilliseconds(MilestoneDate) - DateTimeToMilliseconds(CurrentDate)) / MSecsPerDay;
end; }



end.

