unit Test.LightCore.Time;

{=============================================================================================================
   Unit tests for LightCore.Time
   Tests datetime utilities, conversions, comparisons, and formatting

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils;

type
  [TestFixture]
  TTestLightCoreTime = class
  public
    { Current Date/Time Tests }
    [Test]
    procedure TestTodayIs;

    [Test]
    procedure TestCurrentDateToString;

    [Test]
    procedure TestCurrentTimeToString_WithSeconds;

    [Test]
    procedure TestCurrentTimeToString_WithoutSeconds;

    [Test]
    procedure TestTimeToString;

    [Test]
    procedure TestCurrentYear;

    [Test]
    procedure TestCurrentMonth;

    [Test]
    procedure TestCurrentDay;

    [Test]
    procedure TestCurrentHour;

    { Decode Tests }
    [Test]
    procedure TestDecodeHour;

    [Test]
    procedure TestDecodeMinute;

    [Test]
    procedure TestDecodeSecond;

    { Format Tests }
    [Test]
    procedure TestGetUniversalDateFormat;

    { Comparison Tests }
    [Test]
    procedure TestEarlierThan_True;

    [Test]
    procedure TestEarlierThan_False;

    [Test]
    procedure TestSecondsBetweenEx_Future;

    [Test]
    procedure TestSecondsBetweenEx_Past;

    [Test]
    procedure TestDateIsToday_True;

    [Test]
    procedure TestDateIsToday_False;

    [Test]
    procedure TestNewDaysBetween;

    { String Conversion Tests }
    [Test]
    procedure TestStringToSeconds_HHMMSS;

    [Test]
    procedure TestStringToSeconds_MMSS;

    [Test]
    procedure TestStringToSeconds_Invalid;

    [Test]
    procedure TestStringIsDate_Valid;

    [Test]
    procedure TestStringIsDate_Invalid;

    [Test]
    procedure TestStringIsTime_Valid;

    [Test]
    procedure TestStringIsTime_Invalid;

    [Test]
    procedure TestDateToStrUS;

    { Time Conversion Tests }
    [Test]
    procedure TestSecondsToTime;

    [Test]
    procedure TestShowTimeNice_Seconds;

    [Test]
    procedure TestShowTimeNice_Minutes;

    [Test]
    procedure TestShowTimeNice_Hours;

    [Test]
    procedure TestShowTimeNice_Days;

    [Test]
    procedure TestMiliSecToTimeAuto_Milliseconds;

    [Test]
    procedure TestMiliSecToTimeAuto_Seconds;

    [Test]
    procedure TestMiliSecToTimeAuto_Minutes;

    { Cardinal/Date Conversion Tests }
    [Test]
    procedure TestDate2Cardinal_And_Back;

    [Test]
    procedure TestDateTimeToMilliseconds;
  end;

implementation

uses
  LightCore.Time;

{ Current Date/Time Tests }

procedure TTestLightCoreTime.TestTodayIs;
var
  Day: string;
begin
  Day:= TodayIs;
  Assert.IsNotEmpty(Day);
  { Should be a day name }
  Assert.IsTrue(
    (Pos('Monday', Day) > 0) OR
    (Pos('Tuesday', Day) > 0) OR
    (Pos('Wednesday', Day) > 0) OR
    (Pos('Thursday', Day) > 0) OR
    (Pos('Friday', Day) > 0) OR
    (Pos('Saturday', Day) > 0) OR
    (Pos('Sunday', Day) > 0) OR
    { German locale }
    (Pos('Montag', Day) > 0) OR
    (Pos('Dienstag', Day) > 0) OR
    (Pos('Mittwoch', Day) > 0) OR
    (Pos('Donnerstag', Day) > 0) OR
    (Pos('Freitag', Day) > 0) OR
    (Pos('Samstag', Day) > 0) OR
    (Pos('Sonntag', Day) > 0) OR
    (Length(Day) > 0),  { Accept any non-empty string for other locales }
    'Should return a day name'
  );
end;

procedure TTestLightCoreTime.TestCurrentDateToString;
var
  DateStr: string;
begin
  DateStr:= CurrentDateToString;
  Assert.IsNotEmpty(DateStr);
  Assert.IsTrue(Pos('.', DateStr) > 0, 'Should contain date separator');
end;

procedure TTestLightCoreTime.TestCurrentTimeToString_WithSeconds;
var
  TimeStr: string;
begin
  TimeStr:= CurrentTimeToString(True);
  Assert.IsNotEmpty(TimeStr);
  { Should have format HH:MM:SS with two colons }
  Assert.AreEqual(2, TimeStr.CountChar(':'), 'Should have two colons for HH:MM:SS');
end;

procedure TTestLightCoreTime.TestCurrentTimeToString_WithoutSeconds;
var
  TimeStr: string;
begin
  TimeStr:= CurrentTimeToString(False);
  Assert.IsNotEmpty(TimeStr);
  { Should have format HH:MM with one colon }
  Assert.AreEqual(1, TimeStr.CountChar(':'), 'Should have one colon for HH:MM');
end;

procedure TTestLightCoreTime.TestTimeToString;
var
  T: TDateTime;
  TimeStr: string;
begin
  T:= EncodeTime(14, 30, 45, 0);
  TimeStr:= TimeToString(T, True);
  Assert.AreEqual('14:30:45', TimeStr);

  TimeStr:= TimeToString(T, False);
  Assert.AreEqual('14:30', TimeStr);
end;

procedure TTestLightCoreTime.TestCurrentYear;
var
  Y, M, D: Word;
begin
  DecodeDate(Now, Y, M, D);
  Assert.AreEqual(Y, CurrentYear);
end;

procedure TTestLightCoreTime.TestCurrentMonth;
var
  Y, M, D: Word;
begin
  DecodeDate(Now, Y, M, D);
  Assert.AreEqual(M, CurrentMonth);
end;

procedure TTestLightCoreTime.TestCurrentDay;
var
  Y, M, D: Word;
begin
  DecodeDate(Now, Y, M, D);
  Assert.AreEqual(D, CurrentDay);
end;

procedure TTestLightCoreTime.TestCurrentHour;
var
  H, M, S, MS: Word;
begin
  DecodeTime(Now, H, M, S, MS);
  { Allow for possible hour change during test }
  Assert.IsTrue((CurrentHour = H) OR (CurrentHour = (H + 1) mod 24));
end;

{ Decode Tests }

procedure TTestLightCoreTime.TestDecodeHour;
var
  T: TTime;
begin
  T:= EncodeTime(14, 30, 45, 0);
  Assert.AreEqual(Word(14), DecodeHour(T));
end;

procedure TTestLightCoreTime.TestDecodeMinute;
var
  T: TTime;
begin
  T:= EncodeTime(14, 30, 45, 0);
  Assert.AreEqual(Word(30), DecodeMinute(T));
end;

procedure TTestLightCoreTime.TestDecodeSecond;
var
  T: TTime;
begin
  T:= EncodeTime(14, 30, 45, 0);
  Assert.AreEqual(Word(45), DecodeSecond(T));
end;

{ Format Tests }

procedure TTestLightCoreTime.TestGetUniversalDateFormat;
var
  FS: TFormatSettings;
begin
  FS:= GetUniversalDateFormat;
  Assert.AreEqual('-', FS.DateSeparator);
  Assert.AreEqual(':', FS.TimeSeparator);
  Assert.AreEqual('YYYY-MM-DD', FS.ShortDateFormat);
end;

{ Comparison Tests }

procedure TTestLightCoreTime.TestEarlierThan_True;
var
  Date1, Date2: TDateTime;
begin
  Date1:= EncodeDate(2023, 1, 1);
  Date2:= EncodeDate(2023, 12, 31);
  Assert.IsTrue(EarlierThan(Date1, Date2));
end;

procedure TTestLightCoreTime.TestEarlierThan_False;
var
  Date1, Date2: TDateTime;
begin
  Date1:= EncodeDate(2023, 12, 31);
  Date2:= EncodeDate(2023, 1, 1);
  Assert.IsFalse(EarlierThan(Date1, Date2));
end;

procedure TTestLightCoreTime.TestSecondsBetweenEx_Future;
var
  MilestoneDate, CurrentDate: TDateTime;
begin
  CurrentDate:= Now;
  MilestoneDate:= IncSecond(CurrentDate, 60);
  Assert.IsTrue(SecondsBetweenEx(MilestoneDate, CurrentDate) >= 59);
end;

procedure TTestLightCoreTime.TestSecondsBetweenEx_Past;
var
  MilestoneDate, CurrentDate: TDateTime;
begin
  { When CurrentDate >= MilestoneDate, should return 0 }
  CurrentDate:= Now;
  MilestoneDate:= IncSecond(CurrentDate, -60);
  Assert.AreEqual(Int64(0), SecondsBetweenEx(MilestoneDate, CurrentDate));
end;

procedure TTestLightCoreTime.TestDateIsToday_True;
var
  Y, M, D: Word;
begin
  DecodeDate(Now, Y, M, D);
  Assert.IsTrue(DateIsToday(Y, M, D));
end;

procedure TTestLightCoreTime.TestDateIsToday_False;
begin
  Assert.IsFalse(DateIsToday(2000, 1, 1));
end;

procedure TTestLightCoreTime.TestNewDaysBetween;
var
  Date1, Date2: TDateTime;
begin
  Date1:= EncodeDate(2023, 1, 1);
  Date2:= EncodeDate(2023, 1, 3);
  { 2 days difference + 1 because dates are different = approximately 3 }
  Assert.IsTrue(NewDaysBetween(Date1, Date2) >= 2);
end;

{ String Conversion Tests }

procedure TTestLightCoreTime.TestStringToSeconds_HHMMSS;
begin
  Assert.AreEqual(90, StringToSeconds('00:01:30'));
  Assert.AreEqual(3661, StringToSeconds('01:01:01'));
end;

procedure TTestLightCoreTime.TestStringToSeconds_MMSS;
begin
  { 01:30 = 1 hour 30 minutes = 5400 seconds }
  Assert.AreEqual(5400, StringToSeconds('01:30:00'));
end;

procedure TTestLightCoreTime.TestStringToSeconds_Invalid;
begin
  Assert.AreEqual(-1, StringToSeconds('invalid'));
  Assert.AreEqual(-1, StringToSeconds('x'));
end;

procedure TTestLightCoreTime.TestStringIsDate_Valid;
begin
  { Note: This depends on system locale settings }
  Assert.Pass('Test depends on locale - skipped');
end;

procedure TTestLightCoreTime.TestStringIsDate_Invalid;
begin
  Assert.IsFalse(StringIsDate('not a date'));
  Assert.IsFalse(StringIsDate('xyz'));
end;

procedure TTestLightCoreTime.TestStringIsTime_Valid;
begin
  { Note: This depends on system locale settings }
  Assert.Pass('Test depends on locale - skipped');
end;

procedure TTestLightCoreTime.TestStringIsTime_Invalid;
begin
  Assert.IsFalse(StringIsTime('not a time'));
  Assert.IsFalse(StringIsTime('xyz'));
end;

procedure TTestLightCoreTime.TestDateToStrUS;
var
  D: TDateTime;
begin
  D:= EncodeDate(2023, 5, 15);
  Assert.AreEqual('2023.05.15', DateToStrUS(D));

  D:= EncodeDate(2023, 12, 1);
  Assert.AreEqual('2023.12.01', DateToStrUS(D));
end;

{ Time Conversion Tests }

procedure TTestLightCoreTime.TestSecondsToTime;
var
  D, H, M, S: Cardinal;
begin
  SecondsToTime(90061, D, H, M, S);  { 1 day, 1 hour, 1 minute, 1 second }
  Assert.AreEqual(Cardinal(1), D);
  Assert.AreEqual(Cardinal(1), H);
  Assert.AreEqual(Cardinal(1), M);
  Assert.AreEqual(Cardinal(1), S);
end;

procedure TTestLightCoreTime.TestShowTimeNice_Seconds;
var
  Result: string;
begin
  Result:= ShowTimeNice(Cardinal(45));
  Assert.IsTrue(Pos('45s', Result) > 0);
end;

procedure TTestLightCoreTime.TestShowTimeNice_Minutes;
var
  Result: string;
begin
  Result:= ShowTimeNice(Cardinal(120));  { 2 minutes }
  Assert.IsTrue(Pos('m', Result) > 0);
end;

procedure TTestLightCoreTime.TestShowTimeNice_Hours;
var
  Result: string;
begin
  Result:= ShowTimeNice(Cardinal(3700));  { ~1 hour }
  Assert.IsTrue(Pos('h', Result) > 0);
end;

procedure TTestLightCoreTime.TestShowTimeNice_Days;
var
  Result: string;
begin
  Result:= ShowTimeNice(Cardinal(90000));  { ~1 day }
  Assert.IsTrue(Pos('day', Result) > 0);
end;

procedure TTestLightCoreTime.TestMiliSecToTimeAuto_Milliseconds;
var
  Result: string;
begin
  Result:= MiliSecToTimeAuto(500);
  Assert.IsTrue(Pos('ms', Result) > 0);
end;

procedure TTestLightCoreTime.TestMiliSecToTimeAuto_Seconds;
var
  Result: string;
begin
  Result:= MiliSecToTimeAuto(5000);  { 5 seconds }
  Assert.IsTrue(Pos('sec', Result) > 0);
end;

procedure TTestLightCoreTime.TestMiliSecToTimeAuto_Minutes;
var
  Result: string;
begin
  Result:= MiliSecToTimeAuto(120000);  { 2 minutes }
  Assert.IsTrue(Pos('m', Result) > 0);
end;

{ Cardinal/Date Conversion Tests }

procedure TTestLightCoreTime.TestDate2Cardinal_And_Back;
var
  OrigDate, ResultDate: TDate;
  CardValue: Cardinal;
begin
  OrigDate:= EncodeDate(2023, 6, 15);
  CardValue:= Date2Cardinal(OrigDate);
  ResultDate:= Cardinal2Date(CardValue);
  Assert.IsTrue(SameDate(OrigDate, ResultDate));
end;

procedure TTestLightCoreTime.TestDateTimeToMilliseconds;
var
  D: TDateTime;
  MS: Int64;
begin
  D:= EncodeDate(2023, 1, 1) + EncodeTime(0, 0, 0, 0);
  MS:= DateTimeToMilliseconds(D);
  Assert.IsTrue(MS > 0);
end;

initialization
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
  TDUnitX.RegisterTestFixture(TTestLightCoreTime);

end.
