unit Test.LightVcl.Visual.CalendarCanvas;

{=============================================================================================================
   Unit tests for LightVcl.Visual.CalendarCanvas
   Tests calendar date calculations, leap year detection, week number calculation,
   and date validation functions.

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.DateUtils,
  Vcl.Graphics;

type
  [TestFixture]
  TTestCalendarCanvas = class
  private
    FBitmap: TBitmap;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { IsLeapYear Tests }
    [Test]
    procedure TestIsLeapYear_True_DivisibleBy4;

    [Test]
    procedure TestIsLeapYear_False_DivisibleBy100;

    [Test]
    procedure TestIsLeapYear_True_DivisibleBy400;

    [Test]
    procedure TestIsLeapYear_False_NotDivisibleBy4;

    { DaysInMonth Tests }
    [Test]
    procedure TestDaysInMonth_January;

    [Test]
    procedure TestDaysInMonth_February_NonLeap;

    [Test]
    procedure TestDaysInMonth_February_Leap;

    [Test]
    procedure TestDaysInMonth_April;

    [Test]
    procedure TestDaysInMonth_December;

    { ValidDate / IsValidDate Tests }
    [Test]
    procedure TestIsValidDate_Valid;

    [Test]
    procedure TestIsValidDate_InvalidMonth;

    [Test]
    procedure TestIsValidDate_InvalidDay;

    [Test]
    procedure TestIsValidDate_Feb29_LeapYear;

    [Test]
    procedure TestIsValidDate_Feb29_NonLeapYear;

    [Test]
    procedure TestIsValidDate_Feb30_LeapYear_Invalid;

    [Test]
    procedure TestIsValidDate_InvalidYear;

    { WeeksFirstDay / WeeksLastDay Tests }
    [Test]
    procedure TestWeeksFirstDay_Sunday;

    [Test]
    procedure TestWeeksFirstDay_Wednesday;

    [Test]
    procedure TestWeeksLastDay_Sunday;

    [Test]
    procedure TestWeeksLastDay_Wednesday;

    { CalculateDayOfYear Tests }
    [Test]
    procedure TestCalculateDayOfYear_Jan1;

    [Test]
    procedure TestCalculateDayOfYear_Feb1;

    [Test]
    procedure TestCalculateDayOfYear_Dec31_NonLeap;

    [Test]
    procedure TestCalculateDayOfYear_Dec31_Leap;

    [Test]
    procedure TestCalculateDayOfYear_Mar1_Leap;

    { DayOfYear and DaysInYear Tests }
    [Test]
    procedure TestDayOfYear_Today;

    [Test]
    procedure TestDaysInYear_Leap;

    [Test]
    procedure TestDaysInYear_NonLeap;

    { WeekNumber Tests }
    [Test]
    procedure TestWeekNumber_Jan1;

    [Test]
    procedure TestWeekNumber_MidYear;

    { Calendar Date Operations }
    [Test]
    procedure TestSetCalendarDate;

    [Test]
    procedure TestSetMonth_Valid;

    [Test]
    procedure TestSetDay_Valid;

    [Test]
    procedure TestSetYear_Valid;

    [Test]
    procedure TestSetMonth_DayAdjustment;

    { WkStartMonday (German Date Format) Tests }
    [Test]
    procedure TestWeeksFirstDay_GermanFormat;

    [Test]
    procedure TestWeeksLastDay_GermanFormat;
  end;

implementation

uses
  LightVcl.Visual.CalendarCanvas;


{ The date and the time, so a failed date assertion shows both and a stray time fraction is seen too }
function DateStr(CONST aDate: TDateTime): string;
begin
  Result:= FormatDateTime('yyyy-mm-dd hh:nn:ss', aDate);
end;


{ Setup and TearDown }

procedure TTestCalendarCanvas.Setup;
begin
  FBitmap:= TBitmap.Create;
  FBitmap.SetSize(400, 400);
end;

procedure TTestCalendarCanvas.TearDown;
begin
  FreeAndNil(FBitmap);
end;

{ IsLeapYear Tests }

procedure TTestCalendarCanvas.TestIsLeapYear_True_DivisibleBy4;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsTrue(Calendar.IsLeapYear(2024), '2024 should be a leap year');
    Assert.IsTrue(Calendar.IsLeapYear(2020), '2020 should be a leap year');
    Assert.IsTrue(Calendar.IsLeapYear(2016), '2016 should be a leap year');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestIsLeapYear_False_DivisibleBy100;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsFalse(Calendar.IsLeapYear(1900), '1900 should NOT be a leap year');
    Assert.IsFalse(Calendar.IsLeapYear(2100), '2100 should NOT be a leap year');
    Assert.IsFalse(Calendar.IsLeapYear(2200), '2200 should NOT be a leap year');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestIsLeapYear_True_DivisibleBy400;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsTrue(Calendar.IsLeapYear(2000), '2000 should be a leap year');
    Assert.IsTrue(Calendar.IsLeapYear(1600), '1600 should be a leap year');
    Assert.IsTrue(Calendar.IsLeapYear(2400), '2400 should be a leap year');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestIsLeapYear_False_NotDivisibleBy4;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsFalse(Calendar.IsLeapYear(2023), '2023 should NOT be a leap year');
    Assert.IsFalse(Calendar.IsLeapYear(2021), '2021 should NOT be a leap year');
    Assert.IsFalse(Calendar.IsLeapYear(2019), '2019 should NOT be a leap year');
  finally
    FreeAndNil(Calendar);
  end;
end;

{ DaysInMonth Tests }

procedure TTestCalendarCanvas.TestDaysInMonth_January;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.AreEqual(31, Calendar.DaysInMonth(1, 2023));
    Assert.AreEqual(31, Calendar.DaysInMonth(1, 2024));
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestDaysInMonth_February_NonLeap;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.AreEqual(28, Calendar.DaysInMonth(2, 2023));
    Assert.AreEqual(28, Calendar.DaysInMonth(2, 2021));
    Assert.AreEqual(28, Calendar.DaysInMonth(2, 1900), '1900 is not a leap year');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestDaysInMonth_February_Leap;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.AreEqual(29, Calendar.DaysInMonth(2, 2024));
    Assert.AreEqual(29, Calendar.DaysInMonth(2, 2000), '2000 is a leap year');
    Assert.AreEqual(29, Calendar.DaysInMonth(2, 2020));
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestDaysInMonth_April;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.AreEqual(30, Calendar.DaysInMonth(4, 2023));
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestDaysInMonth_December;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.AreEqual(31, Calendar.DaysInMonth(12, 2023));
  finally
    FreeAndNil(Calendar);
  end;
end;

{ ValidDate / IsValidDate Tests }

procedure TTestCalendarCanvas.TestIsValidDate_Valid;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsTrue(Calendar.IsValidDate(2023, 6, 15));
    Assert.IsTrue(Calendar.IsValidDate(2023, 1, 1));
    Assert.IsTrue(Calendar.IsValidDate(2023, 12, 31));
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestIsValidDate_InvalidMonth;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsFalse(Calendar.IsValidDate(2023, 0, 15), 'Month 0 is invalid');
    Assert.IsFalse(Calendar.IsValidDate(2023, 13, 15), 'Month 13 is invalid');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestIsValidDate_InvalidDay;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsFalse(Calendar.IsValidDate(2023, 6, 0), 'Day 0 is invalid');
    Assert.IsFalse(Calendar.IsValidDate(2023, 6, 31), 'June has only 30 days');
    Assert.IsFalse(Calendar.IsValidDate(2023, 4, 31), 'April has only 30 days');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestIsValidDate_Feb29_LeapYear;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsTrue(Calendar.IsValidDate(2024, 2, 29), 'Feb 29 is valid in leap year 2024');
    Assert.IsTrue(Calendar.IsValidDate(2000, 2, 29), 'Feb 29 is valid in leap year 2000');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestIsValidDate_Feb29_NonLeapYear;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsFalse(Calendar.IsValidDate(2023, 2, 29), 'Feb 29 is invalid in non-leap year 2023');
    Assert.IsFalse(Calendar.IsValidDate(1900, 2, 29), 'Feb 29 is invalid in 1900 (not a leap year)');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestIsValidDate_Feb30_LeapYear_Invalid;
var
  Calendar: TCalendarCanvas;
begin
  { This test verifies the bug fix - Feb 30 should NEVER be valid, even in leap years }
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsFalse(Calendar.IsValidDate(2024, 2, 30), 'Feb 30 is ALWAYS invalid, even in leap years');
    Assert.IsFalse(Calendar.IsValidDate(2000, 2, 30), 'Feb 30 is ALWAYS invalid');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestIsValidDate_InvalidYear;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Assert.IsFalse(Calendar.IsValidDate(0, 6, 15), 'Year 0 is invalid');
    Assert.IsFalse(Calendar.IsValidDate(10000, 6, 15), 'Year 10000 is invalid');
  finally
    FreeAndNil(Calendar);
  end;
end;

{ WeeksFirstDay / WeeksLastDay Tests }

procedure TTestCalendarCanvas.TestWeeksFirstDay_Sunday;
var
  Calendar: TCalendarCanvas;
  TestDate, ResultDate: TDateTime;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    { Jan 7, 2024 is a Sunday. WkStartMonday is FALSE by default, so the week starts on Sunday:
      a Sunday is the first day of its own week }
    TestDate:= EncodeDate(2024, 1, 7);
    ResultDate:= Calendar.WeeksFirstDay(TestDate);
    Assert.AreEqual('2024-01-07 00:00:00', DateStr(ResultDate), 'A Sunday starts its own week (US format)');
    Assert.AreEqual(1, DayOfWeek(ResultDate), 'Week should start on Sunday (US format)');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestWeeksFirstDay_Wednesday;
var
  Calendar: TCalendarCanvas;
  TestDate, ResultDate: TDateTime;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    { Jan 10, 2024 is a Wednesday. The US week (WkStartMonday FALSE, the default) runs Sunday Jan 7 to Saturday Jan 13 }
    TestDate:= EncodeDate(2024, 1, 10);
    ResultDate:= Calendar.WeeksFirstDay(TestDate);
    Assert.AreEqual('2024-01-07 00:00:00', DateStr(ResultDate), 'The week of Wednesday Jan 10 starts on Sunday Jan 7');
    Assert.AreEqual(1, DayOfWeek(ResultDate), 'Week should start on Sunday');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestWeeksLastDay_Sunday;
var
  Calendar: TCalendarCanvas;
  TestDate, ResultDate: TDateTime;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    { Jan 7, 2024 is a Sunday. The US week (WkStartMonday FALSE, the default) runs Sunday Jan 7 to Saturday Jan 13 }
    TestDate:= EncodeDate(2024, 1, 7);
    ResultDate:= Calendar.WeeksLastDay(TestDate);
    Assert.AreEqual('2024-01-13 00:00:00', DateStr(ResultDate), 'The week of Sunday Jan 7 ends on Saturday Jan 13');
    Assert.AreEqual(7, DayOfWeek(ResultDate), 'Week should end on Saturday');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestWeeksLastDay_Wednesday;
var
  Calendar: TCalendarCanvas;
  TestDate, ResultDate: TDateTime;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    { Jan 10, 2024 is a Wednesday. The US week (WkStartMonday FALSE, the default) runs Sunday Jan 7 to Saturday Jan 13 }
    TestDate:= EncodeDate(2024, 1, 10);
    ResultDate:= Calendar.WeeksLastDay(TestDate);
    Assert.AreEqual('2024-01-13 00:00:00', DateStr(ResultDate), 'The week of Wednesday Jan 10 ends on Saturday Jan 13');
    Assert.AreEqual(7, DayOfWeek(ResultDate), 'Week should end on Saturday');
  finally
    FreeAndNil(Calendar);
  end;
end;

{ CalculateDayOfYear Tests }

procedure TTestCalendarCanvas.TestCalculateDayOfYear_Jan1;
begin
  Assert.AreEqual(1, CalculateDayOfYear(2023, 1, 1), 'Jan 1 should be day 1');
  Assert.AreEqual(1, CalculateDayOfYear(2024, 1, 1), 'Jan 1 should be day 1 (leap year)');
end;

procedure TTestCalendarCanvas.TestCalculateDayOfYear_Feb1;
begin
  Assert.AreEqual(32, CalculateDayOfYear(2023, 2, 1), 'Feb 1 should be day 32');
  Assert.AreEqual(32, CalculateDayOfYear(2024, 2, 1), 'Feb 1 should be day 32 (leap year)');
end;

procedure TTestCalendarCanvas.TestCalculateDayOfYear_Dec31_NonLeap;
begin
  Assert.AreEqual(365, CalculateDayOfYear(2023, 12, 31), 'Dec 31 should be day 365 in non-leap year');
  Assert.AreEqual(365, CalculateDayOfYear(2021, 12, 31), 'Dec 31 should be day 365 in non-leap year');
end;

procedure TTestCalendarCanvas.TestCalculateDayOfYear_Dec31_Leap;
begin
  Assert.AreEqual(366, CalculateDayOfYear(2024, 12, 31), 'Dec 31 should be day 366 in leap year');
  Assert.AreEqual(366, CalculateDayOfYear(2000, 12, 31), 'Dec 31 should be day 366 in leap year 2000');
end;

procedure TTestCalendarCanvas.TestCalculateDayOfYear_Mar1_Leap;
begin
  { Mar 1 is day 61 in leap year (31 Jan + 29 Feb + 1) }
  Assert.AreEqual(61, CalculateDayOfYear(2024, 3, 1), 'Mar 1 should be day 61 in leap year');
  { Mar 1 is day 60 in non-leap year (31 Jan + 28 Feb + 1) }
  Assert.AreEqual(60, CalculateDayOfYear(2023, 3, 1), 'Mar 1 should be day 60 in non-leap year');
end;

{ DayOfYear and DaysInYear Tests }

procedure TTestCalendarCanvas.TestDayOfYear_Today;
var
  Calendar: TCalendarCanvas;
  ExpectedDay: Integer;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    { The constructor sets CalendarDate to today. The RTL's System.DateUtils.DayOfTheYear is the independent source.
      CalendarDate (not a second call to Date) is the input, so a run across midnight cannot fail. }
    ExpectedDay:= DayOfTheYear(Calendar.CalendarDate);
    Assert.AreEqual(ExpectedDay, Calendar.DayOfYear, 'DayOfYear of today must match System.DateUtils.DayOfTheYear');

    { Fixed dates whose month and day differ, so a swap of month and day is seen on any day the test runs }
    Calendar.CalendarDate:= EncodeDate(2024, 3, 1);
    Assert.AreEqual(61, Calendar.DayOfYear, 'Mar 1 2024 is day 61 (31 Jan + 29 Feb + 1)');

    Calendar.CalendarDate:= EncodeDate(2023, 12, 31);
    Assert.AreEqual(365, Calendar.DayOfYear, 'Dec 31 2023 is day 365 (not a leap year)');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestDaysInYear_Leap;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.Year:= 2024;
    Assert.AreEqual(366, Calendar.DaysInYear, '2024 should have 366 days');

    Calendar.Year:= 2000;
    Assert.AreEqual(366, Calendar.DaysInYear, '2000 should have 366 days');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestDaysInYear_NonLeap;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.Year:= 2023;
    Assert.AreEqual(365, Calendar.DaysInYear, '2023 should have 365 days');

    Calendar.Year:= 1900;
    Assert.AreEqual(365, Calendar.DaysInYear, '1900 should have 365 days');
  finally
    FreeAndNil(Calendar);
  end;
end;

{ WeekNumber Tests }

procedure TTestCalendarCanvas.TestWeekNumber_Jan1;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.CalendarDate:= EncodeDate(2024, 1, 1);
    { Jan 1, 2024 is a Monday. ISO 8601 puts it in week 1 (the week that holds the first Thursday).
      The product's default rule (WkStartMonday FALSE: weeks start on Sunday, week 1 starts Sunday Dec 31 2023) gives 1 too. }
    Assert.AreEqual(1, Calendar.WeekNumber, 'Jan 1 2024 is in week 1');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestWeekNumber_MidYear;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.CalendarDate:= EncodeDate(2024, 6, 15);
    { Saturday Jun 15 2024 is day 167 of the year. ISO 8601 (week 1 starts Monday Jan 1 2024) gives week 24 (Jun 10-16).
      The product's default rule (WkStartMonday FALSE: week 1 starts Sunday Dec 31 2023) gives 167 div 7 + 1 = 24 (Jun 9-15). }
    Assert.AreEqual(24, Calendar.WeekNumber, 'Jun 15 2024 is in week 24');
  finally
    FreeAndNil(Calendar);
  end;
end;

{ Calendar Date Operations }

procedure TTestCalendarCanvas.TestSetCalendarDate;
var
  Calendar: TCalendarCanvas;
  TestDate: TDateTime;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    TestDate:= EncodeDate(2024, 3, 15);
    Calendar.CalendarDate:= TestDate;
    Assert.AreEqual(2024, Calendar.Year);
    Assert.AreEqual(3, Calendar.Month);
    Assert.AreEqual(15, Calendar.Day);
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestSetMonth_Valid;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.CalendarDate:= EncodeDate(2024, 1, 15);
    Calendar.Month:= 6;
    Assert.AreEqual(6, Calendar.Month, 'Month should be set to 6');
    Assert.AreEqual(15, Calendar.Day, 'Day should remain 15');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestSetDay_Valid;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.CalendarDate:= EncodeDate(2024, 6, 1);
    Calendar.Day:= 20;
    Assert.AreEqual(20, Calendar.Day, 'Day should be set to 20');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestSetYear_Valid;
var
  Calendar: TCalendarCanvas;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.CalendarDate:= EncodeDate(2024, 6, 15);
    Calendar.Year:= 2025;
    Assert.AreEqual(2025, Calendar.Year, 'Year should be set to 2025');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestSetMonth_DayAdjustment;
var
  Calendar: TCalendarCanvas;
begin
  { When switching from a month with 31 days to one with 30,
    the day should be adjusted if necessary }
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.CalendarDate:= EncodeDate(2024, 1, 31);
    Calendar.Month:= 4;  { April has only 30 days }
    Assert.AreEqual(4, Calendar.Month, 'Month should be set to 4');
    Assert.AreEqual(30, Calendar.Day, 'Day should be adjusted to 30');
  finally
    FreeAndNil(Calendar);
  end;
end;

{ WkStartMonday (German Date Format) Tests }

procedure TTestCalendarCanvas.TestWeeksFirstDay_GermanFormat;
var
  Calendar: TCalendarCanvas;
  TestDate, ResultDate: TDateTime;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.WkStartMonday:= True;

    { Jan 10, 2024 is a Wednesday. The ISO 8601 week (Monday first) runs Monday Jan 8 to Sunday Jan 14 }
    TestDate:= EncodeDate(2024, 1, 10);
    ResultDate:= Calendar.WeeksFirstDay(TestDate);
    Assert.AreEqual('2024-01-08 00:00:00', DateStr(ResultDate), 'The German week of Wednesday Jan 10 starts on Monday Jan 8');
    Assert.AreEqual(2, DayOfWeek(ResultDate), 'Week should start on Monday (DayOfWeek=2)');
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure TTestCalendarCanvas.TestWeeksLastDay_GermanFormat;
var
  Calendar: TCalendarCanvas;
  TestDate, ResultDate: TDateTime;
begin
  Calendar:= TCalendarCanvas.Create(FBitmap.Canvas);
  try
    Calendar.WkStartMonday:= True;

    { Jan 10, 2024 is a Wednesday. The ISO 8601 week (Monday first) runs Monday Jan 8 to Sunday Jan 14 }
    TestDate:= EncodeDate(2024, 1, 10);
    ResultDate:= Calendar.WeeksLastDay(TestDate);
    Assert.AreEqual('2024-01-14 00:00:00', DateStr(ResultDate), 'The German week of Wednesday Jan 10 ends on Sunday Jan 14');
    Assert.AreEqual(1, DayOfWeek(ResultDate), 'Week should end on Sunday (DayOfWeek=1)');
  finally
    FreeAndNil(Calendar);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCalendarCanvas);

end.
