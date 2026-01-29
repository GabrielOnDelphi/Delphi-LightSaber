unit Test.LightCore.Types;

{=============================================================================================================
   Unit tests for LightCore.Types
   Tests type definitions, array helpers, and weekday helpers

   Requires: TESTINSIGHT compiler directive for TestInsight integration
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  [TestFixture]
  TTestLightCoreTypes = class
  public
    { TIntegerArray Helper Tests }
    [Test]
    procedure TestIntegerArray_Add;

    [Test]
    procedure TestIntegerArray_Average;

    [Test]
    procedure TestIntegerArray_Average_Empty;

    { TDoubleArray Helper Tests }
    [Test]
    procedure TestDoubleArray_Add;

    [Test]
    procedure TestDoubleArray_Sort;

    { TWeekDays Helper Tests }
    [Test]
    procedure TestWeekDays_NextDay_Monday;

    [Test]
    procedure TestWeekDays_NextDay_Sunday;

    [Test]
    procedure TestWeekDays_ToString;

    [Test]
    procedure TestWeekDays_FullWeek;

    { Constant Tests }
    [Test]
    procedure TestConstants_KB;

    [Test]
    procedure TestConstants_MB;

    [Test]
    procedure TestConstants_GB;

    [Test]
    procedure TestConstants_TB;

    [Test]
    procedure TestConstants_TimeUnits;

    { Character Set Tests }
    [Test]
    procedure TestCharSets_Numbers;

    [Test]
    procedure TestCharSets_Alphabet;

    [Test]
    procedure TestCharSets_HexNumbers;
  end;

implementation

uses
  LightCore.Types;

{ TIntegerArray Helper Tests }

procedure TTestLightCoreTypes.TestIntegerArray_Add;
var
  Arr: TIntegerArray;
begin
  SetLength(Arr, 0);
  Arr.Add(10);
  Arr.Add(20);
  Arr.Add(30);

  Assert.AreEqual(3, Length(Arr));
  Assert.AreEqual(10, Arr[0]);
  Assert.AreEqual(20, Arr[1]);
  Assert.AreEqual(30, Arr[2]);
end;

procedure TTestLightCoreTypes.TestIntegerArray_Average;
var
  Arr: TIntegerArray;
begin
  SetLength(Arr, 0);
  Arr.Add(10);
  Arr.Add(20);
  Arr.Add(30);

  Assert.AreEqual(Single(20.0), Arr.Average);
end;

procedure TTestLightCoreTypes.TestIntegerArray_Average_Empty;
var
  Arr: TIntegerArray;
begin
  SetLength(Arr, 0);
  Assert.AreEqual(Single(0), Arr.Average);
end;

{ TDoubleArray Helper Tests }

procedure TTestLightCoreTypes.TestDoubleArray_Add;
var
  Arr: TDoubleArray;
begin
  SetLength(Arr, 0);
  Arr.Add(1.5);
  Arr.Add(2.5);
  Arr.Add(3.5);

  Assert.AreEqual(3, Length(Arr));
  Assert.AreEqual(Double(1.5), Arr[0]);
  Assert.AreEqual(Double(2.5), Arr[1]);
  Assert.AreEqual(Double(3.5), Arr[2]);
end;

procedure TTestLightCoreTypes.TestDoubleArray_Sort;
var
  Arr: TDoubleArray;
begin
  SetLength(Arr, 0);
  Arr.Add(3.5);
  Arr.Add(1.5);
  Arr.Add(2.5);
  Arr.Sort;

  Assert.AreEqual(Double(1.5), Arr[0]);
  Assert.AreEqual(Double(2.5), Arr[1]);
  Assert.AreEqual(Double(3.5), Arr[2]);
end;

{ TWeekDays Helper Tests }

procedure TTestLightCoreTypes.TestWeekDays_NextDay_Monday;
var
  Day: TWeekDays;
begin
  Day:= TWeekDays.Monday;
  Assert.AreEqual(TWeekDays.Tuesday, Day.NextDay);
end;

procedure TTestLightCoreTypes.TestWeekDays_NextDay_Sunday;
var
  Day: TWeekDays;
begin
  Day:= TWeekDays.Sunday;
  Assert.AreEqual(TWeekDays.Monday, Day.NextDay);  { Wraps around }
end;

procedure TTestLightCoreTypes.TestWeekDays_ToString;
begin
  Assert.AreEqual('Monday', TWeekDays.Monday.ToString);
  Assert.AreEqual('Tuesday', TWeekDays.Tuesday.ToString);
  Assert.AreEqual('Wednesday', TWeekDays.Wednesday.ToString);
  Assert.AreEqual('Thursday', TWeekDays.Thursday.ToString);
  Assert.AreEqual('Friday', TWeekDays.Friday.ToString);
  Assert.AreEqual('Saturday', TWeekDays.Saturday.ToString);
  Assert.AreEqual('Sunday', TWeekDays.Sunday.ToString);
end;

procedure TTestLightCoreTypes.TestWeekDays_FullWeek;
var
  Day: TWeekDays;
  Count: Integer;
begin
  Day:= TWeekDays.Monday;
  Count:= 0;

  repeat
    Inc(Count);
    Day:= Day.NextDay;
  until (Day = TWeekDays.Monday) OR (Count > 10);

  Assert.AreEqual(7, Count, 'Should cycle through 7 days');
end;

{ Constant Tests }

procedure TTestLightCoreTypes.TestConstants_KB;
begin
  Assert.AreEqual(1024, KB);
end;

procedure TTestLightCoreTypes.TestConstants_MB;
begin
  Assert.AreEqual(1048576, MB);
  Assert.AreEqual(1024 * KB, MB);
end;

procedure TTestLightCoreTypes.TestConstants_GB;
begin
  Assert.AreEqual(Int64(1073741824), Int64(GB));
  Assert.AreEqual(Int64(1024) * MB, Int64(GB));
end;

procedure TTestLightCoreTypes.TestConstants_TB;
begin
  Assert.AreEqual(Int64(1099511627776), TB);
  Assert.AreEqual(Int64(1024) * GB, TB);
end;

procedure TTestLightCoreTypes.TestConstants_TimeUnits;
begin
  Assert.AreEqual(1000, Second);   { Milliseconds per second }
  Assert.AreEqual(60, Minute);     { Seconds per minute }
  Assert.AreEqual(3600, Hour);     { Seconds per hour }
  Assert.AreEqual(86400, Day);     { Seconds per day }
  Assert.AreEqual(60000, MSecsPerMin);
  Assert.AreEqual(1440, MinutesPerDay);
end;

{ Character Set Tests }

procedure TTestLightCoreTypes.TestCharSets_Numbers;
begin
  Assert.IsTrue(CharInSet('0', Numbers));
  Assert.IsTrue(CharInSet('5', Numbers));
  Assert.IsTrue(CharInSet('9', Numbers));
  Assert.IsFalse(CharInSet('a', Numbers));
  Assert.IsFalse(CharInSet('A', Numbers));
end;

procedure TTestLightCoreTypes.TestCharSets_Alphabet;
begin
  Assert.IsTrue(CharInSet('a', Alphabet));
  Assert.IsTrue(CharInSet('z', Alphabet));
  Assert.IsTrue(CharInSet('A', Alphabet));
  Assert.IsTrue(CharInSet('Z', Alphabet));
  Assert.IsFalse(CharInSet('0', Alphabet));
  Assert.IsFalse(CharInSet('!', Alphabet));
end;

procedure TTestLightCoreTypes.TestCharSets_HexNumbers;
begin
  Assert.IsTrue(CharInSet('0', HexNumbers));
  Assert.IsTrue(CharInSet('9', HexNumbers));
  Assert.IsTrue(CharInSet('a', HexNumbers));
  Assert.IsTrue(CharInSet('f', HexNumbers));
  Assert.IsTrue(CharInSet('A', HexNumbers));
  Assert.IsTrue(CharInSet('F', HexNumbers));
  Assert.IsFalse(CharInSet('g', HexNumbers));
  Assert.IsFalse(CharInSet('G', HexNumbers));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestLightCoreTypes);

end.
