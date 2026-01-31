unit Test.LightVcl.Common.CursorGuard;

{=============================================================================================================
   Unit tests for LightVcl.Common.CursorGuard.pas
   Tests the RAII-style cursor guard that automatically restores the cursor.

   Note: Tests require VCL initialization (Screen global must be available).
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Forms,
  LightVcl.Common.CursorGuard;

type
  [TestFixture]
  TTestCursorGuard = class
  private
    FSavedCursor: TCursor;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_CursorBusy_ReturnsInterface;

    [Test]
    procedure Test_CursorBusy_SetsCursorToHourglass;

    [Test]
    procedure Test_CursorBusy_RestoresCursorOnRelease;

    [Test]
    procedure Test_CursorBusy_RestoresOriginalCursor;

    [Test]
    procedure Test_CursorBusy_NestedCalls_RestoreCorrectly;

    [Test]
    procedure Test_CursorBusy_MultipleSequentialCalls;
  end;

implementation


procedure TTestCursorGuard.Setup;
begin
  { Save the current cursor so we can verify proper restoration }
  FSavedCursor:= Screen.Cursor;
  { Ensure we start with default cursor }
  Screen.Cursor:= crDefault;
end;


procedure TTestCursorGuard.TearDown;
begin
  { Restore cursor to whatever it was before tests }
  Screen.Cursor:= FSavedCursor;
end;


procedure TTestCursorGuard.Test_CursorBusy_ReturnsInterface;
VAR
  Guard: IUnknown;
begin
  Guard:= TCursorGuard.CursorBusy;
  Assert.IsNotNull(Guard, 'CursorBusy should return a non-nil interface');
end;


procedure TTestCursorGuard.Test_CursorBusy_SetsCursorToHourglass;
VAR
  Guard: IUnknown;
begin
  Guard:= TCursorGuard.CursorBusy;
  Assert.AreEqual(Ord(crHourglass), Ord(Screen.Cursor), 'Cursor should be crHourglass');
end;


procedure TTestCursorGuard.Test_CursorBusy_RestoresCursorOnRelease;
begin
  Screen.Cursor:= crDefault;

  { Create a scope where the guard exists }
  begin
    var Guard: IUnknown;
    Guard:= TCursorGuard.CursorBusy;
    Assert.AreEqual(Ord(crHourglass), Ord(Screen.Cursor), 'Cursor should be crHourglass while guard is active');
  end;
  { Guard is released here }

  Assert.AreEqual(Ord(crDefault), Ord(Screen.Cursor), 'Cursor should be restored to crDefault after guard released');
end;


procedure TTestCursorGuard.Test_CursorBusy_RestoresOriginalCursor;
begin
  { Set a custom cursor }
  Screen.Cursor:= crCross;

  { Create guard in nested scope }
  begin
    var Guard: IUnknown;
    Guard:= TCursorGuard.CursorBusy;
    Assert.AreEqual(Ord(crHourglass), Ord(Screen.Cursor), 'Cursor should be crHourglass');
  end;

  { Should restore to the original custom cursor, not crDefault }
  Assert.AreEqual(Ord(crCross), Ord(Screen.Cursor), 'Cursor should be restored to original crCross');
end;


procedure TTestCursorGuard.Test_CursorBusy_NestedCalls_RestoreCorrectly;
VAR
  OuterGuard: IUnknown;
begin
  Screen.Cursor:= crDefault;

  OuterGuard:= TCursorGuard.CursorBusy;
  Assert.AreEqual(Ord(crHourglass), Ord(Screen.Cursor), 'Outer: Cursor should be crHourglass');

  { Nested call }
  begin
    var InnerGuard: IUnknown;
    InnerGuard:= TCursorGuard.CursorBusy;
    Assert.AreEqual(Ord(crHourglass), Ord(Screen.Cursor), 'Inner: Cursor should still be crHourglass');
  end;
  { Inner guard released - should restore to crHourglass (what outer set it to) }

  Assert.AreEqual(Ord(crHourglass), Ord(Screen.Cursor), 'After inner release: Cursor should still be crHourglass');

  { Release outer guard }
  OuterGuard:= NIL;

  Assert.AreEqual(Ord(crDefault), Ord(Screen.Cursor), 'After outer release: Cursor should be crDefault');
end;


procedure TTestCursorGuard.Test_CursorBusy_MultipleSequentialCalls;
begin
  Screen.Cursor:= crDefault;

  { First call }
  begin
    var Guard1: IUnknown;
    Guard1:= TCursorGuard.CursorBusy;
    Assert.AreEqual(Ord(crHourglass), Ord(Screen.Cursor), 'First call: Cursor should be crHourglass');
  end;
  Assert.AreEqual(Ord(crDefault), Ord(Screen.Cursor), 'After first: Cursor should be crDefault');

  { Second call }
  begin
    var Guard2: IUnknown;
    Guard2:= TCursorGuard.CursorBusy;
    Assert.AreEqual(Ord(crHourglass), Ord(Screen.Cursor), 'Second call: Cursor should be crHourglass');
  end;
  Assert.AreEqual(Ord(crDefault), Ord(Screen.Cursor), 'After second: Cursor should be crDefault');

  { Third call }
  begin
    var Guard3: IUnknown;
    Guard3:= TCursorGuard.CursorBusy;
    Assert.AreEqual(Ord(crHourglass), Ord(Screen.Cursor), 'Third call: Cursor should be crHourglass');
  end;
  Assert.AreEqual(Ord(crDefault), Ord(Screen.Cursor), 'After third: Cursor should be crDefault');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCursorGuard);

end.
