unit Test.LightVcl.Common.Clipboard;

{=============================================================================================================
   Unit tests for LightVcl.Common.Clipboard.pas
   Tests clipboard string operations with retry logic.

   Includes TestInsight support: define TESTINSIGHT in project options.

   Note: These tests interact with the system clipboard. The original clipboard content is saved
   and restored in Setup/TearDown to minimize impact on user's clipboard.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework, WinApi.Windows,
  System.SysUtils, System.Classes, Vcl.ClipBrd,
  LightVcl.Common.Clipboard;

type
  [TestFixture]
  TTestClipboard = class
  private
    FSavedClipboardText: string;
    FSavedHasText: Boolean;
    FMonitor  : TClipboardMonitor;
    FFireCount: Integer;
    procedure SaveClipboard;
    procedure RestoreClipboard;
    { Helper functions with retry logic for clipboard access }
    procedure SetClipboardText(const Text: string);
    function  GetClipboardText: string;
    procedure ClearClipboard;
    { TClipboardMonitor helpers }
    procedure MonitorChanged(Sender: TObject);
    procedure MonitorChangedThatWrites(Sender: TObject);
    procedure PumpMessages(TimeoutMs: Cardinal);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { StringToClipboard Tests }
    [Test]
    procedure TestStringToClipboard_BasicWrite;

    [Test]
    procedure TestStringToClipboard_EmptyString;

    [Test]
    procedure TestStringToClipboard_LongString;

    [Test]
    procedure TestStringToClipboard_UnicodeString;

    [Test]
    procedure TestStringToClipboard_SpecialChars;

    [Test]
    procedure TestStringToClipboard_ReturnsTrue;

    [Test]
    procedure TestStringToClipboard_MultilineText;

    { StringFromClipboard Tests }
    [Test]
    procedure TestStringFromClipboard_BasicRead;

    [Test]
    procedure TestStringFromClipboard_EmptyClipboard;

    [Test]
    procedure TestStringFromClipboard_UnicodeRead;

    [Test]
    procedure TestStringFromClipboard_ReturnsEmptyWhenNoText;

    [Test]
    procedure TestStringFromClipboard_PreservesLineEndings;

    { StringFromClipboardTSL Tests }
    [Test]
    procedure TestStringFromClipboardTSL_BasicRead;

    [Test]
    procedure TestStringFromClipboardTSL_ReturnsNilWhenEmpty;

    [Test]
    procedure TestStringFromClipboardTSL_MultipleLines;

    [Test]
    procedure TestStringFromClipboardTSL_SingleLine;

    [Test]
    procedure TestStringFromClipboardTSL_CallerMustFree;

    { Round-trip Tests }
    [Test]
    procedure TestRoundTrip_SimpleString;

    [Test]
    procedure TestRoundTrip_Unicode;

    [Test]
    procedure TestRoundTrip_Multiline;

    { Parameter Validation Tests }
    [Test]
    procedure TestStringToClipboard_ZeroRetries;

    [Test]
    procedure TestStringFromClipboard_ZeroTimeout;

    { TClipboardMonitor Tests }
    [Test]
    procedure TestMonitor_RegistersOnOwnWindow;

    [Test]
    procedure TestMonitor_FiresOnClipboardChange;

    [Test]
    procedure TestMonitor_DebounceCollapsesBurst;

    [Test]
    procedure TestMonitor_InactiveDoesNotFire;

    [Test]
    procedure TestMonitor_HandlerWriteEchoesOnce;

    [Test]
    procedure TestMonitor_SecondInstanceWorks;
  end;

implementation


{ Helper functions with retry logic to avoid 'Access is denied' errors }

procedure TTestClipboard.SetClipboardText(const Text: string);
var
  Retries: Integer;
begin
  for Retries:= 1 to 10 do
    TRY
      Clipboard.AsText:= Text;
      EXIT;
    except
      on E: Exception do
        if Retries = 10
        then raise
        else Sleep(50);
    END;
end;


function TTestClipboard.GetClipboardText: string;
var
  Retries: Integer;
begin
  Result:= '';
  for Retries:= 1 to 10 do
    TRY
      Result:= Clipboard.AsText;
      EXIT;
    except
      on E: Exception do
        if Retries = 10
        then raise
        else Sleep(50);
    END;
end;


procedure TTestClipboard.ClearClipboard;
var
  Retries: Integer;
begin
  for Retries:= 1 to 10 do
    TRY
      Clipboard.Clear;
      EXIT;
    except
      on E: Exception do
        if Retries = 10
        then raise
        else Sleep(50);
    END;
end;


procedure TTestClipboard.SaveClipboard;
begin
  { Save current clipboard state so we can restore it after tests }
  FSavedHasText:= Clipboard.HasFormat(CF_TEXT) OR Clipboard.HasFormat(CF_UNICODETEXT);
  if FSavedHasText
  then
    TRY
      FSavedClipboardText:= GetClipboardText;
    except
      { If we can't read clipboard, just note that we had text }
      FSavedClipboardText:= '';
    END
  else
    FSavedClipboardText:= '';
end;


procedure TTestClipboard.RestoreClipboard;
begin
  { Restore clipboard to original state }
  TRY
    if FSavedHasText
    then SetClipboardText(FSavedClipboardText)
    else ClearClipboard;
  except
    { Best effort - ignore failures during restore }
  END;
end;


procedure TTestClipboard.Setup;
begin
  SaveClipboard;
end;


procedure TTestClipboard.TearDown;
begin
  RestoreClipboard;
end;


{ StringToClipboard Tests }

procedure TTestClipboard.TestStringToClipboard_BasicWrite;
var
  Success: Boolean;
begin
  Success:= StringToClipboard('Test String');

  Assert.IsTrue(Success, 'StringToClipboard should return True on success');
  Assert.AreEqual('Test String', GetClipboardText, 'Clipboard should contain written text');
end;


procedure TTestClipboard.TestStringToClipboard_EmptyString;
var
  Success: Boolean;
begin
  Success:= StringToClipboard('');

  Assert.IsTrue(Success, 'Should succeed with empty string');
  Assert.AreEqual('', GetClipboardText, 'Clipboard should be empty');
end;


procedure TTestClipboard.TestStringToClipboard_LongString;
var
  LongStr: string;
  Success: Boolean;
begin
  LongStr:= StringOfChar('X', 100000);  { 100K characters }
  Success:= StringToClipboard(LongStr);

  Assert.IsTrue(Success, 'Should handle long strings');
  Assert.AreEqual(Length(LongStr), Length(GetClipboardText), 'Clipboard should preserve length');
end;


procedure TTestClipboard.TestStringToClipboard_UnicodeString;
var
  Success: Boolean;
  UnicodeStr: string;
begin
  UnicodeStr:= 'Hello '#$4E16#$754C;  { "Hello World" with Chinese characters for "world" }
  Success:= StringToClipboard(UnicodeStr);

  Assert.IsTrue(Success, 'Should handle Unicode');
  Assert.AreEqual(UnicodeStr, GetClipboardText, 'Unicode should be preserved');
end;


procedure TTestClipboard.TestStringToClipboard_SpecialChars;
var
  Success: Boolean;
  SpecialStr: string;
begin
  SpecialStr:= 'Tab:'#9' CR:'#13' LF:'#10' Null:'#0;
  Success:= StringToClipboard(SpecialStr);

  Assert.IsTrue(Success, 'Should handle special characters');
end;


procedure TTestClipboard.TestStringToClipboard_ReturnsTrue;
var
  Result: Boolean;
begin
  Result:= StringToClipboard('Test');

  Assert.IsTrue(Result, 'Should return True when clipboard write succeeds');
end;


procedure TTestClipboard.TestStringToClipboard_MultilineText;
var
  Success: Boolean;
  MultilineStr: string;
begin
  MultilineStr:= 'Line 1' + sLineBreak + 'Line 2' + sLineBreak + 'Line 3';
  Success:= StringToClipboard(MultilineStr);

  Assert.IsTrue(Success, 'Should handle multiline text');
  Assert.AreEqual(MultilineStr, GetClipboardText, 'Multiline text should be preserved');
end;


{ StringFromClipboard Tests }

procedure TTestClipboard.TestStringFromClipboard_BasicRead;
var
  ReadText: string;
begin
  SetClipboardText('Test Read');

  ReadText:= StringFromClipboard;

  Assert.AreEqual('Test Read', ReadText, 'Should read clipboard text correctly');
end;


procedure TTestClipboard.TestStringFromClipboard_EmptyClipboard;
var
  ReadText: string;
begin
  ClearClipboard;

  ReadText:= StringFromClipboard;

  Assert.AreEqual('', ReadText, 'Should return empty string when clipboard is clear');
end;


procedure TTestClipboard.TestStringFromClipboard_UnicodeRead;
var
  ReadText, UnicodeStr: string;
begin
  UnicodeStr:= #$00C0#$00C1#$00C2#$00C3;  { Latin capital letters with diacritics }
  SetClipboardText(UnicodeStr);

  ReadText:= StringFromClipboard;

  Assert.AreEqual(UnicodeStr, ReadText, 'Should preserve Unicode characters');
end;


procedure TTestClipboard.TestStringFromClipboard_ReturnsEmptyWhenNoText;
var
  ReadText: string;
begin
  ClearClipboard;

  ReadText:= StringFromClipboard;

  Assert.AreEqual('', ReadText, 'Should return empty string when no text format available');
end;


procedure TTestClipboard.TestStringFromClipboard_PreservesLineEndings;
var
  ReadText, OriginalStr: string;
begin
  OriginalStr:= 'First' + sLineBreak + 'Second';
  SetClipboardText(OriginalStr);

  ReadText:= StringFromClipboard;

  Assert.AreEqual(OriginalStr, ReadText, 'Line endings should be preserved');
end;


{ StringFromClipboardTSL Tests }

procedure TTestClipboard.TestStringFromClipboardTSL_BasicRead;
var
  TSL: TStringList;
begin
  SetClipboardText('Line 1');

  TSL:= StringFromClipboardTSL;
  TRY
    Assert.IsNotNull(TSL, 'Should return TStringList when clipboard has text');
    Assert.AreEqual(1, TSL.Count, 'Should have one line');
    Assert.AreEqual('Line 1', TSL[0], 'Content should match');
  FINALLY
    FreeAndNil(TSL);
  END;
end;


procedure TTestClipboard.TestStringFromClipboardTSL_ReturnsNilWhenEmpty;
var
  TSL: TStringList;
begin
  ClearClipboard;

  TSL:= StringFromClipboardTSL;

  Assert.IsNull(TSL, 'Should return NIL when clipboard is empty');
end;


procedure TTestClipboard.TestStringFromClipboardTSL_MultipleLines;
var
  TSL: TStringList;
begin
  SetClipboardText('Alpha' + sLineBreak + 'Beta' + sLineBreak + 'Gamma');

  TSL:= StringFromClipboardTSL;
  TRY
    Assert.IsNotNull(TSL, 'Should return TStringList');
    Assert.AreEqual(3, TSL.Count, 'Should have three lines');
    Assert.AreEqual('Alpha', TSL[0], 'First line should match');
    Assert.AreEqual('Beta', TSL[1], 'Second line should match');
    Assert.AreEqual('Gamma', TSL[2], 'Third line should match');
  FINALLY
    FreeAndNil(TSL);
  END;
end;


procedure TTestClipboard.TestStringFromClipboardTSL_SingleLine;
var
  TSL: TStringList;
begin
  SetClipboardText('SingleLineNoBreak');

  TSL:= StringFromClipboardTSL;
  TRY
    Assert.IsNotNull(TSL, 'Should return TStringList');
    Assert.AreEqual(1, TSL.Count, 'Should have exactly one line');
  FINALLY
    FreeAndNil(TSL);
  END;
end;


procedure TTestClipboard.TestStringFromClipboardTSL_CallerMustFree;
var
  TSL: TStringList;
begin
  { This test verifies the documented behavior that caller must free result }
  SetClipboardText('Test');

  TSL:= StringFromClipboardTSL;
  Assert.IsNotNull(TSL, 'Should allocate TStringList');

  { Caller's responsibility to free - demonstrate proper usage }
  FreeAndNil(TSL);
  Assert.IsNull(TSL, 'Should be NIL after FreeAndNil');
end;


{ Round-trip Tests }

procedure TTestClipboard.TestRoundTrip_SimpleString;
var
  Original, Retrieved: string;
begin
  Original:= 'Round trip test';

  StringToClipboard(Original);
  Retrieved:= StringFromClipboard;

  Assert.AreEqual(Original, Retrieved, 'Round-trip should preserve content');
end;


procedure TTestClipboard.TestRoundTrip_Unicode;
var
  Original, Retrieved: string;
begin
  Original:= #$0041#$00C0#$0100#$0410;  { Latin A, Latin A-grave, Latin A-macron, Cyrillic A }

  StringToClipboard(Original);
  Retrieved:= StringFromClipboard;

  Assert.AreEqual(Original, Retrieved, 'Unicode round-trip should preserve all characters');
end;


procedure TTestClipboard.TestRoundTrip_Multiline;
var
  Original, Retrieved: string;
begin
  Original:= 'Line1' + sLineBreak + 'Line2' + sLineBreak + 'Line3';

  StringToClipboard(Original);
  Retrieved:= StringFromClipboard;

  Assert.AreEqual(Original, Retrieved, 'Multiline round-trip should preserve all lines');
end;


{ Parameter Validation Tests }

procedure TTestClipboard.TestStringToClipboard_ZeroRetries;
begin
  { With MaxRetries=0, the loop condition is RetryCount >= MaxRetries = 0 >= 0 = TRUE,
    so it should still try once (Inc happens before check) }
  StringToClipboard('Test', 0);

  { Function will attempt at least once before checking retry count }
  Assert.Pass('Function handles zero retries without exception');
end;


procedure TTestClipboard.TestStringFromClipboard_ZeroTimeout;
var
  ReadText: string;
begin
  SetClipboardText('Test');

  { With MaxWaitTime=0, should still attempt at least once }
  ReadText:= StringFromClipboard(0);

  { If clipboard access succeeds on first try, should return text }
  Assert.Pass('Function handles zero timeout without exception');
end;


{-------------------------------------------------------------------------------------------------------------
   TClipboardMonitor
-------------------------------------------------------------------------------------------------------------}

{ Drains this thread message queue for TimeoutMs milliseconds.
  Deliberately NOT Application.ProcessMessages: it is banned by CLAUDE.md, and a DUnitX test EXE is
  not running a VCL message loop anyway. WM_CLIPBOARDUPDATE is POSTED, so it only ever reaches
  TClipboardMonitor.WndProc if we dispatch it ourselves. }
procedure TTestClipboard.PumpMessages(TimeoutMs: Cardinal);
VAR
   StartTime: Cardinal;
   Msg: TMsg;
begin
  StartTime:= GetTickCount;
  REPEAT
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) DO
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    Sleep(5);
  UNTIL GetTickCount - StartTime > TimeoutMs;
end;


procedure TTestClipboard.MonitorChanged(Sender: TObject);
begin
  Inc(FFireCount);
end;


{ A handler that writes to the clipboard. Writes only ONCE, so the echo terminates - see
  TestMonitor_HandlerWriteEchoesOnce. The count cap is a safety net: if the echo ever became a real
  loop, this stops the test EXE from hanging instead of failing. }
procedure TTestClipboard.MonitorChangedThatWrites(Sender: TObject);
begin
  Inc(FFireCount);
  if FFireCount = 1
  then SetClipboardText('echo from inside the handler');
end;


procedure TTestClipboard.TestMonitor_RegistersOnOwnWindow;
begin
  FMonitor:= TClipboardMonitor.Create(0);
  TRY
    { This is the load-bearing fact behind the whole design: AllocateHWnd makes a hidden top-level
      window, and the OS accepts a clipboard format listener on it. If this ever fails, the class
      must go back to borrowing the caller form handle. }
    Assert.IsTrue(FMonitor.Registered, 'AddClipboardFormatListener refused an AllocateHWnd window');
    Assert.IsTrue(FMonitor.Active,     'A freshly created monitor must be Active');
  FINALLY
    FreeAndNil(FMonitor);
  END;
end;


procedure TTestClipboard.TestMonitor_DebounceCollapsesBurst;
begin
  FMonitor:= TClipboardMonitor.Create(150);
  TRY
    FMonitor.OnChange:= MonitorChanged;
    PumpMessages(60);
    FFireCount:= 0;

    { Three writes with NO pump in between, so all three notifications queue up. This is what
      Thunderbird does for one copy: plain text, then HTML, each firing its own message. }
    SetClipboardText('burst one');
    SetClipboardText('burst two');
    SetClipboardText('burst three');
    PumpMessages(500);

    Assert.AreEqual(1, FFireCount, 'Three clipboard writes inside the debounce window must collapse into ONE OnChange');
  FINALLY
    FreeAndNil(FMonitor);
  END;
end;


procedure TTestClipboard.TestMonitor_InactiveDoesNotFire;
begin
  FMonitor:= TClipboardMonitor.Create(0);
  TRY
    FMonitor.OnChange:= MonitorChanged;
    FMonitor.Active:= FALSE;
    Assert.IsFalse(FMonitor.Registered, 'Active:= FALSE must unregister the clipboard listener');

    PumpMessages(60);
    FFireCount:= 0;

    SetClipboardText('this must not be collected');
    PumpMessages(300);
    Assert.AreEqual(0, FFireCount, 'A deactivated monitor must not raise OnChange');

    { and back on again }
    FMonitor.Active:= TRUE;
    Assert.IsTrue(FMonitor.Registered, 'Active:= TRUE must re-register the listener');
    PumpMessages(60);
    FFireCount:= 0;

    SetClipboardText('this one must be collected');
    PumpMessages(300);
    Assert.AreEqual(1, FFireCount, 'A reactivated monitor must raise OnChange again');
  FINALLY
    FreeAndNil(FMonitor);
  END;
end;


{ Pins down real, documented behaviour rather than wishful behaviour. WM_CLIPBOARDUPDATE is POSTED,
  not sent, so a handler that writes to the clipboard does NOT re-enter synchronously - it queues a
  fresh notification that arrives after the handler returned. The FInNotify guard cannot catch that
  one. So the write is echoed back exactly once, and a handler that keeps writing would keep going:
  any handler that writes to the clipboard must be idempotent. }
procedure TTestClipboard.TestMonitor_HandlerWriteEchoesOnce;
begin
  FMonitor:= TClipboardMonitor.Create(0);
  TRY
    FMonitor.OnChange:= MonitorChangedThatWrites;
    PumpMessages(60);
    FFireCount:= 0;

    SetClipboardText('trigger');
    PumpMessages(400);

    Assert.AreEqual(2, FFireCount, 'Expected the original change plus exactly one echo of the handler own write');
  FINALLY
    FreeAndNil(FMonitor);
  END;
end;


{ AllocateHWnd registers one window class for the whole process. Creating a monitor, freeing it and
  creating another must work - otherwise no program could ever restart monitoring. }
procedure TTestClipboard.TestMonitor_SecondInstanceWorks;
VAR FirstOne: TClipboardMonitor;
begin
  FirstOne:= TClipboardMonitor.Create(0);
  TRY
    Assert.IsTrue(FirstOne.Registered, 'First monitor did not register');
  FINALLY
    FreeAndNil(FirstOne);
  END;

  FMonitor:= TClipboardMonitor.Create(0);
  TRY
    FMonitor.OnChange:= MonitorChanged;
    Assert.IsTrue(FMonitor.Registered, 'Second monitor did not register after the first was freed');

    PumpMessages(60);
    FFireCount:= 0;

    SetClipboardText('after recreate');
    PumpMessages(300);
    Assert.AreEqual(1, FFireCount, 'The second monitor must still receive clipboard notifications');
  FINALLY
    FreeAndNil(FMonitor);
  END;
end;


procedure TTestClipboard.TestMonitor_FiresOnClipboardChange;
begin
  FMonitor:= TClipboardMonitor.Create(0);                  { 0 = no debounce, so this test is deterministic }
  TRY
    FMonitor.OnChange:= MonitorChanged;
    Assert.IsTrue(FMonitor.Registered, 'AddClipboardFormatListener refused our own window - nothing else in this class can work');

    PumpMessages(60);                                      { drain whatever Setup left in the queue }
    FFireCount:= 0;

    SetClipboardText('LightSaber TClipboardMonitor test');
    PumpMessages(300);

    Assert.AreEqual(1, FFireCount, 'OnChange must fire exactly once for one clipboard write');
  FINALLY
    FreeAndNil(FMonitor);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestClipboard);

end.
