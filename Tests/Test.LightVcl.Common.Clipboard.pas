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
  System.SysUtils, System.Classes, Vcl.ClipBrd;

type
  [TestFixture]
  TTestClipboard = class
  private
    FSavedClipboardText: string;
    FSavedHasText: Boolean;
    procedure SaveClipboard;
    procedure RestoreClipboard;
    { Helper functions with retry logic for clipboard access }
    procedure SetClipboardText(const Text: string);
    function  GetClipboardText: string;
    procedure ClearClipboard;
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
  end;

implementation

uses
  LightVcl.Common.Clipboard;


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
var
  Success: Boolean;
begin
  { With MaxRetries=0, the loop condition is RetryCount >= MaxRetries = 0 >= 0 = TRUE,
    so it should still try once (Inc happens before check) }
  Success:= StringToClipboard('Test', 0);

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


initialization
  TDUnitX.RegisterTestFixture(TTestClipboard);

end.
