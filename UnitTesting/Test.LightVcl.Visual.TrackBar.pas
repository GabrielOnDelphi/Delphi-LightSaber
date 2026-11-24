unit Test.LightVcl.Visual.TrackBar;

{=============================================================================================================
   2026.11.22
   Unit tests for LightVcl.Visual.TrackBar.pas (TLightTrackBar.OnChangeDone).

   No form is shown: the trackbar sits on a form that is created but never made visible.
   The drag tests send the notifications to the control with Perform, the way the VCL forwards them (CN_HSCROLL).
   The keyboard and wheel tests send real WM_KEYDOWN / WM_KEYUP / WM_MOUSEWHEEL messages to the trackbar's window:
   the Windows control then moves the thumb and notifies its parent, as it does for a user.
   The probe tests record the raw WM_HSCROLL codes the parent gets, and write them to the console.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils, System.Classes,
  Winapi.Windows, Winapi.Messages, Winapi.CommCtrl,
  Vcl.Forms, Vcl.Controls, Vcl.ComCtrls,
  LightVcl.Visual.TrackBar;

type
  [TestFixture]
  TTestLightTrackBar = class
  private
    FTestForm   : TForm;
    FTrack      : TLightTrackBar;
    FDoneCount  : Integer;
    FChangeCount: Integer;
    FCodes      : string;           { The WM_HSCROLL codes the form got from FTrack, separated by spaces }
    FOldFormProc: TWndMethod;
    procedure trackChangeDone(Sender: TObject);
    procedure trackChange(Sender: TObject);
    procedure formProbe(var Msg: TMessage);
    procedure scroll(Code, Pos: Integer);
    procedure dragTo(Pos: Integer);
    procedure keyPress(Key: Word);
    procedure wheelNotch(Delta: Integer);
    procedure startProbe;
    procedure stopProbe;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestDrag_EndTrack_FiresOnce;
    [Test]
    procedure TestTwoGestures_FireTwice;
    [Test]
    procedure TestEndTrack_NoMove_NoFire;
    [Test]
    procedure TestDragBackToStart_NoFire;
    [Test]
    procedure TestPositionInCode_NoFire;
    [Test]
    procedure TestVertical_EndTrack_FiresOnce;
    [Test]
    procedure TestKeyboard_RealKeys_FireOnce;
    [Test]
    procedure TestKeyboard_AtEndOfRange_NoFire;
    [Test]
    procedure TestWheel_RealNotch_FiresOnce;
    [Test]
    procedure TestProbe_KeyboardSendsEndTrack;
    [Test]
    procedure TestProbe_WheelScrollCodes;
  end;


implementation


procedure TTestLightTrackBar.Setup;
begin
  FDoneCount  := 0;
  FChangeCount:= 0;
  FCodes      := '';
  FTestForm:= TForm.CreateNew(NIL);   { Never shown }
  FTrack:= TLightTrackBar.Create(FTestForm);
  FTrack.Parent:= FTestForm;
  FTrack.Min:= 0;
  FTrack.Max:= 10;
  FTrack.Position:= 5;
  FTrack.OnChange:= trackChange;
  FTrack.OnChangeDone:= trackChangeDone;
  FTrack.HandleNeeded;   { The Windows control exists from here on: the messages below go to it }
end;


procedure TTestLightTrackBar.TearDown;
begin
  stopProbe;
  FreeAndNil(FTestForm);   { Owns FTrack }
  FTrack:= NIL;
end;


procedure TTestLightTrackBar.trackChangeDone(Sender: TObject);
begin
  Inc(FDoneCount);
end;


procedure TTestLightTrackBar.trackChange(Sender: TObject);
begin
  Inc(FChangeCount);
end;


procedure TTestLightTrackBar.formProbe(var Msg: TMessage);
begin
  if (Msg.Msg = WM_HSCROLL) AND (HWND(Msg.LParam) = FTrack.Handle)
  then FCodes:= FCodes + IntToStr(LoWord(Msg.WParam)) + ' ';
  FOldFormProc(Msg);
end;


procedure TTestLightTrackBar.startProbe;
begin
  FOldFormProc:= FTestForm.WindowProc;
  FTestForm.WindowProc:= formProbe;
end;


procedure TTestLightTrackBar.stopProbe;
begin
  if Assigned(FOldFormProc) AND (FTestForm <> NIL) then
   begin
    FTestForm.WindowProc:= FOldFormProc;
    FOldFormProc:= NIL;
   end;
end;


{ One notification, as the VCL forwards it from the parent: CN_HSCROLL, code in the low word, position in the high word }
procedure TTestLightTrackBar.scroll(Code, Pos: Integer);
begin
  FTrack.Perform(CN_HSCROLL, MakeWParam(Code, Pos), LPARAM(FTrack.Handle));
end;


{ What a mouse drag does: the Windows control moves the thumb (TBM_SETPOS sends no notification), then notifies TB_THUMBTRACK }
procedure TTestLightTrackBar.dragTo(Pos: Integer);
begin
  SendMessage(FTrack.Handle, TBM_SETPOS, 1, Pos);
  scroll(TB_THUMBTRACK, Pos);
end;


{ A real key press: key down, then key up (lParam bits 30 and 31 set, repeat count 1) }
procedure TTestLightTrackBar.keyPress(Key: Word);
begin
  SendMessage(FTrack.Handle, WM_KEYDOWN, Key, 1);
  SendMessage(FTrack.Handle, WM_KEYUP,   Key, LPARAM($C0000001));
end;


{ One real wheel notch. Delta > 0: the wheel rolled away from the user }
procedure TTestLightTrackBar.wheelNotch(Delta: Integer);
begin
  SendMessage(FTrack.Handle, WM_MOUSEWHEEL, MakeWParam(0, Word(SmallInt(Delta))), 0);
end;



{-------------------------------------------------------------------------------------------------------------
   Notifications sent with Perform
-------------------------------------------------------------------------------------------------------------}

procedure TTestLightTrackBar.TestDrag_EndTrack_FiresOnce;
begin
  dragTo(6);
  dragTo(7);
  dragTo(8);
  Assert.AreEqual(0, FDoneCount, 'Nothing may fire while the drag runs');
  Assert.AreEqual(3, FChangeCount, 'OnChange fires at every step of the drag');

  scroll(TB_THUMBPOSITION, 8);
  scroll(TB_ENDTRACK, 0);
  Assert.AreEqual(1, FDoneCount, 'The release of the button ends the drag');
  Assert.AreEqual(8, FTrack.Position);
end;


procedure TTestLightTrackBar.TestTwoGestures_FireTwice;
begin
  dragTo(7);
  scroll(TB_ENDTRACK, 0);
  dragTo(2);
  scroll(TB_ENDTRACK, 0);
  Assert.AreEqual(2, FDoneCount);
  Assert.AreEqual(2, FTrack.Position);
end;


procedure TTestLightTrackBar.TestEndTrack_NoMove_NoFire;
begin
  scroll(TB_ENDTRACK, 0);              { A release with no gesture before it }
  dragTo(5);                           { A click on the thumb: a gesture at the same value }
  scroll(TB_ENDTRACK, 0);
  Assert.AreEqual(0, FDoneCount);
  Assert.AreEqual(0, FChangeCount);
end;


procedure TTestLightTrackBar.TestDragBackToStart_NoFire;
begin
  dragTo(9);
  dragTo(5);
  scroll(TB_ENDTRACK, 0);
  Assert.AreEqual(0, FDoneCount, 'The drag ended at its start value');
  Assert.AreEqual(2, FChangeCount);
end;


procedure TTestLightTrackBar.TestPositionInCode_NoFire;
begin
  FTrack.Position:= 8;
  FTrack.Position:= 1;
  scroll(TB_ENDTRACK, 0);
  Assert.AreEqual(2, FChangeCount, 'OnChange fires for a Position set in code');
  Assert.AreEqual(0, FDoneCount, 'OnChangeDone does not');
end;


procedure TTestLightTrackBar.TestVertical_EndTrack_FiresOnce;
begin
  FTrack.Orientation:= trVertical;     { Recreates the window }
  FTrack.HandleNeeded;
  SendMessage(FTrack.Handle, TBM_SETPOS, 1, 3);
  FTrack.Perform(CN_VSCROLL, MakeWParam(TB_THUMBTRACK, 3), LPARAM(FTrack.Handle));
  FTrack.Perform(CN_VSCROLL, MakeWParam(TB_ENDTRACK, 0),   LPARAM(FTrack.Handle));
  Assert.AreEqual(1, FDoneCount);
  Assert.AreEqual(3, FTrack.Position);
end;



{-------------------------------------------------------------------------------------------------------------
   Real messages to the Windows control
-------------------------------------------------------------------------------------------------------------}

procedure TTestLightTrackBar.TestKeyboard_RealKeys_FireOnce;
begin
  SendMessage(FTrack.Handle, WM_KEYDOWN, VK_RIGHT, 1);
  SendMessage(FTrack.Handle, WM_KEYDOWN, VK_RIGHT, LPARAM($40000001));   { Auto-repeat: the key is held }
  Assert.AreEqual(7, FTrack.Position, 'Two steps of LineSize 1');
  Assert.AreEqual(0, FDoneCount, 'The key is still down');

  SendMessage(FTrack.Handle, WM_KEYUP, VK_RIGHT, LPARAM($C0000001));
  Assert.AreEqual(1, FDoneCount, 'The release of the key ends the gesture');
end;


procedure TTestLightTrackBar.TestKeyboard_AtEndOfRange_NoFire;
begin
  FTrack.Position:= 10;
  FChangeCount:= 0;
  keyPress(VK_RIGHT);
  Assert.AreEqual(10, FTrack.Position);
  Assert.AreEqual(0, FChangeCount);
  Assert.AreEqual(0, FDoneCount, 'The thumb did not move');
end;


procedure TTestLightTrackBar.TestWheel_RealNotch_FiresOnce;
VAR Before: Integer;
begin
  Before:= FTrack.Position;
  wheelNotch(-WHEEL_DELTA);
  Assert.AreNotEqual(Before, FTrack.Position, 'One wheel notch moves the thumb');
  Assert.AreEqual(1, FDoneCount, 'One notch, one OnChangeDone');

  Before:= FTrack.Position;
  wheelNotch(WHEEL_DELTA);
  Assert.AreNotEqual(Before, FTrack.Position);
  Assert.AreEqual(2, FDoneCount);
end;



{-------------------------------------------------------------------------------------------------------------
   Probes: the raw codes the parent gets. The keyboard probe checks the probe itself against the Microsoft page
-------------------------------------------------------------------------------------------------------------}

{ Microsoft: WM_KEYDOWN of VK_RIGHT sends TB_LINEDOWN (1), WM_KEYUP sends TB_ENDTRACK (8) }
procedure TTestLightTrackBar.TestProbe_KeyboardSendsEndTrack;
begin
  startProbe;
  keyPress(VK_RIGHT);
  stopProbe;
  if IsConsole
  then System.Writeln('  TrackBar probe, one key press (VK_RIGHT): WM_HSCROLL codes ', FCodes);
  Assert.AreEqual('1 8 ', FCodes);
end;


{ Records what one wheel notch sends, for the record: the Microsoft page does not say.
  TLightTrackBar fires once either way (TestWheel_RealNotch_FiresOnce) }
procedure TTestLightTrackBar.TestProbe_WheelScrollCodes;
VAR
  Buf: array[0..MAX_PATH] of Char;
  ComCtl: string;
begin
  SetString(ComCtl, Buf, GetModuleFileName(GetModuleHandle('comctl32.dll'), Buf, Length(Buf)));
  startProbe;
  wheelNotch(-WHEEL_DELTA);
  stopProbe;
  if IsConsole then
   begin
    System.Writeln('  TrackBar probe, one wheel notch: WM_HSCROLL codes ', FCodes);
    System.Writeln('  TrackBar probe, comctl32 loaded from: ', ComCtl);
   end;
  Assert.IsTrue(FCodes <> '', 'A notch that moves the thumb notifies the parent');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightTrackBar);

end.
