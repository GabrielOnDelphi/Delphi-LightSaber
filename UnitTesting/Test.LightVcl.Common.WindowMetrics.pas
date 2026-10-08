unit Test.LightVcl.Common.WindowMetrics;

{=============================================================================================================
   Unit tests for LightVcl.Common.WindowMetrics.pas
   Tests window and scrollbar metric retrieval functions.

   Note: Some tests require a valid window handle. These use Application.MainForm.Handle
   or GetDesktopWindow when no form is available.

   SetScrollbarWidth is NOT tested as it modifies system-wide settings.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.UITypes,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Controls,
  LightVcl.Common.WindowMetrics;

type
  [TestFixture]
  TTestWindowMetrics = class
  private
    FTestForm: TForm;
    FScrollBar: TScrollBar;
    function GetTestHandle: HWnd;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Window Metrics Tests }
    [Test]
    procedure Test_GetCaptionHeight_WithDesktopHandle;

    [Test]
    procedure Test_GetCaptionHeight_WithZeroHandle;

    [Test]
    procedure Test_GetMainMenuHeight_WithDesktopHandle;

    [Test]
    procedure Test_GetMainMenuHeight_WithZeroHandle;

    [Test]
    procedure Test_GetFrameSize_WithDesktopHandle;

    [Test]
    procedure Test_GetWinBorderWidth_ReturnsPositive;

    [Test]
    procedure Test_GetWinBorderHeight_ReturnsPositive;

    [Test]
    procedure Test_GetWin3DBorderWidth_ReturnsPositive;

    [Test]
    procedure Test_GetWin3DBorderHeight_ReturnsPositive;

    { Scrollbar Metrics Tests }
    [Test]
    procedure Test_GetScrollBarWidth_WithDesktopHandle;

    [Test]
    procedure Test_GetScrollBarWidth_WithZeroHandle;

    [Test]
    procedure Test_GetNumScrollLines_ReturnsPositive;

    { Deprecated function still works }
    [Test]
    procedure Test_GetScrollbarSize_Deprecated_StillWorks;

    { Scrollbar Visibility Tests }
    [Test]
    procedure Test_HorizScrollBarVisible_MemoWithHorizBar;

    [Test]
    procedure Test_VertScrollBarVisible_MemoWithVertBar;

    { SetScrollbarWidth Validation Tests }
    [Test]
    procedure Test_SetScrollbarWidth_ZeroWidth_RaisesException;

    [Test]
    procedure Test_SetScrollbarWidth_NegativeWidth_RaisesException;

    { SetProportionalThumbV Tests }
    [Test]
    procedure Test_SetProportionalThumbV_NilScrollbar_RaisesException;

    [Test]
    procedure Test_SetProportionalThumbV_ValidScrollbar_NoException;

    [Test]
    procedure Test_SetProportionalThumbV_ZeroRange_NoException;

    { SetProportionalThumbH Tests }
    [Test]
    procedure Test_SetProportionalThumbH_NilScrollbar_RaisesException;

    [Test]
    procedure Test_SetProportionalThumbH_ValidScrollbar_NoException;

    [Test]
    procedure Test_SetProportionalThumbH_ZeroTrackWidth_NoException;
  end;

implementation


procedure TTestWindowMetrics.Setup;
begin
  FTestForm:= TForm.CreateNew(nil);
  FTestForm.Width:= 400;
  FTestForm.Height:= 300;

  FScrollBar:= TScrollBar.Create(FTestForm);
  FScrollBar.Parent:= FTestForm;
  FScrollBar.Min:= 0;
  FScrollBar.Max:= 100;
  FScrollBar.Position:= 50;
end;


procedure TTestWindowMetrics.TearDown;
begin
  FreeAndNil(FScrollBar);
  FreeAndNil(FTestForm);
end;


function TTestWindowMetrics.GetTestHandle: HWnd;
begin
  { Use desktop window as a reliable handle for testing }
  Result:= GetDesktopWindow;
end;


{ Window Metrics Tests }

procedure TTestWindowMetrics.Test_GetCaptionHeight_WithDesktopHandle;
VAR
  Height: Integer;
begin
  Height:= GetCaptionHeight(GetTestHandle);
  Assert.IsTrue(Height > 0, 'Caption height should be positive');
  Assert.IsTrue(Height < 200, 'Caption height should be reasonable (< 200 pixels)');
end;


procedure TTestWindowMetrics.Test_GetCaptionHeight_WithZeroHandle;
VAR
  Height: Integer;
begin
  { Handle 0 should return metrics for primary monitor }
  Height:= GetCaptionHeight(0);
  Assert.IsTrue(Height > 0, 'Caption height with handle 0 should be positive');
end;


procedure TTestWindowMetrics.Test_GetMainMenuHeight_WithDesktopHandle;
VAR
  Height: Integer;
begin
  Height:= GetMainMenuHeight(GetTestHandle);
  Assert.IsTrue(Height > 0, 'Menu height should be positive');
  Assert.IsTrue(Height < 100, 'Menu height should be reasonable (< 100 pixels)');
end;


procedure TTestWindowMetrics.Test_GetMainMenuHeight_WithZeroHandle;
VAR
  Height: Integer;
begin
  Height:= GetMainMenuHeight(0);
  Assert.IsTrue(Height > 0, 'Menu height with handle 0 should be positive');
end;


{ The five tests below pin WHICH system metric each routine returns. The expected value comes from
  the RTL's DPI-aware Vcl.Controls.GetSystemMetricsForWindow called with the documented SM_ index. }

procedure TTestWindowMetrics.Test_GetFrameSize_WithDesktopHandle;
begin
  Assert.AreEqual(Vcl.Controls.GetSystemMetricsForWindow(SM_CYSIZEFRAME, GetTestHandle), GetFrameSize(GetTestHandle),
    'GetFrameSize must return SM_CYSIZEFRAME');
end;


procedure TTestWindowMetrics.Test_GetWinBorderWidth_ReturnsPositive;
begin
  Assert.AreEqual(Vcl.Controls.GetSystemMetricsForWindow(SM_CXBORDER, GetTestHandle), GetWinBorderWidth(GetTestHandle),
    'GetWinBorderWidth must return SM_CXBORDER');
end;


procedure TTestWindowMetrics.Test_GetWinBorderHeight_ReturnsPositive;
begin
  Assert.AreEqual(Vcl.Controls.GetSystemMetricsForWindow(SM_CYBORDER, GetTestHandle), GetWinBorderHeight(GetTestHandle),
    'GetWinBorderHeight must return SM_CYBORDER');
end;


procedure TTestWindowMetrics.Test_GetWin3DBorderWidth_ReturnsPositive;
begin
  Assert.AreEqual(Vcl.Controls.GetSystemMetricsForWindow(SM_CXEDGE, GetTestHandle), GetWin3DBorderWidth(GetTestHandle),
    'GetWin3DBorderWidth must return SM_CXEDGE');
end;


procedure TTestWindowMetrics.Test_GetWin3DBorderHeight_ReturnsPositive;
begin
  Assert.AreEqual(Vcl.Controls.GetSystemMetricsForWindow(SM_CYEDGE, GetTestHandle), GetWin3DBorderHeight(GetTestHandle),
    'GetWin3DBorderHeight must return SM_CYEDGE');
end;


{ Scrollbar Metrics Tests }

procedure TTestWindowMetrics.Test_GetScrollBarWidth_WithDesktopHandle;
VAR
  Width: Integer;
begin
  Width:= GetScrollBarWidth(GetTestHandle);
  Assert.IsTrue(Width > 0, 'Scrollbar width should be positive');
  Assert.IsTrue(Width < 100, 'Scrollbar width should be reasonable (< 100 pixels)');
end;


procedure TTestWindowMetrics.Test_GetScrollBarWidth_WithZeroHandle;
VAR
  Width: Integer;
begin
  Width:= GetScrollBarWidth(0);
  Assert.IsTrue(Width > 0, 'Scrollbar width with handle 0 should be positive');
end;


procedure TTestWindowMetrics.Test_GetNumScrollLines_ReturnsPositive;
VAR
  Lines: Integer;
begin
  Lines:= GetNumScrollLines;
  { Standard Windows default is 3, but user can configure it }
  Assert.IsTrue(Lines >= 1, 'Scroll lines should be at least 1');
  Assert.IsTrue(Lines <= 100, 'Scroll lines should be reasonable (<= 100)');
end;


procedure TTestWindowMetrics.Test_GetScrollbarSize_Deprecated_StillWorks;
VAR
  Size: Integer;
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  Size:= GetScrollbarSize;
  {$WARN SYMBOL_DEPRECATED ON}
  Assert.IsTrue(Size > 0, 'Deprecated GetScrollbarSize should still return positive value');
end;


{ Scrollbar Visibility Tests }

{ A memo on the hidden test form; TMemo turns ScrollBars into the WS_HSCROLL / WS_VSCROLL window styles }
function CreateMemo(Form: TForm; ScrollBars: System.UITypes.TScrollStyle): TMemo;
begin
  Result:= TMemo.Create(Form);
  Result.Parent:= Form;
  Result.WordWrap:= FALSE;
  Result.ScrollBars:= ScrollBars;
  Result.HandleNeeded;
end;


procedure TTestWindowMetrics.Test_HorizScrollBarVisible_MemoWithHorizBar;
VAR
  Memo: TMemo;
begin
  Memo:= CreateMemo(FTestForm, ssHorizontal);
  Assert.IsTrue (HorizScrollBarVisible(Memo.Handle), 'A memo with ssHorizontal has a horizontal scroll bar');
  Assert.IsFalse(VertScrollBarVisible (Memo.Handle), 'A memo with ssHorizontal has no vertical scroll bar');
end;


procedure TTestWindowMetrics.Test_VertScrollBarVisible_MemoWithVertBar;
VAR
  Memo: TMemo;
begin
  Memo:= CreateMemo(FTestForm, ssVertical);
  Assert.IsTrue (VertScrollBarVisible (Memo.Handle), 'A memo with ssVertical has a vertical scroll bar');
  Assert.IsFalse(HorizScrollBarVisible(Memo.Handle), 'A memo with ssVertical has no horizontal scroll bar');
end;


{ SetScrollbarWidth Validation Tests }

procedure TTestWindowMetrics.Test_SetScrollbarWidth_ZeroWidth_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      SetScrollbarWidth(0);
    end,
    Exception,
    'SetScrollbarWidth should raise exception for width = 0'
  );
end;


procedure TTestWindowMetrics.Test_SetScrollbarWidth_NegativeWidth_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      SetScrollbarWidth(-10);
    end,
    Exception,
    'SetScrollbarWidth should raise exception for negative width'
  );
end;


{ SetProportionalThumbV Tests }

procedure TTestWindowMetrics.Test_SetProportionalThumbV_NilScrollbar_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      SetProportionalThumbV(nil, 300);
    end,
    Exception,
    'SetProportionalThumbV should raise exception for nil scrollbar'
  );
end;


procedure TTestWindowMetrics.Test_SetProportionalThumbV_ValidScrollbar_NoException;
begin
  Assert.WillNotRaiseAny(
    procedure
    begin
      SetProportionalThumbV(FScrollBar, 300);
    end,
    'SetProportionalThumbV should not raise exception for valid scrollbar'
  );
end;


procedure TTestWindowMetrics.Test_SetProportionalThumbV_ZeroRange_NoException;
begin
  { Set Min = Max to create zero range }
  FScrollBar.Min:= 50;
  FScrollBar.Max:= 50;

  Assert.WillNotRaiseAny(
    procedure
    begin
      SetProportionalThumbV(FScrollBar, 300);
    end,
    'SetProportionalThumbV should handle zero range without exception'
  );
end;


{ SetProportionalThumbH Tests }

procedure TTestWindowMetrics.Test_SetProportionalThumbH_NilScrollbar_RaisesException;
begin
  Assert.WillRaise(
    procedure
    begin
      SetProportionalThumbH(nil, 400);
    end,
    Exception,
    'SetProportionalThumbH should raise exception for nil scrollbar'
  );
end;


procedure TTestWindowMetrics.Test_SetProportionalThumbH_ValidScrollbar_NoException;
begin
  FScrollBar.Kind:= sbHorizontal;

  Assert.WillNotRaiseAny(
    procedure
    begin
      SetProportionalThumbH(FScrollBar, 400);
    end,
    'SetProportionalThumbH should not raise exception for valid scrollbar'
  );
end;


procedure TTestWindowMetrics.Test_SetProportionalThumbH_ZeroTrackWidth_NoException;
begin
  FScrollBar.Kind:= sbHorizontal;

  { Pass very small width that will result in zero/negative track width }
  Assert.WillNotRaiseAny(
    procedure
    begin
      SetProportionalThumbH(FScrollBar, 10);
    end,
    'SetProportionalThumbH should handle zero track width without exception'
  );
end;


initialization
  TDUnitX.RegisterTestFixture(TTestWindowMetrics);

end.
