unit Test.LightFmx.Visual.AnimatedMemo;

{=============================================================================================================
   Unit tests for LightFmx.Visual.AnimatedMemo.pas
   Tests the TAnimatedMemo FMX component.

   Note: Timer-based animation behavior cannot be fully tested without an FMX message loop.
   These tests verify component state management, property setters, and public methods.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  FMX.Forms,
  FMX.Types,
  LightFmx.Visual.AnimatedMemo;

type
  [TestFixture]
  TTestAnimatedMemo = class
  private
    FMemo: TAnimatedMemo;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate;

    [Test]
    procedure TestCreate_DefaultInterval;

    [Test]
    procedure TestCreate_InitialState;

    { Interval Property Tests }
    [Test]
    procedure TestSetInterval_ValidValue;

    [Test]
    procedure TestSetInterval_ZeroIgnored;

    [Test]
    procedure TestSetInterval_NegativeIgnored;

    { AnimatedText Property Tests }
    [Test]
    procedure TestSetAnimatedText_EmptyIgnored;

    [Test]
    procedure TestSetAnimatedText_StartsAnimation;

    [Test]
    procedure TestSetAnimatedText_MultipleCallsQueue;

    { IsAnimating Tests }
    [Test]
    procedure TestIsAnimating_InitiallyFalse;

    [Test]
    procedure TestIsAnimating_TrueAfterTextSet;

    { ClearAnimation Tests }
    [Test]
    procedure TestClearAnimation_StopsAnimation;

    [Test]
    procedure TestClearAnimation_WhenNotAnimating;

    { Destructor Tests }
    [Test]
    procedure TestDestroy_WhileAnimating;
  end;

implementation


procedure TTestAnimatedMemo.Setup;
begin
  FMemo:= TAnimatedMemo.Create(nil);
end;


procedure TTestAnimatedMemo.TearDown;
begin
  FreeAndNil(FMemo);
end;


{ Constructor Tests }

procedure TTestAnimatedMemo.TestCreate;
begin
  Assert.IsNotNull(FMemo, 'Memo should be created');
  Assert.InheritsFrom(FMemo.ClassType, TAnimatedMemo, 'Should inherit from TAnimatedMemo');
end;


procedure TTestAnimatedMemo.TestCreate_DefaultInterval;
begin
  Assert.AreEqual(30, FMemo.AnimationInterval, 'Default interval should be 30ms');
end;


procedure TTestAnimatedMemo.TestCreate_InitialState;
begin
  Assert.IsFalse(FMemo.IsAnimating, 'Should not be animating initially');
  Assert.AreEqual('', FMemo.Text, 'Text should be empty initially');
end;


{ Interval Property Tests }

procedure TTestAnimatedMemo.TestSetInterval_ValidValue;
begin
  FMemo.AnimationInterval:= 100;
  Assert.AreEqual(100, FMemo.AnimationInterval, 'Interval should be updated to 100');

  FMemo.AnimationInterval:= 1;
  Assert.AreEqual(1, FMemo.AnimationInterval, 'Interval should accept minimum value of 1');
end;


procedure TTestAnimatedMemo.TestSetInterval_ZeroIgnored;
begin
  FMemo.AnimationInterval:= 50;
  FMemo.AnimationInterval:= 0;
  Assert.AreEqual(50, FMemo.AnimationInterval, 'Zero should be ignored, interval unchanged');
end;


procedure TTestAnimatedMemo.TestSetInterval_NegativeIgnored;
begin
  FMemo.AnimationInterval:= 50;
  FMemo.AnimationInterval:= -10;
  Assert.AreEqual(50, FMemo.AnimationInterval, 'Negative value should be ignored');
end;


{ AnimatedText Property Tests }

procedure TTestAnimatedMemo.TestSetAnimatedText_EmptyIgnored;
begin
  FMemo.AnimatedText:= '';
  Assert.IsFalse(FMemo.IsAnimating, 'Empty text should not start animation');
end;


procedure TTestAnimatedMemo.TestSetAnimatedText_StartsAnimation;
begin
  FMemo.AnimatedText:= 'Test';
  Assert.IsTrue(FMemo.IsAnimating, 'Setting text should start animation');
end;


procedure TTestAnimatedMemo.TestSetAnimatedText_MultipleCallsQueue;
begin
  // Setting multiple texts while first is animating should queue them
  FMemo.AnimatedText:= 'First';
  Assert.IsTrue(FMemo.IsAnimating, 'First text should start animation');

  FMemo.AnimatedText:= 'Second';
  Assert.IsTrue(FMemo.IsAnimating, 'Should still be animating after second text');

  // The texts are queued internally - animation continues
  FMemo.AnimatedText:= 'Third';
  Assert.IsTrue(FMemo.IsAnimating, 'Should still be animating after third text');
end;


{ IsAnimating Tests }

procedure TTestAnimatedMemo.TestIsAnimating_InitiallyFalse;
begin
  Assert.IsFalse(FMemo.IsAnimating, 'Should not be animating before any text is set');
end;


procedure TTestAnimatedMemo.TestIsAnimating_TrueAfterTextSet;
begin
  FMemo.AnimatedText:= 'Hello';
  Assert.IsTrue(FMemo.IsAnimating, 'Should be animating after text is set');
end;


{ ClearAnimation Tests }

procedure TTestAnimatedMemo.TestClearAnimation_StopsAnimation;
begin
  FMemo.AnimatedText:= 'Test text for animation';
  Assert.IsTrue(FMemo.IsAnimating, 'Should be animating after setting text');

  FMemo.ClearAnimation;
  Assert.IsFalse(FMemo.IsAnimating, 'ClearAnimation should stop animation');
end;


procedure TTestAnimatedMemo.TestClearAnimation_WhenNotAnimating;
begin
  // ClearAnimation should be safe to call even when not animating
  Assert.IsFalse(FMemo.IsAnimating, 'Initially not animating');
  FMemo.ClearAnimation;  // Should not raise exception
  Assert.IsFalse(FMemo.IsAnimating, 'Still not animating after ClearAnimation');
end;


{ Destructor Tests }

procedure TTestAnimatedMemo.TestDestroy_WhileAnimating;
var
  LocalMemo: TAnimatedMemo;
begin
  LocalMemo:= TAnimatedMemo.Create(nil);
  try
    LocalMemo.AnimatedText:= 'This text will be interrupted';
    Assert.IsTrue(LocalMemo.IsAnimating, 'Should be animating');
  finally
    FreeAndNil(LocalMemo);  // Should not raise exception
  end;

  Assert.IsNull(LocalMemo, 'Memo should be freed without exception');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestAnimatedMemo);

end.
