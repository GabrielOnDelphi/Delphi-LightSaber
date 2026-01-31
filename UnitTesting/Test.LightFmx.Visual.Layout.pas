unit Test.LightFmx.Visual.Layout;

{=============================================================================================================
   Unit tests for LightFmx.Visual.Layout.pas
   Tests the TLightLayout FMX component with VisibleAtRuntime property.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  FMX.Controls,
  FMX.Layouts,
  LightFmx.Visual.Layout;

type
  { Helper class to access protected Loaded method for testing }
  TLightLayoutTestAccess = class(TLightLayout)
  public
    procedure CallLoaded;
  end;

  [TestFixture]
  TTestLightLayout = class
  private
    FLayout: TLightLayoutTestAccess;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_DefaultVisibleAtRuntime;

    [Test]
    procedure TestCreate_InheritsFromTLayout;

    { Property Tests }
    [Test]
    procedure TestSetVisibleAtRuntime_True;

    [Test]
    procedure TestSetVisibleAtRuntime_False;

    [Test]
    procedure TestSetVisibleAtRuntime_SameValue_NoChange;

    { Loaded Behavior Tests - simulating runtime }
    [Test]
    procedure TestLoaded_VisibleAtRuntimeTrue_ComponentVisible;

    [Test]
    procedure TestLoaded_VisibleAtRuntimeFalse_ComponentNotVisible;

    { Component Registration }
    [Test]
    procedure TestRegister_NoException;
  end;

implementation


{ TLightLayoutTestAccess }

procedure TLightLayoutTestAccess.CallLoaded;
begin
  Loaded;
end;


{ TTestLightLayout }

procedure TTestLightLayout.Setup;
begin
  FLayout:= TLightLayoutTestAccess.Create(NIL);
end;


procedure TTestLightLayout.TearDown;
begin
  FreeAndNil(FLayout);
end;


{ Constructor Tests }

procedure TTestLightLayout.TestCreate_DefaultVisibleAtRuntime;
begin
  Assert.IsTrue(FLayout.VisibleAtRuntime, 'VisibleAtRuntime should default to True');
end;


procedure TTestLightLayout.TestCreate_InheritsFromTLayout;
begin
  Assert.IsTrue(FLayout is TLayout, 'TLightLayout should inherit from TLayout');
end;


{ Property Tests }

procedure TTestLightLayout.TestSetVisibleAtRuntime_True;
begin
  FLayout.VisibleAtRuntime:= False;  // First set to false
  FLayout.VisibleAtRuntime:= True;   // Then set to true
  Assert.IsTrue(FLayout.VisibleAtRuntime);
end;


procedure TTestLightLayout.TestSetVisibleAtRuntime_False;
begin
  FLayout.VisibleAtRuntime:= False;
  Assert.IsFalse(FLayout.VisibleAtRuntime);
end;


procedure TTestLightLayout.TestSetVisibleAtRuntime_SameValue_NoChange;
begin
  // Setting same value should not cause issues
  FLayout.VisibleAtRuntime:= True;
  FLayout.VisibleAtRuntime:= True;
  Assert.IsTrue(FLayout.VisibleAtRuntime);
end;


{ Loaded Behavior Tests }

procedure TTestLightLayout.TestLoaded_VisibleAtRuntimeTrue_ComponentVisible;
begin
  FLayout.VisibleAtRuntime:= True;
  // Simulate loading completion (at runtime, not design time)
  // Note: csDesigning is not in ComponentState when created without a form at design time
  FLayout.CallLoaded;
  Assert.IsTrue(FLayout.Visible, 'Component should be visible when VisibleAtRuntime is True');
end;


procedure TTestLightLayout.TestLoaded_VisibleAtRuntimeFalse_ComponentNotVisible;
begin
  FLayout.VisibleAtRuntime:= False;
  // Simulate loading completion (at runtime)
  FLayout.CallLoaded;
  Assert.IsFalse(FLayout.Visible, 'Component should be hidden when VisibleAtRuntime is False');
end;


{ Component Registration }

procedure TTestLightLayout.TestRegister_NoException;
begin
  Assert.WillNotRaise(
    procedure
    begin
      Register;
    end,
    Exception,
    'Register should not raise an exception');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightLayout);

end.
