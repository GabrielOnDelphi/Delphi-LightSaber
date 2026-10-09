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
    class var RegisteredPage: string;
    class var RegisteredClasses: string;
    class procedure CaptureRegistration(const Page: string; const ComponentClasses: array of TComponentClass); static;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_DefaultVisibleAtRuntime;

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
    procedure TestRegister_RegistersComponents;
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


{ Property Tests }

procedure TTestLightLayout.TestSetVisibleAtRuntime_True;
begin
  FLayout.VisibleAtRuntime:= False;
  Assert.IsFalse(FLayout.VisibleAtRuntime, 'Precondition: VisibleAtRuntime was set to False');
  Assert.IsFalse(FLayout.Visible,          'Precondition: the setter hid the layout');

  FLayout.VisibleAtRuntime:= True;
  Assert.IsTrue(FLayout.VisibleAtRuntime, 'VisibleAtRuntime must go back to True');
  Assert.IsTrue(FLayout.Visible,          'At runtime the setter must show the layout again');
end;


procedure TTestLightLayout.TestSetVisibleAtRuntime_False;
begin
  FLayout.VisibleAtRuntime:= False;
  Assert.IsFalse(FLayout.VisibleAtRuntime);
end;


procedure TTestLightLayout.TestSetVisibleAtRuntime_SameValue_NoChange;
begin
  { Hide the layout directly: VisibleAtRuntime stays True. Writing the same True again must exit early and leave Visible alone. }
  FLayout.Visible:= False;
  Assert.IsTrue(FLayout.VisibleAtRuntime, 'Precondition: Visible does not change VisibleAtRuntime');

  FLayout.VisibleAtRuntime:= True;
  Assert.IsTrue (FLayout.VisibleAtRuntime, 'VisibleAtRuntime must stay True');
  Assert.IsFalse(FLayout.Visible, 'Writing the same value must not touch Visible');
end;


{ Loaded Behavior Tests }

procedure TTestLightLayout.TestLoaded_VisibleAtRuntimeTrue_ComponentVisible;
begin
  { Hide the layout directly, so VisibleAtRuntime stays True: only Loaded can make it visible again }
  FLayout.Visible:= False;
  Assert.IsTrue(FLayout.VisibleAtRuntime, 'Precondition: Visible does not change VisibleAtRuntime');
  // Simulate loading completion (at runtime, not design time)
  // Note: csDesigning is not in ComponentState when created without a form at design time
  FLayout.CallLoaded;
  Assert.IsTrue(FLayout.Visible, 'Loaded must apply VisibleAtRuntime = True to Visible');
end;


procedure TTestLightLayout.TestLoaded_VisibleAtRuntimeFalse_ComponentNotVisible;
begin
  { Show the layout directly after VisibleAtRuntime = False: only Loaded can hide it again }
  FLayout.VisibleAtRuntime:= False;
  FLayout.Visible:= True;
  Assert.IsFalse(FLayout.VisibleAtRuntime, 'Precondition: Visible does not change VisibleAtRuntime');
  Assert.IsTrue (FLayout.Visible, 'Precondition: the layout is visible before Loaded');
  // Simulate loading completion (at runtime)
  FLayout.CallLoaded;
  Assert.IsFalse(FLayout.Visible, 'Component should be hidden when VisibleAtRuntime is False');
end;


{ Component Registration }

{ Stands in for the IDE: outside the IDE RegisterComponentsProc is NIL, and System.Classes.RegisterComponents raises EComponentError. }
class procedure TTestLightLayout.CaptureRegistration(const Page: string; const ComponentClasses: array of TComponentClass);
var
  CompClass: TComponentClass;
begin
  RegisteredPage:= Page;
  for CompClass in ComponentClasses do
    begin
      if RegisteredClasses <> '' then RegisteredClasses:= RegisteredClasses + ',';
      RegisteredClasses:= RegisteredClasses + CompClass.ClassName;
    end;
end;


procedure TTestLightLayout.TestRegister_RegistersComponents;
var
  OldProc: procedure(const Page: string; const ComponentClasses: array of TComponentClass);
begin
  RegisteredPage:= '';
  RegisteredClasses:= '';
  OldProc:= RegisterComponentsProc;
  RegisterComponentsProc:= TTestLightLayout.CaptureRegistration;
  TRY
    Register;
  FINALLY
    RegisterComponentsProc:= OldProc;
  END;

  Assert.AreEqual('LightSaber FMX', RegisteredPage, 'Register must use the LightSaber FMX palette page');
  Assert.AreEqual('TLightLayout,TAutoHeightFlowLayout', RegisteredClasses, 'Register must register both layout classes');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightLayout);

end.
