unit Test.LightFmx.Visual.Panel;

{=============================================================================================================
   2026.01
   Unit tests for LightFmx.Visual.Panel.pas
   Tests TLightPanel VisibleAtRuntime functionality.

   Note: FMX components require an application context. These tests create minimal FMX forms for testing.

   Includes TestInsight support: define TESTINSIGHT in project options.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  FMX.Forms,
  FMX.Types,
  LightFmx.Visual.Panel;

type
  [TestFixture]
  TTestLightPanel = class
  private
    FPanel: TLightPanel;
    FForm: TForm;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Constructor Tests }
    [Test]
    procedure TestCreate_DefaultVisibleAtRuntime;

    [Test]
    procedure TestCreate_VisibleInDesigner;

    { VisibleAtRuntime Property Tests }
    [Test]
    procedure TestVisibleAtRuntime_SetTrue;

    [Test]
    procedure TestVisibleAtRuntime_SetFalse;

    [Test]
    procedure TestVisibleAtRuntime_NoImmediateVisibilityChange;

    { Loaded Behavior Tests }
    [Test]
    procedure TestLoaded_AppliesVisibleAtRuntimeTrue;

    [Test]
    procedure TestLoaded_AppliesVisibleAtRuntimeFalse;

    { Streaming Tests }
    [Test]
    procedure TestStreaming_DefaultValueNotWritten;

    [Test]
    procedure TestStreaming_NonDefaultValueWritten;
  end;

implementation

uses
  System.Rtti;


{ TTestLightPanel }

procedure TTestLightPanel.Setup;
begin
  FForm:= TForm.Create(nil);
  FForm.Width:= 400;
  FForm.Height:= 400;

  FPanel:= TLightPanel.Create(FForm);
  FPanel.Parent:= FForm;
  FPanel.Position.X:= 10;
  FPanel.Position.Y:= 10;
  FPanel.Width:= 200;
  FPanel.Height:= 200;
end;


procedure TTestLightPanel.TearDown;
begin
  FreeAndNil(FPanel);
  FreeAndNil(FForm);
end;


{ Constructor Tests }

procedure TTestLightPanel.TestCreate_DefaultVisibleAtRuntime;
begin
  // Default value should be True (panel visible at runtime by default)
  Assert.IsTrue(FPanel.VisibleAtRuntime, 'Default VisibleAtRuntime should be True');
end;


procedure TTestLightPanel.TestCreate_VisibleInDesigner;
var
  TestPanel: TLightPanel;
begin
  // Even with VisibleAtRuntime = False, the panel should be visible at design time
  // We can't fully test design-time behavior, but we verify the property default
  TestPanel:= TLightPanel.Create(nil);
  try
    Assert.IsTrue(TestPanel.Visible, 'Panel should be visible by default');
  finally
    FreeAndNil(TestPanel);
  end;
end;


{ VisibleAtRuntime Property Tests }

procedure TTestLightPanel.TestVisibleAtRuntime_SetTrue;
begin
  FPanel.VisibleAtRuntime:= FALSE;
  FPanel.VisibleAtRuntime:= TRUE;
  Assert.IsTrue(FPanel.VisibleAtRuntime);
end;


procedure TTestLightPanel.TestVisibleAtRuntime_SetFalse;
begin
  FPanel.VisibleAtRuntime:= FALSE;
  Assert.IsFalse(FPanel.VisibleAtRuntime);
end;


procedure TTestLightPanel.TestVisibleAtRuntime_NoImmediateVisibilityChange;
begin
  // Setting VisibleAtRuntime should NOT immediately change Visible
  // (Visible is only affected in Loaded at runtime)
  FPanel.Visible:= True;
  FPanel.VisibleAtRuntime:= False;

  // Visible should still be True (change only applies after Loaded)
  Assert.IsTrue(FPanel.Visible, 'Visible should not change immediately when VisibleAtRuntime is set');
end;


{ Loaded Behavior Tests }

procedure TTestLightPanel.TestLoaded_AppliesVisibleAtRuntimeTrue;
var
  TestPanel: TLightPanel;
begin
  // Simulate runtime loading with VisibleAtRuntime = True
  TestPanel:= TLightPanel.Create(FForm);
  try
    TestPanel.Parent:= FForm;
    TestPanel.VisibleAtRuntime:= True;

    // Call Loaded to simulate form loading at runtime
    // Note: In real runtime, csDesigning would not be set
    TestPanel.Loaded;

    Assert.IsTrue(TestPanel.Visible, 'Panel should be visible when VisibleAtRuntime is True');
  finally
    FreeAndNil(TestPanel);
  end;
end;


procedure TTestLightPanel.TestLoaded_AppliesVisibleAtRuntimeFalse;
var
  TestPanel: TLightPanel;
begin
  // Create panel without design mode flag
  TestPanel:= TLightPanel.Create(FForm);
  try
    TestPanel.Parent:= FForm;
    TestPanel.VisibleAtRuntime:= False;

    // At runtime (not design time), Loaded should set Visible to False
    // We're calling Loaded directly to simulate this
    TestPanel.Loaded;

    // Note: Since we're running tests, csDesigning is not set,
    // so Loaded should apply VisibleAtRuntime
    Assert.IsFalse(TestPanel.Visible, 'Panel should be hidden when VisibleAtRuntime is False at runtime');
  finally
    FreeAndNil(TestPanel);
  end;
end;


{ Streaming Tests }

procedure TTestLightPanel.TestStreaming_DefaultValueNotWritten;
var
  Stream: TMemoryStream;
  Writer: TWriter;
  StreamContent: string;
begin
  // When VisibleAtRuntime = True (default), it should NOT be written to stream
  FPanel.VisibleAtRuntime:= True;

  Stream:= TMemoryStream.Create;
  try
    Writer:= TWriter.Create(Stream, 4096);
    try
      Writer.WriteComponent(FPanel);
    finally
      FreeAndNil(Writer);
    end;

    // Convert stream to string to check contents
    SetLength(StreamContent, Stream.Size);
    Stream.Position:= 0;
    Stream.Read(StreamContent[1], Stream.Size);

    // The default value True should NOT appear in stream
    // (Delphi optimizes by not writing default values)
    Assert.IsFalse(Pos('VisibleAtRuntime', StreamContent) > 0,
      'Default VisibleAtRuntime value should not be written to stream');
  finally
    FreeAndNil(Stream);
  end;
end;


procedure TTestLightPanel.TestStreaming_NonDefaultValueWritten;
var
  Stream: TMemoryStream;
  Writer: TWriter;
  StreamContent: string;
begin
  // When VisibleAtRuntime = False (non-default), it SHOULD be written to stream
  FPanel.VisibleAtRuntime:= False;

  Stream:= TMemoryStream.Create;
  try
    Writer:= TWriter.Create(Stream, 4096);
    try
      Writer.WriteComponent(FPanel);
    finally
      FreeAndNil(Writer);
    end;

    // Convert stream to string to check contents
    SetLength(StreamContent, Stream.Size);
    Stream.Position:= 0;
    Stream.Read(StreamContent[1], Stream.Size);

    // The non-default value False SHOULD appear in stream
    Assert.IsTrue(Pos('VisibleAtRuntime', StreamContent) > 0,
      'Non-default VisibleAtRuntime value should be written to stream');
  finally
    FreeAndNil(Stream);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestLightPanel);

end.
