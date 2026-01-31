unit Test.LightFmx.Common.CamUtils;

{=============================================================================================================
   2026.01.31
   Unit tests for LightFmx.Common.CamUtils.pas
   Tests cross-platform utilities for camera/gallery operations

   Note: Most functions in this unit are Android-specific and cannot be tested on Windows.
   Only cross-platform functions like GetPublicPicturesFolder are tested here.

   Run with: TestInsight or DUnitX console runner
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.IOUtils;

type
  [TestFixture]
  TTestCamUtils = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    { Cross-platform function tests }
    [Test]
    procedure TestGetPublicPicturesFolder_NotEmpty;

    [Test]
    procedure TestGetPublicPicturesFolder_EndsWithSeparator;

    [Test]
    procedure TestGetPublicPicturesFolder_IsValidPath;

    { Permission request tests - verify non-Android behavior }
    [Test]
    procedure TestRequestCameraPermission_NonAndroid_CallsCallback;

    [Test]
    procedure TestRequestStorageReadPermission_NonAndroid_CallsCallback;

    [Test]
    procedure TestRequestCameraPermission_NilCallback_NoException;

    [Test]
    procedure TestRequestStorageReadPermission_NilCallback_NoException;
  end;

implementation

uses
  LightFmx.Common.CamUtils;


{ TTestCamUtils }

procedure TTestCamUtils.Setup;
begin
  // No setup required
end;


procedure TTestCamUtils.TearDown;
begin
  // No teardown required
end;


procedure TTestCamUtils.TestGetPublicPicturesFolder_NotEmpty;
var
  Path: string;
begin
  Path:= GetPublicPicturesFolder;
  Assert.IsFalse(Path.IsEmpty, 'GetPublicPicturesFolder should not return empty string');
end;


procedure TTestCamUtils.TestGetPublicPicturesFolder_EndsWithSeparator;
var
  Path: string;
begin
  Path:= GetPublicPicturesFolder;
  Assert.IsTrue(Path.EndsWith(TPath.DirectorySeparatorChar),
    'GetPublicPicturesFolder should end with directory separator');
end;


procedure TTestCamUtils.TestGetPublicPicturesFolder_IsValidPath;
var
  Path: string;
begin
  Path:= GetPublicPicturesFolder;
  // Remove trailing separator for DirectoryExists check
  Path:= ExcludeTrailingPathDelimiter(Path);
  Assert.IsTrue(TDirectory.Exists(Path),
    'GetPublicPicturesFolder should return an existing directory path');
end;


procedure TTestCamUtils.TestRequestCameraPermission_NonAndroid_CallsCallback;
var
  CallbackCalled: Boolean;
begin
  CallbackCalled:= False;

  RequestCameraPermission(
    procedure
    begin
      CallbackCalled:= True;
    end);

  // On non-Android platforms, the callback should be called immediately
  Assert.IsTrue(CallbackCalled,
    'RequestCameraPermission should immediately call callback on non-Android platforms');
end;


procedure TTestCamUtils.TestRequestStorageReadPermission_NonAndroid_CallsCallback;
var
  CallbackCalled: Boolean;
begin
  CallbackCalled:= False;

  RequestStorageReadPermission(
    procedure
    begin
      CallbackCalled:= True;
    end);

  // On non-Android platforms, the callback should be called immediately
  Assert.IsTrue(CallbackCalled,
    'RequestStorageReadPermission should immediately call callback on non-Android platforms');
end;


procedure TTestCamUtils.TestRequestCameraPermission_NilCallback_NoException;
begin
  // Should not raise exception when callback is nil
  Assert.WillNotRaise(
    procedure
    begin
      RequestCameraPermission(nil);
    end,
    Exception,
    'RequestCameraPermission should handle nil callback gracefully');
end;


procedure TTestCamUtils.TestRequestStorageReadPermission_NilCallback_NoException;
begin
  // Should not raise exception when callback is nil
  Assert.WillNotRaise(
    procedure
    begin
      RequestStorageReadPermission(nil);
    end,
    Exception,
    'RequestStorageReadPermission should handle nil callback gracefully');
end;


initialization
  TDUnitX.RegisterTestFixture(TTestCamUtils);

end.
