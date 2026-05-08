UNIT Test.ciUpdaterRec;

{=============================================================================================================
   Unit tests for ciUpdaterRec.pas (RNews record).

   This unit is pure (no VCL, no network), so it lives in Tests_LightCore. The full TUpdater object also
   has a CompareVersions function and timer-driven flow worth testing — those need VCL and belong in a VCL
   test project. As of 2026-05-07 the Tests_LightVcl.Common project does not build (pre-existing
   missing-unit error in LightVcl.Common.VclUtils — unrelated), so the TUpdater-side tests will be added
   once that project is repaired.

   What is exercised here:
     - RNews round-trip: SaveTo / LoadFrom against a real INI file in TempPath
     - RNews rejects files with the wrong signature or wrong format version (defends the OnlineNews
       fetch path against accidentally pointing at a 200-OK URL that returns the wrong content).
     - RNews.Clear puts AppVersion='?' so NewVersionFound can detect "uninitialised"
     - NewsBody pipe-encoding round-trip (the protocol uses '|' as a CRLF stand-in inside the INI)
     - All TTargetUser enum values round-trip correctly
=============================================================================================================}

INTERFACE

USES
  DUnitX.TestFramework,
  System.SysUtils, System.IOUtils,
  ciUpdaterRec;

TYPE
  [TestFixture]
  TTestRNews = class
  private
    FFile: string;
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure Clear_PutsAppVersionToQuestionMark;
    [Test] procedure Clear_ResetsAllFields;
    [Test] procedure SaveLoad_RoundTripPreservesAllFields;
    [Test] procedure SaveLoad_NewsBodyMultilineRoundTrip;
    [Test] procedure LoadFrom_NonexistentFile_ReturnsFalse;
    [Test] procedure LoadFrom_WrongSignature_ReturnsFalse;
    [Test] procedure LoadFrom_WrongFormatVersion_ReturnsFalse;
    [Test] procedure LoadFrom_HtmlPage_ReturnsFalse;
    [Test] procedure LoadFrom_EmptyFile_ReturnsFalse;
    [Test] procedure SaveLoad_TargetUserEnumRoundTrip;
  end;


IMPLEMENTATION


procedure TTestRNews.Setup;
begin
  FFile:= TPath.Combine(TPath.GetTempPath, 'TestRNews_'+ TGUID.NewGuid.ToString+ '.ini');
end;


procedure TTestRNews.TearDown;
begin
  if FileExists(FFile)
  then DeleteFile(FFile);
end;


procedure TTestRNews.Clear_PutsAppVersionToQuestionMark;
VAR News: RNews;
begin
  News.AppVersion:= '9.55.0.0';
  News.Clear;
  Assert.AreEqual('?', News.AppVersion, 'Clear must set AppVersion=''?'' so NewVersionFound can detect uninitialised state');
end;


procedure TTestRNews.Clear_ResetsAllFields;
VAR News: RNews;
begin
  News.Comment     := 'leftover';
  News.NewsHeadline:= 'leftover';
  News.NewsBody    := 'leftover';
  News.NewsID      := 99;
  News.TargetUser  := tuRegistered;
  News.CriticalUpd := TRUE;
  News.ShowCounter := 7;

  News.Clear;

  Assert.AreEqual('',          News.Comment);
  Assert.AreEqual('',          News.NewsHeadline);
  Assert.AreEqual('',          News.NewsBody);
  Assert.AreEqual(0,           News.NewsID);
  Assert.AreEqual(Ord(tuAll),  Ord(News.TargetUser));
  Assert.IsFalse  (News.CriticalUpd);
  Assert.AreEqual(1,           News.ShowCounter, 'Default ShowCounter is 1 (display once), not 0');
end;


procedure TTestRNews.SaveLoad_RoundTripPreservesAllFields;
VAR
  Out_, In_: RNews;
begin
  Out_.Clear;
  Out_.Comment     := 'BioniX style comment';
  Out_.AppVersion  := '15.35.0.0';
  Out_.NewsID      := 42;
  Out_.NewsHeadline:= 'Hello world';
  Out_.NewsBody    := 'Single line body';
  Out_.TargetUser  := tuTrial;
  Out_.CriticalUpd := TRUE;
  Out_.ShowCounter := 3;
  Out_.IsBetaVers  := TRUE;
  Out_.SaveTo(FFile);

  Assert.IsTrue(In_.LoadFrom(FFile), 'LoadFrom must succeed on a freshly-saved RNews');

  Assert.AreEqual(Out_.Comment,                 In_.Comment);
  Assert.AreEqual(Out_.AppVersion,              In_.AppVersion);
  Assert.AreEqual(Out_.NewsID,                  In_.NewsID);
  Assert.AreEqual(Out_.NewsHeadline,            In_.NewsHeadline);
  Assert.AreEqual(Out_.NewsBody,                In_.NewsBody);
  Assert.AreEqual(Ord(Out_.TargetUser),         Ord(In_.TargetUser));
  Assert.AreEqual(Out_.CriticalUpd,             In_.CriticalUpd);
  Assert.AreEqual(Out_.ShowCounter,             In_.ShowCounter);
  Assert.AreEqual(Out_.IsBetaVers,              In_.IsBetaVers);
end;


procedure TTestRNews.SaveLoad_NewsBodyMultilineRoundTrip;
VAR
  Out_, In_: RNews;
  Multi    : string;
begin
  { The protocol stores NewsBody with '|' as line separator inside the INI; LoadFrom must restore CRLFs. }
  Multi:= 'Line one'+ #13#10+ 'Line two'+ #13#10+ 'Line three';
  Out_.Clear;
  Out_.AppVersion:= '1.0.0.0';
  Out_.NewsID    := 1;
  Out_.NewsBody  := Multi;
  Out_.SaveTo(FFile);

  Assert.IsTrue(In_.LoadFrom(FFile));
  Assert.AreEqual(Multi, In_.NewsBody, 'CRLFs must survive the pipe-encoding round-trip');
end;


procedure TTestRNews.LoadFrom_NonexistentFile_ReturnsFalse;
VAR News: RNews;
begin
  Assert.IsFalse(News.LoadFrom(FFile+ '.does-not-exist'), 'Missing file must return FALSE, not raise');
end;


procedure TTestRNews.LoadFrom_WrongSignature_ReturnsFalse;
VAR News: RNews;
begin
  TFile.WriteAllText(FFile,
    '[Header]'+ #13#10+
    'Signature=NotLightUpdater'+ #13#10+
    'Version=4'+ #13#10+
    '[News]'+ #13#10+
    'AppVersion=1.0.0.0'+ #13#10+
    'NewsID=1'+ #13#10);
  Assert.IsFalse(News.LoadFrom(FFile), 'A file with the wrong signature must be rejected');
end;


procedure TTestRNews.LoadFrom_WrongFormatVersion_ReturnsFalse;
VAR News: RNews;
begin
  TFile.WriteAllText(FFile,
    '[Header]'+ #13#10+
    'Signature=LightUpdater'+ #13#10+
    'Version=99'+ #13#10+
    '[News]'+ #13#10+
    'AppVersion=1.0.0.0'+ #13#10+
    'NewsID=1'+ #13#10);
  Assert.IsFalse(News.LoadFrom(FFile), 'A file with a format version different from CurrentVersion must be rejected');
end;


procedure TTestRNews.LoadFrom_HtmlPage_ReturnsFalse;
VAR News: RNews;
begin
  { Real-world failure mode: a misconfigured server returns its 404 HTML page with HTTP 200.
    LoadFrom must not parse it as a valid news file. The HTML-detection happens at a higher layer
    (TUpdater.GetNews), but RNews.LoadFrom must at minimum return FALSE here so that layer can act. }
  TFile.WriteAllText(FFile,
    '<!DOCTYPE html><html><head><title>404</title></head>'+
    '<body><h1>Not Found</h1></body></html>');
  Assert.IsFalse(News.LoadFrom(FFile), 'An HTML 404 page must be rejected (no [Header] section, no signature)');
end;


procedure TTestRNews.LoadFrom_EmptyFile_ReturnsFalse;
VAR News: RNews;
begin
  TFile.WriteAllText(FFile, '');
  Assert.IsFalse(News.LoadFrom(FFile), 'An empty file must be rejected (signature absent)');
end;


procedure TTestRNews.SaveLoad_TargetUserEnumRoundTrip;
VAR
  Out_, In_: RNews;
  Each    : TTargetUser;
begin
  for Each:= Low(TTargetUser) to High(TTargetUser) DO
   begin
     Out_.Clear;
     Out_.AppVersion:= '1.0.0.0';
     Out_.NewsID    := 1;
     Out_.TargetUser:= Each;
     Out_.SaveTo(FFile);

     Assert.IsTrue(In_.LoadFrom(FFile));
     Assert.AreEqual(Ord(Each), Ord(In_.TargetUser), 'TargetUser enum round-trip failed for '+ IntToStr(Ord(Each)));
   end;
end;



INITIALIZATION
  TDUnitX.RegisterTestFixture(TTestRNews);

END.
