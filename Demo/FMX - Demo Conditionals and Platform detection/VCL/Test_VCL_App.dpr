program Test_VCL_App;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form3},
  PlatformTest in '..\PlatformTest.pas',
  cbAppDataForm in '..\..\..\FrameVCL\cbAppDataForm.pas',
  cbAppDataVCL in '..\..\..\FrameVCL\cbAppDataVCL.pas',
  ccAppData in '..\..\..\ccAppData.pas',
  ccBinary in '..\..\..\ccBinary.pas',
  ccColors in '..\..\..\ccColors.pas',
  ccCompiler in '..\..\..\ccCompiler.pas',
  ccCore in '..\..\..\ccCore.pas',
  ccEncodeCRC in '..\..\..\ccEncodeCRC.pas',
  ccEncodeMime in '..\..\..\ccEncodeMime.pas',
  ccEncodeXOR in '..\..\..\ccEncodeXOR.pas',
  ccINIFile in '..\..\..\ccINIFile.pas',
  ccIO in '..\..\..\ccIO.pas',
  ccLogLinesAbstract in '..\..\..\ccLogLinesAbstract.pas',
  ccLogLinesM in '..\..\..\ccLogLinesM.pas',
  ccLogLinesS in '..\..\..\ccLogLinesS.pas',
  ccLogRam in '..\..\..\ccLogRam.pas',
  ccLogTypes in '..\..\..\ccLogTypes.pas',
  ccLogUtils in '..\..\..\ccLogUtils.pas',
  ccMath in '..\..\..\ccMath.pas',
  ccMRU in '..\..\..\ccMRU.pas',
  ccPascal in '..\..\..\ccPascal.pas',
  ccPlatformFile in '..\..\..\ccPlatformFile.pas',
  ccRttiSetToString in '..\..\..\ccRttiSetToString.pas',
  ccSearchResult in '..\..\..\ccSearchResult.pas',
  ccStrBuilder in '..\..\..\ccStrBuilder.pas',
  ccStreamBuff in '..\..\..\ccStreamBuff.pas',
  ccStreamBuff2 in '..\..\..\ccStreamBuff2.pas',
  ccStreamFile in '..\..\..\ccStreamFile.pas',
  ccStreamMem in '..\..\..\ccStreamMem.pas',
  ccStreamMem2 in '..\..\..\ccStreamMem2.pas',
  ccStringList in '..\..\..\ccStringList.pas',
  ccStringListA in '..\..\..\ccStringListA.pas',
  ccTextFile in '..\..\..\ccTextFile.pas',
  ccWrapString in '..\..\..\ccWrapString.pas',
  cbCenterControl in '..\..\..\FrameVCL\cbCenterControl.pas',
  cbDialogs in '..\..\..\FrameVCL\cbDialogs.pas',
  cbIniFile in '..\..\..\FrameVCL\cbIniFile.pas',
  cbINIFileQuick in '..\..\..\FrameVCL\cbINIFileQuick.pas',
  cbRegistry in '..\..\..\FrameVCL\cbRegistry.pas',
  cbTranslate in '..\..\..\FrameVCL\cbTranslate.pas',
  cbVclUtils in '..\..\..\FrameVCL\cbVclUtils.pas',
  cbVersion in '..\..\..\FrameVCL\cbVersion.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  AppData.Run;
end.
