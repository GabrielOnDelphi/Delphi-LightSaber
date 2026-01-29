unit Test.LightVcl.Common.IniFile;

{=============================================================================================================
   Unit tests for LightVcl.Common.IniFile.pas
   Tests INI file reading/writing for VCL controls and form state persistence.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils, System.IOUtils, System.UITypes,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.Graphics, Vcl.Dialogs, Vcl.ActnList, Vcl.Menus,
  LightVcl.Common.IniFile;

type
  [TestFixture]
  TTestIniFileApp = class
  private
    FTestIniPath: string;
    procedure CleanupTestIni;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Constructor tests }
    [Test]
    procedure Test_Create_EmptySectionName_ShouldRaise;

    [Test]
    procedure Test_Create_ValidSectionName;

    [Test]
    procedure Test_Create_WithForcedName;

    { SaveForm tests }
    [Test]
    procedure Test_SaveForm_NilForm_ShouldRaise;

    [Test]
    procedure Test_SaveForm_AutoStateNone_ShouldRaise;

    { LoadForm tests }
    [Test]
    procedure Test_LoadForm_NilForm_ShouldRaise;

    [Test]
    procedure Test_LoadForm_AutoStateNone_ShouldRaise;

    { WriteGroup tests }
    [Test]
    procedure Test_WriteGroup_NilControl_ShouldRaise;

    { ReadGroup tests }
    [Test]
    procedure Test_ReadGroup_NilControl_ShouldRaise;

    { IsSupported tests }
    [Test]
    procedure Test_IsSupported_TCheckBox;

    [Test]
    procedure Test_IsSupported_TRadioButton;

    [Test]
    procedure Test_IsSupported_TEdit;

    [Test]
    procedure Test_IsSupported_TComboBox;

    [Test]
    procedure Test_IsSupported_TTrackBar;

    [Test]
    procedure Test_IsSupported_TSplitter;

    [Test]
    procedure Test_IsSupported_TLabel_NotSupported;

    [Test]
    procedure Test_IsSupported_TButton_NotSupported;

    { WriteComp/ReadComp tests }
    [Test]
    procedure Test_WriteComp_NoName_ShouldRaise;

    [Test]
    procedure Test_ReadComp_NoName_ShouldRaise;

    [Test]
    procedure Test_WriteReadComp_CheckBox;

    [Test]
    procedure Test_WriteReadComp_RadioButton;

    [Test]
    procedure Test_WriteReadComp_Edit;

    [Test]
    procedure Test_WriteReadComp_SpinEdit;

    [Test]
    procedure Test_WriteReadComp_TrackBar;

    [Test]
    procedure Test_WriteReadComp_RadioGroup;

    { Font tests }
    [Test]
    procedure Test_WriteFont_EmptyIdent_ShouldRaise;

    [Test]
    procedure Test_WriteFont_NilFont_ShouldRaise;

    [Test]
    procedure Test_ReadFont_EmptyIdent_ShouldRaise;

    [Test]
    procedure Test_ReadFont_NilFont_ShouldRaise;

    [Test]
    procedure Test_WriteReadFont;

    { Color tests }
    [Test]
    procedure Test_WriteColor_EmptyIdent_ShouldRaise;

    [Test]
    procedure Test_ReadColor_EmptyIdent_ShouldRaise;

    [Test]
    procedure Test_WriteReadColor;

    { AsString tests }
    [Test]
    procedure Test_AsString_FileNotExists_ShouldRaise;
  end;

implementation

uses
  Vcl.Samples.Spin,
  LightCore.AppData;


procedure TTestIniFileApp.Setup;
begin
  FTestIniPath:= TPath.Combine(TPath.GetTempPath, 'TestIniFile_' + TGUID.NewGuid.ToString + '.ini');
end;


procedure TTestIniFileApp.TearDown;
begin
  CleanupTestIni;
end;


procedure TTestIniFileApp.CleanupTestIni;
begin
  if FileExists(FTestIniPath)
  then DeleteFile(FTestIniPath);
end;


{ Constructor tests }

procedure TTestIniFileApp.Test_Create_EmptySectionName_ShouldRaise;
begin
  Assert.WillRaise(
    procedure
    var
      Ini: TIniFileApp;
    begin
      Ini:= TIniFileApp.Create('');
    end,
    Exception);
end;


procedure TTestIniFileApp.Test_Create_ValidSectionName;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.IsNotNull(Ini);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_Create_WithForcedName;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Ini.WriteString('TestSection', 'TestKey', 'TestValue');
    Assert.IsTrue(FileExists(FTestIniPath), 'INI file should be created at forced path');
  FINALLY
    FreeAndNil(Ini);
  END;
end;


{ SaveForm tests }

procedure TTestIniFileApp.Test_SaveForm_NilForm_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.SaveForm(NIL);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_SaveForm_AutoStateNone_ShouldRaise;
VAR
  Ini: TIniFileApp;
  Form: TForm;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  Form:= TForm.CreateNew(NIL);
  TRY
    Form.Name:= 'TestForm';
    Assert.WillRaise(
      procedure
      begin
        Ini.SaveForm(Form, asNone);
      end,
      Exception);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


{ LoadForm tests }

procedure TTestIniFileApp.Test_LoadForm_NilForm_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.LoadForm(NIL);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_LoadForm_AutoStateNone_ShouldRaise;
VAR
  Ini: TIniFileApp;
  Form: TForm;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  Form:= TForm.CreateNew(NIL);
  TRY
    Form.Name:= 'TestForm';
    Assert.WillRaise(
      procedure
      begin
        Ini.LoadForm(Form, asNone);
      end,
      Exception);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


{ WriteGroup tests }

procedure TTestIniFileApp.Test_WriteGroup_NilControl_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.WriteGroup(NIL);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


{ ReadGroup tests }

procedure TTestIniFileApp.Test_ReadGroup_NilControl_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.ReadGroup(NIL);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


{ IsSupported tests }

procedure TTestIniFileApp.Test_IsSupported_TCheckBox;
VAR
  Ini: TIniFileApp;
  CheckBox: TCheckBox;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    CheckBox:= TCheckBox.Create(Form);
    CheckBox.Parent:= Form;
    Assert.IsTrue(Ini.IsSupported(CheckBox));
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_IsSupported_TRadioButton;
VAR
  Ini: TIniFileApp;
  RadioButton: TRadioButton;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    RadioButton:= TRadioButton.Create(Form);
    RadioButton.Parent:= Form;
    Assert.IsTrue(Ini.IsSupported(RadioButton));
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_IsSupported_TEdit;
VAR
  Ini: TIniFileApp;
  Edit: TEdit;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Edit:= TEdit.Create(Form);
    Edit.Parent:= Form;
    Assert.IsTrue(Ini.IsSupported(Edit));
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_IsSupported_TComboBox;
VAR
  Ini: TIniFileApp;
  ComboBox: TComboBox;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    ComboBox:= TComboBox.Create(Form);
    ComboBox.Parent:= Form;
    Assert.IsTrue(Ini.IsSupported(ComboBox));
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_IsSupported_TTrackBar;
VAR
  Ini: TIniFileApp;
  TrackBar: TTrackBar;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    TrackBar:= TTrackBar.Create(Form);
    TrackBar.Parent:= Form;
    Assert.IsTrue(Ini.IsSupported(TrackBar));
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_IsSupported_TSplitter;
VAR
  Ini: TIniFileApp;
  Splitter: TSplitter;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Splitter:= TSplitter.Create(Form);
    Splitter.Parent:= Form;
    Assert.IsTrue(Ini.IsSupported(Splitter));
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_IsSupported_TLabel_NotSupported;
VAR
  Ini: TIniFileApp;
  Lbl: TLabel;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Lbl:= TLabel.Create(Form);
    Lbl.Parent:= Form;
    Assert.IsFalse(Ini.IsSupported(Lbl), 'TLabel should not be supported');
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_IsSupported_TButton_NotSupported;
VAR
  Ini: TIniFileApp;
  Btn: TButton;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Btn:= TButton.Create(Form);
    Btn.Parent:= Form;
    Assert.IsFalse(Ini.IsSupported(Btn), 'TButton should not be supported');
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


{ WriteComp/ReadComp tests }

procedure TTestIniFileApp.Test_WriteComp_NoName_ShouldRaise;
VAR
  Ini: TIniFileApp;
  CheckBox: TCheckBox;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    CheckBox:= TCheckBox.Create(Form);
    CheckBox.Parent:= Form;
    { CheckBox.Name is empty }
    Assert.WillRaise(
      procedure
      begin
        Ini.WriteComp(CheckBox);
      end,
      Exception);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_ReadComp_NoName_ShouldRaise;
VAR
  Ini: TIniFileApp;
  CheckBox: TCheckBox;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    CheckBox:= TCheckBox.Create(Form);
    CheckBox.Parent:= Form;
    { CheckBox.Name is empty }
    Assert.WillRaise(
      procedure
      begin
        Ini.ReadComp(CheckBox);
      end,
      Exception);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_WriteReadComp_CheckBox;
VAR
  Ini: TIniFileApp;
  CheckBox: TCheckBox;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    CheckBox:= TCheckBox.Create(Form);
    CheckBox.Name:= 'TestCheckBox';
    CheckBox.Parent:= Form;

    { Write checked state }
    CheckBox.Checked:= TRUE;
    Ini.WriteComp(CheckBox);

    { Change state and read back }
    CheckBox.Checked:= FALSE;
    Ini.ReadComp(CheckBox);

    Assert.IsTrue(CheckBox.Checked, 'CheckBox should be checked after reading from INI');
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_WriteReadComp_RadioButton;
VAR
  Ini: TIniFileApp;
  RadioButton: TRadioButton;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    RadioButton:= TRadioButton.Create(Form);
    RadioButton.Name:= 'TestRadio';
    RadioButton.Parent:= Form;
    RadioButton.TabStop:= FALSE;

    { Write checked state }
    RadioButton.Checked:= TRUE;
    Ini.WriteComp(RadioButton);

    { Change state and read back }
    RadioButton.Checked:= FALSE;
    Ini.ReadComp(RadioButton);

    Assert.IsTrue(RadioButton.Checked, 'RadioButton should be checked after reading from INI');
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_WriteReadComp_Edit;
VAR
  Ini: TIniFileApp;
  Edit: TEdit;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Edit:= TEdit.Create(Form);
    Edit.Name:= 'TestEdit';
    Edit.Parent:= Form;

    { Write text }
    Edit.Text:= 'TestValue123';
    Ini.WriteComp(Edit);

    { Change and read back }
    Edit.Text:= '';
    Ini.ReadComp(Edit);

    Assert.AreEqual('TestValue123', Edit.Text);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_WriteReadComp_SpinEdit;
VAR
  Ini: TIniFileApp;
  SpinEdit: TSpinEdit;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    SpinEdit:= TSpinEdit.Create(Form);
    SpinEdit.Name:= 'TestSpinEdit';
    SpinEdit.Parent:= Form;

    { Write value }
    SpinEdit.Value:= 42;
    Ini.WriteComp(SpinEdit);

    { Change and read back }
    SpinEdit.Value:= 0;
    Ini.ReadComp(SpinEdit);

    Assert.AreEqual(42, SpinEdit.Value);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_WriteReadComp_TrackBar;
VAR
  Ini: TIniFileApp;
  TrackBar: TTrackBar;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    TrackBar:= TTrackBar.Create(Form);
    TrackBar.Name:= 'TestTrackBar';
    TrackBar.Parent:= Form;
    TrackBar.Max:= 100;

    { Write position }
    TrackBar.Position:= 75;
    Ini.WriteComp(TrackBar);

    { Change and read back }
    TrackBar.Position:= 0;
    Ini.ReadComp(TrackBar);

    Assert.AreEqual(75, TrackBar.Position);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_WriteReadComp_RadioGroup;
VAR
  Ini: TIniFileApp;
  RadioGroup: TRadioGroup;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    RadioGroup:= TRadioGroup.Create(Form);
    RadioGroup.Name:= 'TestRadioGroup';
    RadioGroup.Parent:= Form;
    RadioGroup.Items.Add('Option 1');
    RadioGroup.Items.Add('Option 2');
    RadioGroup.Items.Add('Option 3');

    { Write selected index }
    RadioGroup.ItemIndex:= 2;
    Ini.WriteComp(RadioGroup);

    { Change and read back }
    RadioGroup.ItemIndex:= 0;
    Ini.ReadComp(RadioGroup);

    Assert.AreEqual(2, RadioGroup.ItemIndex);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


{ Font tests }

procedure TTestIniFileApp.Test_WriteFont_EmptyIdent_ShouldRaise;
VAR
  Ini: TIniFileApp;
  Font: TFont;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  Font:= TFont.Create;
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.Write('', Font);
      end,
      Exception);
  FINALLY
    FreeAndNil(Font);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_WriteFont_NilFont_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.Write('TestFont', NIL);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_ReadFont_EmptyIdent_ShouldRaise;
VAR
  Ini: TIniFileApp;
  Font: TFont;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  Font:= TFont.Create;
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.Read('', Font);
      end,
      Exception);
  FINALLY
    FreeAndNil(Font);
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_ReadFont_NilFont_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.Read('TestFont', NIL);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_WriteReadFont;
VAR
  Ini: TIniFileApp;
  WriteFont, ReadFont: TFont;
  Success: Boolean;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  WriteFont:= TFont.Create;
  ReadFont:= TFont.Create;
  TRY
    { Set font properties }
    WriteFont.Name:= 'Courier New';
    WriteFont.Size:= 14;
    WriteFont.Color:= clRed;
    WriteFont.Style:= [fsBold, fsItalic];

    { Write font }
    Ini.Write('TestFont', WriteFont);

    { Read font back }
    Success:= Ini.Read('TestFont', ReadFont);

    Assert.IsTrue(Success, 'Read should return True');
    Assert.AreEqual('Courier New', ReadFont.Name);
    Assert.AreEqual(14, ReadFont.Size);
    Assert.AreEqual(Integer(clRed), Integer(ReadFont.Color));
    Assert.IsTrue(fsBold in ReadFont.Style, 'Font should be bold');
    Assert.IsTrue(fsItalic in ReadFont.Style, 'Font should be italic');
  FINALLY
    FreeAndNil(ReadFont);
    FreeAndNil(WriteFont);
    FreeAndNil(Ini);
  END;
end;


{ Color tests }

procedure TTestIniFileApp.Test_WriteColor_EmptyIdent_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.WriteColor('', clRed);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_ReadColor_EmptyIdent_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.ReadColor('', clBlack);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestIniFileApp.Test_WriteReadColor;
VAR
  Ini: TIniFileApp;
  ReadColor: TColor;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    { Write color }
    Ini.WriteColor('TestColor', clNavy);

    { Read color back }
    ReadColor:= Ini.ReadColor('TestColor', clBlack);

    Assert.AreEqual(Integer(clNavy), Integer(ReadColor));
  FINALLY
    FreeAndNil(Ini);
  END;
end;


{ AsString tests }

procedure TTestIniFileApp.Test_AsString_FileNotExists_ShouldRaise;
begin
  { Delete any existing ini file first }
  CleanupTestIni;

  { AsString uses the default AppData ini file, not our test file.
    We test this by verifying it raises when file doesn't exist.
    Note: This test may pass/fail depending on AppData state. }
  Assert.WillRaise(
    procedure
    begin
      { Force a non-existent path scenario }
      if NOT FileExists(TAppDataCore.IniFile)
      then TIniFileApp.AsString;
    end,
    Exception);
end;


initialization
  TDUnitX.RegisterTestFixture(TTestIniFileApp);

end.
