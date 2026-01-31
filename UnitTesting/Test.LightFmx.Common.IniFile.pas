unit Test.LightFmx.Common.IniFile;

{=============================================================================================================
   Unit tests for LightFmx.Common.IniFile.pas
   Tests INI file reading/writing for FMX controls and form state persistence.
=============================================================================================================}

interface

uses
  DUnitX.TestFramework,
  System.SysUtils, System.IOUtils, System.UITypes,
  FMX.Forms, FMX.Controls, FMX.StdCtrls, FMX.Edit, FMX.SpinBox, FMX.NumberBox,
  FMX.Graphics, FMX.Dialogs, FMX.ActnList, FMX.Menus, FMX.ListBox, FMX.Colors,
  FMX.Types,
  LightFmx.Common.IniFile;

type
  [TestFixture]
  TTestFmxIniFileApp = class
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
    procedure Test_IsSupported_TSwitch;

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
    procedure Test_WriteComp_NoOwner_ShouldRaise;

    [Test]
    procedure Test_ReadComp_NoOwner_ShouldRaise;

    [Test]
    procedure Test_WriteReadComp_CheckBox;

    [Test]
    procedure Test_WriteReadComp_RadioButton;

    [Test]
    procedure Test_WriteReadComp_Edit;

    [Test]
    procedure Test_WriteReadComp_SpinBox;

    [Test]
    procedure Test_WriteReadComp_NumberBox;

    [Test]
    procedure Test_WriteReadComp_TrackBar;

    [Test]
    procedure Test_WriteReadComp_Switch;

    [Test]
    procedure Test_WriteReadComp_ComboBox;

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
  end;

implementation

uses
  LightCore.AppData;


procedure TTestFmxIniFileApp.Setup;
begin
  FTestIniPath:= TPath.Combine(TPath.GetTempPath, 'TestFmxIniFile_' + TGUID.NewGuid.ToString + '.ini');
end;


procedure TTestFmxIniFileApp.TearDown;
begin
  CleanupTestIni;
end;


procedure TTestFmxIniFileApp.CleanupTestIni;
begin
  if FileExists(FTestIniPath)
  then DeleteFile(FTestIniPath);
end;


{ Constructor tests }

procedure TTestFmxIniFileApp.Test_Create_ValidSectionName;
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


procedure TTestFmxIniFileApp.Test_Create_WithForcedName;
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

procedure TTestFmxIniFileApp.Test_SaveForm_NilForm_ShouldRaise;
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


procedure TTestFmxIniFileApp.Test_SaveForm_AutoStateNone_ShouldRaise;
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

procedure TTestFmxIniFileApp.Test_LoadForm_NilForm_ShouldRaise;
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


procedure TTestFmxIniFileApp.Test_LoadForm_AutoStateNone_ShouldRaise;
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


{ IsSupported tests }

procedure TTestFmxIniFileApp.Test_IsSupported_TCheckBox;
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


procedure TTestFmxIniFileApp.Test_IsSupported_TRadioButton;
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


procedure TTestFmxIniFileApp.Test_IsSupported_TEdit;
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


procedure TTestFmxIniFileApp.Test_IsSupported_TComboBox;
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


procedure TTestFmxIniFileApp.Test_IsSupported_TTrackBar;
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


procedure TTestFmxIniFileApp.Test_IsSupported_TSplitter;
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


procedure TTestFmxIniFileApp.Test_IsSupported_TSwitch;
VAR
  Ini: TIniFileApp;
  Switch: TSwitch;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Switch:= TSwitch.Create(Form);
    Switch.Parent:= Form;
    Assert.IsTrue(Ini.IsSupported(Switch));
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_IsSupported_TLabel_NotSupported;
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


procedure TTestFmxIniFileApp.Test_IsSupported_TButton_NotSupported;
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

procedure TTestFmxIniFileApp.Test_WriteComp_NoName_ShouldRaise;
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


procedure TTestFmxIniFileApp.Test_ReadComp_NoName_ShouldRaise;
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


procedure TTestFmxIniFileApp.Test_WriteComp_NoOwner_ShouldRaise;
VAR
  Ini: TIniFileApp;
  CheckBox: TCheckBox;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    CheckBox:= TCheckBox.Create(NIL);  { No owner }
    CheckBox.Name:= 'TestCheckBox';
    TRY
      Assert.WillRaise(
        procedure
        begin
          Ini.WriteComp(CheckBox);
        end,
        Exception);
    FINALLY
      FreeAndNil(CheckBox);
    END;
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_ReadComp_NoOwner_ShouldRaise;
VAR
  Ini: TIniFileApp;
  CheckBox: TCheckBox;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    CheckBox:= TCheckBox.Create(NIL);  { No owner }
    CheckBox.Name:= 'TestCheckBox';
    TRY
      Assert.WillRaise(
        procedure
        begin
          Ini.ReadComp(CheckBox);
        end,
        Exception);
    FINALLY
      FreeAndNil(CheckBox);
    END;
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_WriteReadComp_CheckBox;
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
    CheckBox.IsChecked:= TRUE;
    Ini.WriteComp(CheckBox);

    { Change state and read back }
    CheckBox.IsChecked:= FALSE;
    Ini.ReadComp(CheckBox);

    Assert.IsTrue(CheckBox.IsChecked, 'CheckBox should be checked after reading from INI');
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_WriteReadComp_RadioButton;
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

    { Write checked state }
    RadioButton.IsChecked:= TRUE;
    Ini.WriteComp(RadioButton);

    { Change state and read back }
    RadioButton.IsChecked:= FALSE;
    Ini.ReadComp(RadioButton);

    Assert.IsTrue(RadioButton.IsChecked, 'RadioButton should be checked after reading from INI');
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_WriteReadComp_Edit;
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


procedure TTestFmxIniFileApp.Test_WriteReadComp_SpinBox;
VAR
  Ini: TIniFileApp;
  SpinBox: TSpinBox;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    SpinBox:= TSpinBox.Create(Form);
    SpinBox.Name:= 'TestSpinBox';
    SpinBox.Parent:= Form;
    SpinBox.Max:= 100;

    { Write value }
    SpinBox.Value:= 42;
    Ini.WriteComp(SpinBox);

    { Change and read back }
    SpinBox.Value:= 0;
    Ini.ReadComp(SpinBox);

    Assert.AreEqual(42.0, SpinBox.Value, 0.001);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_WriteReadComp_NumberBox;
VAR
  Ini: TIniFileApp;
  NumberBox: TNumberBox;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    NumberBox:= TNumberBox.Create(Form);
    NumberBox.Name:= 'TestNumberBox';
    NumberBox.Parent:= Form;
    NumberBox.Max:= 1000;

    { Write value }
    NumberBox.Value:= 123.45;
    Ini.WriteComp(NumberBox);

    { Change and read back }
    NumberBox.Value:= 0;
    Ini.ReadComp(NumberBox);

    Assert.AreEqual(123.45, NumberBox.Value, 0.001);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_WriteReadComp_TrackBar;
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
    TrackBar.Value:= 75;
    Ini.WriteComp(TrackBar);

    { Change and read back }
    TrackBar.Value:= 0;
    Ini.ReadComp(TrackBar);

    Assert.AreEqual(75.0, TrackBar.Value, 0.001);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_WriteReadComp_Switch;
VAR
  Ini: TIniFileApp;
  Switch: TSwitch;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Switch:= TSwitch.Create(Form);
    Switch.Name:= 'TestSwitch';
    Switch.Parent:= Form;

    { Write checked state }
    Switch.IsChecked:= TRUE;
    Ini.WriteComp(Switch);

    { Change state and read back }
    Switch.IsChecked:= FALSE;
    Ini.ReadComp(Switch);

    Assert.IsTrue(Switch.IsChecked, 'Switch should be checked after reading from INI');
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_WriteReadComp_ComboBox;
VAR
  Ini: TIniFileApp;
  ComboBox: TComboBox;
  Form: TForm;
begin
  Form:= TForm.CreateNew(NIL);
  Form.Name:= 'TestForm';
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    ComboBox:= TComboBox.Create(Form);
    ComboBox.Name:= 'TestComboBox';
    ComboBox.Parent:= Form;
    ComboBox.Items.Add('Option 1');
    ComboBox.Items.Add('Option 2');
    ComboBox.Items.Add('Option 3');

    { Write selected index }
    ComboBox.ItemIndex:= 2;
    Ini.WriteComp(ComboBox);

    { Change and read back }
    ComboBox.ItemIndex:= 0;
    Ini.ReadComp(ComboBox);

    Assert.AreEqual(2, ComboBox.ItemIndex);
  FINALLY
    FreeAndNil(Form);
    FreeAndNil(Ini);
  END;
end;


{ Font tests }

procedure TTestFmxIniFileApp.Test_WriteFont_EmptyIdent_ShouldRaise;
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


procedure TTestFmxIniFileApp.Test_WriteFont_NilFont_ShouldRaise;
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


procedure TTestFmxIniFileApp.Test_ReadFont_EmptyIdent_ShouldRaise;
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


procedure TTestFmxIniFileApp.Test_ReadFont_NilFont_ShouldRaise;
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


procedure TTestFmxIniFileApp.Test_WriteReadFont;
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
    WriteFont.Family:= 'Courier New';
    WriteFont.Size:= 14;
    WriteFont.Style:= [TFontStyle.fsBold, TFontStyle.fsItalic];

    { Write font }
    Ini.Write('TestFont', WriteFont);

    { Read font back }
    Success:= Ini.Read('TestFont', ReadFont);

    Assert.IsTrue(Success, 'Read should return True');
    Assert.AreEqual('Courier New', ReadFont.Family);
    Assert.AreEqual(14.0, ReadFont.Size, 0.001);
    Assert.IsTrue(TFontStyle.fsBold in ReadFont.Style, 'Font should be bold');
    Assert.IsTrue(TFontStyle.fsItalic in ReadFont.Style, 'Font should be italic');
  FINALLY
    FreeAndNil(ReadFont);
    FreeAndNil(WriteFont);
    FreeAndNil(Ini);
  END;
end;


{ Color tests }

procedure TTestFmxIniFileApp.Test_WriteColor_EmptyIdent_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.WriteColor('', TAlphaColorRec.Red);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_ReadColor_EmptyIdent_ShouldRaise;
VAR
  Ini: TIniFileApp;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    Assert.WillRaise(
      procedure
      begin
        Ini.ReadColor('', TAlphaColorRec.Black);
      end,
      Exception);
  FINALLY
    FreeAndNil(Ini);
  END;
end;


procedure TTestFmxIniFileApp.Test_WriteReadColor;
VAR
  Ini: TIniFileApp;
  ReadColor: TColor;
begin
  Ini:= TIniFileApp.Create('TestSection', FTestIniPath);
  TRY
    { Write color }
    Ini.WriteColor('TestColor', TAlphaColorRec.Navy);

    { Read color back }
    ReadColor:= Ini.ReadColor('TestColor', TAlphaColorRec.Black);

    Assert.AreEqual(Integer(TAlphaColorRec.Navy), Integer(ReadColor));
  FINALLY
    FreeAndNil(Ini);
  END;
end;


initialization
  TDUnitX.RegisterTestFixture(TTestFmxIniFileApp);

end.
