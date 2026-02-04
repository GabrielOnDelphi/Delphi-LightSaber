UNIT LightVcl.Common.IniFile;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
  Same as LightCore.INIFile but adds support for forms to save themselves to disk.
  Ini file name/file path is automatically calculated.
--------------------------------------------------------------------------------------------------------------

  Self saving forms:
     Using SaveForm/LoadForm, a form with lots of controls (like checkboxes/radiobuttons) can save its status
     to disk on shutdown and resume exactly from where it left on application startup.
     Example:
       - Call TLightForm.SaveForm. LoadForm is automatically called by AppData.

  Storing only individual controls:
     You can also save individual controls.
     Example:
        To store the status of a checkbox to a INI file use TIniFileApp.Write(MyCheckBox).

     Default value:
        The user doesn't have to provide a default value.
        If value does not exist in INI file, the value from GUI (MyCheckBox.Checked) will be used.

  Completeness:
     Most common VCL controls are supported.
     Check IsSupported() to see a list of supported controls.
     Support for more controls can be easily added with just an 'if/then'.

  OnClick execution:
     The events are executed when the component status is changed when it is
     read from the ini file ONLY IF its INI status is different than its DFM status!
     In other words, if you want to autoexecute some OnClick event, you
     will have to call the ControlOnClick(sender) after the program loaded.

     The TAction.OnExecute is NEVER executed.

  Dependencies on AppData:
     AppData global var is used to obtain the file name of the INI file.

     Therefore, before using TIniFileApp you must set the AppData.AppName var like this: AppData:= TAppData.Create('DelphiLightSaber').
     The TIniFileApp class will use AppName to automatically determine the INI file name/path which is %AppData%\AppName.Ini.
     Example: If the AppData.AppName is set to "DelphiLightSaber",
     the ini file will be "c:\Users\UserName\AppData\Roaming\DelphiLightSaber\DelphiLightSaber.ini"
     See LightCore.AppData.pas for details.
     The TIniFileApp class will also automatically save the LightCore.AppData.AppData.LastUsedFolder variable to the INI file.


  Important:
     SaveForm will fail to save the controls of a form if they have been re-parented (moved to another form). But no exception will rise.
     The new parent form will also fail to save them.
     To fix this, we need to move the controls back to their original parent (form) BEFORE we call SaveForm!

  Notices
    * DO NOT USE SELF as parameter in TIniFileVCL.Create if you call TIniFileVCL.Create in FormCreate because the Self is not yet ready! But TIniFileVCL.Create(FrmMain) works.
    * LoadForm is called after the components on the form are created but BEFORE their CreateWnd is called!

  UNICODE
     INI file does not support unicode chars. Unicode chars are replaced with '?'.
     Sorry. This is not a limitation in my code but in Delphi's RTL.


  ____________

  Demo code:
     procedure TCoreSv.saveBuildID;
     begin
       var INI := LightVcl.Common.IniFile.TIniFileApp.Create('SectionName');
       TRY
         INI.Write('LastID', 0);
       FINALLY
         FreeAndNil(INI);
       END;
     end;
  ____________

  Tester:
     c:\Myprojects\Project Testers\IniFile tester\Tester.dpr
     https://github.com/GabrielOnDelphi/Dephi-LightSaber-GUI_AutoSave

=============================================================================================================}

{ToDo: add support for TCheckListBox}
{ToDo: add support for TPageCtrl (active tab)}
{ToDo: add support for TMemo}
{ToDo 2: Prevent controls from firing the OnClick event (and others like this) when loading its state from INI.
       Do it like so: set control's event handler to NIL, load its value from INI file
       then set the event handler back to original value.
       How can I check a checkbox without triggering of OnClick event?
       MyCheckBox.Perform(BM_SETCHECK, 1, 0);
       http://www.scalabium.com/faq/dct0012.htm }

INTERFACE

USES
   System.Classes, System.IniFiles, System.SysUtils, System.UITypes,
   Vcl.Graphics, Vcl.Forms, Vcl.FileCtrl, Vcl.Menus, Vcl.ExtCtrls, Vcl.NumberBox, Vcl.ComCtrls, Vcl.WinXCtrls, Vcl.Samples.Spin, Vcl.ActnList, Vcl.Dialogs, Vcl.Controls, Vcl.StdCtrls,
   LightCore.INIFile, LightCore.AppData;

{$WARN GARBAGE OFF}              {Silence the: 'W1011 Text after final END' warning }

TYPE
 TIniFileApp = class(TIniFileEx)    //todo: rename this to  TIniFileCommon
  protected
    procedure readCtrlPos  (Ctrl: TControl);
    procedure writeCtrlPos (Ctrl: TControl);
    procedure writeSplitter(Comp: TComponent);
    procedure readSplitter (Comp: TComponent);
  public
    ShowPositionWarn: Boolean;

    constructor Create(SectionName: string; ForcedName: string = ''); reintroduce; overload;

    function IsSupported(WinCtrl: TComponent): Boolean; virtual;
    class function AsString: string;

    procedure SaveForm (Form: TForm; AutoState: TAutoState = asPosOnly);      { Save ALL supported controls on this form }
    procedure LoadForm (Form: TForm; AutoState: TAutoState = asPosOnly);

    { Font - this requires VCL framework so it cannot be moved to LightCore.INIFile }
    function  Read  (CONST Ident: string; Font: TFont): Boolean;  overload;
    procedure Write (CONST Ident: string; Font: TFont);           overload;

    { Color - this requires VCL framework so it cannot be moved to LightCore.INIFile }
    function  ReadColor  (CONST Ident: string; Default: TColor): TColor;
    procedure WriteColor (CONST Ident: string; Value: TColor);

    { Read/write controls directly }
    function  WriteComp (Comp: TComponent): Boolean; virtual;
    function  ReadComp  (Comp: TComponent): Boolean; virtual;

    procedure ReadGroup  (WinCtrl: TWinControl);
    procedure WriteGroup (WinCtrl: TWinControl);
  end;


IMPLEMENTATION

USES
   LightCore.IO, LightCore.TextFile, LightCore;


{-----------------------------------------------------------------------------------------------------------------------
   CONSTRUCTOR
-----------------------------------------------------------------------------------------------------------------------}
constructor TIniFileApp.Create(SectionName: string; ForcedName: string = '');
VAR
  Path: string;
begin
  if SectionName = ''
  then raise Exception.Create('TIniFileApp.Create: Empty SectionName');

  if ForcedName = ''
  then Path:= TAppDataCore.IniFile
  else Path:= ForcedName;

  inherited Create(SectionName, Path);
  ShowPositionWarn:= TRUE;
end;


{-----------------------------------------------------------------------------------------------------------------------
   SAVE/LOAD FORM

   Save ALL supported controls on this form.

   Note:
     Components[] just yields the components that are OWNED by the form.
     So, if we are iterating over it will miss any components that are added dynamically, and not owned by the form, or components that are owned by frames.
     Update 2021: Now frames are supported. All sub-components of a frame are stored to the INI file.
-----------------------------------------------------------------------------------------------------------------------}
procedure TIniFileApp.SaveForm(Form: TForm; AutoState: TAutoState = asPosOnly);

  procedure WriteComponentsOf(Component: TComponent);
  VAR
    i: Integer;
  begin
    for i:= 0 to Component.ComponentCount - 1 do
      if IsSupported(Component.Components[i]) then
        if Component.Components[i].InheritsFrom(TFrame) //Frames
        then WriteComponentsOf(Component.Components[i])
        else WriteComp(Component.Components[i]);
  end;

begin
  if Form = NIL
  then raise Exception.Create('TIniFileApp.SaveForm: Form is nil');

  if AutoState = asNone
  then raise Exception.Create('TIniFileApp.SaveForm: AutoState = asNone is invalid');

  WriteComp(Form);
  if AutoState = asFull
  then WriteComponentsOf(Form);
end;


procedure TIniFileApp.LoadForm(Form: TForm; AutoState: TAutoState = asPosOnly);

  procedure ReadComponentsOf(Component: TComponent);
  VAR i: Integer;
  begin
    for i:= 0 to Component.ComponentCount - 1 do
      if IsSupported(Component.Components[i]) then
        if Component.Components[i].InheritsFrom(TFrame)
        then ReadComponentsOf(Component.Components[i])
        else ReadComp(Component.Components[i]);
  end;

begin
  if Form = NIL
  then raise Exception.Create('TIniFileApp.LoadForm: Form parameter cannot be nil');

  if AutoState = asNone
  then raise Exception.Create('TIniFileApp.LoadForm: AutoState = asNone is invalid');   //todo: explain why

  ReadComp(Form);
  if AutoState = asFull
  then ReadComponentsOf(Form);
end;


{-----------------------------------------------------------------------------------------------------------------------
   WRITE CONTROL POSITION
-----------------------------------------------------------------------------------------------------------------------}
procedure TIniFileApp.WriteCtrlPos(Ctrl: TControl);
begin
  Assert(Ctrl <> NIL, 'TIniFileApp.WriteCtrlPos: Ctrl is nil');

  if Ctrl.InheritsFrom(TForm) then
    begin
      { Unmaximize form in order to save form position correctly }
      if TForm(Ctrl).WindowState = wsMaximized
      then TForm(Ctrl).WindowState:= wsNormal;

      { Save window state - MUST be above WindowState:= wsNormal }
      WriteInteger(Ctrl.Name, 'WindowState', Ord(TForm(Ctrl).WindowState));

      { Save form position }
      WriteInteger(Ctrl.Name, 'Top',    Ctrl.Top);
      WriteInteger(Ctrl.Name, 'Left',   Ctrl.Left);
      WriteInteger(Ctrl.Name, 'Width',  Ctrl.Width);
      WriteInteger(Ctrl.Name, 'Height', Ctrl.Height);
    end
  else
    begin
      { Save control position relative to owner }
      WriteInteger(Ctrl.Owner.Name, Ctrl.Name + '.Top',    Ctrl.Top);
      WriteInteger(Ctrl.Owner.Name, Ctrl.Name + '.Left',   Ctrl.Left);
      WriteInteger(Ctrl.Owner.Name, Ctrl.Name + '.Width',  Ctrl.Width);
      WriteInteger(Ctrl.Owner.Name, Ctrl.Name + '.Height', Ctrl.Height);
    end;
end;


{-----------------------------------------------------------------------------------------------------------------------
   READ CONTROL POSITION

   Note: For Vcl.Forms, LightVcl.Visual.AppDataForm, we cannot read/write the ClientWidth.
   We need to use Width instead.
-----------------------------------------------------------------------------------------------------------------------}
procedure TIniFileApp.ReadCtrlPos(Ctrl: TControl);
VAR
  IsNonResizable: Boolean;
begin
  Assert(Ctrl <> NIL, 'TIniFileApp.ReadCtrlPos: Ctrl is nil');

  if Ctrl.InheritsFrom(TForm) then
    begin
      { Check if form is non-resizable.
        Don't read the size of the form if it's not resizable - use design-time size. }
      IsNonResizable:=
           (TForm(Ctrl).BorderStyle = bsNone)
        OR (TForm(Ctrl).BorderStyle = bsDialog)
        OR (TForm(Ctrl).BorderStyle = bsToolWindow);

      { Read position - center on screen if no saved position }
      if ValueExists(Ctrl.Name, 'Top')
      then Ctrl.Top:= ReadInteger (Ctrl.Name, 'Top' , 0)     { For "normal" controls default values are ignored because of ValueExists, therefore the current window 'physical' width/height are used }
      else Ctrl.Top:= (Screen.Height - Ctrl.Height) DIV 2;   { For forms the top/left position is calculated so that the form is centered on screen, at first startup }

      if ValueExists(Ctrl.Name, 'Left')
      then Ctrl.Left:= ReadInteger(Ctrl.Name, 'Left', 0)
      else Ctrl.Left:= (Screen.Width - Ctrl.Width) DIV 2;

      { Read size only for resizable forms and only if value exists.
        Using 0 as default would corrupt the form size. }
      if (NOT IsNonResizable) AND ValueExists(Ctrl.Name, 'Width')
      then Ctrl.Width:= ReadInteger(Ctrl.Name, 'Width', Ctrl.Width);
      //todo: !!!!!!!!!!!!!!!! use Ctrl.width instead of 0 and get rid of ValueExists

      if (NOT IsNonResizable) 
	  //AND ValueExists(Ctrl.Name, 'Height')
      then Ctrl.Height:= ReadInteger(Ctrl.Name, 'Height', Ctrl.Height);

      { Validate form position setting }
      if ShowPositionWarn 
	  AND (TForm(Ctrl).Position <> poDesigned)
      then raise Exception.Create('Form.Position is not ''poDesigned'' for form ' + Ctrl.Name + '!');

      { Restore window state }
      if ValueExists(Ctrl.Name, 'WindowState')
      then TForm(Ctrl).WindowState:= TWindowState(ReadInteger(Ctrl.Name, 'WindowState', 0));
    end
  else
    begin
      { Read control position - use current values as defaults }
      Ctrl.Top   := ReadInteger(Ctrl.Owner.Name, Ctrl.Name + '.Top',    Ctrl.Top);
      Ctrl.Left  := ReadInteger(Ctrl.Owner.Name, Ctrl.Name + '.Left',   Ctrl.Left);
      Ctrl.Width := ReadInteger(Ctrl.Owner.Name, Ctrl.Name + '.Width',  Ctrl.Width);
      Ctrl.Height:= ReadInteger(Ctrl.Owner.Name, Ctrl.Name + '.Height', Ctrl.Height);
    end;
end;


{-----------------------------------------------------------------------------------------------------------------------
   WRITE COMPONENT

   Write a supported control to INI file.
   Returns True if the control type is supported and was written.
-----------------------------------------------------------------------------------------------------------------------}
function TIniFileApp.WriteComp(Comp: TComponent): Boolean;
VAR s: string;
begin
  Assert(AppDataCore <> NIL, 'TIniFileApp.WriteComp: AppDataCore is not initialized');

  if Comp.Name = '' then
    begin
      s:= 'TIniFileApp.WriteComp: The control has no name! Class: ' + Comp.ClassName;
      if Comp.InheritsFrom(TControl) 
	  AND (TControl(Comp).Parent <> NIL)
      then s:= s + '. Parent: ' + TControl(Comp).Parent.Name;
      raise Exception.Create(s);
    end;

  Result:= TRUE;

  { Form }
  if Comp.InheritsFrom(TForm) then
    begin
      WriteCtrlPos(TControl(Comp));
      if Comp = Application.MainForm then
        begin
         // Font. All other forms will use main form's font
          Write('MainFormFont', TForm(Comp).Font);
          WriteString(Comp.Name, 'LastUsedFolder', AppDataCore.LastUsedFolder);
        end;
    end

  { Actions }
  else if Comp.InheritsFrom(TAction)
  then WriteBool(Comp.Owner.Name, Comp.Name, TAction(Comp).Checked)

  { Checkboxes }
  else if Comp.InheritsFrom(TCheckBox)
  then WriteInteger(Comp.Owner.Name, Comp.Name, Ord(TCheckBox(Comp).State))

  { Radio buttons }
  else if Comp.InheritsFrom(TRadioButton)
  then WriteBool(Comp.Owner.Name, Comp.Name, TRadioButton(Comp).Checked)

  { Combo boxes }
  else if Comp.InheritsFrom(TCustomComboBox)
  then WriteInteger(Comp.Owner.Name, Comp.Name, TCustomComboBox(Comp).ItemIndex)

  { Number boxes }
  else if Comp.InheritsFrom(TNumberBox)
  then WriteFloat(Comp.Owner.Name, Comp.Name, TNumberBox(Comp).Value)

  { Labeled edits }
  else if Comp.InheritsFrom(TCustomLabeledEdit)
  then WriteString(Comp.Owner.Name, Comp.Name, TCustomLabeledEdit(Comp).Text)

  else if Comp.InheritsFrom(TLabeledEdit)
  then WriteString(Comp.Owner.Name, Comp.Name, TLabeledEdit(Comp).Text)

  { Edit boxes - Don't go down to TCustomEdit to avoid RichLog }
  else if Comp.InheritsFrom(TEdit)
  then WriteString(Comp.Owner.Name, Comp.Name, TEdit(Comp).Text)

  { Spin edits }
  else if Comp.InheritsFrom(TSpinEdit)
  then WriteInteger(Comp.Owner.Name, Comp.Name, TSpinEdit(Comp).Value)

  { Page controls }
  else if Comp.InheritsFrom(TPageControl)
  then WriteInteger(Comp.Owner.Name, Comp.Name, TPageControl(Comp).ActivePageIndex)

  { Hot keys }
  else if Comp.InheritsFrom(THotKey)
  then WriteInteger(Comp.Owner.Name, Comp.Name, THotKey(Comp).HotKey)

  { Radio groups }
  else if Comp.InheritsFrom(TRadioGroup)
  then WriteInteger(Comp.Owner.Name, Comp.Name, TRadioGroup(Comp).ItemIndex)

  { Track bars }
  else if Comp.InheritsFrom(TTrackBar)
  then WriteInteger(Comp.Owner.Name, Comp.Name, TTrackBar(Comp).Position)

  { Shapes (color) }
  else if Comp.InheritsFrom(TShape)
  then WriteColor(Comp.Name, TShape(Comp).Brush.Color)

  { Color dialogs }
  else if Comp.InheritsFrom(TColorDialog)
  then WriteColor(Comp.Name, TColorDialog(Comp).Color)

  { Splitters }
  else if Comp.InheritsFrom(TSplitter)
  then WriteSplitter(Comp)

  { File list boxes }
  else if Comp.InheritsFrom(TFileListBox)
  then WriteString(Comp.Owner.Name, Comp.Name, TFileListBox(Comp).FileName)

  { Scroll bars }
  else if Comp.InheritsFrom(TScrollBar)
  then WriteInteger(Comp.Owner.Name, Comp.Name, TScrollBar(Comp).Position)

  { Menu items }
  else if Comp.InheritsFrom(TMenuItem)
  then WriteBool(Comp.Owner.Name, Comp.Name, TMenuItem(Comp).Checked)

  { Toggle switches }
  else if Comp.InheritsFrom(TToggleSwitch)
  then WriteBool(Comp.Owner.Name, Comp.Name, TToggleSwitch(Comp).State = tssOn)

  { Open dialogs - handle unicode limitation }
  else if Comp.InheritsFrom(TOpenDialog) then
    begin
      s:= TOpenDialog(Comp).FileName;     { NOTE: INI file does not support unicode chars. The unicode chars are replaced by '?' so we better replace this with a ' ' or something that is not '?'. This way if the filename is fucked up, we at least could 'recover' the folder name - which will be used for InitialDir! }
      s:= ReplaceUnicodeChars(s, '_');
      if s <> ''
      then WriteString(Comp.Owner.Name, Comp.Name, s);
    end

  { Save dialogs - handle unicode limitation }
  else if Comp.InheritsFrom(TSaveDialog) then
    begin
      s:= TSaveDialog(Comp).FileName;     { NOTE: INI file does not support unicode chars. The unicode chars are replaced by '?' so we better replace this with a ' ' or something that is not '?'. This way if the filename is fucked up, we at least could 'recover' the folder name - which will be used for InitialDir! }
      s:= ReplaceUnicodeChars(s, '_');
      if s <> ''
      then WriteString(Comp.Owner.Name, Comp.Name, s);
    end

  { Font dialogs }
  else if Comp.InheritsFrom(TFontDialog)
  then Write(Comp.Name, TFontDialog(Comp).Font)

  { Directory list boxes }
  else if Comp.InheritsFrom(TDirectoryListBox)
  then WriteString(Comp.Owner.Name, Comp.Name, TDirectoryListBox(Comp).Directory)

  else
    Result:= FALSE;
end;


{-----------------------------------------------------------------------------------------------------------------------
   READ COMPONENT

   Read a supported control from INI file.
   Returns True if the control type is supported and was read.

   Important:
     The LightVcl.Visual.RadioButton will NOT be automatically resized if you call
     LoadForm(self) in FormCreate (canvas not ready). You need to call LoadForm(self)
     in LateInitialize.
-----------------------------------------------------------------------------------------------------------------------}
function TIniFileApp.ReadComp(Comp: TComponent): Boolean;
VAR s: string;
begin
  if Comp.Name = ''
  then raise Exception.Create('TIniFileApp.ReadComp: The control has no name! Class: ' + Comp.ClassName);

  Result:= TRUE;

  { Read form }
  if Comp.InheritsFrom(TForm) then
    begin
      { Warning: Main menu is not shown if form is set to bsDialog }
      if (TForm(Comp).Menu <> NIL) 
	  AND (TForm(Comp).BorderStyle = bsDialog)
      then ShowMessage(Comp.Name + CRLFw + 'The main menu will not appear because the form is set to bsDialog!');

      ReadCtrlPos(TControl(Comp));

      if Comp = Application.MainForm then
        begin
          Read('MainFormFont', TForm(Comp).Font);
          AppDataCore.LastUsedFolder:= ReadString(Comp.Name, 'LastUsedFolder', '');
        end;
    end

  { Read controls - only if value exists in INI }
  else if ValueExists(Comp.Owner.Name, Comp.Name) then
    begin
      { Warning: Setting action's properties won't invoke its events. So, the OnExecute assigned to this action will not be executed. We have to call it manually otherwise (if there is a checkbox assigned to this action) its OnClick event (not the one of the action) will be triggered by gui.  Details: https://stackoverflow.com/questions/46253875/taction-onexecute-is-not-executed }
      if Comp.InheritsFrom(TAction)
      then TAction(Comp).Checked:= ReadBool(Comp.Owner.Name, Comp.Name, False)

      { Checkboxes }
      else 
	  if Comp.InheritsFrom(TCheckBox)
      then TCheckBox(Comp).State:= TCheckBoxState(ReadInteger(Comp.Owner.Name, Comp.Name, 0))

      { Radio buttons }
      else 
	  if Comp.InheritsFrom(TRadioButton) then
        begin
         { https://stackoverflow.com/questions/47476603/major-flaw-radio-buttons-are-not-correctly-set-while-the-form-is-invisible
           https://community.embarcadero.com/blogs/entry/the-hidden-dangers-of-tradiobuttontabstop-37987
           Reason: A random radiobtn might be clicked when the user navigates with Tab  }
          if TRadioButton(Comp).TabStop 
          AND NOT TRadioButton(Comp).Checked  // Delphi will force TabStop to true if the control is checked
          then raise Exception.Create('TabStop should not be true for ' + Comp.Name + ' on ' + TRadioButton(Comp).Parent.Parent.Name);
          TRadioButton(Comp).Checked:= ReadBool(Comp.Owner.Name, Comp.Name, FALSE);
        end

      { Spin edits }
      else 
	  if Comp is TSpinEdit
      then TSpinEdit(Comp).Value:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)

      { Edit boxes }
      else 
	  if Comp.InheritsFrom(TEdit)
      then TEdit(Comp).Text:= ReadString(Comp.Owner.Name, Comp.Name, '')

      { Number boxes }
      else 
	  if Comp.InheritsFrom(TNumberBox)
      then TNumberBox(Comp).Value:= ReadFloat(Comp.Owner.Name, Comp.Name, 0)

      { Labeled edits }
      else if Comp.InheritsFrom(TLabeledEdit)
      then TLabeledEdit(Comp).Text:= ReadString(Comp.Owner.Name, Comp.Name, '')

      else 
	  if Comp.InheritsFrom(TCustomLabeledEdit)
      then TCustomLabeledEdit(Comp).Text:= ReadString(Comp.Owner.Name, Comp.Name, '')

      { Scroll bars }
      else 
	  if Comp.InheritsFrom(TScrollBar)
      then TScrollBar(Comp).Position:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)

      { Page controls }
      else 
	  if Comp.InheritsFrom(TPageControl)
      then TPageControl(Comp).ActivePageIndex:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)

      { Hot keys }
      else 
	  if Comp.InheritsFrom(THotKey)
      then THotKey(Comp).HotKey:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)

      { Filter combo boxes - special handling for FileList update }
      else 
	  if Comp.InheritsFrom(TFilterComboBox) 
	  then
        begin
          TFilterComboBox(Comp).ItemIndex:= ReadInteger(Comp.Owner.Name, Comp.Name, 0);  // the ItemIndex is changed but the FileList is not informed about the change...
          if TFilterComboBox(Comp).FileList <> NIL
          then TFilterComboBox(Comp).FileList.Mask := TFilterComboBox(Comp).Mask; // ...so we need this
        end

      { Combo boxes }
      else if Comp.InheritsFrom(TCustomComboBox)
      then TCustomComboBox(Comp).ItemIndex:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)

      { Radio groups }
      else if Comp.InheritsFrom(TRadioGroup)
      then TRadioGroup(Comp).ItemIndex:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)

      { Track bars }
      else if Comp.InheritsFrom(TTrackBar)
      then TTrackBar(Comp).Position:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)

      { Splitters }
      else if Comp.InheritsFrom(TSplitter)
      then ReadSplitter(Comp)

      { Menu items }
      else if Comp.InheritsFrom(TMenuItem)
      then TMenuItem(Comp).Checked:= ReadBool(Comp.Owner.Name, Comp.Name, FALSE)

      { Toggle switches }
      else if Comp.InheritsFrom(TToggleSwitch) then
        begin
          if ReadBool(Comp.Owner.Name, Comp.Name, FALSE)
          then TToggleSwitch(Comp).State:= tssOn
          else TToggleSwitch(Comp).State:= tssOff;
        end

      { Directory list boxes }
      else if Comp.InheritsFrom(TDirectoryListBox) then
        begin
          s:= ReadString(Comp.Owner.Name, Comp.Name, '');
          if DirectoryExists(s)
          then TDirectoryListBox(Comp).Directory:= s;
        end

      { Color dialogs }
      else if Comp.InheritsFrom(TColorDialog)
      then TColorDialog(Comp).Color:= ReadColor(Comp.Name, 0)

      { Open dialogs }
      else if Comp.InheritsFrom(TOpenDialog) then
        begin
          s:= ReadString (Comp.Owner.Name, Comp.Name, GetMyDocuments);  { NOTE: INI file does not support unicode chars }
          if DirectoryExists(ExtractFilePath(s))
          then TOpenDialog(Comp).InitialDir:= ExtractFilePath(s);
        end

      { Save dialogs }
      else if Comp.InheritsFrom(TSaveDialog) then
        begin
          s:= ReadString(Comp.Owner.Name, Comp.Name, GetMyDocuments);
          if DirectoryExists(ExtractFilePath(s))
          then TSaveDialog(Comp).InitialDir:= ExtractFilePath(s);
        end

      { Font dialogs }
      else if Comp.InheritsFrom(TFontDialog)
      then Read(Comp.Name, TFontDialog(Comp).Font)

      { Shapes (color) }
      else if Comp.InheritsFrom(TShape)
      then TShape(Comp).Brush.Color:= ReadColor(Comp.Name, 0)

      { File list boxes }
      else if Comp.InheritsFrom(TFileListBox) then
        begin
          s:= ReadString(Comp.Owner.Name, Comp.Name, '');
          if FileExists(s)
          then TFileListBox(Comp).FileName:= s;
        end

      else
        Result:= FALSE;
    end;
end;


{-----------------------------------------------------------------------------------------------------------------------
   IS SUPPORTED

   Returns True if the control type is supported for INI read/write.
   Optimization: Most common controls are at the top of the list for faster exit.
-----------------------------------------------------------------------------------------------------------------------}
function TIniFileApp.IsSupported(WinCtrl: TComponent): Boolean;
begin
  Result:= WinCtrl.InheritsFrom(TRadioButton)
        OR WinCtrl.InheritsFrom(TCheckBox)
        OR WinCtrl.InheritsFrom(TAction)
        OR WinCtrl.InheritsFrom(TSpinEdit)
        OR WinCtrl.InheritsFrom(TEdit)
        OR WinCtrl.InheritsFrom(TNumberBox)
        OR WinCtrl.InheritsFrom(TLabeledEdit)
        OR WinCtrl.InheritsFrom(TMenuItem)
        OR WinCtrl.InheritsFrom(TFilterComboBox)
        OR WinCtrl.InheritsFrom(TCustomLabeledEdit)
        OR WinCtrl.InheritsFrom(TCustomComboBox)
        OR WinCtrl.InheritsFrom(TTrackBar)
        OR WinCtrl.InheritsFrom(TScrollBar)
        OR WinCtrl.InheritsFrom(TDirectoryListBox)
        OR WinCtrl.InheritsFrom(TFileListBox)
        OR WinCtrl.InheritsFrom(TPageControl)
        OR WinCtrl.InheritsFrom(TRadioGroup)
        OR WinCtrl.InheritsFrom(TSplitter)
        OR WinCtrl.InheritsFrom(TFontDialog)
        OR WinCtrl.InheritsFrom(TOpenDialog)
        OR WinCtrl.InheritsFrom(TSaveDialog)
        OR WinCtrl.InheritsFrom(TColorDialog)
        OR WinCtrl.InheritsFrom(TFrame)
        OR WinCtrl.InheritsFrom(THotKey)
        OR WinCtrl.InheritsFrom(TShape)
        OR WinCtrl.InheritsFrom(TToggleSwitch);
end;


{-----------------------------------------------------------------------------------------------------------------------
   READ/WRITE GROUP

   Read/write all supported items (checkboxes, radioboxes, etc) found in a panel/groupbox/etc.
-----------------------------------------------------------------------------------------------------------------------}
procedure TIniFileApp.WriteGroup(WinCtrl: TWinControl);
VAR i: Integer;
begin
  if WinCtrl = NIL
  then raise Exception.Create('TIniFileApp.WriteGroup: WinCtrl parameter cannot be nil');

  for i:= 0 to WinCtrl.ControlCount - 1 do
    if IsSupported(WinCtrl.Controls[i])
    then WriteComp(WinCtrl.Controls[i]);
end;


procedure TIniFileApp.ReadGroup(WinCtrl: TWinControl);
VAR
  i: Integer;
begin
  if WinCtrl = NIL
  then raise Exception.Create('TIniFileApp.ReadGroup: WinCtrl parameter cannot be nil');

  for i:= 0 to WinCtrl.ControlCount - 1 do
    if IsSupported(WinCtrl.Controls[i])
    then ReadComp(WinCtrl.Controls[i]);
end;


{-----------------------------------------------------------------------------------------------------------------------
   SPLITTER

   Note: TSplitter is a TGraphicControl, not TWinControl.
   For horizontal splitters (alLeft/alRight), save Left position.
   For vertical splitters (alTop/alBottom), save Top position.
-----------------------------------------------------------------------------------------------------------------------}
procedure TIniFileApp.WriteSplitter(Comp: TComponent);
begin
  Assert(Comp.InheritsFrom(TSplitter), 'TIniFileApp.WriteSplitter: Comp is not a TSplitter');

  if (TSplitter(Comp).Align = alLeft) OR (TSplitter(Comp).Align = alRight)
  then WriteInteger(Comp.Owner.Name, Comp.Name, TSplitter(Comp).Left)
  else if (TSplitter(Comp).Align = alTop) OR (TSplitter(Comp).Align = alBottom)
  then WriteInteger(Comp.Owner.Name, Comp.Name, TSplitter(Comp).Top);
end;


procedure TIniFileApp.ReadSplitter(Comp: TComponent);
begin
  Assert(Comp.InheritsFrom(TSplitter), 'TIniFileApp.ReadSplitter: Comp is not a TSplitter');

  { Use current position as default to avoid moving splitter to 0 }
  if (TSplitter(Comp).Align = alLeft) OR (TSplitter(Comp).Align = alRight)
  then TSplitter(Comp).Left:= ReadInteger(Comp.Owner.Name, Comp.Name, TSplitter(Comp).Left)
  else if (TSplitter(Comp).Align = alTop) OR (TSplitter(Comp).Align = alBottom)
  then TSplitter(Comp).Top:= ReadInteger(Comp.Owner.Name, Comp.Name, TSplitter(Comp).Top);
end;


{-----------------------------------------------------------------------------------------------------------------------
   AS STRING

   Returns the entire content of the default INI file as a string.
   Don't forget to force the app to save its INI file to disk before calling this.
-----------------------------------------------------------------------------------------------------------------------}
class function TIniFileApp.AsString: string;
VAR
  IniPath: string;
begin
  IniPath:= TAppDataCore.IniFile;
  if NOT FileExists(IniPath)
  then raise Exception.Create('TIniFileApp.AsString: INI file does not exist: ' + IniPath);

  Result:= StringFromFile(IniPath);
end;


{-----------------------------------------------------------------------------------------------------------------------
   FONT

   Read/Write font properties to INI file.
   If the INI file does not contain font information, Read returns FALSE
   and no modification is done to the Font object.
-----------------------------------------------------------------------------------------------------------------------}
function TIniFileApp.Read(CONST Ident: string; Font: TFont): Boolean;
begin
  if Ident = ''
  then raise Exception.Create('TIniFileApp.Read(Font): Ident parameter cannot be empty');

  if Font = NIL
  then raise Exception.Create('TIniFileApp.Read(Font): Font parameter cannot be nil');

  Result:= ValueExists(FSection, Ident + 'Name');
  if Result then
    begin
      Font.Name   := ReadString(FSection, Ident + 'Name', 'Arial');
      Font.CharSet:= TFontCharSet(ReadInteger(FSection, Ident + 'CharSet', 0));
      Font.Color  := TColor(ReadInteger(FSection, Ident + 'Color', 0));
      Font.Size   := ReadInteger(FSection, Ident + 'Size', 8);
      Font.Style  := TFontStyles(Byte(ReadInteger(FSection, Ident + 'Style', 0)));
    end;
end;


procedure TIniFileApp.Write(CONST Ident: string; Font: TFont);
begin
  if Ident = ''
  then raise Exception.Create('TIniFileApp.Write(Font): Ident parameter cannot be empty');

  if Font = NIL
  then raise Exception.Create('TIniFileApp.Write(Font): Font parameter cannot be nil');

  WriteString (FSection, Ident,  '');      // I need this here so I can find the font by its identifier (name). Otherwise it will be filtered out by TIniFileVCL.Read: if ValueExists(FSection, Comp.Name) then
  WriteString(FSection, Ident + 'Name', Font.Name);
  WriteInteger(FSection, Ident + 'CharSet', Font.CharSet);
  WriteInteger(FSection, Ident + 'Color', Font.Color);
  WriteInteger(FSection, Ident + 'Size', Font.Size);
  WriteInteger(FSection, Ident + 'Style', Byte(Font.Style));
end;


{-----------------------------------------------------------------------------------------------------------------------
   COLORS
-----------------------------------------------------------------------------------------------------------------------}
function TIniFileApp.ReadColor(CONST Ident: string; Default: TColor): TColor;
begin
  if Ident = ''
  then raise Exception.Create('TIniFileApp.ReadColor: Ident parameter cannot be empty');

  Result:= StringToColor(ReadString(FSection, Ident, ColorToString(Default)));
end;


procedure TIniFileApp.WriteColor(CONST Ident: string; Value: TColor);
begin
  if Ident = ''
  then raise Exception.Create('TIniFileApp.WriteColor: Ident parameter cannot be empty');

  WriteString(FSection, Ident, ColorToString(Value));
end;


end.
