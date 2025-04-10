UNIT LightFMX.IniFile;

{=============================================================================================================
   www.GabrielMoraru.com
   2024.09
   See Copyright file
--------------------------------------------------------------------------------------------------------------

  Same as ccINIFile but adds support for forms to save themselves to disk.
  Ini file name/file path is automatically calculated.

--------------------------------------------------------------------------------------------------------------

  Self saving forms:
     Using SaveForm/LoadForm, a form with lots of controls (like checkboxes/radiobuttons) can save its status
     to disk on shutdown and resume exaclty from where it left on application startup.
     Example:
       - Call TLightForm.SaveForm. LoadForm is automatically called by AppData.

  Storing only individual controls:
     You can also save individual controls.
     Example:
        To store the status of a checkbox to a INI file use TIniFileApp.Write(MyCheckBox).

     Default value:
        The user doesn't have to provide a default value.
        If value does not exist in INI file, the value from GUI (MyCheckBox.Checked) will be used.

  Compleatness:
     Most common VCL controls are supported.
     Check IsSupported() to see a list of supported controls.
     Support for more controls can be easily added with just an 'if/then'.

  OnClick execution:
     The events are executed when the component status is changed when it is
     read from the ini file ONLY IF its INI staus is different than its DFM status!
     In other words, if you want to autoexecute some OnClick event, you
     will have to call the ControlOnClick(sender) after the program loaded.

     The TAction.OnExecute is NEVER executed.

  Dependencies on AppData:
     AppData global var is used to obtain the file name of the INI file.

     Therefore, before using TIniFileApp you must set the AppData.AppName var like this: AppData:= TAppData.Create('DelphiLightSaber').
     The TIniFileApp class will use AppName to automatically determine the INI file name/path which is %AppData%\AppName.Ini.
     Example: If the AppData.AppName is set to "DelphiLightSaber",
     the ini file will be "c:\Users\UserName\AppData\Roaming\DelphiLightSaber\DelphiLightSaber.ini"
     See ccAppData.pas for details.
     The TIniFileApp class will also automatically save the ccAppData.AppData.LastUsedFolder variable to the INI file.


  Important:
     SaveFrom will fail to save the controls of a form if they have been re-parented (moved to another form). But no exception will rise.
     The new parent form will also fail to save them.
     To fix this, we need to move the controls back to their original parent (form) BEFORE we call SaveFrom!

  Notices
    * DO NOT USE SELF as parameter in TIniFileVCL.Create if you call TIniFileVCL.Create in FormCreate because the Self is not yet ready! But TIniFileVCL.Create(FrmMain) works.
    * LoadForm is called after the components on the form are created but BEFORE their CreateWnd is called!

  UNICODE
     INI file does not support unicode chars. Unicode chars are replaced with '?'. Sorry. This is not a limitation in my code but in Delphi's RTL.


  ____________

  Demo code:
     procedure TCoreSv.saveBuildID;
     begin
       var INI := cbINIFile.TIniFileApp.Create('SectionName');
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
{ToDo 2: Prevent controls for fireing the OnClick event (and others like this) when loading its state from INI. Do it like so: set control's event handler to NIL, load its value from INI file than set the event handler back to original value
       How can I check a checkbox without triggering of OnClick event?
       MyCheckBox.Perform(BM_SETCHECK, 1, 0);
       http://www.scalabium.com/faq/dct0012.htm }

INTERFACE

USES
   System.Classes, System.IniFiles, System.SysUtils, System.UITypes, System.UIConsts,
   FMX.Graphics, FMX.Forms, FMX.Controls, FMX.StdCtrls, FMX.NumberBox, FMX.Colors,
   FMX.ActnList, FMX.Edit, FMX.SpinBox, FMX.Menus, FMX.Types, FMX.Dialogs, FMX.ListBox,
   ccINIFile;

TYPE

 TIniFileApp = class(TIniFileEx)
  private
  protected
    procedure readCtrlPos  (Ctrl: TControl);
    procedure writeCtrlPos (Ctrl: TControl);
    procedure writeSplitter(Comp: TComponent);
    procedure readSplitter (Comp: TComponent);
  public
    ShowPositionWarn: Boolean;
    constructor Create(SectionName: string; ForcedName: string= ''); reintroduce; overload;
    function IsSupported(WinCtrl: TComponent): Boolean; virtual;
    class function AsString: string;

    procedure SaveForm (Form: TForm; AutoState: TAutoState= asPosOnly);      { Save ALL supported controls on this form }
    procedure LoadForm (Form: TForm; AutoState: TAutoState= asPosOnly);

    { Font - this requires VCL framework so it cannot be moved to ccINIFile }
    function  Read       (CONST Ident: string; Font: TFont): Boolean;  overload;
    procedure Write      (CONST Ident: string; Font: TFont);           overload;

    { Color - this requires VCL framework so it cannot be moved to ccINIFile }
    function  ReadColor  (CONST Ident: string; Default: TColor): TColor;
    procedure WriteColor (CONST Ident: string; Value: TColor);

    { Read/write controls directly }
    function  WriteComp  (Comp: TComponent): Boolean; virtual;
    function  ReadComp   (Comp: TComponent): Boolean; virtual;

    procedure ReadGroup  (WinCtrl: TControl);
    procedure WriteGroup (WinCtrl: TControl);
  end;


IMPLEMENTATION

USES
   ccIO, ccTextFile, ccCore, LightFMX.AppData;


{-----------------------------------------------------------------------------------------------------------------------
   MAIN
-----------------------------------------------------------------------------------------------------------------------}
constructor TIniFileApp.Create(SectionName: string; ForcedName: string= '');                             { Open INI file for writing }
VAR Path: string;
begin
 if ForcedName= ''
 then Path:= TAppData.IniFile
 else Path:= ForcedName;

 inherited Create(SectionName, Path);
 ShowPositionWarn:= TRUE;
end;



{-----------------------------------------------------------------------------------------------------------------------
   SAVE/LOAD FORM
-----------------------------------------------------------------------------------------------------------------------}
{ Save ALL supported controls on this form

  Note:
     Components[] just yields the components that are OWNED by the form.
     So, if we are iterating over it will miss any components that are added dynamically, and not owned by the form, or components that are owned by frames.
     Update 2021: Now frames are supported. All sub-components of a frame are stored to the INI file  }
procedure TIniFileApp.SaveForm(Form: TForm; AutoState: TAutoState= asPosOnly);

  procedure WriteComponentsOf(Component: TComponent);
  VAR i: Integer;
  begin
    for i:= 0 to Component.ComponentCount-1 DO
     if IsSupported(Component.Components[i]) then
      if Component.Components[i].InheritsFrom(TFrame) //Frames
      then WriteComponentsOf(Component.Components[i])
      else WriteComp(Component.Components[i]);
  end;

begin
 Assert(Form <> NIL);
 Assert(AutoState <> asNone, 'AutoState = asNone detected in SaveFrom');

 WriteComp(Form);
 if AutoState= asFull
 then WriteComponentsOf(Form);
end;


procedure TIniFileApp.LoadForm(Form: TForm; AutoState: TAutoState= asPosOnly);

   procedure ReadComponentsOf(Component: TComponent);
   VAR i: Integer;
   begin
     for i:= 0 to Component.ComponentCount-1 DO
       if IsSupported(Component.Components[i]) then
         if Component.Components[i].InheritsFrom(TFrame)
         then ReadComponentsOf(Component.Components[i])
         else ReadComp(Component.Components[i]);
   end;

begin
 Assert(Form <> NIL);
 Assert(AutoState <> asNone, 'AutoState = asNone detected in SaveFrom');

 ReadComp(Form);             { Read form itself }
 if AutoState= asFull        { Read form's sub-components }
 then ReadComponentsOf(Form);
end;



procedure TIniFileApp.WriteCtrlPos(Ctrl: TControl);
begin
 if Ctrl.InheritsFrom(TForm)
 then
  begin
   VAR Form:= TForm(Ctrl);
   if Form.WindowState = TWindowState.wsMaximized                    { Unmaximize form in order to save form position correctly }
   then Form.WindowState:= TWindowState.wsNormal;
   WriteInteger(Ctrl.Name,'WindowState', Ord(Form.WindowState));     { ATENTION! This MUST be above Window:= wsNormal }

   Assert(Assigned(Form.Handle));
   Assert(Form.IsHandleAllocated);

   { Save Ctrl position }
   WriteFloat(Ctrl.Name, 'Top'   , Form.Top);
   WriteFloat(Ctrl.Name, 'Left'  , Form.Left);
   WriteFloat(Ctrl.Name, 'Width' , Form.Width);
   WriteFloat(Ctrl.Name, 'Height', Form.Height);
  end
 else
  begin
   { Save Ctrl position }
   WriteFloat(Ctrl.Owner.Name, Ctrl.Name+ '.Top'   , Ctrl.Position.Y);
   WriteFloat(Ctrl.Owner.Name, Ctrl.Name+ '.Left'  , Ctrl.Position.X);
   WriteFloat(Ctrl.Owner.Name, Ctrl.Name+ '.Width' , Ctrl.Width);
   WriteFloat(Ctrl.Owner.Name, Ctrl.Name+ '.Height', Ctrl.Height);
  end;
end;



{ For strange reasons, for Vcl.Forms, cbAppDataForm, I cannot read/write the ClientWidth. I need to use Width. }
procedure TIniFileApp.ReadCtrlPos(Ctrl: TControl);
var
  IsNonResizable: Boolean;
begin
  if Ctrl.InheritsFrom(TForm) then
    begin
      VAR Form:= TForm(Ctrl);

      { Non-resizable form?
        Don't read the size of the form if the form is not resizable. In this case the design-time size will be used. }
      IsNonResizable :=
           (Form.BorderStyle = TFmxFormBorderStyle.None)
        OR (Form.BorderStyle = TFmxFormBorderStyle.ToolWindow)
        OR (Form.BorderStyle = TFmxFormBorderStyle.SizeToolWin);

      if ValueExists(Form.Name, 'Top')
      then Form.Top := ReadInteger(Form.Name, 'Top', 0)                  { For "normal" controls default values are ignored because of ValueExists, therefore the current window 'physical' width/height are used }
      else Form.Top := Round((Screen.Height - Form.Height) / 2);         { For forms the top/left position is calculated so that the form is centered on screen, at first startup }

      if ValueExists(Form.Name, 'Left')
      then Form.Left := ReadInteger(Form.Name, 'Left', 0)
      else Form.Left := round((Screen.Width - Form.Width) / 2);

      {We only read the width/height if it exists. Otherwise we will use the default value, which is ZERO which will fuck up the form size }
      if (NOT IsNonResizable) AND ValueExists(Form.Name, 'Width')
      then Form.Width := ReadInteger (Form.Name, 'Width' , 0);

      //todo: !!!!!!!!!!!!!!!! use Ctrl.width instead of 0 and get rid of ValueExists

      if (NOT IsNonResizable) AND ValueExists(Form.Name, 'Height')
      then Form.Height:= ReadInteger (Form.Name, 'Height', 0);    //todo 1: !!!!!!!!!!!!! use Ctrl.Height instead of 0 and get rid of ValueExists

      if ShowPositionWarn
      AND (Form.Position <> TFormPosition.Designed)
      then RAISE Exception.Create('Position is not ''poDesigned'' for form '+ Form.Name +'!');

      if ValueExists(Form.Name, 'WindowState')
      then Form.WindowState:= TWindowState(ReadInteger (Form.Name, 'WindowState', 0));      //  TWindowState = (wsNormal, wsMinimized, wsMaximized);
    end
  else
    begin
      Ctrl.Position.Y := ReadFloat (Ctrl.Owner.Name, Ctrl.Name+ '.Top'   , Ctrl.Position.Y);
      Ctrl.Position.X := ReadFloat (Ctrl.Owner.Name, Ctrl.Name+ '.Left'  , Ctrl.Position.X);
      Ctrl.Width      := ReadFloat (Ctrl.Owner.Name, Ctrl.Name+ '.Width' , Ctrl.Width);
      Ctrl.Height     := ReadFloat (Ctrl.Owner.Name, Ctrl.Name+ '.Height', Ctrl.Height);
    end
end;






{-----------------------------------------------------------------------------------------------------------------------
   READ/WRITE INDIVIDUAL CTRLS
-----------------------------------------------------------------------------------------------------------------------}
//ToDo: here make an array of supported controls (class references?) then go with InheritsFrom through the list
function TIniFileApp.WriteComp(Comp: TComponent): Boolean;                                                             { Write 'any' control to INI file }
VAR s: String;
begin
 Assert(AppData <> NIL);

 if Comp.Name = '' then
  begin
    s := '[TIniFileApp.WriteComp] The control has no name! Class: ' + Comp.ClassName;
    if (Comp is TControl)
	and (TControl(Comp).Parent <> nil)
    then s := s + '. Parent: ' + TControl(Comp).Parent.Name;
   RAISE Exception.Create(s);
  end;

  Result:= TRUE;

  if Comp is TForm then
  begin
    WriteCtrlPos (TControl(Comp));
    //Write('FormFont', TForm(Comp).Font);
    if Comp = Application.MainForm
    then WriteString(Comp.Name, 'LastUsedFolder', AppData.LastUsedFolder); //todo: make it a parameter for TIniFileApp
  end else

  if Comp is TAction      then WriteBool(Comp.Owner.Name, Comp.Name, TAction(Comp).Checked) else
  if Comp is TCheckBox    then WriteInteger(Comp.Owner.Name, Comp.Name, Ord(TCheckBox(Comp).IsChecked)) else
  if Comp is TRadioButton then WriteBool(Comp.Owner.Name, Comp.Name, TRadioButton(Comp).IsChecked) else
  if Comp is TComboBox    then WriteInteger(Comp.Owner.Name, Comp.Name, TComboBox(Comp).ItemIndex) else
  if Comp is TNumberBox   then WriteFloat(Comp.Owner.Name, Comp.Name, TNumberBox(Comp).Value) else
  if Comp is TEdit        then WriteString(Comp.Owner.Name, Comp.Name, TEdit(Comp).Text) else  { Do not go down to TCustomEdit because I will accidentaly read the RichLog which is derived from there! }
  if Comp is TSpinBox     then WriteFloat(Comp.Owner.Name, Comp.Name, TSpinBox(Comp).Value) else
  if Comp is TTrackBar    then WriteFloat(Comp.Owner.Name, Comp.Name, TTrackBar(Comp).Value) else
  if Comp is TSplitter    then WriteSplitter(Comp) else
  if Comp is TScrollBar   then WriteFloat(Comp.Owner.Name, Comp.Name, TScrollBar(Comp).Value) else
  if Comp is TMenuItem    then WriteBool(Comp.Owner.Name, Comp.Name, TMenuItem(Comp).IsChecked) else
  if Comp is TSwitch      then WriteBool(Comp.Owner.Name, Comp.Name, TSwitch(Comp).IsChecked) else

  if Comp is TColorBox then WriteColor(Comp.Name, TColorBox(Comp).Color) else
  //if Comp is TPageControl then WriteInteger(Comp.Owner.Name, Comp.Name, TPageControl(Comp).TabIndex) else

  if Comp is TOpenDialog
  then
    begin
     s:= TOpenDialog(Comp).FileName;     { NOTE: INI file does not support unicode chars. The unicode chars are replaced by '?' so we better replace this with a ' ' or something that is not '?'. This way if the filename is fucked up, we at least could 'recover' the folder name - which will be used for InitialDir! }
     s:= ReplaceUnicodeChars(s, '_');
     if s > ''
     then WriteString (Comp.Owner.Name, Comp.Name, s);
   end else

  if Comp.InheritsFrom(TSaveDialog)
  then
    begin
     s:= TSaveDialog(Comp).FileName;     { NOTE: INI file does not support unicode chars. The unicode chars are replaced by '?' so we better replace this with a ' ' or something that is not '?'. This way if the filename is fucked up, we at least could 'recover' the folder name - which will be used for InitialDir! }
     s:= ReplaceUnicodeChars(s, '_');
     if s > ''
     then WriteString (Comp.Owner.Name, Comp.Name, s);
    end
  else
    Result:= FALSE; //RAISE Exception.Create('Unsupported control: '+ Comp.ClassName+ ', '+ Comp.Name);
end;



{Important:
    The cvRadioButton/cvRadioButton will NOT be automatically resized if you call LoadForm(self) in FormCreate (canvas not ready). You need to call LoadForm(self) in LateInitialize. }
function TIniFileApp.ReadComp(Comp: TComponent): Boolean;
VAR s: string;
begin
  Assert(Comp.Name <> '', '[TIniFileApp.ReadComp] The control has no name! Class: ' + Comp.ClassName);
  Result:= TRUE;

 { Read form }
 if Comp.InheritsFrom(TForm)
 then
   begin
     ReadCtrlPos(TControl(Comp));
    //ReadFont('FormFont', TForm(Comp).Font);
  end
  else if ValueExists(Comp.Owner.Name, Comp.Name) then
  begin
    if Comp is TAction         then TAction(Comp).Checked        := ReadBool(Comp.Owner.Name, Comp.Name, FALSE) else
    if Comp is TCheckBox       then TCheckBox(Comp).IsChecked    := ReadInteger(Comp.Owner.Name, Comp.Name, 0) <> 0 else
    if Comp is TRadioButton    then TRadioButton(Comp).IsChecked := ReadBool(Comp.Owner.Name, Comp.Name, False) else
    if Comp is TComboBox       then TComboBox(Comp).ItemIndex    := ReadInteger(Comp.Owner.Name, Comp.Name, 0) else
    if Comp is TSpinBox        then TSpinBox(Comp).Value         := ReadFloat(Comp.Owner.Name, Comp.Name, 0) else
    if Comp is TEdit           then TEdit(Comp).Text             := ReadString(Comp.Owner.Name, Comp.Name, '') else

    if Comp is TNumberBox      then TNumberBox(Comp).Value       := ReadFloat(Comp.Owner.Name, Comp.Name, 0) else
    if Comp is TTrackBar       then TTrackBar(Comp).Value        := ReadFloat(Comp.Owner.Name, Comp.Name, 0) else
    if Comp is TSplitter       then ReadSplitter(Comp) else
    if Comp is TScrollBar      then TScrollBar(Comp).Value       := ReadInteger(Comp.Owner.Name, Comp.Name, 0) else
    if Comp is TMenuItem       then TMenuItem(Comp).IsChecked    := ReadBool(Comp.Owner.Name, Comp.Name, False) else
    //if Comp is TSwitch then TSwitch(Comp).IsChecked := ReadBool(Comp.Owner.Name, Comp.Name, False) else

    //if Comp is TPageControl then TPageControl(Comp).TabIndex := ReadInteger(Comp.Owner.Name, Comp.Name, 0) else
    if Comp is TColorBox then TColorBox(Comp).Color := ReadColor(Comp.Name, 0) else
    if Comp is TOpenDialog
    then
      begin
        s:= ReadString(Comp.Owner.Name, Comp.Name, '');
        if DirectoryExists(ExtractFilePath(s)) then TOpenDialog(Comp).InitialDir := ExtractFilePath(s);
      end
    else
      if Comp is TSaveDialog
      then
        begin
          s:= ReadString(Comp.Owner.Name, Comp.Name, '');
          if DirectoryExists(ExtractFilePath(s)) then TSaveDialog(Comp).InitialDir := ExtractFilePath(s);
        end
      else
        Result:= FALSE; //RAISE Exception.Create('Unsupported control: '+ Comp.ClassName+ ', '+ Comp.Name);
  end;
end;


{ Optimization trick: the most common/used controls are on top of the list. This way we have a higher chance to exit faster the list, without going through all lines }
function TIniFileApp.IsSupported(WinCtrl: TComponent): Boolean;
begin
  Result := WinCtrl is TRadioButton
    or (WinCtrl is TCheckBox)
    or (WinCtrl is TAction)
    or (WinCtrl is TSpinBox)
    or (WinCtrl is TEdit)
    or (WinCtrl is TNumberBox)
    or (WinCtrl is TComboBox)
    or (WinCtrl is TTrackBar)
    or (WinCtrl is TScrollBar)
    or (WinCtrl is TMenuItem)
    //or (WinCtrl is TPageControl)
    //or (WinCtrl is TFontDialog)
    //or (WinCtrl is TOpenDialog)
    //or (WinCtrl is TSaveDialog)
    //or (WinCtrl is TColorBox)
    or (WinCtrl is TSplitter);
    //or (WinCtrl is TToggleSwitch);
end;









{---------------
   CHILDREN
----------------}
{ Read/write all supported items (checkboxes, radioboxes, etc) found in a panel/groupbox/etc }
procedure TIniFileApp.WriteGroup(WinCtrl: TControl);  { Save/load all  in a groupbox/panel }
VAR i: Integer;
begin
 for i:= 0 to WinCtrl.ControlsCount-1 DO
   if IsSupported(WinCtrl.Controls[i])
   then WriteComp(WinCtrl.Controls[i]);
end;


{ The section name is automatically retrieved from WinCtrl.Name }
procedure TIniFileApp.ReadGroup(WinCtrl: TControl);
VAR i: Integer;
begin
 for i:= 0 to WinCtrl.ControlsCount-1 DO
   if IsSupported(WinCtrl.Controls[i])
   then ReadComp(WinCtrl.Controls[i]);
end;

















procedure TIniFileApp.writeSplitter(Comp: TComponent);
begin
  if TControl(Comp).Align = TAlignLayout.Left
  then WriteFloat(Comp.Owner.Name, Comp.Name, TControl(Comp).Position.X)
  else
    if TControl(Comp).Align = TAlignLayout.Top
	then WriteFloat(Comp.Owner.Name, Comp.Name, TControl(Comp).Position.Y);
end;



procedure TIniFileApp.readSplitter(Comp: TComponent);
begin
  if TControl(Comp).Align = TAlignLayout.Left
  then TControl(Comp).Position.X := ReadInteger(Comp.Owner.Name, Comp.Name, 0)
  else
    if TControl(Comp).Align = TAlignLayout.Top
    then TControl(Comp).Position.Y := ReadInteger(Comp.Owner.Name, Comp.Name, 0);
end;





{ Returns the entire content of the default INI file, as string.
  Don't forget to force the app to save its INI file to disk before loading the file. }             { Old name: IniFileToString }
class function TIniFileApp.AsString: string;
begin
  Result:= StringFromFile(AppData.IniFile);
end;



{ Result:
    If the INI file does not contains informations about font then this function will return FALSE
    and no modification will be done to the 'Font' object passed as parameter. }
function TIniFileApp.Read(CONST Ident: string; Font: TFont): Boolean;
begin
 Result:= ValueExists(FSection, Ident+ 'Name');
 if Result then
   begin
    Font.Family := ReadString   (FSection,              Ident+ 'Name',    'Arial');
    Font.Size   := ReadInteger  (FSection,              Ident+ 'Size',    8);
    Font.Style  := TFontStyles (BYTE
                                (ReadInteger (FSection, Ident+ 'Style',   0)) );
   end;
end;

procedure TIniFileApp.Write(CONST Ident: string; Font: TFont);
begin
  WriteString (FSection, Ident,  '');      // I need this here so I can find the font by its identifier (name). Otherwise it will be filtered out by TIniFileVCL.Read: if ValueExists(FSection, Comp.Name) then
  WriteString (FSection, Ident + 'Name',    Font.Family);
  WriteFloat  (FSection, Ident + 'Size',    Font.Size);
  WriteInteger(FSection, Ident + 'Style',   Byte(Font.Style));
end;




{---------------
   COLORS
----------------}
function TIniFileApp.ReadColor(CONST Ident: string; Default: TColor): TColor;
begin
  Result:= StringToColor(ReadString(FSection, Ident, ColorToString(Default)));
end;


procedure TIniFileApp.WriteColor(CONST Ident: string; Value: TColor);
begin
  WriteString(FSection, Ident, ColorToString(Value));
end;



end.
