UNIT ccINIFileVcl; 

{=============================================================================================================
  CubicDesign
  2022-04-03

  Do you have applications with forms with lots of controls (like checkboxes/radiobuttons) and you want to
  save its status to disk on shutdown and resume exaclty from where you left on application startup
  with just one function call?
  Use SaveForm/LoadForm.

  Example:
  - Call SaveForm(MySettingsForm) in TMySettingsForm.OnDestroy
  - Call LoadForm(MySettingsForm) after the creation of TMySettingsForm

==============================================================================================================

  Storing the state of the entire form:
    Just call LoadForm(MyForm) when your application starts (preferably after the form was fully created) and SaveForm(MyForm) in form's destructor.

  Storing individual controls:
     You can also save individual controls. Example: store the status of a checkbox to INI file with: TCubicIniFileVcl.Write(MyCheckBox).
     The user doesn't have to provide a default value.
     If value does not exist in INI file, the value from GUI (MyCheckBox.Checked) will be used.

  ____________

  Compleateness:
     Most common VCL controls are supported.
     Support for more controls can be easily added with just an if/then.
     Check IsSupported() to see a list of supported controls

  OnClick execution:
     The events are executed when the component status is changed when it is read from the ini file
      ONLY IF its INI staus is different than its DFM status!
     In other words, the programmer needs to call the ControlOnClick(sender) after the program loaded.

     The TAction.OnExecute is NEVER executed.

  ____________

  Utils:
     Use ccIniFile.AppData.IniFile to obtain the file name of the INI file (where to save the data).
     Use ccIniFile.AppData.AppDataFolder to get the path where the INI file is stored.
     The class will also automatically save the ccAppData.AppData.LastUsedFolder variable

  ____________

  Important:
     SaveFrom will fail to save the controls of a form if they have been re-parented (moved to another form).
     The new parent form will also fail to save them.
     To fix this, we need to move the controls back to their original parent (form) BEFORE we call SaveFrom!

  Important:
     DO NOT USE SELF as parameter in TCubicIniFileEx.Create if you call TCubicIniFileEx.Create in FormCreate because the Self is not yet ready!
     But TCubicIniFileEx.Create(FrmMain) works.

  Important:
     INI file does not support unicode chars. Unicode chars are replaced with '?'

  //LoadForm is called after the components on the form are created but BEFORE thier CreateWnd is called!

  Tester:
     c:\Myprojects\Project Testers\IniFile tester\Tester.dpr
=======================================================================================================================}

{ToDo: add support for TCheckListBox}
{ToDo 2: Prevent controls for fireing the OnClick event (and others like this) when loading its state from INI. Do it like so: set control's event handler to NIL, load its value from INI file than set the event handler back to original value
       How can I check a checkbox without triggering of OnClick event?
       MyCheckBox.Perform(BM_SETCHECK, 1, 0);
       http://www.scalabium.com/faq/dct0012.htm }

INTERFACE

USES
   WinApi.Windows, System.Classes, Vcl.ActnList, Vcl.Dialogs, System.SysUtils, System.UITypes, Vcl.Graphics, Vcl.Forms, Vcl.FileCtrl,
   Vcl.Menus, Vcl.ExtCtrls, Vcl.NumberBox, Vcl.ComCtrls, Vcl.WinXCtrls, Vcl.Samples.Spin,
   Vcl.Controls, Vcl.StdCtrls, System.IniFiles, ccCore, ccINIFile;

{$WARN UNIT_PLATFORM OFF}
{$WARN GARBAGE OFF}              {Silence the: 'W1011 Text after final END' warning }

TYPE
 TCubicIniFileVcl = class(TCubicIniFile)
  private
  protected
    procedure readCtrlPos  (Ctrl: TControl);
    procedure writeCtrlPos (Ctrl: TControl);
    procedure writeSplitter(Comp: TComponent);
    procedure readSplitter (Comp: TComponent);
  public
    ShowPositionWarn: Boolean;
    constructor Create (SectionName: string; ForcedPath: string= ''); override;
    function  IsSupported(WinCtrl: TComponent): Boolean; virtual;

    procedure SaveForm (Form: TForm; PosOnly:Boolean= FALSE);       { Save ALL supported controls on this form }
    procedure LoadForm (Form: TForm; PosOnly:Boolean= FALSE);

    { Read/write controls directly }
    function WriteComp  (Comp: TComponent): Boolean; virtual;
    function ReadComp   (Comp: TComponent): Boolean; virtual;
    procedure ReadGroup  (WinCtrl: TWinControl);
    procedure WriteGroup (WinCtrl: TWinControl);
  end;


{ These only support standard VCL controls. If you want to save also the custom (Cubic) controls see cvIniFile }
procedure SaveForm (Form: TForm; OnlyFormPos: Boolean= FALSE);
procedure LoadForm (Form: TForm; OnlyFormPos: Boolean= FALSE);


IMPLEMENTATION

USES
   ccIO, ccAppData;


{-----------------------------------------------------------------------------------------------------------------------
   MAIN
-----------------------------------------------------------------------------------------------------------------------}
constructor TCubicIniFileVcl.Create(SectionName: string; ForcedPath: string= '');                              { Open INI file for writing }
begin
 inherited Create(SectionName, forcedpath);
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
procedure TCubicIniFileVcl.SaveForm(Form: TForm; PosOnly:Boolean= FALSE);

  procedure WriteComponentsOf(Component: TComponent);
  VAR i: Integer;
  begin
    for i:= 0 to Component.ComponentCount-1 DO
     if IsSupported(Component.Components[i]) then
      if Component.Components[i].InheritsFrom(TFrame)
      then WriteComponentsOf(Component.Components[i])
      else WriteComp(Component.Components[i]);
  end;

begin
 Assert(Form <> NIL);
 WriteComp(Form);
 if NOT PosOnly
 then WriteComponentsOf(Form);
end;


procedure TCubicIniFileVcl.LoadForm(Form: TForm; PosOnly: Boolean= FALSE);

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
 ReadComp(Form);         { Read form itself }
 if NOT PosOnly          { Read form's sub-components }
 then ReadComponentsOf(Form);
end;



procedure TCubicIniFileVcl.WriteCtrlPos(Ctrl: TControl);
begin
 if Ctrl.InheritsFrom(TForm)
 then
  begin
   if TForm(Ctrl).WindowState = wsMaximized                                 { Unmaximize form in order to save form position correctly }
   then TForm(Ctrl).WindowState:= wsNormal;
   WriteInteger(Ctrl.Name,'WindowState', Ord(TForm(Ctrl).WindowState));     { ATENTION! This MUST be above Window:= wsNormal }

   { Save Ctrl position }
   WriteInteger(Ctrl.Name, 'Top'   , Ctrl.Top);
   WriteInteger(Ctrl.Name, 'Left'  , Ctrl.Left);
   WriteInteger(Ctrl.Name, 'Width' , Ctrl.Width);
   WriteInteger(Ctrl.Name, 'Height', Ctrl.Height);
  end
 else
  begin
   { Save Ctrl position }
   WriteInteger(Ctrl.Owner.Name, Ctrl.Name+ '.Top'   , Ctrl.Top);
   WriteInteger(Ctrl.Owner.Name, Ctrl.Name+ '.Left'  , Ctrl.Left);
   WriteInteger(Ctrl.Owner.Name, Ctrl.Name+ '.Width' , Ctrl.Width);
   WriteInteger(Ctrl.Owner.Name, Ctrl.Name+ '.Height', Ctrl.Height);
  end;
end;



{ For strange reasons, for forms, I cannot read/write the ClientWidth. I need to use Width. }
procedure TCubicIniFileVcl.ReadCtrlPos(Ctrl: TControl);
begin
 if Ctrl.InheritsFrom(TForm)
 then
  begin
   { Non-resizable form?
     Don't read the size of the form if the form is not resizable. In this case the design-time size will be used. }
    VAR IsNonResizable:=
         (TForm(Ctrl).BorderStyle = bsNone)
      OR (TForm(Ctrl).BorderStyle = bsDialog)
      OR (TForm(Ctrl).BorderStyle = bsToolWindow);

    if ValueExists(Ctrl.Name, 'Top')
    then Ctrl.Top:= ReadInteger (Ctrl.Name, 'Top' , 0)     { For "normal" controls default values are ignored because of ValueExists, therefore the current window 'physical' width/height are used }
    else Ctrl.Top:= (Screen.Height - Ctrl.Height) DIV 2;   { For forms the top/left position is calculated so that the form is centered on screen, at first startup }

    if ValueExists(Ctrl.Name, 'Left')
    then Ctrl.Left := ReadInteger (Ctrl.Name, 'Left', 0)
    else Ctrl.Left:= (Screen.Width - Ctrl.Width)  DIV 2;

    {We only read the width/height if it exists. Otherwise we will use the default value, which is ZERO which will fuck up the form size }
    if (NOT IsNonResizable) AND ValueExists(Ctrl.Name, 'Width')
    then Ctrl.Width := ReadInteger (Ctrl.Name, 'Width' , 0);
    //todo: !!!!!!!!!!!!!!!! use Ctrl.width instead of 0 and get rid of ValueExists

    {Note: For strange reasons, for forms, I cannot read/write the ClientWidth (maybe because I call this routine from FormCreate?). I need to use Width. }
    {Note: If the form was maximized when it was closed, the Ctrl.Width will be different than W }

    if (NOT IsNonResizable) AND ValueExists(Ctrl.Name, 'Height')
    then Ctrl.Height:= ReadInteger (Ctrl.Name, 'Height', 0);    //todo 1: !!!!!!!!!!!!! use Ctrl.Height instead of 0 and get rid of ValueExists

    if ShowPositionWarn
    AND (TForm(Ctrl).Position <> poDesigned)
    then RAISE Exception.Create('Position is not ''poDesigned'' for form '+ Ctrl.Name +'!');

    if ValueExists(Ctrl.Name, 'WindowState')
    then TForm(Ctrl).WindowState:= TWindowState(ReadInteger (Ctrl.Name, 'WindowState', 0));      //  TWindowState = (wsNormal, wsMinimized, wsMaximized);
  end
 else
  begin
    Ctrl.Top   := ReadInteger (Ctrl.Owner.Name, Ctrl.Name+ '.Top'   , Ctrl.Top);
    Ctrl.Left  := ReadInteger (Ctrl.Owner.Name, Ctrl.Name+ '.Left'  , Ctrl.Left);
    Ctrl.Width := ReadInteger (Ctrl.Owner.Name, Ctrl.Name+ '.Width' , Ctrl.Width);
    Ctrl.Height:= ReadInteger (Ctrl.Owner.Name, Ctrl.Name+ '.Height', Ctrl.Height);
  end;
end;






{-----------------------------------------------------------------------------------------------------------------------
   READ/WRITE INDIVIDUAL CTRLS
-----------------------------------------------------------------------------------------------------------------------}
//ToDo: here make an array of supported controls (class references?) then go with InheritsFrom through the list
function TCubicIniFileVcl.WriteComp(Comp: TComponent): Boolean;                                                             { Write 'any' control to INI file }
VAR s: String;
begin
 if Comp.Name = '' then
  begin
   s:= 'TCubicIniFileEx. The control has no name! Class: '+ Comp.ClassName;
   if (Comp.InheritsFrom(TControl))
   AND ((Comp as TControl).Parent <> NIL)
   then s:= s+ '. Parent: '+ (Comp as TControl).Parent.Name;
   RAISE Exception.Create(s);
  end;

 Result:= TRUE;

 if Comp.InheritsFrom(TForm)
 then
  begin
   WriteCtrlPos (TControl(Comp));
   inherited Write('FormFont', TForm(Comp).Font);
   if Comp = Application.MainForm
   then WriteString(Comp.Name, 'LastUsedFolder', AppData.LastUsedFolder);
  end
 else

  if Comp.InheritsFrom(TAction)
  then WriteBool    (Comp.Owner.Name, Comp.Name, TAction(Comp).Checked) else

  if Comp.InheritsFrom(TCheckBox)
  then WriteInteger (Comp.Owner.Name, Comp.Name, Ord(TCheckBox(Comp).State)) else

  if Comp.InheritsFrom(TRadioButton)
  then WriteBool    (Comp.Owner.Name, Comp.Name, TRadioButton(Comp).Checked) else

  if Comp.InheritsFrom(TCustomComboBox)
  then WriteInteger (Comp.Owner.Name, Comp.Name, TCustomComboBox(Comp).ItemIndex) else

  if Comp.InheritsFrom(TNumberBox)
  then WriteFloat   (Comp.Owner.Name, Comp.Name, TNumberBox(Comp).Value) else

  if Comp.InheritsFrom(TCustomLabeledEdit)
  then WriteString  (Comp.Owner.Name, Comp.Name, TCustomLabeledEdit(Comp).Text) else

  if Comp.InheritsFrom(TLabeledEdit)
  then WriteString  (Comp.Owner.Name, Comp.Name, TLabeledEdit(Comp).Text) else      { Do not go down to TCustomEdit because I will accidentaly read the RichLog which is derived from there! }

  if Comp.InheritsFrom(TEdit)
  then WriteString  (Comp.Owner.Name, Comp.Name, TEdit(Comp).Text) else      { Do not go down to TCustomEdit because I will accidentaly read the RichLog which is derived from there! }

  if Comp.InheritsFrom(TSpinEdit)
  then WriteInteger (Comp.Owner.Name, Comp.Name, TSpinEdit(Comp).Value) else    { Details about InheritsFrom: http://stackoverflow.com/questions/5255341/how-can-i-determine-whether-a-delphi-object-is-of-a-specific-class-and-not-any-d }

  if Comp.InheritsFrom(TPageControl)
  then WriteInteger (Comp.Owner.Name, Comp.Name, TPageControl(Comp).ActivePageIndex) else

  if Comp.InheritsFrom(THotKey)
  then WriteInteger (Comp.Owner.Name, Comp.Name, THotKey(Comp).HotKey) else

  if Comp.InheritsFrom(TRadioGroup)
  then WriteInteger (Comp.Owner.Name, Comp.Name, TRadioGroup(Comp).ItemIndex) else

  if Comp.InheritsFrom(TTrackBar)
  then WriteInteger (Comp.Owner.Name, Comp.Name, Ttrackbar(Comp).Position) else

  if Comp.InheritsFrom(TShape)
  then WriteColor  (Comp.Name, TShape(Comp).Brush.Color) else

  if Comp.InheritsFrom(TColorDialog)
  then WriteColor  (Comp.Name, TColorDialog(Comp).Color) else

  if Comp.InheritsFrom(TSplitter)
  then WriteSplitter(Comp) else

  if Comp.InheritsFrom(TFileListBox)
  then WriteString (Comp.Owner.Name, Comp.Name, TFileListBox(Comp).FileName) else

  if Comp.InheritsFrom(TScrollBar)
  then WriteInteger (Comp.Owner.Name, Comp.Name, TScrollBar(Comp).Position) else

  if Comp.InheritsFrom(TMenuItem)
  then WriteBool (Comp.Owner.Name, Comp.Name, TMenuItem(Comp).Checked) else

  if Comp.InheritsFrom(TToggleSwitch)
  then WriteBool (Comp.Owner.Name, Comp.Name, TToggleSwitch(Comp).State= tssOn) else

  if Comp.InheritsFrom(TOpenDialog)
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
   end else

  if Comp.InheritsFrom(TFontDialog)
  then inherited Write (Comp.Name, TFontDialog(Comp).Font) else

  if Comp.InheritsFrom(TDirectoryListBox)
  then WriteString(Comp.Owner.Name, Comp.Name, TDirectoryListBox(Comp).Directory)
  else
      Result:= FALSE; //RAISE Exception.Create('Unsupported control: '+ Comp.ClassName+ ', '+ Comp.Name);
end;



{Important:
    The cvRadioButton/cvRadioButton will NOT be automatically resized if you call LoadForm(self) in FormCreate (canvas not ready). You need to call LoadForm(self) in LateInitialize. }
function TCubicIniFileVcl.ReadComp(Comp: TComponent): Boolean;
VAR s: string;
begin
 Assert(Comp.Name > '', 'TCubicIniFileEx-The control has no name! Class: '+ Comp.ClassName);

 Result:= TRUE;

 { Read form }
 if Comp.InheritsFrom(TForm)
 then
   begin
     if  (TForm(Comp).Menu <> NIL)
     AND (TForm(Comp).BorderStyle = bsDialog)    { Just a check up: The main menu is not shown if form is set to bsDialog. Note! The menu will still appear if skins are applyed to this form }
     then MesajWarning(Comp.Name+ CRLF+ 'The main menu will not appear because the form is set to bsDialog!');

     ReadCtrlPos(TControl(Comp));
     inherited Read('FormFont', TForm(Comp).Font);

     if Comp = Application.MainForm
     then AppData.LastUsedFolder:= ReadString(Comp.Name, 'LastUsedFolder', '');
   end
 else

  { Read controls }
  if ValueExists(Comp.Owner.Name, Comp.Name) then
   begin
      { Warning! Setting an action's properties won't invoke its events. So, the OnExecute assigned to this action will not be executed. We have to call it manually otherwise (if there is a checkbox assigned to this action) its OnClick event (not the one of the action) will be triggered by gui.  Details: https://stackoverflow.com/questions/46253875/taction-onexecute-is-not-executed }
      if Comp.InheritsFrom(TAction)
      then TAction(Comp).Checked:= ReadBool(Comp.Owner.Name, Comp.Name, False)
      else

      if Comp.InheritsFrom(TCheckBox)
      then TCheckBox (Comp).State := TCheckBoxState (ReadInteger (Comp.Owner.Name, Comp.Name, 0))
      else

      if Comp.InheritsFrom(TRadioButton)
      then
       begin
         { https://stackoverflow.com/questions/47476603/major-flaw-radio-buttons-are-not-correctly-set-while-the-form-is-invisible
           https://community.embarcadero.com/blogs/entry/the-hidden-dangers-of-tradiobuttontabstop-37987
           Reason: A random radiobtn might be clicked when the user navigates with Tab  }
         if TRadioButton(Comp).TabStop
         AND NOT TRadioButton(Comp).Checked  // Delphi will force TabStop to true if the control is checked
         then RAISE Exception.Create('TabStop should not be true for ' + Comp.Name+ ' on '+ TRadioButton(Comp).Parent.Parent.Name);
         TRadioButton (Comp).Checked:= ReadBool (Comp.Owner.Name, Comp.Name, FALSE);
       end
       else

      if Comp.InheritsFrom(TSpinEdit)
      then TSpinEdit (Comp).Value := ReadInteger (Comp.Owner.Name, Comp.Name, 0)
      else

      if Comp.InheritsFrom(TEdit)
      then TEdit (Comp).Text := Self.ReadString (Comp.Owner.Name, Comp.Name, '')
      else { Do not go down to TCustomEdit because I will accidentaly read the RichLog which is derived from there! }

      if Comp.InheritsFrom(TNumberBox)
      then TNumberBox(Comp).Value:= Self.ReadFloat (Comp.Owner.Name, Comp.Name, 0)
      else

      if Comp.InheritsFrom(TLabeledEdit)
      then TLabeledEdit (Comp).Text := Self.ReadString (Comp.Owner.Name, Comp.Name, '')
      else

      if Comp.InheritsFrom(TCustomLabeledEdit)
      then TCustomLabeledEdit(Comp).Text := Self.ReadString (Comp.Owner.Name, Comp.Name, '')
      else

      if Comp.InheritsFrom(TScrollBar)
      then TScrollBar(Comp).Position:= ReadInteger (Comp.Owner.Name, Comp.Name, 0)
      else

      if Comp.InheritsFrom(TPageControl)
      then TPageControl(Comp).ActivePageIndex := ReadInteger(Comp.Owner.Name, Comp.Name, 0)
      else

      if Comp.InheritsFrom(THotKey)
      then THotKey(Comp).HotKey:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)
      else

      if Comp.InheritsFrom(TFilterComboBox)
      then
       begin
         TFilterComboBox(Comp).ItemIndex:= ReadInteger(Comp.Owner.Name, Comp.Name, 0);  // the ItemIndex is changed but the FileList is not informed about the change...
         if TFilterComboBox(Comp).FileList <> Nil
         then TFilterComboBox(Comp).FileList.Mask := TFilterComboBox(Comp).Mask; // ...so we need this
       end
      else

      if Comp.InheritsFrom(TCustomComboBox)
      then TCustomComboBox (Comp).ItemIndex:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)
      else

      if Comp.InheritsFrom(TRadioGroup)
      then TRadioGroup (Comp).ItemIndex:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)
      else

      if Comp.InheritsFrom(TTrackBar)
      then TTrackBar (Comp).Position:= ReadInteger(Comp.Owner.Name, Comp.Name, 0)
      else

      if Comp.InheritsFrom(TSplitter)
      then ReadSplitter (Comp)
      else

      if Comp.InheritsFrom(TMenuItem)
      then TMenuItem(Comp).Checked:= Readbool (Comp.Owner.Name, Comp.Name, FALSE)
      else

      if Comp.InheritsFrom(TToggleSwitch)
      then
        case Readbool (Comp.Owner.Name, Comp.Name, FALSE) of
          TRUE : TToggleSwitch(Comp).State:= tssOn;
          FALSE: TToggleSwitch(Comp).State:= tssOff;
        end
      else

      if Comp.InheritsFrom(TDirectoryListBox)
      then
        if DirectoryExists(Self.ReadString (Comp.Owner.Name, Comp.Name, '') )
        then TDirectoryListBox (Comp).Directory:= Self.ReadString (Comp.Owner.Name, Comp.Name, '')
        else
      else

      if Comp.InheritsFrom(TColorDialog)
      then TColorDialog (Comp).Color := ReadColor(Comp.Name, 0)
      else

      if Comp.InheritsFrom(TOpenDialog) then
       begin
         s:= ReadString (Comp.Owner.Name, Comp.Name, GetMyDocuments);  { NOTE: INI file does not support unicode chars }
         if DirectoryExists(ExtractFilePath(s))
         then TOpenDialog(Comp).InitialDir:= ExtractFilePath(s);
       end
      else

      if Comp.InheritsFrom(TSaveDialog) then
       begin
         s:= ReadString (Comp.Owner.Name, Comp.Name, GetMyDocuments);  { NOTE: INI file does not support unicode chars }
         if DirectoryExists(ExtractFilePath(s))
         then TSaveDialog(Comp).InitialDir:= ExtractFilePath(s);
       end
      else

      if Comp.InheritsFrom(TFontDialog)
      then inherited Read(Comp.Name, TFontDialog(Comp).Font)
      else

      if Comp.InheritsFrom(TShape)
      then TShape (Comp).Brush.Color:= ReadColor(Comp.Name, 0)
      else

         Result:= FALSE; //RAISE Exception.Create('Unsupported control: '+ Comp.ClassName+ ', '+ Comp.Name);
   end;
end;


{ Optimization trick: the most common/used controls are on top of the list. This way we have a higher chance to exit faster the list, without going through all lines }
function TCubicIniFileVcl.IsSupported(WinCtrl: TComponent): Boolean;
begin
 Result:= WinCtrl.InheritsFrom (TRadioButton)
       OR WinCtrl.InheritsFrom (TCheckBox)
       OR WinCtrl.InheritsFrom (TAction)
       OR WinCtrl.InheritsFrom (TSpinEdit)
       OR WinCtrl.InheritsFrom (TEdit)
       OR WinCtrl.InheritsFrom (TNumberBox)
       OR WinCtrl.InheritsFrom (TLabeledEdit)
       OR WinCtrl.InheritsFrom (TMenuItem)
       OR WinCtrl.InheritsFrom (TFilterComboBox)
       OR WinCtrl.InheritsFrom (TCustomLabeledEdit)
       OR WinCtrl.InheritsFrom (TCustomComboBox)
       OR WinCtrl.InheritsFrom (TTrackBar)
       OR WinCtrl.InheritsFrom (TScrollBar)
       OR WinCtrl.InheritsFrom (TDirectoryListBox)
       OR WinCtrl.InheritsFrom (TPageControl)
       OR WinCtrl.InheritsFrom (TRadioGroup)
       OR WinCtrl.InheritsFrom (TSplitter)
       OR WinCtrl.InheritsFrom (TFontDialog)
       OR WinCtrl.InheritsFrom (TOpenDialog)
       OR WinCtrl.InheritsFrom (TSaveDialog)
       OR WinCtrl.InheritsFrom (TColorDialog)
       OR WinCtrl.InheritsFrom (TFrame)   { see: implemented in WriteComponentsOf }
       OR WinCtrl.InheritsFrom (THotKey)
       OR WinCtrl.InheritsFrom (TShape)
       OR WinCtrl.InheritsFrom (TToggleSwitch);
end;









{---------------
   CHILDREN
----------------}
{ Read/write all supported items (checkboxes, radioboxes, etc) found in a panel/groupbox/etc }
procedure TCubicIniFileVcl.WriteGroup(WinCtrl: TWinControl);  { Save/load all  in a groupbox/panel }
VAR i: Integer;
begin
 for i:= 0 to WinCtrl.ControlCount-1 DO
   if IsSupported(WinCtrl)
   then WriteComp(WinCtrl)
end;


{ The section name is automatically retrieved from WinCtrl.Name }
procedure TCubicIniFileVcl.ReadGroup(WinCtrl: TWinControl);
VAR i: Integer;
begin
 for i:= 0 to WinCtrl.ControlCount-1 DO
   if IsSupported(WinCtrl)
   then ReadComp(WinCtrl);
end;

















procedure TCubicIniFileVcl.writeSplitter(Comp: TComponent);
begin
 if (TWinControl(Comp).Align = alLeft)
 OR (TWinControl(Comp).Align = alRight)
 then WriteInteger(Comp.Owner.Name, Comp.Name, TWinControl(Comp).Left)
 else
    if (TWinControl(Comp).Align = alTop)
    OR (TWinControl(Comp).Align = alBottom)
    then WriteInteger(Comp.Owner.Name, Comp.Name, TWinControl(Comp).Top);
end;



procedure TCubicIniFileVcl.readSplitter(Comp: TComponent);
begin
 if (TWinControl(Comp).Align= alLeft)
 OR (TWinControl(Comp).Align= alRight)
 then TWinControl(Comp).Left:= ReadInteger (Comp.Owner.Name, Comp.Name, 0)
 else
    if (TWinControl(Comp).Align= alTop)
    OR (TWinControl(Comp).Align= alBottom)
    then TWinControl(Comp).Top:= ReadInteger (Comp.Owner.Name, Comp.Name, 0)
end;













{-----------------------------------------------------------------------------------------------------------------------
   MAIN

   Load/Save all controls on this form to their initial state.

   Parameters:
         OnlyFormPos=False  ->  Save all supported controls on this form
         OnlyFormPos=True   ->  Save only the position of the form (no width/height/WndState)
-----------------------------------------------------------------------------------------------------------------------}

procedure SaveForm(Form: TForm; OnlyFormPos: Boolean= FALSE);
VAR
   IniFile: TCubicIniFileVcl;
begin
 if TAppData.Initializing
 AND (Form = Application.MainForm) then
  begin
   if TAppData.RunningHome
   then MesajError('Closing application while still initializing!');
   Exit;{ We don't save anything if the start up was improper! }
  end;

 IniFile:= TCubicIniFileVcl.Create(Form.Name);
 TRY
  TRY
   IniFile.SaveForm(Form, OnlyFormPos);
  EXCEPT
   ON EIniFileexception
     DO WinApi.Windows.OutputDebugString( PChar('Cannot save INI file: '+ IniFile.FileName) );
  END;
 FINALLY
   FreeAndNil(IniFile);
 END;
end;


{ Extra:
    * LoadForm will also set the font for all forms to be the same as the font of the MainForm.
    * If the form is out of screen, LoadForm will also bring the form back to screen. }
procedure LoadForm(Form: TForm; OnlyFormPos: Boolean= FALSE);
VAR
   IniFile: TCubicIniFileVcl;
begin
 if Application.MainForm <> NIL
 then Form.Font:= Application.MainForm.Font;

 IniFile:= TCubicIniFileVcl.Create(Form.Name);
 TRY
  TRY
   IniFile.LoadForm(Form, OnlyFormPos);
   CorrectFormPositionScreen(Form);
  EXCEPT
   ON EIniFileException
     DO WinApi.Windows.OutputDebugString( PChar('Cannot load INI file: '+ IniFile.FileName) );  { The log is not available yet here! }
  END;
 FINALLY
   FreeAndNil(IniFile);
 END;
end;






end.

(*==========================================================================


{
procedure TCubicIniFileEx.WriteCheckbox(CONST FSection, Ident: string; State: TCheckBoxState);
begin
 case State of
  cbUnchecked: WriteInteger(FSection, Ident, 0);                                                    { The numbers (0,1,2) corespond to the order of the elements in the TCheckBoxState enumeration and it cannot be changed.
  cbChecked  : WriteInteger(FSection, Ident, 1);
  cbGrayed   : WriteInteger(FSection, Ident, 2);
 end;
end;

function TCubicIniFileEx.ReadCheckbox(CONST FSection, Ident: string; DefaultState: TCheckBoxState): TCheckBoxState;
VAR i: Integer;
begin
 i:= ReadInteger(FSection, Ident, Ord(DefaultState));
 case i of
    0: Result := cbUnchecked;
    1: Result := cbChecked;
    2: Result := cbGrayed;
  else
    RAISE exception.Create('Cannot read checkbox state from INI for '+ Ident);
 end;
end;
 }
