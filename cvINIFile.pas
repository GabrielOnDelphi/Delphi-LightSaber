UNIT cvIniFile;

{=============================================================================================================
   Gabriel Moraru
   2024.09
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Extension of TIniFileApp.
  Same functionality as TIniFileApp but it adds support for own VCL controls (like cvPathEdit)

  Important:
     The cvRadioButton/cvCheckbox will NOT be automatically resized if you call LoadForm(self) in FormCreate (canvas not ready).
     You need to call LoadForm(self) in after the form was fully created.
=======================================================================================================================}

INTERFACE

USES
   System.Classes, System.SysUtils, System.IniFiles, Vcl.Forms, Vcl.Controls,
   cbDialogs, cbINIFile, ccINIFile;

{$WARN DUPLICATE_CTOR_DTOR OFF}                                                                               {Silence the: W1029 Duplicate constructor  with identical parameters will be inacessible from C++ }

TYPE
 TIniFileVCL = class(TIniFileApp)   
  public
    function IsSupported(WinCtrl: TComponent): Boolean; override;

    function WriteComp(Comp: TComponent): Boolean; override;
    function ReadComp (Comp: TComponent): Boolean; override;
  end;

{ These add support for custom (cubic) VCL controls }
procedure SaveForm (Form: TForm; Loading: TFormLoading= flPosOnly);
procedure LoadForm (Form: TForm; Loading: TFormLoading= flPosOnly);


IMPLEMENTATION

USES
   cvFloatSpinEdit, cvFileListBox, cvSpinEdit, cvSpinEditDelayed, cvPathEdit,
   cbCenterControl, cbAppData, cvLog, cbLogUtils;





{-----------------------------------------------------------------------------------------------------------------------
   READ/WRITE INDIVIDUAL CTRLS
   We handle here components of LightSaber libbbrary.
   Classic VCL compoents are handled by "inherided"
-----------------------------------------------------------------------------------------------------------------------}
function TIniFileVCL.WriteComp(Comp: TComponent): Boolean;                                                             { Write 'any' control to INI file }
begin
  Result:= inherited WriteComp(Comp);
  if Result then EXIT; { We handled this component in the parent class. Nothing to do here. }

  if Comp.InheritsFrom(TCubicPathEdit)
  then WriteString(Comp.Owner.Name, Comp.Name, TCubicPathEdit(Comp).Path) else

  if Comp.InheritsFrom(TCubicSpinEditSplit)
  then WriteInteger (Comp.Owner.Name, Comp.Name, TCubicSpinEditSplit(Comp).Value) else

  if Comp.InheritsFrom(TFloatSpinEdit)
  then WriteFloat(Comp.Owner.Name, Comp.Name, TFloatSpinEdit(Comp).Value) else

  if Comp.InheritsFrom(TLogGrid)     { This MUST be before InheritsFrom(TTrackBar) }
  then WriteInteger(Comp.Owner.Name, Comp.Name, ord(TLogGrid(Comp).Verbosity)) else

  if Comp.InheritsFrom(TCubicFileList)
  then WriteString(Comp.Owner.Name, Comp.Name, TCubicFileList(Comp).SelectedItem)

  else
      RAISE Exception.Create('Unsupported control: '+ Comp.ClassName+ ', '+ Comp.Name);
end;



{Important:
    The cvRadioButton/cvRadioButton will NOT be automatically resized if you call LoadForm(self) in FormCreate (canvas not ready). You need to call LoadForm(self) in LateInitialize. }
function TIniFileVCL.ReadComp(Comp: TComponent): Boolean;
VAR s: string;
begin
  Assert(Comp.Name > '', 'TIniFileVCL-The control has no name! Class: '+ Comp.ClassName);
  Result:= inherited ReadComp(Comp);
  if Result then EXIT; { We handled this component in the parent class. Nothing to do here. }

  { Read controls }
  if ValueExists(Comp.Owner.Name, Comp.Name) then
   begin
     if Comp.InheritsFrom(TCubicSpinEditD)
     then TCubicSpinEditD(Comp).StopTimer   { Stop the event from triggering some seconds later after the value of this spinbox was loaded from the INI file }
     else

     if Comp.InheritsFrom(TLogGrid)     { This MUST be before InheritsFrom(TTrackBar) }
     then TLogGrid(Comp).Verbosity:= TLogVerbLvl(ReadInteger(Comp.Owner.Name, Comp.Name, 0))
     else

     if Comp.InheritsFrom(TFloatSpinEdit)
     then TFloatSpinEdit (Comp).Value := ReadFloat(Comp.Owner.Name, Comp.Name, 0)
     else

     if Comp.InheritsFrom(TCubicSpinEditSplit)
     then TCubicSpinEditSplit (Comp).Value := ReadInteger(Comp.Owner.Name, Comp.Name, 0)
     else

     if Comp.InheritsFrom(TCubicPathEdit)
     then TCubicPathEdit (Comp).Path := Self.ReadString (Comp.Owner.Name, Comp.Name, AppData.CurFolder)
     else

     {NOTE! The last item will be selected only if the TCubicPathEdit associated with this component was read from INI before this. For this, make sure that the TCubicPathEdit appears in the DFM before this component. A simply cut and paste for this component (in form designed) is enough. }
     if Comp.InheritsFrom(TCubicFileList)
     then
      begin
       s:= ReadString (Comp.Owner.Name, Comp.Name, '');
       if FileExists(s)
       then TCubicFileList (Comp).SelectItem(s)
      end
     else
       RAISE exception.Create('Unsupported control: '+ Comp.ClassName+ ', '+ Comp.Name);
   end;
end;


{ Optimization trick: the most common/used controls are on top of the list. This way we have a higher chance to exit faster the list, without going through all lines }
function TIniFileVCL.IsSupported(WinCtrl: TComponent): Boolean;
begin
 Result:= inherited IsSupported(WinCtrl)
       OR WinCtrl.InheritsFrom (TCubicSpinEditSplit)
       OR WinCtrl.InheritsFrom (TCubicPathEdit)
       OR WinCtrl.InheritsFrom (TFloatSpinEdit)
       OR WinCtrl.InheritsFrom (TCubicFileList)
       OR WinCtrl.InheritsFrom (TCubicSpinEditD)
       OR WinCtrl.InheritsFrom (TLogGrid)  //Note: The "master" of the verbosity events is the Grid not the trackbar
       ;
end;











{-----------------------------------------------------------------------------------------------------------------------
   MAIN

   Load/Save all controls on this form to their initial state.

   Parameters:
         OnlyFormPos=False  ->  Save all supported controls on this form
         OnlyFormPos=True   ->  It will only save the position of the form (only Left/Top, no width/height/WndState)
-----------------------------------------------------------------------------------------------------------------------}

procedure SaveForm(Form: TForm; Loading: TFormLoading= flPosOnly);
VAR
   IniFile: TIniFileVCL;
begin
 if TAppData.Initializing
 AND (Form = Application.MainForm) then
  begin
   if TAppData.RunningHome
   then MesajError('Closing application while still initializing!');
   Exit; // We don't save anything if the start up was improper!
  end;

 Assert(AppData <> NIL, '!!!');
 IniFile:= TIniFileVCL.Create(Form.Name);
 TRY
  TRY
    IniFile.SaveForm(Form, Loading);
  EXCEPT
    ON EIniFileexception DO
      if AppData <> NIL
      then AppData.LogWarn('Cannot save INI file: '+ IniFile.FileName);
  END;
 FINALLY
   FreeAndNil(IniFile);
 END;
end;


{ It also does:
    * LoadForm will also set the font for all forms to be the same as the font of the MainForm.
    * If the form is out of screen, LoadForm will also bring the form back to screen. }
procedure LoadForm(Form: TForm; Loading: TFormLoading= flPosOnly);
VAR
   IniFile: TIniFileVCL;
begin
 if AppData = NIL then                { If AppData exists, let it deal with the font }
   if (Application.MainForm <> NIL)     { Set font only for secondary forms }
   AND (Form <> Application.MainForm)
   then Form.Font:= Application.MainForm.Font;

 IniFile:= TIniFileVCL.Create(Form.Name);
 TRY
  TRY
    IniFile.LoadForm(Form, Loading);
    CorrectFormPositionScreen(Form);
  EXCEPT
    ON EIniFileException DO
      if appdata <> NIL
      then appdata.LogWarn('Cannot load INI file: '+ IniFile.FileName);
  END;
 FINALLY
   FreeAndNil(IniFile);
 END;
end;






end.
