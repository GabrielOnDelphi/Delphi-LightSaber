UNIT cvEdit;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   See Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Features:
     Added OnPressEnter event - Event is triggered when the user pressed Enter
     If CheckFileExistence=true then the control gets red if the text entered is not an existent file


  Known bug:
    Why does TEdit.OnChange trigger when Ctrl+A is pressed?            http://stackoverflow.com/questions/42230077/why-does-tedit-onchange-trigger-when-ctrla-is-pressed

=============================================================================================================}

INTERFACE

USES
  System.SysUtils, Winapi.Windows, System.Classes, Vcl.StdCtrls, Vcl.Controls, Vcl.Graphics;
  {$WARN GARBAGE OFF}                                                                                               {Silent the: 'W1011 Text after final END' warning }

TYPE
  TCubicEdit = class(TEdit)
   private
     FCheckFileEx: Boolean;   { Make it red if file does not exist }
     FCheckDirEx : Boolean;
     FPressEnter : TNotifyEvent;
   protected
     procedure Change; override;
     procedure KeyPress (VAR Key: Char); override;
   public
     constructor Create(AOwner: TComponent); override;
     procedure UpdateBkgColor;
     procedure SetTextNoEvent(CONST aText: string);
   published
     property OnPressEnter: TNotifyEvent  read FPressEnter  write FPressEnter;
     property CheckFileExistence: Boolean read FCheckFileEx write FCheckFileEx default FALSE;  {TODO 2: on click, recheck the existence of the file/folder }
     property CheckDirExistence : Boolean read FCheckDirEx  write FCheckDirEx  default FALSE;
  end;

procedure Register;

IMPLEMENTATION
USES cGraphUtil;



procedure TCubicEdit.SetTextNoEvent(CONST aText: string);
VAR
   OldOnChange: TNotifyEvent;
begin
  begin
    OldOnChange := OnChange;
    TRY
      OnChange := NIL;
      Text := aText;
    FINALLY
      OnChange:= OldOnChange;
    END;
  end;
end;



constructor TCubicEdit.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FCheckFileEx:= FALSE;
 FCheckDirEx := FALSE;
end;


procedure TCubicEdit.Change;
begin
 UpdateBkgColor;
 inherited;  // if you wont to run standard event handler
end;


procedure TCubicEdit.UpdateBkgColor;
begin
 if CheckFileExistence then
  if FileExists(Text)
  then Color:= clWindow
  else Color:= clRedFade;

 if CheckDirExistence then
  if DirectoryExists(Text)
  then Color:= clWindow
  else Color:= clRedFade;
end;


procedure TCubicEdit.KeyPress(VAR Key: Char);
begin
 inherited;
 if (Ord(Key) = VK_RETURN)
 AND Assigned(FPressEnter)
 then FPressEnter(Self);
end;





procedure Register;
begin
  RegisterComponents('LightSaber', [TCubicEdit]);
end;



end.



 //if csCreating in ControlState then exit;
