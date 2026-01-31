UNIT LightVcl.Visual.Edit;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
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
USES LightVcl.Common.Colors;



{---------------------------------------------------------------------------------------------------------------
  Sets the Text property without triggering the OnChange event.
  Useful when programmatically changing Text to avoid cascading events.
---------------------------------------------------------------------------------------------------------------}
procedure TCubicEdit.SetTextNoEvent(CONST aText: string);
VAR
   OldOnChange: TNotifyEvent;
begin
  OldOnChange:= OnChange;
  TRY
    OnChange:= NIL;
    Text:= aText;
  FINALLY
    OnChange:= OldOnChange;
  END;
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
 inherited;  // Call inherited to run the standard event handler
end;


{---------------------------------------------------------------------------------------------------------------
  Updates the background color based on file/directory existence.
  Note: Only enable CheckFileExistence OR CheckDirExistence, not both.
        If both are enabled, CheckDirExistence takes precedence (runs last).
---------------------------------------------------------------------------------------------------------------}
procedure TCubicEdit.UpdateBkgColor;
begin
 if CheckFileExistence then
   if (Text = '') OR FileExists(Text)
   then Color:= clWindow
   else Color:= clRedFade;

 if CheckDirExistence then
   if (Text = '') OR DirectoryExists(Text)
   then Color:= clWindow
   else Color:= clRedFade;
end;


{---------------------------------------------------------------------------------------------------------------
  Intercepts key presses to detect Enter key and fire OnPressEnter event.
---------------------------------------------------------------------------------------------------------------}
procedure TCubicEdit.KeyPress(VAR Key: Char);
begin
 inherited;
 if (Ord(Key) = VK_RETURN)
 AND Assigned(FPressEnter)
 then FPressEnter(Self);
end;





procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TCubicEdit]);
end;



end.
