UNIT LightVcl.Visual.Edit;

{=============================================================================================================
   2026.03
   www.GabrielMoraru.com
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

TYPE
  TLightEdit = class(TEdit)
   private
     FCheckFileEx: Boolean;   { Make it red if file does not exist }
     FCheckDirEx : Boolean;
     FPressEnter : TNotifyEvent;
     procedure SetCheckFileEx(CONST Value: Boolean);
     procedure SetCheckDirEx (CONST Value: Boolean);
   protected
     procedure Change; override;
     procedure Click; override;
     procedure KeyPress (VAR Key: Char); override;
     procedure Loaded; override;
   public
     constructor Create(AOwner: TComponent); override;
     procedure UpdateBkgColor;
     procedure SetTextNoEvent(CONST aText: string);
   published
     property OnPressEnter: TNotifyEvent  read FPressEnter  write FPressEnter;
     property CheckFileExistence: Boolean read FCheckFileEx write SetCheckFileEx default FALSE;
     property CheckDirExistence : Boolean read FCheckDirEx  write SetCheckDirEx  default FALSE;
  end;

procedure Register;

IMPLEMENTATION
USES LightVcl.Common.Colors;



{---------------------------------------------------------------------------------------------------------------
  Sets the Text property without triggering the OnChange event.
  Useful when programmatically changing Text to avoid cascading events.
---------------------------------------------------------------------------------------------------------------}
procedure TLightEdit.SetTextNoEvent(CONST aText: string);
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



constructor TLightEdit.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FCheckFileEx:= FALSE;
 FCheckDirEx := FALSE;
end;


procedure TLightEdit.Loaded;
begin
 inherited;
 UpdateBkgColor;
end;


procedure TLightEdit.SetCheckFileEx(CONST Value: Boolean);
begin
 if FCheckFileEx <> Value then
   begin
    FCheckFileEx:= Value;
    UpdateBkgColor;
   end;
end;


procedure TLightEdit.SetCheckDirEx(CONST Value: Boolean);
begin
 if FCheckDirEx <> Value then
   begin
    FCheckDirEx:= Value;
    UpdateBkgColor;
   end;
end;


procedure TLightEdit.Change;
begin
 UpdateBkgColor;
 inherited;  // Call inherited to run the standard event handler
end;


{---------------------------------------------------------------------------------------------------------------
  Updates the background color based on file/directory existence.
  Note: Only enable CheckFileExistence OR CheckDirExistence, not both.
        If both are enabled, CheckDirExistence takes precedence (runs last).
---------------------------------------------------------------------------------------------------------------}
procedure TLightEdit.UpdateBkgColor;
begin
 if csLoading in ComponentState then EXIT;

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
  Recheck file/folder existence when the user clicks the edit.
  The file/folder may have been created or deleted since the last text change.
---------------------------------------------------------------------------------------------------------------}
procedure TLightEdit.Click;
begin
  inherited Click;
  UpdateBkgColor;
end;


{---------------------------------------------------------------------------------------------------------------
  Intercepts key presses to detect Enter key and fire OnPressEnter event.
---------------------------------------------------------------------------------------------------------------}
procedure TLightEdit.KeyPress(VAR Key: Char);
begin
 inherited;
 if (Ord(Key) = VK_RETURN)
 AND Assigned(FPressEnter)
 then FPressEnter(Self);
end;





procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TLightEdit]);
end;



end.
