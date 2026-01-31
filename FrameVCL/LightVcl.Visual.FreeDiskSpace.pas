UNIT LightVcl.Visual.FreeDiskSpace;

{=============================================================================================================
   Gabriel Moraru
   2026.01
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------

  Note:
     Don't set 'Parent:= Owner' in constructor.
     http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
     http://stackoverflow.com/questions/56859524/where-to-initialize-subcomponent-parent
=============================================================================================================}

INTERFACE

USES
   System.Classes, System.SysUtils,
   Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics, Vcl.Controls, Vcl.ComCtrls;

TYPE
 TFreeDiskSpace= class(TScrollBox)
  private
    Initialized: Boolean;
    HeaderLabel: TLabel;
    DriveLabels: array of TLabel;
    ProgressBars: array of TProgressBar;
    DriveNumbers: array of Byte;            // Stores the drive number (3=C, 4=D, etc.) for each entry
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function DriveCount: Integer;
 end;


procedure Register;



IMPLEMENTATION
{$R LightVcl.Visual.FreeDiskSpace.res}

USES LightCore, LightCore.Time, LightCore.Types, LightVcl.Common.IO;



{ Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create }
constructor TFreeDiskSpace.Create(AOwner: TComponent);
VAR
  CurDrive: Byte;
  Index: Integer;
begin
 inherited Create(AOwner);

 Width := 300;
 Height:= 100;

 { Create header label - will be freed by the Parent control (Self) }
 HeaderLabel:= TLabel.Create(Self);
 HeaderLabel.Parent:= Self;

 //ToDo: re-create the labels and progress bars when the DriveCount changes (drive removed/added to the system)
 if not (csDesigning in ComponentState) then
  begin
   { First pass: count valid drives. Drive 0=Current, 1=A, 2=B, 3=C, etc. We skip A and B (floppy drives). }
   Index:= 0;
   for CurDrive:= 3 to 26 DO
    if DiskInDrive(CurDrive)        { Note: THIS IS VERY SLOW IF THE DISK IS NOT IN DRIVE! }
    then Inc(Index);

   { Allocate arrays }
   SetLength(DriveLabels, Index);
   SetLength(ProgressBars, Index);
   SetLength(DriveNumbers, Index);

   { Second pass: create controls for each valid drive }
   Index:= 0;
   for CurDrive:= 3 to 26 DO
    if DiskInDrive(CurDrive) then
     begin
      DriveNumbers[Index]:= CurDrive;

      DriveLabels[Index]:= TLabel.Create(Self);
      DriveLabels[Index].Parent:= Self;

      ProgressBars[Index]:= TProgressBar.Create(Self);
      ProgressBars[Index].Parent:= Self;

      Inc(Index);
     end;
  end;
end;


{ CreateWnd can be called more than once: http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html }
procedure TFreeDiskSpace.CreateWnd;
VAR
  i: Integer;
  CurDrive: Byte;
  LastBottom: Integer;
  DriveLetter: string;
begin
 inherited CreateWnd;

 if NOT Initialized then { Make sure we don't call this code twice }
  begin
   Initialized:= TRUE;

   { Configure header label }
   HeaderLabel.Caption:= 'Free disk space:';
   HeaderLabel.Left   := 1;
   HeaderLabel.Top    := 2;
   HeaderLabel.Font.Style:= [fsBold];

   LastBottom:= 20;

   { Configure each drive's label and progress bar }
   if not (csDesigning in ComponentState) then
    for i:= 0 to High(DriveLabels) DO
     begin
      CurDrive:= DriveNumbers[i];
      DriveLetter:= Chr(Ord('A') + CurDrive - 1) + ':';

      { Configure drive label }
      DriveLabels[i].Caption:= DriveLetter;
      DriveLabels[i].Left   := 4;
      DriveLabels[i].Top    := LastBottom + 10;

      { Configure progress bar }
      ProgressBars[i].Top    := LastBottom + 10;
      ProgressBars[i].Left   := 20;
      ProgressBars[i].Height := 14;
      ProgressBars[i].Width  := 270;
      ProgressBars[i].Anchors:= [akLeft, akRight, akTop];
      ProgressBars[i].ShowHint:= TRUE;

      { Calculate and display free space percentage. Guard against DiskSize < 1 to prevent "invalid floating point op" }
      if DiskSize(CurDrive) < 1
      then ProgressBars[i].Position:= 0
      else ProgressBars[i].Position:= Round(DiskFree(CurDrive) * 100 / DiskSize(CurDrive));

      ProgressBars[i].Hint:=
           '[Drive '+ DriveLetter + ']   '
           + Real2Str(DiskFree(CurDrive) / GB, 2) + 'GB of '
           + Real2Str(DiskSize(CurDrive) / GB, 1) + 'GB free.';

      LastBottom:= ProgressBars[i].Top + ProgressBars[i].Height;
     end;
  end;
end;



destructor TFreeDiskSpace.Destroy;
begin
 { Child controls (labels, progressbars) are owned by Self and freed automatically }
 inherited Destroy;
end;


function TFreeDiskSpace.DriveCount: Integer;
begin
 Result:= Length(DriveLabels);
end;








procedure Register;
begin
  RegisterComponents('LightSaber VCL', [TFreeDiskSpace]);
end;


end.
