UNIT cvFreeDiskSpace;

{=============================================================================================================
   Gabriel Moraru
   2024.05
   www.GabrielMoraru.com
   See Copyright file
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
    LastBottom: Integer;
  protected
    procedure CreateWnd; override;
  public
    progres : TProgressBar;
    DrvLabel: TLabel;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
 end;


procedure Register;



IMPLEMENTATION
{$R cvFreeDiskSpace.res}

USES ccCore, LightCom.IO;



constructor TFreeDiskSpace.Create(AOwner: TComponent);  // Note: Don't set 'Parent:= Owner' in constructor. Details: http://stackoverflow.com/questions/6403217/how-to-set-a-tcustomcontrols-parent-in-create
VAR
  CurDrive: Byte;
begin
 inherited Create(AOwner);

 Width := 300;
 Height:= 100;
 //Disks:= ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];
 DrvLabel:= TLabel.create(self);                                                           { It will be freed by the Parent control }
 //DrvLabel.SetSubComponent(True);
 DrvLabel.Parent := Self;     // Here I can set the parent
 LastBottom:= 20;

 //ToDo: re-create the drvLabel and Progress when the DriveCount changes (drive removed/added to the system)
 if not (csDesigning in ComponentState) then
   for CurDrive:= 3 to 26 DO                                                                         { 26 litere in alfabet. 0=Current, 1=A, 2=B, 3=C, etc. Drive A si B (drive 0 and 1) nu ma intereseaza }
    {DEL - se apre ca merge si fara ea - if ValidDrive(Drive2Char(CurDrive)) AND }
    if DiskInDrive(CurDrive) then                                                                    { %%%% THIS IS VERY SLOW IF THE DISK IS NOT IN DRIVE !!!!!! }
     begin
      DrvLabel        := TLabel.Create(self);
      DrvLabel.Parent := Self;               // Here I can set the parent
      progres         := TProgressBar.Create(Self);
      progres.Parent  := Self;
     end;
end;


//CreateWnd can be called more than once:  http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Controls_TWinControl_CreateWnd.html
procedure TFreeDiskSpace.CreateWnd;
VAR
  CurDrive: Byte;
begin
 inherited CreateWnd;

 if NOT Initialized then { Make sure we don't call this code twice }
  begin
   Initialized:= TRUE;

   DrvLabel.Caption:= 'Free disk space:';
   DrvLabel.Left   := 1;
   DrvLabel.Top    := 2;
   DrvLabel.Font.Style:= [fsBold];

   if not (csDesigning in ComponentState) then
     for CurDrive:= 3 to 26 DO                                                                         { 26 litere in alfabet. 0=Current, 1=A, 2=B, 3=C, etc. Drive A si B (drive 0 and 1) nu ma intereseaza }
      {DEL - se apre ca merge si fara ea - if ValidDrive(Drive2Char(CurDrive)) AND }
      if DiskInDrive(CurDrive) then                                                                    { %%%% THIS IS VERY SLOW IF THE DISK IS NOT IN DRIVE !!!!!! }
       begin
        DrvLabel.Caption:= Chr(Ord('A')+ CurDrive-1) + ':';
        DrvLabel.Left   := 4;
        DrvLabel.Top    := LastBottom+ 10;

        progres.Top     := LastBottom+ 10;
        progres.Left    := 20;
        progres.Height  := 14;
        progres.Width   := 270;
        if DiskSize(CurDrive)< 1                                                                       { To prevent "invalid floating point op". Am avut un bug report dasta aici. }
        then progres.Position:= 0
        else progres.Position:= round( DiskFree(CurDrive) * 100/DiskSize(CurDrive) );
        progres.Anchors := [akLeft, akRight, akTop];
        progres.Hint:=
             '[Drive '+ DrvLabel.Caption+ ']   '
             + Real2Str( DiskFree(CurDrive)/GB, 2)+ 'GB of '
             + Real2Str( DiskSize(CurDrive)/GB, 1)+ 'GB free.';

        progres.ShowHint:= TRUE;
        LastBottom:= progres.Top+ progres.Height;
       end;
  end;
end;



destructor TFreeDiskSpace.Destroy;
begin
 inherited Destroy;                                                                                {I don't need to free all created progressbars because they are owned (and destroied) by the Parent (Self) }
end;








procedure Register;
begin
  RegisterComponents('LightSaber', [TFreeDiskSpace]);
end;


end.
