UNIT MainForm1;

{=============================================================================================================
   2025.03.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to save an simple object to a binary file.
=============================================================================================================}

INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls, Vcl.Samples.Spin,
  LightVcl.Common.AppDataForm, LightCore.StreamBuff, LightCore.StreamBuff2, uSoldier_v1;

type
  TfrmMain = class(TLightForm)
    Label1 : TLabel;
    btnSave: TButton;
    ntnLoad: TButton;
    spnLife: TSpinEdit;
    edtName: TLabeledEdit;
    Label2 : TLabel;
    procedure FormCreate  (Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure ntnLoadClick(Sender: TObject);
    procedure FormDestroy (Sender: TObject);
  private
    Soldier: TSoldier;
  public
  end;


IMPLEMENTATION {$R *.dfm}

USES
  LightCore.AppData, LightVcl.Common.AppData;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if AppData.RunningFirstTime then
   begin
    spnLife.Value := 100;
    edtName.Text  := 'Ryan';
   end;

  Soldier:= TSoldier.Create;

  //btnSaveClick(Sender);
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Soldier.Free;
end;






function GetBinaryFileName: string;
CONST
  MyFile= 'Soldier.bin';
begin
  Result:= AppData.ExeFolder+ MyFile;  // The files is saved where the EXE file is.
end;


procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  // Read data from GUI
  Soldier.Life:= spnLife.Value;
  Soldier.Name:= edtName.Text;
  Soldier.Ammo:= 3;

  // Prepare binary file
  VAR Stream:= TCubicBuffStream2.CreateWrite(GetBinaryFileName);
  TRY
    Soldier.Save(Stream);  // Write Soldier to binary file
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TfrmMain.ntnLoadClick(Sender: TObject);
begin
  // Prepare binary file
  VAR Stream:= TCubicBuffStream2.CreateRead(GetBinaryFileName);
  TRY
    Soldier.Load(Stream); // Read Soldier from binary file
  FINALLY
    FreeAndNil(Stream);
  END;

  // Put data back into the GUI
  spnLife.Value := Soldier.Life;
  edtName.Text  := Soldier.Name;
end;



end.
