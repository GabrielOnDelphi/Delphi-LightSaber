UNIT MainForm2;

{=============================================================================================================
   2025.03.16
   www.GabrielMoraru.com
--------------------------------------------------------------------------------------------------------------
   This demonstrates how to save/load a simple object from a binary file, using versioning.
=============================================================================================================}


INTERFACE

USES
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls, Vcl.Samples.Spin,
  LightVcl.Common.AppDataForm, LightCore.StreamBuff, uSoldier_v2;

type
  TfrmMain = class(TLightForm)
    Label1 : TLabel;
    btnSave: TButton;
    btnLoad: TButton;
    spnLife: TSpinEdit;
    edtName: TLabeledEdit;
    Label2 : TLabel;
    procedure FormCreate  (Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormDestroy (Sender: TObject);
  private
    Soldier2: TSoldier2;
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

  Soldier2:= TSoldier2.Create;

  //btnSaveClick(Sender);
end;


procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Soldier2.Free;
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
  Soldier2.Life:= spnLife.Value;
  Soldier2.Name:= edtName.Text;
  Soldier2.Ammo:= 3;

  // Prepare binary file
  VAR Stream:= TLightStream.CreateWrite(GetBinaryFileName);
  TRY
    Soldier2.Save(Stream);  // Write Soldier to binary file
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  // Prepare binary file
  VAR Stream:= TLightStream.CreateRead(GetBinaryFileName);
  TRY
    Soldier2.Load(Stream); // Read Soldier from binary file
    Caption:= Soldier2.ShowVersion;
  FINALLY
    FreeAndNil(Stream);
  END;

  // Put data back into the GUI
  spnLife.Value := Soldier2.Life;
  edtName.Text  := Soldier2.Name;
end;



end.
