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
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls, Vcl.Samples.Spin,
  LightVcl.Visual.AppDataForm, LightCore.StreamBuff, uSoldier_v1;

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
    Soldier: TSoldier;
  public
  end;


IMPLEMENTATION {$R *.dfm}

USES
  LightCore.AppData, LightVcl.Visual.AppData;


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
  Result:= Appdata.AppFolder+ MyFile;  // The files is saved where the EXE file is.
end;


procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  // Read data from GUI
  Soldier.Life:= spnLife.Value;
  Soldier.Name:= edtName.Text;
  Soldier.Ammo:= 3;

  // Prepare binary file
  VAR Stream:= TLightStream.CreateWrite(GetBinaryFileName);
  TRY
    Soldier.Save(Stream);  // Write Soldier to binary file
  FINALLY
    FreeAndNil(Stream);
  END;
end;


procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  // Prepare binary file
  VAR Stream:= TLightStream.CreateRead(GetBinaryFileName);
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
