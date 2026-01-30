UNIT FormRichLog;

{=============================================================================================================
   2026.01.29
   www.GabrielMoraru.com

--------------------------------------------------------------------------------------------------------------
   RICH LOG FORM

   A visual log window based on TRichLog (TRichEdit descendant).
   Displays log messages with different verbosity levels and colors.

   USAGE:
     It is CRITICAL to create the AppData object as soon as the application starts.
     Preferably in the DPR file before creating the main form!

     DPR:
       AppData:= TAppData.Create('MyCollApp');

     OnLateInitialize:
       AppData.Initializing:= False;

     AppData is automatically destroyed by its Finalization section.

   FEATURES:
     - Verbosity-based message filtering (via TRichLogTrckbr trackbar)
     - Auto-show on error/warning (controlled by chkAutoOpen checkbox)
     - Clear log button
     - Container panel for easy reparenting to other forms

   RELATED UNITS:
     - LightVcl.Visual.RichLog.pas - The TRichLog component
     - LightVcl.Visual.RichLogUtils.pas - Verbosity types and colors
     - LightVcl.Visual.RichLogTrack.pas - Verbosity trackbar component
     - LightCore.LogRam.pas - Non-visual RAM-based log (newer implementation)

   TESTER:
     c:\Projects\LightSaber\Demo\LightLog\
=============================================================================================================}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages,
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  LightVcl.Visual.RichLogTrack, LightVcl.Visual.RichLog, LightVcl.Visual.AppDataForm;

TYPE
  TfrmRichLog = class(TLightForm)
    Log        : TRichLog;         // The main log control (TRichEdit descendant)
    Container  : TPanel;           // Container for all controls - enables reparenting
    pnlBottom  : TPanel;           // Bottom panel with controls
    btnClear   : TButton;          // Clear log button
    chkAutoOpen: TCheckBox;        // Auto-show form on error/warning
    trkLogVerb : TRichLogTrckbr;   // Verbosity filter trackbar
    procedure btnClearClick(Sender: TObject);
    procedure LogError(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    { Called after the main form was fully initialized.
      Override to perform post-initialization tasks. }
    procedure FormPostInitialize; override;
  end;


IMPLEMENTATION {$R *.dfm}

USES
  LightVcl.Visual.INIFile,
  LightCore.AppData,
  LightVcl.Visual.AppData;



{--------------------------------------------------------------------------------------------------
   FORM LIFECYCLE
--------------------------------------------------------------------------------------------------}

{ Initializes the form and connects error/warning events to auto-show handler }
procedure TfrmRichLog.FormCreate(Sender: TObject);
begin
  // Connect log events to auto-show the form on errors/warnings
  Log.OnWarn:= LogError;
  Log.OnError:= LogError;
end;


{ Called after the main form is fully initialized.
  Can be used to perform deferred initialization tasks. }
procedure TfrmRichLog.FormPostInitialize;
begin
  inherited FormPostInitialize;
  // Note: LoadForm is called automatically by AppData infrastructure
end;


{ Cleanup when form is destroyed.
  Moves Container back to Self for proper INI file saving. }
procedure TfrmRichLog.FormDestroy(Sender: TObject);
begin
  Assert(AppData <> NIL, 'AppData is gone already!');

  // Move Container back to this form for proper INI saving
  // (in case it was reparented to another form)
  Container.Parent:= Self;

  // Note: SaveForm is called automatically by AppData
end;



{--------------------------------------------------------------------------------------------------
   EVENT HANDLERS
--------------------------------------------------------------------------------------------------}

{ Clears all messages from the log }
procedure TfrmRichLog.btnClearClick(Sender: TObject);
begin
  Log.Clear;
end;


{ Called when an error or warning is added to the log.
  Shows the form if auto-open is enabled. }
procedure TfrmRichLog.LogError(Sender: TObject);
begin
  if chkAutoOpen.Checked
  then Show;
end;


end.
