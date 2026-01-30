UNIT LightVcl.Common.CursorGuard;

{=============================================================================================================
   Gabriel Moraru
   2026.01.30
   www.GabrielMoraru.com
   Github.com/GabrielOnDelphi/Delphi-LightSaber/blob/main/System/Copyright.txt
--------------------------------------------------------------------------------------------------------------
   RAII-style cursor guard that sets the cursor to crHourglass and automatically restores it
   when the interface reference goes out of scope. Eliminates the need for try-finally blocks.

   Usage:
     procedure DoSlowWork;
     begin
       TCursorGuard.CursorBusy;   // Cursor changes to hourglass
       SlowCode;
     end;                         // Cursor automatically restored here when interface is released

   Note: You must capture the result in a local variable or let it be implicitly held by the
   function result. The cursor is restored when the interface reference count reaches zero.

=============================================================================================================}

INTERFACE

USES
   Vcl.Controls, Vcl.Forms;

TYPE
  TCursorGuard = class(TInterfacedObject, IUnknown)
  private
    FOldCursor: TCursor;
    constructor Create;
  public
    destructor Destroy; override;
    class function CursorBusy: IUnknown;
  end;


IMPLEMENTATION


constructor TCursorGuard.Create;
begin
  inherited Create;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
end;


destructor TCursorGuard.Destroy;
begin
  Screen.Cursor := FOldCursor;
  inherited;
end;


class function TCursorGuard.CursorBusy: IUnknown;
begin
  Result := TCursorGuard.Create;
end;


end.
