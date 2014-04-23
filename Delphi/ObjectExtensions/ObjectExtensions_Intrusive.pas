unit ObjectExtensions_Intrusive;
interface

{ Extends a given object }
procedure ExtendObject(const AObject, AExtension: TObject);

{ Retrieves the extension object associated with another object }
function GetObjectExtension(const AObject: TObject): TObject;

{ Removes the extension and returns it }
function RemoveObjectExtension(const AObject: TObject): TObject;

implementation
uses
  SysUtils, Windows;

(*
   UTILITY 1: CLASS HELPER THAT ALLOWS OBTAINING ADDRESSES OF THE FUNCTIONS
              WE NEED TO TAP INTO.
*)

var
  { HACK! to allow class helper to work properly }
  M: TMonitor;

type
  PPMonitorWithObject = ^PMonitorWithObject;
  PMonitorWithObject = ^TMonitorWithObject;
  TMonitorWithObject = record
    FMonitor: TMonitor;
    FObject: TObject;
  end;


type
  { Declare a record heler for us }
  TMonitorHackHelper = record helper for TMonitor
    { Define a function to access the internal methods of TMonitor }
    procedure GetMethodAddresses(var _Create, _Destroy: Pointer);
    procedure ContextDestroy(AObject: TObject);
     function GetMonitorInContext(AObject: TObject): PMonitorWithObject;
  end;

{ TMonitorHackHelper }

procedure TMonitorHackHelper.ContextDestroy(AObject: TObject);
var
  MonitorFld: PPMonitorWithObject;
  Monitor: PMonitorWithObject;
begin
  MonitorFld := PPMonitorWithObject(TMonitor.GetFieldAddress(AObject));

  if MonitorFld^ <> nil then
  begin
    Monitor := MonitorFld^;
    MonitorFld^ := nil;

    { Call the object destructor }
    Monitor.FObject.Free;

    { Kill the monitor! }
    if (MonitorSupport <> nil) and (Monitor.FMonitor.FLockEvent <> nil) then
      MonitorSupport.FreeSyncObject(Monitor.FMonitor.FLockEvent);

    { Dispose the whole monitor }
    Dispose(Monitor);
  end;
end;

procedure TMonitorHackHelper.GetMethodAddresses(var _Create, _Destroy: Pointer);
begin
  _Create := @TMonitor.Create;
  _Destroy := @TMonitor.Destroy;
end;

function TMonitorHackHelper.GetMonitorInContext(AObject: TObject): PMonitorWithObject;
begin
  { That's it ... }
  Result := PMonitorWithObject(TMonitor.GetMonitor(AObject));
end;

{ The funtions written by me that will take over the lifetime management }

procedure TMonitorDestroy(AObject: TObject);
begin
  { Call the destroy method in TMonitor context }
  M.ContextDestroy(AObject);
end;

function TMonitorCreate: PMonitorWithObject;
begin
  { Create a resulting object and fill it with zeroes }
  New(Result);
  FillChar(Result^, SizeOf(Result^), 0);
end;

(*
   UTILITY 2: PATHCHING OF THE FUNCTIONS.
*)

function PatchMethod(OldMethod, NewMethod: Pointer): Boolean;
const
  SizeOfJump = 5;

var
  OldFlags : Cardinal;
  __Jump   : ^Byte;
  __Addr   : ^Integer;

begin

  Result := False;

  try
    { Change the code protection to write }
    VirtualProtect(OldMethod, SizeOfJump, PAGE_READWRITE, OldFlags);
  except
    begin Exit; end;
  end;

  try
    { Insert the jump instruction }
    __Jump := OldMethod;
    __Jump^ := $E9;

    { Insert the jump address = (OLD - NEW - SIZEOFJUMP)}
    __Addr := Ptr(Integer(OldMethod) + 1);
    __Addr^ := Integer(NewMethod) - Integer(OldMethod) - SizeOfJump;
  finally
    { Change the protection back to what it was }
    VirtualProtect(OldMethod, SizeOfJump, OldFlags, OldFlags);
  end;

  { Set status to success }
  Result := True;
end;

procedure PatchTMonitorMethods();
var
  _Create, _Destroy: Pointer;
begin
  { Get the addresses of the methods to be patched }
  M.GetMethodAddresses(_Create, _Destroy);

  if PatchMethod(_Create, @TMonitorCreate) then
     PatchMethod(_Destroy, @TMonitorDestroy);
end;

(*
   UTILITY 3: THE ACTUAL CODE!
*)

{ Extends a given object }
procedure ExtendObject(const AObject, AExtension: TObject);
var
  Monitor: PMonitorWithObject;
begin
  if AObject = nil then
    raise EArgumentException.Create('AObject is nil');

  if AExtension = nil then
    raise EArgumentException.Create('AObject is nil');

  { Extact the monitor }
  Monitor := M.GetMonitorInContext(AObject);

  { Free the old object }
  Monitor^.FObject.Free;
  Monitor^.FObject := AObject;
end;

{ Retrieves the extension object associated with another object }
function GetObjectExtension(const AObject: TObject): TObject;
var
  Monitor: PMonitorWithObject;
begin
  if AObject = nil then
    raise EArgumentException.Create('AObject is nil');

  { Extact the monitor }
  Monitor := M.GetMonitorInContext(AObject);

  Result := Monitor^.FObject;
end;

{ Removes the extension and returns it }
function RemoveObjectExtension(const AObject: TObject): TObject;
var
  Monitor: PMonitorWithObject;
begin
  if AObject = nil then
    raise EArgumentException.Create('AObject is nil');

  { Extact the monitor }
  Monitor := M.GetMonitorInContext(AObject);

  { Return and update }
  Result := Monitor^.FObject;
  Monitor^.FObject := nil;
end;


initialization
  { Call the patching routines }
  //PatchTMonitorMethods();

end.
