unit ObjectExtensions_NonIntrusive;
interface
uses
  SysUtils;

{ Assign the AExtension to AObject. (extends AObject with AExtension) }
procedure ExtendObject(const AObject: TObject; const AExtension: TObject);

{ Retrieves the extension object assigned to another object. }
function GetObjectExtension(const AObject: TObject): TObject;

{ Removes the extension from an object and returns it }
function RemoveObjectExtension(const AObject: TObject): TObject;

implementation

type
  { .. Sync Object }
  PHackedSyncObject = ^THackedSyncObject;
  THackedSyncObject = record
    FRealHandle: Pointer;
    FLinkedObject: TObject;
  end;

var
  { Old, sys-utils introduced monitor support }
  OldMonitorSupport: PMonitorSupport;

{ Hacked monitor support routines }

function HackNewSyncObj: Pointer;
var
  SyncObject: PHackedSyncObject;
begin
  { Create a hacked monitor }
  New(SyncObject);

  { And assign a real monitor value to it }
  SyncObject^.FRealHandle := OldMonitorSupport^.NewSyncObject();
  SyncObject^.FLinkedObject := nil;

  Result := SyncObject;
end;

procedure HackFreeSyncObj(SyncObject: Pointer);
begin
  { Free the enclosed handle }
  OldMonitorSupport^.FreeSyncObject(PHackedSyncObject(SyncObject)^.FRealHandle);

  { Free enclosed extension object }
  PHackedSyncObject(SyncObject)^.FLinkedObject.Free;

  { Free the memory of the record }
  Dispose(PHackedSyncObject(SyncObject));
end;

function HackNewWaitObj: Pointer;
var
  WaitObject: PPointer;
begin
  { Create a hacked monitor }
  New(WaitObject);

  { And assign a real monitor value to it }
  WaitObject^ := OldMonitorSupport^.NewWaitObject();
  Result := WaitObject;
end;

procedure HackFreeWaitObj(WaitObject: Pointer);
begin
  { Free the enclosed handle }
  OldMonitorSupport^.FreeWaitObject(PPointer(WaitObject)^);

  { Free the memory of the record }
  Dispose(WaitObject);
end;

function HackWaitAndOrSignalObj(SignalObject, WaitObject: Pointer; Timeout: Cardinal): Cardinal;
var
  RealSignalObject, RealWaitObject: Pointer;
begin
  { Get the handle to the signal object }
  if SignalObject <> nil then
    RealSignalObject := PPointer(SignalObject)^
  else
    RealSignalObject := nil;

  { Get the handle to the wait object }
  if WaitObject <> nil then
    RealWaitObject := PPointer(WaitObject)^
  else
    RealWaitObject := nil;

  { Call the real function }
  Result := OldMonitorSupport^.WaitAndOrSignalObject(RealSignalObject, RealWaitObject, Timeout);
end;

const
  { New hacked monitor support }
  HackMonitorSupport: TMonitorSupport = (
    NewSyncObject: HackNewSyncObj;
    FreeSyncObject: HackFreeSyncObj;
    NewWaitObject: HackNewWaitObj;
    FreeWaitObject: HackFreeWaitObj;
    WaitAndOrSignalObject: HackWaitAndOrSignalObj;
  );

type
  TMonitorHelper = record helper for TMonitor
    function InjectedGetMonitor(const AObject: TObject): Pointer;
    function InjectedGetEvent(): Pointer;
  end;

{ TMonitorHelper }

function TMonitorHelper.InjectedGetMonitor(const AObject: TObject): Pointer;
begin
  Result := Self.GetMonitor(AObject);
end;

function TMonitorHelper.InjectedGetEvent(): Pointer;
begin
  Result := Self.GetEvent();
end;

function GetHackedSyncObject(const AObject: TObject): PHackedSyncObject;
var
  M: PMonitor;
begin
  M := M^.InjectedGetMonitor(AObject);
  Result := PHackedSyncObject(M^.InjectedGetEvent());
end;

{
  *************************************
    FUNCTIONS THAT DO THE EXTENSIONS
  *************************************
}

procedure ExtendObject(const AObject: TObject; const AExtension: TObject);
begin
  if AObject = nil then
    raise EArgumentException.Create('AObject is nil!');

  GetHackedSyncObject(AObject)^.FLinkedObject.Free;
  GetHackedSyncObject(AObject)^.FLinkedObject := AExtension;
end;

function GetObjectExtension(const AObject: TObject): TObject;
begin
  if AObject = nil then
    raise EArgumentException.Create('AObject is nil!');

  Result := GetHackedSyncObject(AObject)^.FLinkedObject;
end;

function RemoveObjectExtension(const AObject: TObject): TObject;
begin
  if AObject = nil then
    raise EArgumentException.Create('AObject is nil!');

  Result := GetHackedSyncObject(AObject)^.FLinkedObject;
  GetHackedSyncObject(AObject)^.FLinkedObject := nil;
end;

initialization
  { Obtain the old monitor support block }
  OldMonitorSupport := System.MonitorSupport;

  { And register the new one }
  System.MonitorSupport := @HackMonitorSupport;
end.
