unit Future;
interface
uses SysUtils, Classes;

type
  TAsyncEval<T> = reference to function() : T;

  TFuture<T> = record
  private
  type
    TFutureThread = class(TThread)
    private
      FAsyncEval: TAsyncEval<T>;
      FResult   : T;
      FExc      : Exception;

    protected
      constructor Create(const AsyncEval: TAsyncEval<T>);

      procedure Execute(); override;
    end;

  var
    FIIntf  : IInterface;
    FThread : TFutureThread;
    FResult : T;
    FExc    : Exception;

    function GetFuture: T;
  public
    constructor Create(const AsyncEval: TAsyncEval<T>);

    property Value : T read GetFuture;
  end;

implementation


{ TFuture<T>.TFutureThread }

constructor TFuture<T>.TFutureThread.Create(const AsyncEval: TAsyncEval<T>);
begin
  { Assign the function reference }
  FAsyncEval := AsyncEval;
  FExc := nil;

  { And call the prev constructor }
  inherited Create(false);
end;

procedure TFuture<T>.TFutureThread.Execute;
begin
  { Simply execute the code and retreive the value }
  try
    FResult := FAsyncEval();
  except
    { Record any exceptions for later raising }
    on Ex : Exception do
       FExc := Ex;
  end;
end;

{ TFuture<T> }

constructor TFuture<T>.Create(const AsyncEval: TAsyncEval<T>);
begin
  { Initialize an interface! -- it's a trick to determine is a value is
    actually initialized }
  FIIntf := TInterfacedObject.Create();
  { Construct a new thread and start it }
  FThread := TFutureThread.Create(AsyncEval);
end;

function TFuture<T>.GetFuture: T;
begin
  if FIIntf = nil then
     raise EInvalidOperation.Create('Future value not initialized!');

  { Wait for the thread to terminate before we can retreive the value }
  if (FThread <> nil) then
  begin
    { Wait for thread termination }
    if (not FThread.Terminated) then
       FThread.WaitFor;

    { Copy the result locally }
    FResult := FThread.FResult;
    FExc := FThread.FExc;

    { Kill the object }
    FreeAndNil(FThread);
  end;

  { Throw the exception if there was one }
  if FExc <> nil then
     raise FExc;

  { Retreive the value }
  Result := FResult;
end;

end.
