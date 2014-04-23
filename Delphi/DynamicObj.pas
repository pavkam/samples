unit DynamicObj;
interface
uses SysUtils;

type
  ///  <summary>Exception raised when invoking a method or property is invalid.</summary>
  ///  <remarks>This exception is raised when trying to invoke a method/property with invalid parameters. There are many more
  ///  reasons such an invoke might fail.</remarks>
  EDynamicException = class(Exception);

  ///  <summary>Creates a new <c>Variant</c> wrapper enclosing the given object.</summary>
  ///  <param name="AObject">The object to wrap.</param>
  ///  <param name="AManageLifetime">Specifies whether the wrapper will manage the lifetime of the enclosed object.</param>
  ///  <returns>A new <c>Variant</c> wrapper.</returns>
  ///  <exception cref="SysUtils|EArgumentNilException">If <paramref name="AObject"/> is <c>nil</c>.</exception>
  function AsDynamic(const AObject: TObject; const AManageLifetime: Boolean = True): Variant;

implementation
uses
  Variants,
  VarUtils,
  TypInfo,
  Rtti,
  Generics.Collections;

resourcestring
  SObjectIsNil = 'The provided object is nil. Cannot continue.';
  SConstructorDestructorNotAllowed = 'Method "%s" is a constructor or a destructor which is not allowed!';
  SNotEnoughRtti = 'Not enough RTTI information for the enclosed object.';
  SMethodNotFound = 'Method "%s" was not found in the enclosed object.';
  SMethodNotPublic = 'Method "%s" is not public or published.';
  SArgumentTypeNotSupported = 'Argument %d is not supported!';
  SPropertyNotFound = 'Property "%s" was not found in the enclosed object.';
  SPropertyNotPublic = 'Property "%s" is not public or published.';
  SPropertyNotReadable = 'Property "%s" is not readable.';
  SPropertyNotWritable = 'Property "%s" is not writable.';
  SPropertyTypeNotSupported = 'The type of the "%s" property is not supported!';

type
  TObjectWrapper = class;

  { Interface that will be passed around }
  IObjectWrapper = interface
    function GetSelf(): TObjectWrapper;
  end;

  { The special type that contains RTTI and the original object }
  TObjectWrapper = class(TInterfacedObject, IObjectWrapper)
  private
    FContext: TRttiContext;
    FObject: TObject;
    FType: TRttiInstanceType;
    FManage: Boolean;

    destructor Destroy; override;
    function GetSelf(): TObjectWrapper;
  end;

  { The Variant structure for the wrapper type }
  TWrapperVarData = packed record
    { Var type, will be assigned at runtime }
    VType: TVarType;
    { Reserved stuff }
    Reserved1, Reserved2, Reserved3: Word;
    { A reference to the enclosed dictionary }
    FWrapper: IObjectWrapper;
    { Reserved stuff }
    Reserved4: LongWord;
  end;

  { Manager for our variant type }
  TWrapperVariantType = class(TInvokeableVariantType)
  private
    function TryVarDataToTValue(const AVarData: TVarData; out AValue: TValue): Boolean;
    function TryTValueToVarData(const AValue: TValue; var AVarData: TVarData; const AByRef: Boolean): Boolean;

  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
    function GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string; const Value: TVarData): Boolean; override;
    function DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
  end;

var
  { Our singleton that manages our variant type }
  FSingleton: TWrapperVariantType;

function AsDynamic(const AObject: TObject; const AManageLifetime: Boolean): Variant;
var
  LWrapper: TObjectWrapper;
begin
  if not Assigned(AObject) then
    raise EArgumentNilException.CreateRes(@SObjectIsNil);

  { Clear out the result }
  VarClear(Result);

  with TWrapperVarData(Result) do
  begin
    { Assign the new variant the var type that was allocated for us }
    VType := FSingleton.VarType;

    { Create a new instance of the wrapper object }
    LWrapper := TObjectWrapper.Create();
    LWrapper.FContext := TRttiContext.Create;
    LWrapper.FObject := AObject;
    LWrapper.FManage := AManageLifetime;

    { Get the RTTI }
    LWrapper.FType := TRttiInstanceType(LWrapper.FContext.GetType(AObject.ClassType));

    { Extract the interface reference }
    FWrapper := LWrapper;
  end;
end;

{ TObjectWrapper }

function TObjectWrapper.GetSelf(): TObjectWrapper;
begin
  Exit(Self);
end;

destructor TObjectWrapper.Destroy;
begin
  if FManage then
    FObject.Free;

  inherited;
end;

{ TWrapperVariantType }

procedure TWrapperVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var
  LTemp: TVarData;
  LWrapper: TObjectWrapper;
  LStr: string;
begin
  if Source.VType = VarType then
  begin
    { Only continue if we're invoked for our data type }
    LWrapper := TWrapperVarData(Source).FWrapper.GetSelf();

    { Initilize the destination }
    VarDataInit(Dest);

    { Call ToString() }
    LStr := LWrapper.FObject.ToString();

    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, LStr);

      varString:
        VarDataFromLStr(Dest, AnsiString(LStr));

      varUString:
        VarDataFromStr(Dest, LStr);

      else
      begin
        { No default conversion found! Trying to use the string }
        try
          VarDataInit(LTemp);
          VarDataFromStr(LTemp, LStr);
          VarDataCastTo(Dest, LTemp, AVarType);
        finally
          { Dispose our variant }
          VarDataClear(LTemp);
        end;
      end;
    end;
  end else
    inherited;
end;

procedure TWrapperVariantType.Clear(var V: TVarData);
begin
  { Clear the variant type }
  V.VType := varEmpty;

  { And dispose the value }
  TWrapperVarData(V).FWrapper := nil;
end;

procedure TWrapperVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    with TWrapperVarData(Dest) do
    begin
      { Initialize the destination and copy our stuff over }
      VarDataInit(Dest);
      VType := VarType;
      FWrapper := TWrapperVarData(Source).FWrapper;
    end;
  end;
end;

function TWrapperVariantType.DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;
var
  LMethod: TRttiMethod;
  LWrapper: TObjectWrapper;
  LParams: array of TValue;
  LResult: TValue;
  LArgBase, X: Integer;
begin
  Result := True;
  LWrapper := TWrapperVarData(V).FWrapper.GetSelf();

  { Find the property by the given name }
  if LWrapper.FType = nil then
    raise EDynamicException.CreateRes(@SNotEnoughRtti);

  LMethod := LWrapper.FType.GetMethod(Name);

  { Check that the method exists and is public }
  if not Assigned(LMethod) then
    raise EDynamicException.CreateResFmt(@SMethodNotFound, [Name]);

  if (LMethod.Visibility in [mvPrivate, mvProtected]) then
    raise EDynamicException.CreateResFmt(@SMethodNotPublic, [Name]);

  if LMethod.IsConstructor or LMethod.IsDestructor then
    raise EDynamicException.CreateResFmt(@SConstructorDestructorNotAllowed, [Name]);

  { Convert the parameters form variants to what ever we need }
  LArgBase := 0;
  if Length(Arguments) > 0 then
  begin
    { Treat the special case accordingly }
    if Arguments[0].VType = varError then
      Inc(LArgBase);

    SetLength(LParams, Length(Arguments) - LArgBase);

    { Convert each parameter excluding the special one (if it's there) }
    for X := LArgBase to Length(Arguments) - 1 do
      if not TryVarDataToTValue(Arguments[X], LParams[X - LArgBase]) then
        raise EDynamicException.CreateResFmt(@SArgumentTypeNotSupported, [X]);
  end else
    LParams := nil; // zero elements

  { Attempt to call the method }
  if LMethod.IsClassMethod or LMethod.IsStatic then
    LResult := LMethod.Invoke(LWrapper.FObject.ClassType, LParams)
  else
    LResult := LMethod.Invoke(LWrapper.FObject, LParams);

  { Store the result of the function }
  VarDataInit(Dest);
  if not TryTValueToVarData(LResult, Dest, False) then
    raise EDynamicException.CreateResFmt(@SArgumentTypeNotSupported, [-1]);

  if Length(Arguments) > 0 then
  begin
    { Convert all "by-ref" parameters back. }
    for X := LArgBase to Length(Arguments) - 1 do
      if VarDataIsByRef(Arguments[X]) then
        if not TryTValueToVarData(LParams[X - LArgBase], Arguments[X], True) then
          raise EDynamicException.CreateResFmt(@SArgumentTypeNotSupported, [X]);
  end;
end;

function TWrapperVariantType.DoProcedure(const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;
var
  LDummy: TVarData;
begin
  Result := DoFunction(LDummy, V, Name, Arguments);
  VarDataClear(LDummy);
end;

function TWrapperVariantType.GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean;
var
  LProperty: TRttiProperty;
  LMethod: TRttiMethod;
  LWrapper: TObjectWrapper;
  LValue: TValue;
begin
  Result := True;
  LWrapper := TWrapperVarData(V).FWrapper.GetSelf();

  { Find the property by the given name }
  if not Assigned(LWrapper.FType) then
    raise EDynamicException.CreateRes(@SNotEnoughRtti);

  LProperty := LWrapper.FType.GetProperty(Name);

  { Check that the property exists, is writable and is public }
  if not Assigned(LProperty)then
  begin
    { ... Maybe it's a method then? }
    LMethod := LWrapper.FType.GetMethod(Name);
    if Assigned(LMethod) and (Length(LMethod.GetParameters()) = 0) then
    begin
      Result := DoFunction(Dest, V, Name, nil);
      Exit;
    end else
      raise EDynamicException.CreateResFmt(@SPropertyNotFound, [Name]);
  end;

  if not LProperty.IsReadable then
    raise EDynamicException.CreateResFmt(@SPropertyNotReadable, [Name]);

  if LProperty.Visibility in [mvPrivate, mvProtected] then
    raise EDynamicException.CreateResFmt(@SPropertyNotPublic, [Name]);

  { Now, do actually set the value! }
  LValue := LProperty.GetValue(LWrapper.FObject);

  { Try to convert the value to a TValue }
  if not TryTValueToVarData(LValue, Dest, False) then
    raise EDynamicException.CreateResFmt(@SPropertyTypeNotSupported, [Name]);
end;

function TWrapperVariantType.SetProperty(const V: TVarData; const Name: string; const Value: TVarData): Boolean;
var
  LProperty: TRttiProperty;
  LWrapper: TObjectWrapper;
  LValue: TValue;
begin
  Result := True;
  LWrapper := TWrapperVarData(V).FWrapper.GetSelf();

  { Find the property by the given name }
  if not Assigned(LWrapper.FType) then
    raise EDynamicException.CreateRes(@SNotEnoughRtti);

  LProperty := LWrapper.FType.GetProperty(Name);

  { Check that the property exists, is writable and is public }
  if not Assigned(LProperty)then
    raise EDynamicException.CreateResFmt(@SPropertyNotFound, [Name]);

  if not LProperty.IsWritable then
    raise EDynamicException.CreateResFmt(@SPropertyNotWritable, [Name]);

  if LProperty.Visibility in [mvPrivate, mvProtected] then
    raise EDynamicException.CreateResFmt(@SPropertyNotPublic, [Name]);

  { Try to convert the value to a TValue }
  if not TryVarDataToTValue(Value, LValue) then
    raise EDynamicException.CreateResFmt(@SPropertyTypeNotSupported, [Name]);

  { Now, do actually set the value! }
  LProperty.SetValue(LWrapper.FObject, LValue);
end;

function TWrapperVariantType.TryTValueToVarData(const AValue: TValue; var AVarData: TVarData; const AByRef: Boolean): Boolean;
var
  LTempVar: Variant;
begin
  Result := True;

  if AByRef then
  begin
    { Parameter sent by reference }
    case (AVarData.VType and not varByRef) of
      varSmallint:
        PSmallInt(AVarData.VPointer)^ := AValue.AsType<SmallInt>;
      varInteger:
        PInteger(AVarData.VPointer)^ := AValue.AsType<Integer>;
      varSingle:
        PSingle(AVarData.VPointer)^ := AValue.AsType<Single>;
      varDouble:
        PDouble(AVarData.VPointer)^ := AValue.AsType<Double>;
      varCurrency:
        PCurrency(AVarData.VPointer)^ := AValue.AsType<Currency>;
      varDate:
        PDateTime(AVarData.VPointer)^ := AValue.AsType<TDateTime>;
      varOleStr:
      begin
        LTempVar := AValue.AsType<WideString>;
        PPWideChar(AVarData.VPointer)^ := TVarData(LTempVar).VOleStr;
        TVarData(LTempVar).VOleStr := nil;
        TVarData(LTempVar).VType := varEmpty;
      end;
      varBoolean:
        PBoolean(AVarData.VPointer)^ := AValue.AsType<Boolean>;
      varShortInt:
        PShortInt(AVarData.VPointer)^ := AValue.AsType<ShortInt>;
      varByte:
        PByte(AVarData.VPointer)^ := AValue.AsType<Byte>;
      varWord:
        PWord(AVarData.VPointer)^ := AValue.AsType<Word>;
      varLongWord:
        PLongWord(AVarData.VPointer)^ := AValue.AsType<LongWord>;
      varInt64:
        PInt64(AVarData.VPointer)^ := AValue.AsType<Int64>;
      varUInt64:
        PUInt64(AVarData.VPointer)^ := AValue.AsType<UInt64>;
      varString:
        PRawByteString(AVarData.VPointer)^ := AValue.AsType<RawByteString>;
      varUString:
        PUnicodeString(AVarData.VPointer)^ := AValue.AsType<UnicodeString>;
      else
        Result := False;
    end;
  end else
  begin
    try
      VarDataInit(AVarData);
      Variant(AVarData) := AValue.AsVariant;
    except
      Result := False;
    end;
  end;
end;

function TWrapperVariantType.TryVarDataToTValue(const AVarData: TVarData; out AValue: TValue): Boolean;
begin
  Result := True;

  if (AVarData.VType and varByRef) <> 0 then
  begin
    { Parameter sent by reference }
    case (AVarData.VType and not varByRef) of
      varSmallint: AValue := TValue.From(PSmallInt(AVarData.VPointer)^);
      varInteger: AValue := TValue.From(PInteger(AVarData.VPointer)^);
      varSingle: AValue := TValue.From(PSingle(AVarData.VPointer)^);
      varDouble: AValue := TValue.From(PDouble(AVarData.VPointer)^);
      varCurrency: AValue := TValue.From(PCurrency(AVarData.VPointer)^);
      varDate: AValue := TValue.From(PDateTime(AVarData.VPointer)^);
      varOleStr: AValue := TValue.From(WideString(PPWideChar(AVarData.VPointer)^));
      varBoolean: AValue := TValue.From(PBoolean(AVarData.VPointer)^);
      varShortInt: AValue := TValue.From(PShortInt(AVarData.VPointer)^);
      varByte: AValue := TValue.From(PByte(AVarData.VPointer)^);
      varWord: AValue := TValue.From(PWord(AVarData.VPointer)^);
      varLongWord: AValue := TValue.From(PLongWord(AVarData.VPointer)^);
      varInt64: AValue := TValue.From(PInt64(AVarData.VPointer)^);
      varUInt64: AValue := TValue.From(PUint64(AVarData.VPointer)^);
      varString: AValue := TValue.From(PRawByteString(AVarData.VPointer)^);
      varUString: AValue := TValue.From(PUnicodeString(AVarData.VPointer)^);
      else
        Result := False;
    end;
  end else
  begin
    { Parameter sent directly }
    try
      AValue := TValue.FromVariant(Variant(AVarData));
    except
      Result := False;
    end;
  end;
end;

initialization
  { Register our custom variant type }
  FSingleton := TWrapperVariantType.Create();

finalization
  { Uregister our custom variant type }
  FreeAndNil(FSingleton);

end.
