unit VDictionary;
interface

{ Creates a new Variant Dictionary }
function VDict(): Variant;

implementation
uses
  SysUtils,
  Variants,
  VarUtils,
  Generics.Collections;

type
  { Declare the String/Variant dictionary that will hold the real data }
  TSVDictionary = TDictionary<String, Variant>;

  { Mapping the TSVDictionary into TVarData structure }
  TSVDictionaryVarData = packed record
    { Var type, will be assigned at runtime }
    VType: TVarType;
    { Reserved stuff }
    Reserved1, Reserved2, Reserved3: Word;
    { A reference to the enclosed dictionary }
    FDictionary: TSVDictionary;
    { Reserved stuff }
    Reserved4: LongWord;
  end;

  { Manager for our variant type }
  TSVDictionaryVariantType = class(TInvokeableVariantType)
  private
    function DictionaryToString(const ADict: TSVDictionary): String;

  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name:
      string; const Value: TVarData): Boolean; override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
  end;

var
  { Our singleton that manages our variant type }
  SgtSVDictionaryVariantType: TSVDictionaryVariantType;

function VDict(): Variant;
begin
  { Clear out the result }
  VarClear(Result);

  with TSVDictionaryVarData(Result) do
  begin
    { Assign the new variant the var type that was allocated for us }
    VType := SgtSVDictionaryVariantType.VarType;

    { Create a new instance of the dictionary object }
    FDictionary := TSVDictionary.Create();
  end;
end;

{ TSVDictionaryVariantType }

procedure TSVDictionaryVariantType.CastTo(
  var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var
  Temp: TVarData;
  Dict: TSVDictionary;
begin
  if Source.VType = VarType then
  begin
    { Only continue if we're invoked for our data type }
    Dict := TSVDictionaryVarData(Source).FDictionary;

    { Initilize the destination }
    VarDataInit(Dest);

    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, DictionaryToString(Dict));

      varString, varUString:
        VarDataFromStr(Dest, DictionaryToString(Dict));
      else
      begin
        { No default convertion found! Trying to use the string }
        try
          VarDataInit(Temp);
          VarDataFromStr(Temp, DictionaryToString(Dict));
          VarDataCastTo(Dest, Temp, AVarType);
        finally
          { Dispose our variant }
          VarDataClear(Temp);
        end;
      end;
    end;
  end else
    inherited;
end;

procedure TSVDictionaryVariantType.Clear(var V: TVarData);
begin
  { Clear the variant type }
  V.VType := varEmpty;

  { And dispose the value }
  FreeAndNil(TSVDictionaryVarData(V).FDictionary);
end;

procedure TSVDictionaryVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    with TSVDictionaryVarData(Dest) do
    begin
      { Copy the variant type }
      VType := VarType;

      { Create a new dictionary and copy contents }
      FDictionary := TSVDictionary.Create(TSVDictionaryVarData(Source).FDictionary);
    end;
  end;
end;

function TSVDictionaryVariantType.DictionaryToString(
  const ADict: TSVDictionary): String;
var
  KV: TPair<String, Variant>;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;

  { Iterate over each K/V pair in the dictionary }
  for KV in ADict do
  begin
    Builder.Append(KV.Key);
    Builder.Append(':');

    try
      {
        Use the exception handling, maybe we cannot convert
        to string (from variant)
      }
      Builder.Append(String(KV.Value));
    except
      Builder.Append('[...]');
    end;

    Builder.AppendLine;
  end;

  { Return the value }
  Result := Builder.ToString();
  Builder.Free;
end;

function TSVDictionaryVariantType.DoFunction(
  var Dest: TVarData; const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
begin
  Result := False;

  { We do not support arguments here }
  if Length(Arguments) > 1 then
    Exit;

  { Is the first parameter an error variant? }
  if (Length(Arguments) = 1) and (Arguments[0].VType <> varError) then
    Exit;

  { Requesting dictionary count? }
  if Name = 'COUNT' then
  begin
    VariantInit(Dest);
    Dest.VType := varInteger;
    Dest.VInteger := TSVDictionaryVarData(V).FDictionary.Count;

    Exit(true);
  end;

  { Try to call the procedures }
  Result := DoProcedure(V, Name, Arguments);
end;

function TSVDictionaryVariantType.DoProcedure(
  const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
var
  Key: String;
begin
  Result := False;

  { We do not support arguments here }
  if Length(Arguments) > 1 then
    Exit;

  { Is the first parameter an error variant? }
  if (Length(Arguments) = 1) and (Arguments[0].VType <> varError) then
    Exit;

  { Check if this is a removal call }
  if Pos('REMOVE', Name) = 1 then
  begin
    { Remve the prefix }
    Key := System.Copy(Name, Length('REMOVE') + 1, Length(Name));
    TSVDictionaryVarData(V).FDictionary.Remove(Key);

    Exit(true);
  end;

  { Is this a CLEAR call? }
  if Name = 'NAME' then
  begin
    TSVDictionaryVarData(V).FDictionary.Clear();

    Exit(true);
  end;
end;

function TSVDictionaryVariantType.GetProperty(
  var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  { Type cast to our data type }
  with TSVDictionaryVarData(V) do
  begin
    { Try to read the value from the dictionary }
    if not FDictionary.TryGetValue(Name, Variant(Dest)) then
      Clear(Dest);
  end;

  { Return true in any possible case }
  Result := True;
end;

function TSVDictionaryVariantType.SetProperty(
  const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  { Type cast to our data type }
  with TSVDictionaryVarData(V) do
    { Update the value in dictionary dictionary }
    FDictionary.AddOrSetValue(Name, Variant(Value));

  { Return true in any possible case }
  Result := True;
end;

initialization
  { Register our custom variant type }
  SgtSVDictionaryVariantType := TSVDictionaryVariantType.Create();

finalization
  { Uregister our custom variant }
  FreeAndNil(SgtSVDictionaryVariantType);

end.
