unit CowArrays;
interface
uses Types, SysUtils;

type
  { The buffer for performing the byte-oriented operations }
  TByteArray = record
  private const
    { Set this to 1 to replicate String's 1 stating index }
    CIndexStart = 0;

  private
    FBuffer: RawByteString;

    function GetLength(): Integer;
    procedure SetLength(const ALength: Integer);
    function GetByte(const AIndex: Integer): Byte;
    procedure SetByte(const AIndex: Integer; const Value: Byte);
    function GetRefCount: Integer;

    class function StringOf(const ABytes: array of Byte): RawByteString; static;
  public
    { Constructors }
    constructor Create(const ABuffer: TByteArray); overload;
    constructor Create(const ABytes: array of Byte); overload;
    constructor Create(const AString: RawByteString); overload;

    { Operators }
    class operator Add(const ABuffer: TByteArray; const AByte: Byte): TByteArray;
    class operator Add(const ABuffer: TByteArray; const A2Buffer: TByteArray): TByteArray;
    class operator Add(const ABuffer: TByteArray; const ABytes: TBytes): TByteArray;
    class operator Add(const ABuffer: TByteArray; const AString: RawByteString): TByteArray;

    class operator Equal(const ABuffer: TByteArray; const A2Buffer: TByteArray): Boolean;
    class operator Equal(const ABuffer: TByteArray; const ABytes: TBytes): Boolean;
    class operator Equal(const ABuffer: TByteArray; const AString: RawByteString): Boolean;
    class operator NotEqual(const ABuffer: TByteArray; const A2Buffer: TByteArray): Boolean;
    class operator NotEqual(const ABuffer: TByteArray; const ABytes: TBytes): Boolean;
    class operator NotEqual(const ABuffer: TByteArray; const AString: RawByteString): Boolean;

    class operator Implicit(const AString: RawByteString): TByteArray;
    class operator Implicit(const ABytes: TBytes): TByteArray;

    { Obtain the pointer to the first item in the buffer }
    property Length: Integer read GetLength write SetLength;
    property Bytes[const AIndex: Integer]: Byte read GetByte write SetByte; default;
    property RefCount: Integer read GetRefCount;

    { Obtains the reference to a give element }
    function RefOf(const AIndex: Integer): PByte;

    { Extend the buffer with one byte }
    procedure Extend(const ADelta: Integer);

    { The normal string-like functions. The difference is that is returns the new buffer }
    function Copy(const AIndex, ACount: Integer): TByteArray;
    function Slice(const ACount: Integer): TByteArray;

    { Pos overloads }
    function Pos(const ASub: TByteArray): Integer; overload;
    function Pos(const ASub: array of Byte): Integer; overload;
    function Pos(const ASub: RawByteString): Integer; overload;

    { String Replaces }
    function Replace(const OldPattern, NewPattern: TByteArray): TByteArray; overload;
    function Replace(const OldPattern, NewPattern: RawByteString): TByteArray; overload;
    function Replace(const OldPattern, NewPattern: array of Byte): TByteArray; overload;

    { Delete "operation" }
    procedure Delete(const AIndex, ACount: Integer);

    { The notoriuos ToString}
    function ToString(): RawByteString;
  end;

  { Array that has string-like properties }
  TCOWArray<T> = record
  public type
    TItemRef = ^T;

  private
    FArray: TArray<T>;

    { Super-functions! }
     procedure EnsureUnique(const ADelta: Integer = 0);

    { Setters and getters }
    function GetLength(): Integer;
    procedure SetLength(const ALength: Integer);
    function GetItem(const AIndex: Integer): T;
    procedure SetItem(const AIndex: Integer; const Value: T);
    function GetRefCount: Integer;
  public
    { Constructors }
    constructor Create(const AArray: TCOWArray<T>); overload;
    constructor Create(const AItems: array of T); overload;

    { Operators }
    class operator Add(const AArray: TCOWArray<T>; const AItem: T): TCOWArray<T>;
    class operator Add(const AArray: TCOWArray<T>; const A2Array: TCOWArray<T>): TCOWArray<T>;

    class operator Equal(const AArray: TCOWArray<T>; const A2Array: TCOWArray<T>): Boolean;
    class operator NotEqual(const AArray: TCOWArray<T>; const A2Array: TCOWArray<T>): Boolean;

    { Obtain the pointer to the first item in the buffer }
    property Length: Integer read GetLength write SetLength;
    property Items[const AIndex: Integer]: T read GetItem write SetItem; default;
    property RefCount: Integer read GetRefCount;

    { Returns a pointer to the elemnt given by index }
    function RefOf(const AIndex: Integer): TItemRef;

    { Extend the buffer with one byte }
    procedure Extend(const ADelta: Integer);

    { The normal string-like functions. The difference is that is returns the new buffer }
    function Copy(const AIndex, ACount: Integer): TCOWArray<T>;
    function Slice(const ACount: Integer): TCOWArray<T>;

    { Delete "operation" }
    procedure Delete(const AIndex, ACount: Integer);
  end;

implementation

{$POINTERMATH ON}

{ TByteArray }

class operator TByteArray.Add(const ABuffer, A2Buffer: TByteArray): TByteArray;
begin
  { Simple addition }
  Result.FBuffer := ABuffer.FBuffer + A2Buffer.FBuffer;
end;

class operator TByteArray.Add(const ABuffer: TByteArray; const ABytes: TBytes): TByteArray;
begin
  { Simple addition }
  Result.FBuffer := ABuffer.FBuffer + StringOf(ABytes);
end;

class operator TByteArray.Add(const ABuffer: TByteArray; const AString: RawByteString): TByteArray;
begin
  { Simple addition }
  Result.FBuffer := ABuffer.FBuffer + AString;
end;

class operator TByteArray.Add(const ABuffer: TByteArray; const AByte: Byte): TByteArray;
begin
  { Add one byte }
  Result.FBuffer := ABuffer.FBuffer + AnsiChar(AByte);
end;

function TByteArray.Copy(const AIndex, ACount: Integer): TByteArray;
begin
  Result.FBuffer := System.Copy(FBuffer, (AIndex - CIndexStart + 1), ACount);
end;

constructor TByteArray.Create(const AString: RawByteString);
begin
  { Copy the string }
  FBuffer := AString;
end;

constructor TByteArray.Create(const ABytes: array of Byte);
begin
  { Use the TBytes as a string }
  FBuffer := StringOf(ABytes);
end;

constructor TByteArray.Create(const ABuffer: TByteArray);
begin
  { Copy the reference }
  FBuffer := ABuffer.FBuffer;
end;

procedure TByteArray.Delete(const AIndex, ACount: Integer);
begin
  System.Delete(FBuffer, (AIndex - CIndexStart + 1), ACount);
end;

class operator TByteArray.Equal(const ABuffer, A2Buffer: TByteArray): Boolean;
begin
  { String comparison }
  Result := (ABuffer.FBuffer = A2Buffer.FBuffer);
end;

class operator TByteArray.Equal(const ABuffer: TByteArray; const ABytes: TBytes): Boolean;
begin
  { String comparison }
  Result := (ABuffer.FBuffer = StringOf(ABytes));
end;

class operator TByteArray.Equal(const ABuffer: TByteArray; const AString: RawByteString): Boolean;
begin
  { String comparison }
  Result := (ABuffer.FBuffer = AString);
end;

procedure TByteArray.Extend(const ADelta: Integer);
begin
  System.SetLength(FBuffer, System.Length(FBuffer) + 1);
end;

function TByteArray.GetByte(const AIndex: Integer): Byte;
begin
  Result := Byte(FBuffer[AIndex - CIndexStart + 1]);
end;

function TByteArray.GetLength: Integer;
begin
  Result := System.Length(FBuffer);
end;

function TByteArray.GetRefCount: Integer;
begin
  Result := StringRefCount(FBuffer);
end;

class operator TByteArray.Implicit(const AString: RawByteString): TByteArray;
begin
  { Simple copy }
  Result.FBuffer := AString;
end;

class operator TByteArray.Implicit(const ABytes: TBytes): TByteArray;
begin
  { Create the buffer }
  Result.FBuffer := StringOf(ABytes);
end;

class operator TByteArray.NotEqual(const ABuffer, A2Buffer: TByteArray): Boolean;
begin
  { String comparison }
  Result := (ABuffer.FBuffer <> A2Buffer.FBuffer);
end;

class operator TByteArray.NotEqual(const ABuffer: TByteArray; const AString: RawByteString): Boolean;
begin
  { String comparison }
  Result := (ABuffer.FBuffer <> AString);
end;

function TByteArray.Pos(const ASub: array of Byte): Integer;
begin
  { Use system pos }
  Result := System.Pos(StringOf(ASub), FBuffer);
end;

function TByteArray.Pos(const ASub: TByteArray): Integer;
begin
  { Use the system pos }
  Result := System.Pos(ASub.FBuffer, FBuffer);
end;

function TByteArray.RefOf(const AIndex: Integer): PByte;
begin
  { Pin this instance }
  UniqueString(AnsiString(FBuffer));

  { Obtain the refernce to a given element }
  Result := @FBuffer[AIndex - CIndexStart + 1];
end;

function TByteArray.Replace(const OldPattern, NewPattern: TByteArray): TByteArray;
begin
  Result := Replace(OldPattern.FBuffer, NewPattern.FBuffer);
end;

function TByteArray.Replace(const OldPattern, NewPattern: RawByteString): TByteArray;
var
  LTempStr: RawByteString;
  LPatternIdx: Integer;
begin
  LTempStr := FBuffer;
  Result.FBuffer := '';

  while System.Length(LTempStr) > 0 do
  begin
    { Find the next LPatternIdx of the matched pattern }
    LPatternIdx := System.Pos(OldPattern, LTempStr);

    { Finish if no more patterns are found }
    if LPatternIdx = 0 then
    begin
      Result.FBuffer := Result.FBuffer + LTempStr;
      Exit;
    end;

    { Copy the string piece + new pattern }
    Result.FBuffer := Result.FBuffer + System.Copy(LTempStr, 1, LPatternIdx - 1) + NewPattern;
    LTempStr := System.Copy(LTempStr, LPatternIdx + System.Length(OldPattern), System.Length(LTempStr));
  end;
end;

function TByteArray.Replace(const OldPattern, NewPattern: array of Byte): TByteArray;
begin
  Result := Replace(StringOf(OldPattern), StringOf(NewPattern));
end;

class operator TByteArray.NotEqual(const ABuffer: TByteArray; const ABytes: TBytes): Boolean;
begin
  { String comparison }
  Result := (ABuffer.FBuffer <> StringOf(ABytes));
end;

procedure TByteArray.SetByte(const AIndex: Integer; const Value: Byte);
begin
  FBuffer[AIndex - CIndexStart + 1] := AnsiChar(Value);
end;

procedure TByteArray.SetLength(const ALength: Integer);
begin
  System.SetLength(FBuffer, ALength);
end;

function TByteArray.Slice(const ACount: Integer): TByteArray;
begin
  Result.FBuffer := System.Copy(FBuffer, 1, ACount);
end;

class function TByteArray.StringOf(const ABytes: array of Byte): RawByteString;
var
  ILen: Integer;
begin
  ILen := System.Length(ABytes);
  System.SetLength(Result, ILen);

  { Obtain a string out of the bytes parameter }
  if ILen > 0 then
    Move(ABytes[0], Result[1], ILen);
end;

function TByteArray.ToString: RawByteString;
begin
  Result := FBuffer;
end;

function TByteArray.Pos(const ASub: RawByteString): Integer;
begin
  { Use system pos }
  Result := System.Pos(ASub, FBuffer);
end;

{ TCOWArray<T> }

class operator TCOWArray<T>.Add(const AArray, A2Array: TCOWArray<T>): TCOWArray<T>;
var
  I, L1, L2: Integer;

begin
  L1 := System.Length(AArray.FArray);
  L2 := System.Length(A2Array.FArray);

  { Select second array if the first is void }
  if L1 = 0 then
  begin
    Result.FArray := A2Array.FArray;
    Exit;
  end;

  { Select first array if the second is void }
  if L2 = 0 then
  begin
    Result.FArray := AArray.FArray;
    Exit;
  end;

  { New Array's length is cummulative }
  System.SetLength(Result.FArray, L1 + L2);

  { Copy first array }
  for I := 0 to L1 - 1 do
    Result.FArray[I] := AArray.FArray[I];

  { Copy the second array }
  for I := 0 to L2 - 1 do
    Result.FArray[L1 + I] := A2Array.FArray[I];
end;

class operator TCOWArray<T>.Add(const AArray: TCOWArray<T>; const AItem: T): TCOWArray<T>;
var
  I, L1: Integer;
begin
  L1 := System.Length(AArray.FArray);

  { New Array's length is cummulative }
  System.SetLength(Result.FArray, L1 + 1);

  { Copy first array }
  for I := 0 to L1 - 1 do
    Result.FArray[I] := AArray.FArray[I];

  { Set the new element }
  Result.FArray[L1] := AItem;
end;

function TCOWArray<T>.Copy(const AIndex, ACount: Integer): TCOWArray<T>;
var
  I: Integer;
begin
  { Set the length of the resulting array }
  System.SetLength(Result.FArray, ACount);

  { Copy the elements into the copied array }
  for I := 0 to ACount - 1 do
    Result.FArray[I] := FArray[AIndex + I];
end;

constructor TCOWArray<T>.Create(const AItems: array of T);
var
  I, L1: Integer;
begin
  L1 := System.Length(AItems);

  { Set the length of the resulting array }
  System.SetLength(FArray, L1);

  { Copy the elements into the copied array }
  for I := 0 to L1 - 1 do
    FArray[I] := AItems[I];
end;

constructor TCOWArray<T>.Create(const AArray: TCOWArray<T>);
begin
  { Copy the array reference }
  FArray := AArray.FArray;
end;

procedure TCOWArray<T>.Delete(const AIndex, ACount: Integer);
var
  I: Integer;
begin
  { Make unique }
  EnsureUnique();

  { Move the elements over the deleted part }
  for I := AIndex + ACount to System.Length(FArray) - 1 do
    FArray[I - ACount] := FArray[I];

  { Resize the array properly }
  System.SetLength(FArray, System.Length(FArray) - ACount);
end;

procedure TCOWArray<T>.EnsureUnique(const ADelta: Integer = 0);
var
  L1: Integer;
  LArray: TArray<T>;
  I: Integer;
begin
  { Obtain the ref-count of the array }
  if RefCount > 1 then
  begin
    { Copy the array now. Create a local copy. }
    L1 := System.Length(FArray);
    System.SetLength(LArray, L1 + ADelta);

    { Copy the elements }
    for I := 0 to L1 - 1 do
      LArray[I] := FArray[I];

    { And switch the references }
    FArray := LArray;
  end else
  begin
    if ADelta <> 0 then
      System.SetLength(FArray, L1 + ADelta);
  end;
end;

class operator TCOWArray<T>.Equal(const AArray, A2Array: TCOWArray<T>): Boolean;
var
  L1, L2: Integer;
begin
  L1 := System.Length(AArray.FArray);
  L2 := System.Length(A2Array.FArray);

  { Exit on different lengths }
  if L1 <> L2 then
    Exit(false);

  { Compare the memory }
  Result := CompareMem(AArray.FArray, A2Array.FArray, L1 * SizeOf(T));
end;

procedure TCOWArray<T>.Extend(const ADelta: Integer);
begin
  { Unique-fy the array }
  EnsureUnique(ADelta);
end;

function TCOWArray<T>.GetItem(const AIndex: Integer): T;
begin
  Result := FArray[AIndex];
end;

function TCOWArray<T>.GetLength: Integer;
begin
  Result := System.Length(FArray);
end;

function TCOWArray<T>.GetRefCount: Integer;
begin
  if FArray <> nil then
    Result := (PInteger(FArray) - 2)^
  else
    Result := -1;
end;

function TCOWArray<T>.RefOf(const AIndex: Integer): TItemRef;
begin
  { Ensure uniquiness }
  EnsureUnique();

  { Obtain the element at AIndex }
  Result := @(FArray[AIndex]);
end;

class operator TCOWArray<T>.NotEqual(const AArray, A2Array: TCOWArray<T>): Boolean;
var
  L1, L2: Integer;
begin
  L1 := System.Length(AArray.FArray);
  L2 := System.Length(A2Array.FArray);

  { Exit on different lengths }
  if L1 <> L2 then
    Exit(true);

  { Compare the memory }
  Result := not CompareMem(AArray.FArray, A2Array.FArray, L1 * SizeOf(T));
end;

procedure TCOWArray<T>.SetItem(const AIndex: Integer; const Value: T);
begin
  { Ensure the array is unique }
  EnsureUnique();

  { Now set the value }
  FArray[AIndex] := Value;
end;

procedure TCOWArray<T>.SetLength(const ALength: Integer);
begin
  { Ensure the array is unique }
  EnsureUnique();

  { Now set the new length }
  System.SetLength(FArray, ALength);
end;

function TCOWArray<T>.Slice(const ACount: Integer): TCOWArray<T>;
begin
  { Slice the first part of the array }
  Result := Copy(0, ACount);
end;

end.
