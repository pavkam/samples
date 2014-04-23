(*
* Copyright (c) 2009, Ciobanu Alexandru
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit ByteBuffer;
interface
uses Types, SysUtils;

type
  { The buffer for performing the byte-oriented operations }
  TByteArray = record
  strict private const
    { Set this to 1 to replicate String's 1 stating index }
    CIndexStart = 0;

  strict private
    FBuffer: RawByteString;

    function GetLength(): Integer; inline;
    procedure SetLength(const ALength: Integer); inline;
    function GetByte(const AIndex: Integer): Byte; inline;
    procedure SetByte(const AIndex: Integer; const Value: Byte); inline;
    function GetRefCount: Integer; inline;

    class function Compare(const P1, P2: PByte; const L1, L2: Integer): Boolean; static;
    class function StringOf(const ABytes: array of Byte): RawByteString; static;
  public
    { Constructors }
    constructor Create(const ABuffer: TByteArray); overload;
    constructor Create(const ABytes: array of Byte); overload;
    constructor Create(const AString: RawByteString); overload;

    { Operators }
    class operator Add(const ABuffer: TByteArray; const AByte: Byte): TByteArray;
    class operator Add(const ABuffer: TByteArray; const A2Buffer: TByteArray): TByteArray; inline;
    class operator Add(const ABuffer: TByteArray; const ABytes: TBytes): TByteArray;
    class operator Add(const ABuffer: TByteArray; const AString: RawByteString): TByteArray; inline;

    class operator Equal(const ABuffer: TByteArray; const A2Buffer: TByteArray): Boolean;
    class operator Equal(const ABuffer: TByteArray; const ABytes: TBytes): Boolean;
    class operator Equal(const ABuffer: TByteArray; const AString: RawByteString): Boolean;
    class operator NotEqual(const ABuffer: TByteArray; const A2Buffer: TByteArray): Boolean;
    class operator NotEqual(const ABuffer: TByteArray; const ABytes: TBytes): Boolean;
    class operator NotEqual(const ABuffer: TByteArray; const AString: RawByteString): Boolean;

    class operator Implicit(const AString: RawByteString): TByteArray; inline;
    class operator Implicit(const ABytes: TBytes): TByteArray; inline;

    { Obtain the pointer to the first item in the buffer }
    property Length: Integer read GetLength write SetLength;
    property Bytes[const AIndex: Integer]: Byte read GetByte write SetByte; default;
    property RefCount: Integer read GetRefCount;

    { Obtains the reference to a give element }
    function RefOf(const AIndex: Integer): PByte;

    { Extend the buffer with one byte }
    procedure Extend(const ADelta: Integer); inline;

    { The normal string-like functions. The difference is that is returns the new buffer }
    function Copy(const AIndex, ACount: Integer): TByteArray; inline;
    function Slice(const ACount: Integer): TByteArray; inline;

    { Pos overloads }
    function Pos(const ASub: TByteArray): Integer; overload; inline;
    function Pos(const ASub: array of Byte): Integer; overload;
    function Pos(const ASub: RawByteString): Integer; overload; inline;

    { String Replaces }
    function Replace(const OldPattern, NewPattern: TByteArray): TByteArray; overload; inline;
    function Replace(const OldPattern, NewPattern: RawByteString): TByteArray; overload;
    function Replace(const OldPattern, NewPattern: array of Byte): TByteArray; overload;

    { Delete "operation" }
    procedure Delete(const AIndex, ACount: Integer); overload; inline;
    procedure Delete(const ACount: Integer); overload; inline;

    { FillChar "operation" }
    procedure Fill(const AFromIndex, ACount: Integer; const AByte: Byte); overload; inline;
    procedure Fill(const ACount: Integer; const AByte: Byte); overload; inline;
    procedure Fill(const AByte: Byte); overload; inline;

    { The Move "operation" }
    procedure Move(const AFromIndex: Integer; var Destination; const ACount: Integer); overload; inline;
    procedure Move(var Destination; const ACount: Integer); overload; inline;
    procedure Move(var Destination); overload; inline;

    { The notoriuos ToString}
    function ToString(): RawByteString; inline;
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
var
  L1, L2: Integer;
begin
  { Obtain the sizes of both buffers }
  L1 := System.Length(ABuffer.FBuffer);
  L2 := System.Length(ABytes);

  { Create a new one }
  System.SetLength(Result.FBuffer, L1 + L2);

  { Copy buffer 1 if it's Length > 0 }
  if L1 > 0 then
    System.Move(ABuffer.FBuffer[1], Result.FBuffer[1], L1);

  { Copy buffer 2 if it's Length > 0 }
  if L2 > 0 then
    System.Move(ABytes[0], Result.FBuffer[L1 + 1], L2);
end;

class operator TByteArray.Add(const ABuffer: TByteArray; const AString: RawByteString): TByteArray;
begin
  { Simple addition }
  Result.FBuffer := ABuffer.FBuffer + AString;
end;

class operator TByteArray.Add(const ABuffer: TByteArray; const AByte: Byte): TByteArray;
var
  L1: Integer;
begin
  { Obtain the size of the buffer }
  L1 := System.Length(ABuffer.FBuffer);

  { Create the new buffer to hold the result }
  System.SetLength(Result.FBuffer, L1 + 1 { one more AnsiChar});

  { Copy the original content }
  if L1 > 0 then
    System.Move(ABuffer.FBuffer[1], Result.FBuffer[1], L1);

  { Add one more byte }
  Result.FBuffer[L1 + 1] := AnsiChar(AByte);
end;

class function TByteArray.Compare(const P1, P2: PByte; const L1, L2: Integer): Boolean;
begin
  { Not equal for different lengths }
  if L1 <> L2 then
    Exit(False);

  { Equal for both lengths = 0 }
  if L1 = 0 then
    Exit(True);

  { Call SysUtils.CompareMem }
  Result := CompareMem(P1, P2, L1);
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
  { Use the system delete routine }
  System.Delete(FBuffer, (AIndex - CIndexStart + 1), ACount);
end;

procedure TByteArray.Delete(const ACount: Integer);
begin
  { Use the system delete routine }
  System.Delete(FBuffer, 1, ACount);
end;

class operator TByteArray.Equal(const ABuffer, A2Buffer: TByteArray): Boolean;
begin
  { Memory comparison }
  Result := Compare(Pointer(ABuffer.FBuffer), Pointer(A2Buffer.FBuffer),
    System.Length(ABuffer.FBuffer),
    System.Length(ABuffer.FBuffer)
  );
end;

class operator TByteArray.Equal(const ABuffer: TByteArray; const ABytes: TBytes): Boolean;
begin
  { Memory comparison }
  Result := Compare(Pointer(ABuffer.FBuffer), Pointer(ABytes),
    System.Length(ABuffer.FBuffer),
    System.Length(ABytes)
  );
end;

class operator TByteArray.Equal(const ABuffer: TByteArray; const AString: RawByteString): Boolean;
begin
  { Memory comparison }
  Result := Compare(Pointer(ABuffer.FBuffer), Pointer(AString),
    System.Length(ABuffer.FBuffer),
    System.Length(AString)
  );
end;

procedure TByteArray.Extend(const ADelta: Integer);
begin
  { Extend the internal string by 1 }
  System.SetLength(FBuffer, System.Length(FBuffer) + 1);
end;

procedure TByteArray.Fill(const ACount: Integer; const AByte: Byte);
begin
  { Fill the buffer (start at 1)}
  FillChar(FBuffer[1], ACount, AByte);
end;

procedure TByteArray.Fill(const AByte: Byte);
begin
  { Fill the buffer (start at 1 + max length) }
  FillChar(FBuffer[1], System.Length(FBuffer), AByte);
end;

procedure TByteArray.Fill(const AFromIndex, ACount: Integer; const AByte: Byte);
begin
  { Fill the buffer }
  FillChar(FBuffer[AFromIndex - CIndexStart + 1], ACount, AByte);
end;

function TByteArray.GetByte(const AIndex: Integer): Byte;
begin
  { Obtain the byte at the given index }
  Result := Byte(FBuffer[AIndex - CIndexStart + 1]);
end;

function TByteArray.GetLength: Integer;
begin
  { Use System.Length }
  Result := System.Length(FBuffer);
end;

function TByteArray.GetRefCount: Integer;
begin
  { Use StringRefCount on the internal string }
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

procedure TByteArray.Move(var Destination; const ACount: Integer);
begin
  { Use System.Move (assume at Index 1) }
  System.Move(FBuffer[1], Destination, ACount);
end;

procedure TByteArray.Move(var Destination);
begin
  { Use System.Move (assume at Index 1 + max length) }
  System.Move(FBuffer[1], Destination, System.Length(FBuffer));
end;

procedure TByteArray.Move(const AFromIndex: Integer; var Destination; const ACount: Integer);
begin
  { Use System.Move }
  System.Move(FBuffer[AFromIndex], Destination, ACount);
end;

class operator TByteArray.NotEqual(const ABuffer, A2Buffer: TByteArray): Boolean;
begin
  { Memory comparison }
  Result := not Compare(Pointer(ABuffer.FBuffer), Pointer(A2Buffer.FBuffer),
    System.Length(ABuffer.FBuffer),
    System.Length(ABuffer.FBuffer)
  );
end;

class operator TByteArray.NotEqual(const ABuffer: TByteArray; const ABytes: TBytes): Boolean;
begin
  { Memory comparison }
  Result := not Compare(Pointer(ABuffer.FBuffer), Pointer(ABytes),
    System.Length(ABuffer.FBuffer),
    System.Length(ABytes)
  );
end;

class operator TByteArray.NotEqual(const ABuffer: TByteArray; const AString: RawByteString): Boolean;
begin
  { Memory comparison }
  Result := Compare(Pointer(ABuffer.FBuffer), Pointer(AString),
    System.Length(ABuffer.FBuffer),
    System.Length(AString)
  );
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

function TByteArray.Pos(const ASub: RawByteString): Integer;
begin
  { Use system pos }
  Result := System.Pos(ASub, FBuffer);
end;

function TByteArray.RefOf(const AIndex: Integer): PByte;
begin
  { Pin this instance }
  UniqueString(AnsiString(FBuffer));

  { Obtain the reference to a given element }
  Result := @(FBuffer[AIndex - CIndexStart + 1]);
end;

function TByteArray.Replace(const OldPattern, NewPattern: TByteArray): TByteArray;
begin
  { Use the other version }
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
  { Use the other version }
  Result := Replace(StringOf(OldPattern), StringOf(NewPattern));
end;

procedure TByteArray.SetByte(const AIndex: Integer; const Value: Byte);
begin
  { Set the byte at the given location }
  FBuffer[AIndex - CIndexStart + 1] := AnsiChar(Value);
end;

procedure TByteArray.SetLength(const ALength: Integer);
begin
  { Use System.SetLength }
  System.SetLength(FBuffer, ALength);
end;

function TByteArray.Slice(const ACount: Integer): TByteArray;
begin
  { Copy the firts part of the internal string }
  Result.FBuffer := System.Copy(FBuffer, 1, ACount);
end;

class function TByteArray.StringOf(const ABytes: array of Byte): RawByteString;
var
  L1: Integer;
begin
  L1 := System.Length(ABytes);
  System.SetLength(Result, L1);

  { Obtain a string out of the bytes parameter }
  if L1 > 0 then
    System.Move(ABytes[0], Result[1], L1);
end;

function TByteArray.ToString: RawByteString;
begin
  { Return the internal string }
  Result := FBuffer;
end;

end.
