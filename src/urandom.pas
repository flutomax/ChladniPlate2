{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uRandom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TRandom }

  TRandom = class
  private
    fMt: array of longword;  // array of the state vector
    fMag01: array[0..1] of longword;
    fMTi: longint;
    fSeed: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(aSeed: longint = 0);
    procedure Randomize;
    function Random: double; overload;  // [0 - 1)
    function Random(const aMin, aMax: double): double; overload;
    function RandInt: longint; overload;  // integer to maxint
    function RandInt(const ARange: longint): longint; overload;
    function RandDWord: longword;
    function RandLW(const aRange: longword): longword;
    function RandInt64(const aRange: int64): int64;
    function RandUint64(const aRange: UInt64): UInt64;
  end;

implementation

{ TRandom }

const
  cN = 624;
  cM = 397;
  cMatrix_A = $9908b0df;
  cUpperMask = $80000000;
  cLowerMask = $7fffffff;

constructor TRandom.Create;
begin
  SetLength(fMt, cN);
  fMTi := cN + 1;
  Randomize;
end;

destructor TRandom.Destroy;
begin
  SetLength(fMt, 0);
  inherited Destroy;
end;

procedure TRandom.Init(aSeed: longint);
var
  mti: longword;
begin
  if aSeed = 0 then
    aSeed := LongInt($d928f4df);
  fMt[0] := longword(aSeed);
  for mti := 1 to cN - 1 do
  begin
    fMT[mti] := (1812433253 * (fMt[mti - 1] xor (fMt[mti - 1] shr 30)) + mti);
  end;
  fMTi := cN;
  fMag01[0] := 0;
  fMag01[1] := cMatrix_A;
end;

procedure TRandom.Randomize;
var
  OldRandSeed: longint;
begin
  OldRandSeed := System.Randseed;
  System.Randomize;
  Init(System.RandSeed);
  System.RandSeed := OldRandSeed;
end;

function TRandom.RandDWord: longword;
var
  y: longword;
  kk: integer;
begin
  if fMTi > cN then
    Init(5489);
  if fMTi >= cN then
  begin // generate N word sat one time
    for kk := 0 to cN - cM - 1 do
    begin
      y := (fMt[kk] and cUpperMask) or (fMT[kk + 1] and cLowerMask);
      fMT[kk] := fMT[kk + cM] xor (y shr 1) xor fmag01[y and $1];
    end;
    for kk := cN - cM to cN - 2 do
    begin
      y := (fMt[kk] and cUpperMask) or (fMT[kk + 1] and cLowerMask);
      fMT[kk] := fMT[kk + (cM - cN)] xor (y shr 1) xor fMag01[y and $1];
    end;
    y := (fMT[cN - 1] and cUpperMask) or (fMT[0] and cLowerMask);
    fMt[cN - 1] := fMT[cM - 1] xor (y shr 1) xor fMag01[y and $1];
    fMTi := 0;
  end;
  y := fMT[fMTi];
  Inc(fMTi);
  y := y xor (y shr 11);
  y := y xor ((y shl 7) and $9d2c5680);
  y := y xor ((y shl 15) and $efc60000);
  y := y xor (y shr 18);
  Result := y;
end;

function TRandom.Random: double;
const
  cMultA: double = 67108864.0;
  cMultB: double = 1.0 / 9007199254740992.0;
var
  a, b: cardinal;
  x, y: double;
begin
  a := RandDWord;
  x := a shr 5;
  b := RandDWord;
  y := b shr 6;
  Result := (x * cMultA + y) * cMultB;
end;

function TRandom.Random(const aMin, aMax: double): double;
var
  range: double;
begin
  range := aMax - aMin;
  Result := self.Random * range + aMin;
end;

function TRandom.RandInt: longint;
begin
  Result := RandInt(Maxint);
end;

function TRandom.RandInt(const ARange: longint): longint;
var
  y: longword;
begin
  y := RandDWord;
  // ensure positive integer (base range from 0 - maxint)
  Result := (longint(y and $7FFFFFFF)) mod aRange;
end;

function TRandom.RandLW(const aRange: longword): longword;
var
  y: longword;
begin
  y := RandDWord;
  Result := y mod aRange;
end;

function TRandom.RandInt64(const aRange: int64): int64;
var
  val1, val2: longword;
begin
  val1 := RandLW($8FFFFFFF);
  val2 := RandLW($FFFFFFFF);
  Result := ((int64(val1) shl 32) + int64(val2)) mod aRange;
end;

function TRandom.RandUint64(const aRange: UInt64): UInt64;
var
  val1, val2: longword;
begin
  val1 := RandLW($FFFFFFFF);
  val2 := RandLW($FFFFFFFF);
  Result := ((UInt64(val1) shl 32) + UInt64(val2)) mod aRange;
end;

end.
