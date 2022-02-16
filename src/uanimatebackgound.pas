{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uAnimateBackgound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Forms, Graphics, LCLType, FPImage,
  GraphType, IntfGraphics;

type

  TMatrix = array of array of double;

  { TMap }

  TMap = class(TFPPalette)
  public
    function ColorSmooth(const Value: double): TFPColor;
  end;

  TBackgoundStage = (bgsNone, bgsFadeIn, bgsFadeOut);

  { TAnimateBackgound }

  TAnimateBackgound = class(TComponent)
  private
    fRuleStr: string;
    fMatrix: TMatrix;
    fBuffer: TBitmap;
    fIntf: TLazIntfImage;
    fMap: TMap;
    fValMin: double;
    fValMax: double;
    fStep: integer;
    fCount: integer;
    fVariant: integer;
    fAlpha: word;
    fStage: TBackgoundStage;
    fOnDraw: TNotifyEvent;
    function CalcRule(const x, y: double): double;
    procedure DoNextStep(Sender: TObject; var Done: boolean);
    procedure NextVariant;
    procedure MakeMap;
    procedure MakeFrame;
    procedure MakeMatrix;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property Buffer: TBitmap read fBuffer;
    property OnDraw: TNotifyEvent read fOnDraw write fOnDraw;
  end;

implementation

uses
  Math, StrUtils;

const
  S_RULES: array[0..29] of string = (
    'BqWLRFPw', 'eyGdLaR', 'aQpObesKc', 'BwJc', 'TMOYaVslmGN', 'GIpmfmvplp', 'jGrpN',
    'KA', 'RtI', 'xXYXOFHHYQW', 'pMRtmAe', 'wnXPVnBcQ', 'XN', 'hnGOVPGewe', 'Me',
    'yVLHnQYw', 'NuTGXhl', 'Bg', 'Mel', 'ebsXCqyTf', 'mNDHBB', 'fhXNggmvDxn',
    'FLmJeh', 'PmfsbtF', 'uJxUeQgQSYt', 'ffQ', 'WCEmOOYUb', 'Nt', 'LbMtBX', 'QMsAH');



function FMod(const a, b: double): double; inline;
begin
  Result := a - b * Int(a / b);
end;

{ TMap }

function TMap.ColorSmooth(const Value: double): TFPColor;
var
  n1, n2: byte;
  r: double;
  c1, c2: TFPColor;
begin
  r := Frac(Value);
  n1 := EnsureRange(Trunc(Value), 0, Count - 1);
  n2 := EnsureRange(n1 + 1, 0, Count - 1);
  c1 := Color[n1];
  c2 := Color[n2];
  Result.red := Round(c1.red + ((c2.red - c1.red) * r));
  Result.green := Round(c1.green + ((c2.green - c1.green) * r));
  Result.blue := Round(c1.blue + ((c2.blue - c1.blue) * r));
  Result.alpha := alphaOpaque;
end;

{ TAnimateBackgound }


constructor TAnimateBackgound.Create(AOwner: TComponent);
var
  c: TWinControl;
begin
  inherited Create(AOwner);
  if not (AOwner is TWinControl) then
    raise Exception.Create('Owner must be TWinControl only!');
  c := TWinControl(AOwner);
  fVariant := -1;
  fCount := 0;
  fStep := 0;
  fStage := bgsFadeIn;
  fAlpha := alphaTransparent;
  SetLength(fMatrix, c.Width, c.Height);
  fBuffer := TBitmap.Create;
  fBuffer.SetSize(c.Width, c.Height);
  fIntf := fBuffer.CreateIntfImage;
  fIntf.FillPixels(colWhite);
  fMap := TMap.Create(256);
  MakeMap;
  NextVariant;
  MakeFrame;
end;

destructor TAnimateBackgound.Destroy;
begin
  fMatrix := nil;
  fBuffer.Free;
  fIntf.Free;
  fMap.Free;
  inherited Destroy;
end;

procedure TAnimateBackgound.Start;
begin
  Application.OnIdle := @DoNextStep;
end;

procedure TAnimateBackgound.Stop;
begin
  Application.OnIdle := nil;
end;

function TAnimateBackgound.CalcRule(const x, y: double): double;

  function CheckZero(const a: double): double;
  begin
    if IsZero(a) then
      Result := 1E-7 * IfThen(a < 0, -1, 1)
    else
      Result := a;
  end;

var
  i: integer;
begin
  Result := 0.0;
  for i := 1 to Length(fRuleStr) do
  begin
    case fRuleStr[i] of
      'A': Result := Result + sin(x * x + y * y);
      'B': Result := Result + sin(x * x) * cos(y * y);
      'C': Result := Result + sin(x / CheckZero(y)) * cos(x / CheckZero(y));
      'D': Result := Result + cos(x / CheckZero(y));
      'E': Result := Result + sin(y / CheckZero(x));
      'F': Result := Result + abs(y) - x;
      'G': Result := Result + x + abs(y);
      'H': Result := Result + abs(x);
      'I': Result := Result + abs(y);
      'J': Result := Result + abs(x) * abs(y);
      'K': Result := Result + sin(x) * cos(y);
      'L': Result := Result + sin(x * y) * cos(x * y);
      'M': Result := Result + sin(sqr(abs(x))) - cos(sqr(abs(y)));
      'N': Result := Result + sin(x * x - y * y);
      'O': Result := Result + y - abs(x);
      'P': Result := Result + abs(x) + y;
      'Q': Result := Result + cos(x) * sin(y) * cos(x * y);
      'R': Result := Result + sin(cos(x) * abs(y) * abs(y));
      'S': Result := Result + sin(x * x * x - y * y * y);
      'T': Result := Result + sin(y * y * y) + sin(x * x * x);
      'U': Result := Result + cos(y * y * y + x * x * x);
      'V': Result := Result + cos(y * y * y) + cos(x * x * x);
      'W': Result := Result + abs(y * 3);
      'X': Result := Result + abs(x * 3);
      'Y': Result := Result + sin(x * x / CheckZero(y) - y * y / CheckZero(x));
      'Z': Result := Result + cos(x * x / CheckZero(y)) + sin(y * y / CheckZero(x));
      'a': Result := Result + sin(x) + sin(x) + cos(y) + cos(y);
      'b': Result := Result + cos(x) + cos(x) + sin(y) + sin(y);
      'c': Result := Result + sin(x) + cos(x) + sin(y) + cos(y);
      'd': Result := Result * cos(y) + sin(y) + cos(x) + sin(x);
      'e': Result := Result - tan(cos(sqrt(x * y * x * y)));
      'f': Result := Result * y - sin(x);
      'g': Result := Result * x - cos(y);
      'h': Result := Result * sqrt(abs(x) + abs(y));
      'i': Result := Result * sin(x * y * x) + cos(y * x * y);
      'j': Result := Result + sin(x * x);
      'k': Result := Result + sqr(cos(x) + sqr(x) * sin(y) + sqr(y));
      'l': Result := Result * sin(Result) * cos(x) * sin(x * y);
      'm': Result := Result * sin(Result) * cos(y) * sin(x * y);
      'n': Result := Result + sin(x + y * x * y + x * x);
      'o': Result := Result + sin(y + x * y * x + y * y);
      'p': Result := Result + abs(x * y + x * x + y * y);
      'q': Result := Result + ((x + y) * y * x * sin(x) * cos(y));
      'r': Result := Result + ((x + y * x) + sin(x * y) + cos(y / CheckZero(x)));
      's': Result := Result + sin(x * y + x) + cos(y * x + y);
      't': Result := Result * cos(x + y) * sin(x + y) / 2;
      'u': Result := Result + cos(sqr(x + y)) * y + sqr(cos(y) * sin(x));
      'v': Result := Result + sin(sqr(y + x)) * x + sqr(sin(x) * cos(y));
      'w': Result := Result + cos(x) * sin(x) + cos(y) * sin(y);
      'x': Result := Result + sqr(sin(sqr(x) / CheckZero(sqr(y))));
      'y': Result := Result + sin(abs(cos(x)) + abs(sin(y)));
      'z': Result := Result + sin(abs(cos(x + y)) + abs(cos(y * x * y)));
    end;
  end;
end;

procedure TAnimateBackgound.DoNextStep(Sender: TObject; var Done: boolean);
const
  AlphaStep = 8192;
  AlphaStep2 = 4096;
  AlphaHi = alphaOpaque - AlphaStep;
begin
  case fStage of
    bgsFadeIn:
    begin
      Inc(fAlpha, AlphaStep);
      if fAlpha >= AlphaHi then
      begin
        fAlpha := alphaOpaque;
        fStage := bgsNone;
      end;
    end;
    bgsFadeOut:
    begin
      if fAlpha > AlphaStep then
        Dec(fAlpha, AlphaStep)
      else
        Dec(fAlpha, AlphaStep2);
      if fAlpha <= AlphaStep2 then
      begin
        fAlpha := alphaTransparent;
        fStage := bgsFadeIn;
        NextVariant;
      end;
    end
  end;
  MakeFrame;
  Inc(fStep);
  if fStep >= 256 then
  begin
    fStep := 0;
    Inc(fCount);
  end;
  if fCount > 1 then
  begin
    fCount := 0;
    fStage := bgsFadeOut;
  end;
  Done := False;
end;

procedure TAnimateBackgound.NextVariant;
var
  n: integer;
begin
  // Always new variant! :^)
  repeat
    n := Random(High(S_RULES) + 1);
  until n <> fVariant;
  fRuleStr := S_RULES[n];
  fVariant := n;
  MakeMatrix;
end;

procedure TAnimateBackgound.MakeMap;
const
  k = 360 / 255;
  eHigh = 255;
  eLow = 239;
  repe = 7;
var
  i, x, h: integer;
  v: double;
begin
  h := eHigh - eLow;
  fMap.Clear;
  for i := 0 to 255 do
  begin
    v := repe * degtorad(k * i);
    v := 0.5 + Cos(v) * 0.5;
    x := eLow + Round(v * h);
    fMap.Add(TColorToFPColor(RGBToColor(x, x, x)));
  end;
end;

procedure TAnimateBackgound.MakeFrame;
var
  i, j: integer;
  r, v, w: double;
  c: TFPColor;
begin
  if fValMax - fValMin = 0 then
    Exit;
  r := 256 / (fValMax - fValMin);
  for j := 0 to fBuffer.Height - 1 do
  begin
    for i := 0 to fBuffer.Width - 1 do
    begin
      w := FMod(fStep + (fMatrix[i, j] - fValMin) * r, 256);
      c := fMap.ColorSmooth(w);
      c.alpha := fAlpha;
      fIntf.Colors[i, j] := AlphaBlend(colWhite, c);
    end;
  end;

  fBuffer.LoadFromIntfImage(fIntf);
  if Assigned(fOnDraw) then
    fOnDraw(self);
end;

procedure TAnimateBackgound.MakeMatrix;
const
  edScale = 3.75;
var
  xMin, xMax, yMin, yMax, xDelta, yDelta, x, y: double;
  i, j: integer;
begin
  fValMin := MaxInt;
  fValMax := -MaxInt;
  xMin := -edScale;
  xMax := edScale;
  yMin := -edScale;
  yMax := edScale;
  xDelta := (xMax - xMin) / fBuffer.Width;
  yDelta := (yMax - yMin) / fBuffer.Height;
  for j := 0 to fBuffer.Height - 1 do
  begin
    y := yMax - j * yDelta;
    for i := 0 to fBuffer.Width - 1 do
    begin
      x := xMin + i * xDelta;
      fMatrix[i, j] := CalcRule(x, y);
      fValMin := Min(fValMin, fMatrix[i, j]);
      fValMax := Max(fValMax, fMatrix[i, j]);
    end;
  end;
end;


end.
