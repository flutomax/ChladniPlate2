{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uColorMaps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  TACColor = packed record
    case integer of
      0: (R, G, B: Byte);
      1: (C: packed array[0..2] of Byte);
  end;

  TACPalette = array[0..255] of TACColor;

  EACColorMap = class(Exception);

  { TACNotifyListener }

  TACNotifyListener = class
  private
    fEvent: TNotifyEvent;
  public
    constructor Create(AEvent: TNotifyEvent);
    property Event: TNotifyEvent read fEvent;
  end;

  { TACColorMap }

  TACColorMap = class(TComponent, IStreamPersist)
  private
    fMaxIter: double;
    fRecMaxCol: double;
    fMapName: string;
    fPalette: TACPalette;
    fOffset: double;
    fOffs: double;
    fRepeating: double;
    fOnChange: TNotifyEvent;
    fOnLoad: TNotifyEvent;
    procedure Calc;
    procedure SetMaxIter(const NewIter: double);
    procedure RaiseInvalidStream;
    function GetLastColor: TColor;
    procedure SetMapName(Value: string);
    function GetFirstColor: TColor;
    procedure SetOffset(const Value: double);
    procedure SetRepeating(const Value: double);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure DrawToCanvas(const Canvas: TCanvas; const Horizontal: boolean;
      const DstRect: TRect);
    function IterToColor(Iter: double): TColor;
    function IterToACColor(Iter: double): TACColor;
  published
    property Offset: double read fOffset write SetOffset;
    property Repeating: double read fRepeating write SetRepeating;
    property MaxIter: double read fMaxIter write SetMaxIter;
    property FirstColor: TColor read GetFirstColor;
    property LastColor: TColor read GetLastColor;
    property MapName: string read fMapName write SetMapName;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnLoad: TNotifyEvent read fOnLoad write fOnLoad;
  end;

implementation

uses Math, uFunctions;

const
  K_PERCENT = 100;

{ TACNotifyListener }

constructor TACNotifyListener.Create(AEvent: TNotifyEvent);
begin
  fEvent := AEvent;
end;

{ TACColorMap }

constructor TACColorMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MaxIter := 100;
  fOffset := 0;
  fOffs := 0;
  fRepeating := 1;
  fMapName := 'Default';
end;

destructor TACColorMap.Destroy;
begin

  inherited;
end;

procedure TACColorMap.Reset;
begin
  FillChar(fPalette, sizeof(fPalette), 0);
  MaxIter := 100;
  fMapName := '';
end;

procedure TACColorMap.Changed;
begin
  if Assigned(fOnChange) then
    fOnChange(self);
end;

procedure TACColorMap.Assign(Source: TPersistent);
var
  m: TMemoryStream;
begin
  if Source is TACColorMap then
  begin
    m := TMemoryStream.Create;
    try
      TACColorMap(Source).SaveToStream(m);
      m.Seek(0, 0);
      LoadFromStream(m);
    finally
      m.Free;
    end;
    Changed;
  end
  else
  begin
    inherited; // raise exception
  end;
end;

procedure TACColorMap.DrawToCanvas(const Canvas: TCanvas;
  const Horizontal: boolean; const DstRect: TRect);
var
  x, y: integer;
begin
  if Horizontal then
  begin
    MaxIter := DstRect.Width;
    for x := DstRect.Left to DstRect.Right do
    begin
      Canvas.Pen.Color := IterToColor(x - DstRect.Left);
      Canvas.Line(x, DstRect.Top, x, DstRect.Bottom + 1);
    end;
  end else begin
    MaxIter := DstRect.Height;
    for y := DstRect.Top to DstRect.Bottom do
    begin
      Canvas.Pen.Color := IterToColor(y - DstRect.Top);
      Canvas.Line(DstRect.Left, y, DstRect.Right + 1, y);
    end;
  end;
end;

procedure TACColorMap.Calc;
begin
  if fMaxIter > 0 then
    fRecMaxCol := 1 / fMaxIter
  else
    fRecMaxCol := 0;
  fOffs := fOffset * fMaxIter * 0.01;
end;

procedure TACColorMap.SetMaxIter(const NewIter: double);
begin
  if fMaxIter = NewIter then
    exit;
  fMaxIter := NewIter;
  Calc;
end;

procedure TACColorMap.SetMapName(Value: string);
begin
  if Length(Value) > 31 then
    Value := Copy(Value, 1, 31);
  if fMapName <> Value then
  begin
    fMapName := Value;
    Changed;
  end;
end;

procedure TACColorMap.SetOffset(const Value: double);
begin
  fOffset := EnsureRange(Value, 0, 100);
  Calc;
end;

procedure TACColorMap.SetRepeating(const Value: double);
begin
  fRepeating := EnsureRange(Value, 0, 100);
end;

function TACColorMap.GetLastColor: TColor;
begin
  Result := TACColorToTColor(fPalette[255]);
end;

function TACColorMap.GetFirstColor: TColor;
begin
  Result := TACColorToTColor(fPalette[0]);
end;

function TACColorMap.IterToColor(Iter: double): TColor;
begin
  Result := TACColorToTColor(IterToACColor(Iter));
end;

function TACColorMap.IterToACColor(Iter: double): TACColor;
var
  i, w: integer;
  n: double;
begin
  if Iter >= fMaxIter then
  begin
    Result := fPalette[255];
    exit;
  end;
  if Iter <= 0 then
  begin
    Result := fPalette[0];
    exit;
  end;

  n := Frac(Iter * fRecMaxCol);
  n := n * 255;
  i := Trunc(n);
  n := n - i;
  w := Round(n * K_PERCENT);
  w := EnsureRange(w, 0, K_PERCENT);
  Result := MixColors(fPalette[i + 1], fPalette[i], w);
end;

procedure TACColorMap.RaiseInvalidStream;
begin
  raise EStreamError.Create('The color map file is not valid.');
end;

procedure TACColorMap.LoadFromStream(Stream: TStream);
begin
  fMapName := ReadStringFromStream(Stream);
  Stream.Read(fPalette, SizeOf(fPalette));
  if Assigned(fOnLoad) then
    fOnLoad(Self);
end;

procedure TACColorMap.SaveToStream(Stream: TStream);
begin
  WriteStringToStream(Stream, fMapName);
  Stream.Write(fPalette, SizeOf(fPalette));
end;


end.

