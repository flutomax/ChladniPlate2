{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ImgList, uColorMaps;

function MixColors(Color1, Color2: TColor; Percent: integer): TColor; overload;
function MixColors(Color1, Color2: TACColor; Percent: integer): TACColor; overload;
function TACColorToTColor(const Color: TACColor): TColor;
function ReadStringFromStream(Stream: TStream): string;
procedure WriteStringToStream(Stream: TStream; const Text: string);
procedure DrawDisabledImagelist(src, dst: TCustomImageList);

implementation

uses
  LCLType, Math, FPImage, IntfGraphics, GraphType;

function MixColors(Color1, Color2: TColor; Percent: integer): TColor;
begin
  Result := (((Color1 and $FF) * Percent + (Color2 and $FF) * (100 - Percent)) div
    100) or (((Color1 shr 8 and $FF) * Percent + (Color2 shr 8 and $FF) *
    (100 - Percent)) div 100 shl 8) or
    (((Color1 shr 16 and $FF) * Percent + (Color2 shr 16 and $FF) * (100 - Percent)) div
    100 shl 16);
end;

function MixColors(Color1, Color2: TACColor; Percent: integer): TACColor;
begin
  Result.R := (Color1.R * Percent + Color2.R * (100 - Percent)) div 100;
  Result.G := (Color1.G * Percent + Color2.G * (100 - Percent)) div 100;
  Result.B := (Color1.B * Percent + Color2.B * (100 - Percent)) div 100;
end;

function TACColorToTColor(const Color: TACColor): TColor;
begin
  with Color do
    Result := R or (G shl 8) or (B shl 16);
end;


procedure WriteStringToStream(Stream: TStream; const Text: string);
var
  len, d, r: integer;
  b: byte;
  s: RawByteString;
begin
  s := UTF8Encode(Text);
  len := Length(s);
  d := len and $7F;
  r := len shr 7;
  while r <> 0 do
  begin
    d := d shl 8;
    d := d or ((r and $7f) or $80);
    r := r shr 7;
  end;
  repeat
    b := byte(d and $FF);
    Stream.Write(b, 1);
    if (b and $80) = $80 then
      d := d shr 8
    else
      break;
  until b = 0;
  Stream.Write(s[1], len);
end;

function ReadStringFromStream(Stream: TStream): string;
var
  b: byte;
  i: integer;
  s: RawByteString;
begin
  i := 0;
  repeat
    Stream.Read(b, 1);
    i := (i shl 7) + (b and $7f);
  until (b and $80) = 0;
  SetLength(s, i);
  Stream.Read(s[1], i);
  Result := UTF8Decode(s);
end;

procedure DrawDisabledImagelist(src, dst: TCustomImageList);
var
  i, x, y: integer;
  b: TBitmap;
  m: TLazIntfImage;
  fd: TRawImageDescription;
  c: TFPColor;
  ih, mh: HBitmap;
begin
  dst.Clear;
  b := TBitmap.Create;
  m := TLazIntfImage.Create(0, 0);
  try
    b.Width := src.Width;
    b.Height := src.Height;
    fd.Init_BPP32_B8G8R8_BIO_TTB(b.Width, b.Height);
    m.DataDescription := fd;
    for i := 0 to src.Count - 1 do
    begin
      src.GetBitmap(i, b);
      m.LoadFromBitmap(b.Handle, b.MaskHandle);
      for y := 0 to m.Height - 1 do
        for x := 0 to m.Width - 1 do
        begin
          c := m.Colors[x, y];
          c.alpha := c.alpha div 3;
          m.Colors[x, y] := c;
        end;
      m.CreateBitmaps(ih, mh, False);
      b.SetHandles(ih, mh);
      dst.Add(b, nil);
    end;
  finally
    b.Free;
    m.Free;
  end;
end;

end.

