{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uCladni;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Graphics, uColorMaps, uColorMapsLib;

const

  AppTitle = 'Chladni Plate 2';
  MIN_AMPLITUDE = -1.0;
  MAX_AMPLITUDE = 1.0;
  MIN_ANGLE = -360.0;
  MAX_ANGLE = 360.0;
  ITERATION_MULTIPLIER = 256;

var

  MIN_FREQ_RATIO: double = 0.1;
  MAX_FREQ_RATIO: double = 20.0;

type

  PSingleArray = ^TSingleArray;
  TSingleArray = array[0..0] of single;

  TWaveInfo = record
    &On: boolean;
    Amplitude: single;
    Frequency: single;
    Phase: single;
  end;
  TWaveInfoArray = array of TWaveInfo;

  TCladni = class;
  ECladni = class(Exception);

  { TValueMap }

  TValueMap = class
  private
    fBits: PSingleArray;
    fHeight: integer;
    fWidth: integer;
    fOnChange: TNotifyEvent;
    fOnResize: TNotifyEvent;
    procedure SetHeight(NewHeight: integer);
    procedure SetWidth(NewWidth: integer);
    procedure ChangeSize(var aWidth, aHeight: integer; NewWidth, NewHeight: integer);
    function GetValPtr(X, Y: integer): PSingle;
    function GetValue(X, Y: integer): single;
    function GetScanline(Y: integer): PSingleArray;
    procedure SetValue(X, Y: integer; const Value: single);
  public
    constructor Create(aWidth, aHeight: integer);
    destructor Destroy; override;
    procedure Delete;
    procedure Changed;
    procedure Clear;
    procedure Resized;
    function Empty: boolean;
    function SetSize(NewWidth, NewHeight: integer): boolean;
    property Height: integer read fHeight write SetHeight;
    property Width: integer read fWidth write SetWidth;
    property ValPtr[X, Y: integer]: PSingle read GetValPtr;
    property Value[X, Y: integer]: single read GetValue write SetValue; default;
    property Scanline[Y: integer]: PSingleArray read GetScanline;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
  end;

  TRenderState = (rsStart, rsRun, rsEnd);
  TRenderProgressEvent = procedure(Sender: TObject; State: TRenderState;
    Value: integer) of object;

  { TRenderThread }

  TRenderThread = class(TThread)
  private
    fCladni: TCladni;
    fProgressMax: integer;
    fProgressValue: integer;
    fOnProgress: TRenderProgressEvent;
    procedure DoStart;
    procedure DoRun;
    procedure DoStop;
  public
    constructor Create(aCladni: TCladni; aTerminate: TNotifyEvent; aProgress: TRenderProgressEvent);
    procedure Execute; override;
  end;

  { TCladni }

  TCladni = class(TComponent)
  private
    fFileName: string;
    fBitmap: TBitmap;
    fMapsLibrary: TACColorMapsLibrary;
    fValueMap: TValueMap;
    fHeight: integer;
    fWidth: integer;
    fMapIndex: integer;
    fCapacity: integer;
    fModified: boolean;
    fNormalize: boolean;
    fSaveLevelMapWithFile: boolean;
    fRWidth: double;
    fRHeight: double;
    fValMax: double;
    fInfo: TWaveInfoArray;
    fThread: TRenderThread;
    fOnChange: TNotifyEvent;
    fOnLoad: TNotifyEvent;
    fOnRenderingDone: TNotifyEvent;
    fOnProgress: TRenderProgressEvent;
    procedure DoValueMapChanged(Sender: TObject);
    function GetTitle: string;
    function GetWaveInfo(index: integer): TWaveInfo;
    procedure SetCapacity(NewCapacity: integer);
    procedure SetMapIndex(AValue: integer);
    procedure SetMapsLibrary(Value: TACColorMapsLibrary);
    procedure LibraryChange(Sender: TObject);
    procedure SetNormalize(AValue: boolean);
    procedure SetSaveLevelMapWithFile(AValue: boolean);
    procedure ThreadStopped(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SetSize(NewWidth, NewHeight: integer): boolean;
    procedure Reset;
    procedure Changed;
    procedure SetParam(ACol, ARow: integer; const Value: string);
    procedure CalcLevelMap;
    procedure StopCalc;
    procedure MakeBitmap;
    procedure RandomParametrs;
    procedure SaveToFile(const aFileName: string);
    procedure LoadFromFile(const aFileName: string);
    function IsRendering: boolean;
    property Bitmap: TBitmap read fBitmap;
    property Height: integer read fHeight;
    property Width: integer read fWidth;
    property Capacity: integer read fCapacity write SetCapacity;
    property Title: string read GetTitle;
    property FileName: string read fFileName;
    property Modified: boolean read fModified;
    property MapIndex: integer read fMapIndex write SetMapIndex;
    property WaveInfo[index: integer]: TWaveInfo read GetWaveInfo;
    property MapsLibrary: TACColorMapsLibrary read fMapsLibrary write SetMapsLibrary;
    property Normalize: boolean read fNormalize write SetNormalize;
    property SaveLevelMapWithFile: boolean read fSaveLevelMapWithFile write SetSaveLevelMapWithFile;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnLoad: TNotifyEvent read fOnLoad write fOnLoad;
    property OnProgress: TRenderProgressEvent read fOnProgress write fOnProgress;
    property OnRenderingDone: TNotifyEvent read fOnRenderingDone write fOnRenderingDone;
  end;


implementation

uses Math, GraphType, ZStream, uRandom;

type

  TID = packed array[0..3] of UTF8Char;

  TFileHeader = packed record
    ID: TID;
    Version: word;
  end;


const
  Chl_Untitled = 'Untitled.chl';
  Chl_ID: TID = #164'CHL';


{ TValueMap }

constructor TValueMap.Create(aWidth, aHeight: integer);
begin
  inherited Create;
  fBits := nil;
  SetSize(aWidth, aHeight);
end;

destructor TValueMap.Destroy;
begin
  FreeMem(fBits);
  inherited Destroy;
end;

procedure TValueMap.Delete;
begin
  SetSize(0, 0);
end;

procedure TValueMap.Changed;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TValueMap.Clear;
begin
  FillChar(fBits^, Width * Height * SizeOf(single), 0);
  Changed;
end;

function TValueMap.Empty: boolean;
begin
  Result := not Assigned(fBits);
end;

procedure TValueMap.SetHeight(NewHeight: integer);
begin
  SetSize(Width, NewHeight);
end;

procedure TValueMap.SetWidth(NewWidth: integer);
begin
  SetSize(NewWidth, Height);
end;

procedure TValueMap.ChangeSize(var aWidth, aHeight: integer;
  NewWidth, NewHeight: integer);
begin
  aWidth := NewWidth;
  aHeight := NewHeight;
  ReallocMem(fBits, NewWidth * NewHeight * SizeOf(single));
end;

function TValueMap.GetValPtr(X, Y: integer): PSingle;
begin
  Result := @fBits^[X + Y * Width];
end;

function TValueMap.GetValue(X, Y: integer): single;
begin
  Result := fBits^[X + Y * Width];
end;

function TValueMap.GetScanline(Y: integer): PSingleArray;
begin
  Result := @fBits^[Y * Width];
end;

procedure TValueMap.SetValue(X, Y: integer; const Value: single);
begin
  fBits^[X + Y * Width] := Value;
end;

procedure TValueMap.Resized;
begin
  if Assigned(fOnResize) then
    fOnResize(Self);
end;

function TValueMap.SetSize(NewWidth, NewHeight: integer): boolean;
begin
  if NewWidth < 0 then
    NewWidth := 0;
  if NewHeight < 0 then
    NewHeight := 0;
  Result := (NewWidth <> fWidth) or (NewHeight <> fHeight);
  if Result then
  begin
    ChangeSize(fWidth, fHeight, NewWidth, NewHeight);
    Changed;
    Resized;
  end;
end;


{ TRenderThread }

constructor TRenderThread.Create(aCladni: TCladni; aTerminate: TNotifyEvent;
  aProgress: TRenderProgressEvent);
begin
  fProgressMax := 0;
  fProgressValue := 0;
  fCladni := aCladni;
  fOnProgress := aProgress;
  FreeOnTerminate := True;
  OnTerminate := aTerminate;
  inherited Create(False);
end;

procedure TRenderThread.DoStart;
begin
  if Assigned(fOnProgress) then
    fOnProgress(self, rsStart, fProgressMax);
end;

procedure TRenderThread.DoRun;
begin
  if Assigned(fOnProgress) then
    fOnProgress(self, rsRun, fProgressValue);
end;

procedure TRenderThread.DoStop;
begin
  if Assigned(fOnProgress) then
    fOnProgress(self, rsEnd, fProgressValue);
end;

procedure TRenderThread.Execute;
var
  x, y, z, g: integer;
  q, p, rx, ry, rp: double;
  v: single;
begin
  fProgressMax := fCladni.fHeight;
  rp := 1 / fProgressMax;
  Synchronize(@DoStart);
  fCladni.fValMax := 0;
  for y := 0 to fCladni.fHeight - 1 do
  begin
    ry := y * fCladni.fRHeight;
    for x := 0 to fCladni.fWidth - 1 do
    begin
      v := 0;
      rx := x * fCladni.fRWidth;
      for z := 0 to fCladni.fCapacity - 1 do
      begin
        if (not fCladni.fInfo[z].&On) or (fCladni.fInfo[z].Frequency < MIN_FREQ_RATIO) then
          continue;
        p := DegToRad(fCladni.fInfo[z].Phase);
        q := 2 * pi * fCladni.fInfo[z].Frequency;
        v += fCladni.fInfo[z].Amplitude * cos(q * rx + p) * cos(q * ry + p);
        if Terminated then
          break;
      end;
      v := Abs(v) * ITERATION_MULTIPLIER;
      fCladni.fValueMap[x, y] := v;
      fCladni.fValMax := Max(fCladni.fValMax, v);
      if Terminated then
        break;
    end;
    if Terminated then
      break;
    g := Round(100 * y * rp);
    if g > fProgressValue then
    begin
      fProgressValue := g;
      Synchronize(@DoRun);
    end;
  end;
  Synchronize(@DoStop);
end;

{ TCladni }

constructor TCladni.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fThread := nil;
  fValueMap := TValueMap.Create(0, 0);
  fValueMap.OnChange := @DoValueMapChanged;
  fBitmap := TBitmap.Create;
  fCapacity := 100;
  fHeight := 500;
  fWidth := 500;
  fNormalize := true;
  Reset;
end;

destructor TCladni.Destroy;
begin
  fValueMap.Free;
  fBitmap.Free;
  SetLength(fInfo, 0);
  SetMapsLibrary(nil);
  inherited Destroy;
end;

procedure TCladni.Reset;
begin
  fValMax := 0;
  fBitmap.Clear;
  fValueMap.Clear;
  SetSize(fWidth, fHeight);
  SetLength(fInfo, fCapacity);
  FillChar(fInfo[0], fCapacity * sizeof(TWaveInfo), 0);
  fModified := False;
  fFileName := Chl_Untitled;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TCladni.SetSize(NewWidth, NewHeight: integer): boolean;
begin
  NewWidth := EnsureRange(NewWidth, 10, 16384);
  NewHeight := EnsureRange(NewHeight, 10, 16384);
  fBitmap.SetSize(NewWidth, NewHeight);
  Result := fValueMap.SetSize(NewWidth, NewHeight);
  if Result then
  begin
    fWidth := NewWidth;
    fHeight := NewHeight;
    fRWidth := 1.0 / fWidth;
    fRHeight := 1.0 / fHeight;
  end;
end;

procedure TCladni.Changed;
begin
  fModified := True;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TCladni.RandomParametrs;
var
  i: integer;
  r: TRandom;
begin
  r := TRandom.Create;
  try
    for i := 0 to fCapacity - 1 do
    begin
      if (not fInfo[i].&On) then
        continue;
      fInfo[i].Amplitude := r.Random(MIN_AMPLITUDE, MAX_AMPLITUDE);
      fInfo[i].Frequency := r.Random(MIN_FREQ_RATIO, MAX_FREQ_RATIO);
      fInfo[i].Phase := r.Random(MIN_ANGLE, MAX_ANGLE);
    end;
  finally
    r.Free;
  end;
  if Assigned(fOnLoad) then
    fOnLoad(self);
end;

procedure TCladni.SetParam(ACol, ARow: integer; const Value: string);
var
  v: single;
  e: integer;
begin
  if (not (ACol in [1..4])) or (ACol > fCapacity) or (ARow = 0) then
    exit;
  Val(Value, v, e);
  if e <> 0 then
    v := 0;
  case ACol of
    1: fInfo[ARow - 1].&On := v > 0.5;
    2: fInfo[ARow - 1].Amplitude := EnsureRange(v, MIN_AMPLITUDE, MAX_AMPLITUDE);
    3: fInfo[ARow - 1].Frequency := EnsureRange(v, MIN_FREQ_RATIO, MAX_FREQ_RATIO);
    4: fInfo[ARow - 1].Phase := EnsureRange(v, MIN_ANGLE, MAX_ANGLE);
  end;
  Changed;
end;

procedure TCladni.DoValueMapChanged(Sender: TObject);
begin
  Changed;
end;

function TCladni.GetTitle: string;
begin
  Result := ExtractFileName(fFileName);
  if fModified then
    Result := '*' + Result;
end;

function TCladni.GetWaveInfo(index: integer): TWaveInfo;
begin
  FillChar(result, sizeof(result), 0);
  if (index >= 0) and (index < fCapacity) then
    result := fInfo[index];
end;

procedure TCladni.SetCapacity(NewCapacity: integer);
begin
  if fCapacity = NewCapacity then
    Exit;
  fCapacity := Max(1, NewCapacity);
  SetLength(fInfo, fCapacity);
  Changed;
end;

procedure TCladni.SetMapIndex(AValue: integer);
begin
  if fMapIndex = AValue then
    Exit;
  fMapIndex := AValue;
  MakeBitmap;
  Changed;
end;

procedure TCladni.SetMapsLibrary(Value: TACColorMapsLibrary);
begin
  if fMapsLibrary <> Value then
  begin
    if Assigned(fMapsLibrary) then
    begin
      fMapsLibrary.RemoveListener(@LibraryChange);
      fMapsLibrary.RemoveFreeNotification(Self);
    end;
    fMapsLibrary := Value;
    if Assigned(fMapsLibrary) then
    begin
      if fMapsLibrary.Items.Count > 0 then
        fMapsLibrary.FreeNotification(Self);
      fMapsLibrary.AddListener(@LibraryChange);
    end;
  end;
end;

procedure TCladni.LibraryChange(Sender: TObject);
begin
  // nothing
end;

procedure TCladni.SetNormalize(AValue: boolean);
begin
  if fNormalize = AValue then
    Exit;
  fNormalize := AValue;
  MakeBitmap();
  if Assigned(fOnRenderingDone) then
    fOnRenderingDone(self);
  Changed;
end;

procedure TCladni.SetSaveLevelMapWithFile(AValue: boolean);
begin
  if fSaveLevelMapWithFile = AValue then
    Exit;
  fSaveLevelMapWithFile := AValue;
  Changed;
end;

procedure TCladni.ThreadStopped(Sender: TObject);
begin
  fThread := nil;
  MakeBitmap();
  if Assigned(fOnRenderingDone) then
    fOnRenderingDone(self);
end;

procedure TCladni.CalcLevelMap;
begin
  if not IsRendering then
    fThread := TRenderThread.Create(self, @ThreadStopped, fOnProgress);
end;

procedure TCladni.StopCalc;
begin
  if IsRendering then
    fThread.Terminate;
end;

function TCladni.IsRendering: boolean;
begin
  result := Assigned(fThread);
end;

procedure TCladni.MakeBitmap;
var
  p: PByte;
  x, y: integer;
  bpp, ro, go, bo: byte;
  bto: TRawImageByteOrder;
  c: TACColor;
  map: TACColorMap;
begin
  if fMapsLibrary = nil then
    exit;
  map := fMapsLibrary.Items[fMapIndex].Map;
  if (fNormalize and (fValMax > 0)) then
    map.MaxIter := fValMax
  else
    map.MaxIter := ITERATION_MULTIPLIER;

  bto := fBitmap.RawImage.Description.ByteOrder;
  bpp := fBitmap.RawImage.Description.BitsPerPixel div 8;
  ro := fBitmap.RawImage.Description.RedShift div 8;
  go := fBitmap.RawImage.Description.GreenShift div 8;
  bo := fBitmap.RawImage.Description.BlueShift div 8;
  if bto = riboMSBFirst then
  begin
    ro := bpp - 1 - ro;
    go := bpp - 1 - go;
    bo := bpp - 1 - bo;
  end;
  fBitmap.BeginUpdate;
  try
    for y := 0 to fBitmap.Height - 1 do
    begin
      p := fBitmap.RawImage.GetLineStart(y);
      for x := 0 to fBitmap.Width - 1 do
      begin
        c := map.IterToACColor(fValueMap[x, y]);
        (p + ro)^ := c.R;
        (p + go)^ := c.G;
        (p + bo)^ := c.B;
        inc(p, bpp);
      end;
    end;
  finally
    fBitmap.EndUpdate();
  end;
end;

procedure TCladni.SaveToFile(const aFileName: string);
var
  fs: TGZFileStream;
  ms: TMemoryStream;
  v: word;
  c, y: integer;
begin
  v := IfThen(fSaveLevelMapWithFile, 11, 10);
  fs := TGZFileStream.Create(aFilename, gzopenwrite);
  try
    ms := TMemoryStream.Create;
    try
      ms.Write(Chl_ID, 4);
      ms.WriteWord(v);
      ms.WriteDWord(fCapacity);
      ms.WriteDWord(fMapIndex);
      ms.WriteDWord(fWidth);
      ms.WriteDWord(fHeight);
      ms.WriteByte(Ord(fNormalize));
      ms.Write(fInfo[0], sizeof(TWaveInfo) * fCapacity);
      if fSaveLevelMapWithFile then
        for y := 0 to fHeight - 1 do
          ms.Write(fValueMap.Scanline[y]^, sizeof(single) * fWidth);
      c := fs.Write(ms.Memory^, ms.Size);
      if c < ms.Size then
        raise ECladni.Create('Write error "' + aFilename + '"!');
    finally
      ms.Free;
    end;
  finally
    fs.Free;
  end;
  fModified := false;
  fFileName := aFileName;
end;

procedure TCladni.LoadFromFile(const aFileName: string);
const
  CHUNKSIZE = 4096;
var
  fs: TGZFileStream;
  ms: TMemoryStream;
  buf: PByteArray;
  c, y, w, h: integer;
  id: TID;
  v: word;
begin
  fs := TGZFileStream.Create(aFilename, gzopenread);
  try
    ms := TMemoryStream.Create;
    try
      buf := AllocMem(CHUNKSIZE);
      try
        repeat
          c := fs.read(buf^, CHUNKSIZE);
          ms.Write(buf^, c);
        until c < CHUNKSIZE;
      finally
        FreeMem(buf);
      end;
      ms.Seek(0, 0);
      ms.Read(id, 4);
      if id <> Chl_ID then
        raise ECladni.Create('Error reading "'+aFilename + '"!'+ sLineBreak +
          'Bad file signature or file corrupted.');
      v := ms.ReadWord();
      if v > 11 then
        raise ECladni.Create('Error reading "'+aFilename + '"!'+ sLineBreak +
          'Unknown file version or file corrupted.');
      fSaveLevelMapWithFile := v > 10;
      // read data
      fCapacity := ms.ReadDWord();
      fMapIndex := ms.ReadDWord();
      w := ms.ReadDWord();
      h := ms.ReadDWord();
      fNormalize := ms.ReadByte > 0;
      SetLength(fInfo, fCapacity);
      ms.Read(fInfo[0], sizeof(TWaveInfo) * fCapacity);
      fValueMap.Delete;
      SetSize(w, h);
      if fSaveLevelMapWithFile then
        for y := 0 to fHeight - 1 do
          ms.Read(fValueMap.Scanline[y]^, sizeof(single) * fWidth);
    finally
      ms.Free;
    end;
  finally
    fs.Free;
  end;
  fFileName := aFileName;
  fModified := false;
  if Assigned(fOnLoad) then
    fOnLoad(self);
end;


end.
