{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uColorMapsLib;

{$mode objfpc}{$H+}

interface

uses
  Classes, uColorMaps;

type

  TTextEvent = procedure(Sender: TObject; const Text: string) of object;

  { TACColorMapItem }

  TACColorMapItem = class(TCollectionItem)
  private
    fMap: TACColorMap;
    procedure MapChange(Sender: TObject);
    procedure SetMap(Value: TACColorMap);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Map: TACColorMap read fMap write SetMap;
  end;

  TACColorMapItemEvent = procedure(Sender: TObject; Item: TACColorMapItem) of object;

  TACColorMapCollection = class(TCollection)
  private
    fName: string;
    fOwner: TPersistent;
    fModified: boolean;
    fOnChange: TACColorMapItemEvent;
    procedure SetName(Value: string);
  protected
    function GetOwner: TPersistent; override;
    function GetItem(Index: integer): TACColorMapItem; reintroduce;
    procedure SetItem(Index: integer; Value: TACColorMapItem); reintroduce;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; AOnChange: TACColorMapItemEvent);
    procedure Assign(Source: TPersistent); override;
    function Add: TACColorMapItem;
    function Insert(Index: integer): TACColorMapItem;
    property Name: string read FName write SetName;
    property Items[Index: integer]: TACColorMapItem read GetItem write SetItem; default;
    property Modified: boolean read fModified write fModified;
  end;

  TACColorMapsLibrary = class(TComponent)
  private
    fItems: TACColorMapCollection;
    fLoading: boolean;
    fListeners: TList;
    fOnChange: TACColorMapItemEvent;
    fOnBeginLoad: TNotifyEvent;
    fOnLoadMapFile: TTextEvent;
    fOnEndLoad: TNotifyEvent;
    procedure ItemsChange(Sender: TObject; Item: TACColorMapItem);
    procedure NotifyListeners;
    procedure SetItems(Value: TACColorMapCollection);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddListener(Event: TNotifyEvent);
    procedure RemoveListener(Event: TNotifyEvent);
    procedure LoadItems;
    function LoadItem(const path: string): TACColorMapItem;
    property Loading: boolean read fLoading;
    function FindColorMap(const MapName: string): TACColorMap;
  published
    property Items: TACColorMapCollection read fItems write SetItems;
    property OnChange: TACColorMapItemEvent read fOnChange write fOnChange;
    property OnBeginLoad: TNotifyEvent read fOnBeginLoad write fOnBeginLoad;
    property OnLoadMapFile: TTextEvent read fOnLoadMapFile write fOnLoadMapFile;
    property OnEndLoad: TNotifyEvent read fOnEndLoad write fOnEndLoad;
  end;


implementation

{$R colormap.res}

uses Forms, SysUtils, uFunctions;

type
  TChunkID = array[0..3] of UTF8Char;

  TACMLHeader = packed record
    ID: TChunkID;
    Count: integer;
  end;

{ TACColorMapItem }

constructor TACColorMapItem.Create(ACollection: TCollection);
begin
  fMap := TACColorMap.Create(nil);
  fMap.Name := 'Untitled';
  fMap.OnChange := @MapChange;
  inherited Create(ACollection);
end;

destructor TACColorMapItem.Destroy;
begin
  fMap.Free;
  inherited;
end;

procedure TACColorMapItem.Assign(Source: TPersistent);
begin
  if Source is TACColorMapItem then
  begin
    fMap.Assign(TACColorMapItem(Source).fMap);
  end
  else
  begin
    inherited; //raises an exception
  end;
end;

function TACColorMapItem.GetDisplayName: string;
begin
  Result := fMap.Name;
end;

procedure TACColorMapItem.MapChange(Sender: TObject);
begin
  Changed(False);
end;

procedure TACColorMapItem.SetMap(Value: TACColorMap);
begin
  fMap.Assign(Value);
  Changed(False);
end;

{ TACColorMapCollection }

constructor TACColorMapCollection.Create(AOwner: TPersistent;
  AOnChange: TACColorMapItemEvent);
begin
  fOwner := AOwner;
  fOnChange := AOnChange;
  inherited Create(TACColorMapItem);
end;

function TACColorMapCollection.Add: TACColorMapItem;
begin
  Result := TACColorMapItem(inherited Add);
  fModified := True;
end;

function TACColorMapCollection.Insert(Index: integer): TACColorMapItem;
begin
  Result := TACColorMapItem(inherited Insert(Index));
  fModified := True;
end;

procedure TACColorMapCollection.Assign(Source: TPersistent);
begin
  if Source is TACColorMapCollection then
  begin
    fName := TACColorMapCollection(Source).fName;
    fModified := True;
  end;
  inherited;
end;

function TACColorMapCollection.GetItem(Index: integer): TACColorMapItem;
begin
  Result := TACColorMapItem(inherited GetItem(Index));
end;

procedure TACColorMapCollection.SetItem(Index: integer; Value: TACColorMapItem);
begin
  fModified := True;
  inherited SetItem(Index, Value);
end;

function TACColorMapCollection.GetOwner: TPersistent;
begin
  Result := fOwner;
end;

procedure TACColorMapCollection.SetName(Value: string);
begin
  if Length(Value) > 31 then
    Value := Copy(Value, 1, 31);
  if fName <> Value then
  begin
    fName := Value;
    Changed;
  end;
end;

procedure TACColorMapCollection.Update(Item: TCollectionItem);
begin
  inherited;
  fModified := True;
  if Assigned(FOnChange) then
    fOnChange(Self, TACColorMapItem(Item));
end;

{ TACColorMapsLibrary }

constructor TACColorMapsLibrary.Create(AOwner: TComponent);
begin
  inherited;
  fLoading := False;
  fItems := TACColorMapCollection.Create(Self, @ItemsChange);
  fListeners := TList.Create;
end;

destructor TACColorMapsLibrary.Destroy;
var
  i: integer;
  Listener: TACNotifyListener;
begin
  for i := 0 to fListeners.Count - 1 do
  begin
    Listener := TACNotifyListener(fListeners[i]);
    Listener.Free;
  end;
  fListeners.Free;
  fItems.Free;
  inherited;
end;

procedure TACColorMapsLibrary.AddListener(Event: TNotifyEvent);
var
  Listener: TACNotifyListener;
begin
  Listener := TACNotifyListener.Create(Event);
  fListeners.Add(Listener);
end;

procedure TACColorMapsLibrary.RemoveListener(Event: TNotifyEvent);
var
  i: integer;
  Listener: TACNotifyListener;
begin
  for i := fListeners.Count - 1 downto 0 do
  begin
    Listener := TACNotifyListener(fListeners[i]);
    if (TMethod(Listener.Event).Code = TMethod(Event).Code) and
      (TMethod(Listener.Event).Data = TMethod(Event).Data) then
      fListeners.Delete(i);
  end;
end;

procedure TACColorMapsLibrary.NotifyListeners;
var
  i: integer;
  Listener: TACNotifyListener;
begin
  for i := 0 to fListeners.Count - 1 do
  begin
    Listener := TACNotifyListener(fListeners[i]);
    if Assigned(Listener.Event) then
      Listener.Event(Self);
  end;
end;

procedure TACColorMapsLibrary.ItemsChange(Sender: TObject; Item: TACColorMapItem);
begin
  if Assigned(fOnChange) then
    fOnChange(Self, Item);
  NotifyListeners;
end;

procedure TACColorMapsLibrary.SetItems(Value: TACColorMapCollection);
begin
  fItems.Assign(Value);
end;

function TACColorMapsLibrary.LoadItem(const path: string): TACColorMapItem;
begin
  Result := fItems.Add;
  //result.fMap.LoadFromFile(path);
end;

procedure TACColorMapsLibrary.LoadItems;
var
  i: integer;
  f: TResourceStream;
  m: TACColorMapItem;
  h: TACMLHeader;
begin
  fLoading := True;
  if Assigned(fOnBeginLoad) then
    fOnBeginLoad(Self);
  try
    f := TResourceStream.Create(hInstance, 'COLOR_MAPS.ACML', 'MAP');
    try
      f.Read(h, SizeOf(h));
      for i := 0 to h.Count - 1 do
      begin
        m := fItems.Add;
        m.fMap.LoadFromStream(f);
      end;
    finally
      f.Free;
    end;
  finally
    fLoading := False;
  end;
  NotifyListeners;
  if Assigned(fOnEndLoad) then
    fOnEndLoad(Self);
end;

function TACColorMapsLibrary.FindColorMap(const MapName: string): TACColorMap;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to fItems.Count - 1 do
    if AnsiSameText(fItems[i].fMap.Name, MapName) then
    begin
      Result := fItems[i].fMap;
      exit;
    end;
  raise EACColorMap.CreateFmt('Error: Color Map "%s" not found!', [MapName]);
end;


end.
