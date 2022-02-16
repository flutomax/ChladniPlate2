{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uVirtualListBox;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLProc, LCLIntf, LMessages, Classes, SysUtils, Graphics, Controls,
  Forms, uColorMaps, uColorMapsLib;

type
  TACVirtualItemEvent = procedure(Sender: TObject; Index: integer) of object;
  TACVirtualItemDrawEvent = procedure(Sender: TObject; Index: integer;
    Canvas: TCanvas; ItemRect: TRect; State: TOwnerDrawState) of object;
  TACVirtualItemMeasureEvent = procedure(Sender: TObject; Index: integer;
    var ItemHeight: integer) of object;
  TACVirtualItemMouseEvent = procedure(Sender: TObject; Index: integer;
    Button: TMouseButton; Shift: TShiftState; X, Y: integer) of object;
  TACVirtualItemMouseMoveEvent = procedure(Sender: TObject; Index: integer;
    Shift: TShiftState; X, Y: integer) of object;
  TACVirtualItemMovedEvent = procedure(Sender: TObject;
    FromIndex, ToIndex: longint) of object;

  TACVirtualListItem = record
    Pos: integer;
    Height: integer;
  end;

  TACVirtualListBoxOption = (avoAutoSelect, avoSeparateLast, avoAllowMoving,
    avoEraseBackground);
  TACVirtualListBoxOptions = set of TACVirtualListBoxOption;


  { TACCustomVirtualListBox }

  TACCustomVirtualListBox = class(TCustomControl)
  private
    fItemList: array of TACVirtualListItem;
    fScrollSize: integer;
    fScrollPos: integer;
    fCount: integer;
    fCapacity: integer;
    fItemIndex: integer;
    fOptions: TACVirtualListBoxOptions;
    fSeparatorSize: integer;
    fSeparatorColor: TColor;
    fSeparatorDisabled: TColor;
    fBorderStyle: TBorderStyle;
    fDropIndex: integer;
    fScrollSpeed: integer;
    fDragPos: integer;
    fDragIndex: integer;
    fMousePos: TPoint;
    fMouseIndex: integer;
    fMouseDown: boolean;
    fHotTracking: boolean;
    fHotTrack: boolean;
    fChildFocused: boolean;
    fOnItemDraw: TACVirtualItemDrawEvent;
    fOnItemMeasure: TACVirtualItemMeasureEvent;
    fOnItemMoved: TACVirtualItemMovedEvent;
    fOnItemMouseMove: TACVirtualItemMouseMoveEvent;
    fOnItemMouseDown: TACVirtualItemMouseEvent;
    fOnItemMouseUp: TACVirtualItemMouseEvent;
    fOnItemMouseEnter: TACVirtualItemEvent;
    fOnItemMouseLeave: TACVirtualItemEvent;
    fOnItemSelected: TACVirtualItemEvent;
    procedure ChangeScrollSize(NewSize: integer);
    procedure CMWantSpecialKey(var Msg: TLMessage); message CM_WANTSPECIALKEY;
    procedure CMMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TLMessage); message CM_MOUSEENTER;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
    procedure WMTimer(var Msg: TLMTimer); message LM_TIMER;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    procedure SetItemIndex(Value: integer);
    procedure SetOptions(Value: TACVirtualListBoxOptions);
    procedure SetHotTrack(const Value: boolean);
    procedure SetSeparatorSize(Value: integer);
    procedure SetSeparatorColor(Value: TColor);
    procedure SetSeparatorDisabled(Value: TColor);
    procedure SetOnItemDraw(Value: TACVirtualItemDrawEvent);
    procedure SetOnItemMeasure(Value: TACVirtualItemMeasureEvent);
  protected
    function GetClientRect: TRect; override;
    procedure SetCount(Value: integer);
    procedure ItemDraw(Index: integer; ACanvas: TCanvas; ItemRect: TRect;
      State: TOwnerDrawState); virtual;
    procedure ItemMeasure(Index: integer; var ItemHeight: integer); virtual;
    procedure ItemMoved(FromIndex, ToIndex: integer); virtual;
    procedure ItemMouseMove(Index: integer; Shift: TShiftState; X, Y: integer); virtual;
    procedure ItemMouseDown(Index: integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer); virtual;
    procedure ItemMouseUp(Index: integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer); virtual;
    procedure ItemMouseEnter(Index: integer); virtual;
    procedure ItemMouseLeave(Index: integer); virtual;
    procedure ItemSelected(Index: integer); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetEnabled(Value: boolean); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: integer); override;
    procedure DragOver(Source: TObject; X, Y: integer; State: TDragState;
      var Accept: boolean); override;
    procedure AddItems(const ACount: integer = 1); virtual;
    procedure InsertItems(const Index: integer; const ACount: integer = 1); virtual;
    procedure DeleteItem(const Index: integer); virtual;
    procedure DeleteItems(const FromIndex, ToIndex: integer); virtual;
    procedure ClearItems; virtual;
    function MovingAllowed: boolean; virtual;
    property Count: integer read fCount;
    property ItemIndex: integer read fItemIndex write SetItemIndex;
    property Options: TACVirtualListBoxOptions read fOptions write SetOptions;
    property SeparatorSize: integer read fSeparatorSize write SetSeparatorSize;
    property SeparatorColor: TColor read fSeparatorColor write SetSeparatorColor;
    property SeparatorDisabled: TColor read fSeparatorDisabled
      write SetSeparatorDisabled;
    property BorderStyle: TBorderStyle
      read fBorderStyle write SetBorderStyle default bsSingle;
    property OnItemDraw: TACVirtualItemDrawEvent read fOnItemDraw write SetOnItemDraw;
    property OnItemMeasure: TACVirtualItemMeasureEvent
      read fOnItemMeasure write SetOnItemMeasure;
    property OnItemMoved: TACVirtualItemMovedEvent read fOnItemMoved write fOnItemMoved;
    property OnItemMouseMove: TACVirtualItemMouseMoveEvent
      read fOnItemMouseMove write fOnItemMouseMove;
    property OnItemMouseDown: TACVirtualItemMouseEvent
      read fOnItemMouseDown write fOnItemMouseDown;
    property OnItemMouseUp: TACVirtualItemMouseEvent
      read fOnItemMouseUp write fOnItemMouseUp;
    property OnItemMouseEnter: TACVirtualItemEvent
      read fOnItemMouseEnter write fOnItemMouseEnter;
    property OnItemMouseLeave: TACVirtualItemEvent
      read fOnItemMouseLeave write fOnItemMouseLeave;
    property OnItemSelected: TACVirtualItemEvent
      read fOnItemSelected write fOnItemSelected;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    procedure Scroll(DeltaY: integer);
    procedure DragDrop(Source: TObject; X, Y: integer); override;
    function ItemAtPos(Y: integer): integer;
    function SeparatorAtPos(Y: integer): integer;
    procedure InvalidateItems(FirstIndex, LastIndex: integer);
    procedure InvalidateAllItems;
    procedure MakeVisible(Index: integer; PartialOK: boolean); overload;
    procedure SelectItem(Index: integer; MoveItem: boolean = False);
    property HotTracking: boolean read fHotTracking;
  published
    property HotTrack: boolean read fHotTrack write SetHotTrack default True;
  end;

  TACColorControlLook = (aclRect, aclRoundedRect, aclLowered, aclRaised);


  { TACColorMapsListBox }

  TACColorMapsListBox = class(TACCustomVirtualListBox)
  private
    fMapsLibrary: TACColorMapsLibrary;
    fDitherBitmap: TBitmap;
    fLook: TACColorControlLook;
    fOnApply: TNotifyEvent;
    fOnChange: TNotifyEvent;
    procedure SetCollection(Value: TACColorMapCollection);
    procedure SetLook(Value: TACColorControlLook);
    procedure SetMapsLibrary(const Value: TACColorMapsLibrary);
    procedure LibraryChange(Sender: TObject);
    procedure MakeDitherBitmap;
    function GetSelectedMap: TACColorMap;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopupContextMenu(const X, Y: integer); virtual;
    procedure ItemDraw(Index: integer; ACanvas: TCanvas; ItemRect: TRect;
      State: TOwnerDrawState); override;
    procedure ItemMeasure(Index: integer; var ItemHeight: integer); override;
    procedure ItemMoved(FromIndex, ToIndex: integer); override;
    procedure DblClick; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItems(const ACount: integer = 1); override;
    procedure InsertItems(const Index: integer; const ACount: integer = 1); override;
    procedure DeleteItem(const Index: integer); override;
    procedure DeleteItems(const FromIndex, ToIndex: integer); override;
    procedure ClearItems; override;
    procedure SelectMapName(const aMapName: string);
    procedure SelectDefaultMap;
    function IndexOfMapName(const aName: string): integer;
    property Count;
    property ItemIndex;
    property SelectedMap: TACColorMap read GetSelectedMap;
  published
    property MapsLibrary: TACColorMapsLibrary read fMapsLibrary write SetMapsLibrary;
    property Look: TACColorControlLook read fLook write SetLook default aclRoundedRect;
    property Options;
    property SeparatorSize;
    property SeparatorColor;
    property SeparatorDisabled;
    property BorderStyle;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnApply: TNotifyEvent read fOnApply write fOnApply;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnItemDraw;
    property OnItemMeasure;
    property OnItemMoved;
    property OnItemMouseMove;
    property OnItemMouseDown;
    property OnItemMouseUp;
    property OnItemMouseEnter;
    property OnItemMouseLeave;
    property OnItemSelected;
  end;

implementation

uses uFunctions;

const
  ScrollInterval = 20;
  ScrollMaxSpeed = 200;
  DefaultColorMapItemHeight = 40;

{ TACCustomVirtualListBox }

constructor TACCustomVirtualListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCount := 0;
  fCapacity := 16;
  fScrollSize := 0;
  fScrollPos := 0;
  fItemIndex := -1;
  fSeparatorSize := 1;
  fSeparatorColor := clBtnFace;
  fSeparatorDisabled := clBtnShadow;
  fBorderStyle := bsSingle;
  fOptions := [avoAutoSelect, avoSeparateLast, avoEraseBackground];
  fDropIndex := -1;
  fScrollSpeed := 0;
  fDragPos := -1;
  fMouseIndex := -1;
  fMouseDown := False;
  fHotTrack := True;
  fOnItemDraw := nil;
  fOnItemMeasure := nil;
  fOnItemMoved := nil;
  fOnItemMouseMove := nil;
  fOnItemMouseDown := nil;
  fOnItemMouseUp := nil;
  fOnItemMouseEnter := nil;
  fOnItemMouseLeave := nil;
  fOnItemSelected := nil;
  SetLength(fItemList, fCapacity);
  TabStop := True;
  ParentColor := False;
  Color := clWindow;
end;

destructor TACCustomVirtualListBox.Destroy;
begin
  SetLength(fItemList, 0);
  inherited Destroy;
end;

procedure TACCustomVirtualListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_VSCROLL;
  Params.Style := Params.Style and not WS_BORDER;
  Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
end;

function TACCustomVirtualListBox.GetClientRect: TRect;
begin
  if WindowHandle <> 0 then
    Result := inherited GetClientRect
  else
    Result := Rect(0, 0, Width, Height);
end;

procedure TACCustomVirtualListBox.Paint;
var
  Index: integer;
  State: TOwnerDrawState;
  ItemRect: TRect;
begin
  inherited;
  ItemRect.Left := ClientRect.Left;
  ItemRect.Right := ClientRect.Right;
  ItemRect.Top := Canvas.ClipRect.Top;
  ItemRect.Bottom := ItemRect.Top;
  Index := ItemAtPos(ItemRect.Top);
  if Index >= 0 then
  begin
    ItemRect.Top := fItemList[Index].Pos - fScrollPos;
    while (Index < fCount) and (ItemRect.Top < Canvas.ClipRect.Bottom) do
    begin
      ItemRect.Bottom := ItemRect.Top + fItemList[Index].Height;
      Canvas.Brush.Style := bsSolid;
      Canvas.Pen.Style := psSolid;
      if not Enabled then
      begin
        State := [odGrayed];
        if Index = fItemIndex then
          Include(State, odSelected);
        Canvas.Brush.Color := clBtnFace;
        Canvas.Font.Color := clBtnShadow;
        Canvas.Pen.Color := clBtnShadow;
      end
      else if Index = fItemIndex then
      begin
        if Focused then
        begin
          State := [odSelected, odFocused];
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;
          Canvas.Pen.Color := clHighlightText;
        end
        else
        begin
          State := [odSelected];
          Canvas.Brush.Color :=
            MixColors(ColorToRGB(clHighlight), ColorToRGB(Color), 50);
          Canvas.Font.Color := clWindowText;
          Canvas.Pen.Color := clWindowText;
        end;
      end
      else
      begin
        State := [];
        Canvas.Brush.Color := Color;
        Canvas.Font.Color := clWindowText;
        Canvas.Pen.Color := clWindowText;
      end;
      if Index = fDropIndex then
      begin
        Inc(ItemRect.Top, 2);
        Inc(ItemRect.Bottom, 2);
      end;
      if avoEraseBackground in fOptions then
        Canvas.FillRect(ItemRect);
      ItemDraw(Index, Canvas, ItemRect, State);
      if Index = fDropIndex then
      begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := fSeparatorColor;
        Canvas.FillRect(Rect(ItemRect.Left, ItemRect.Top - fSeparatorSize -
          2, ItemRect.Right, ItemRect.Top));
      end;
      if (fSeparatorSize > 0) and ((Index < fCount - 1) or
        (avoSeparateLast in fOptions)) then
      begin
        ItemRect.Top := ItemRect.Bottom;
        ItemRect.Bottom := ItemRect.Top + fSeparatorSize;
        Canvas.Brush.Style := bsSolid;
        if Enabled then
          Canvas.Brush.Color := fSeparatorColor
        else
          Canvas.Brush.Color := fSeparatorDisabled;
        Canvas.FillRect(ItemRect);
      end;
      ItemRect.Top := ItemRect.Bottom;
      Inc(Index);
    end;
    if Index = fDropIndex then
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := fSeparatorColor;
      Canvas.FillRect(Rect(ItemRect.Left, ItemRect.Top - fSeparatorSize -
        2, ItemRect.Right, ItemRect.Top));
    end;
  end;
  if ItemRect.Bottom < Canvas.ClipRect.Bottom then
  begin
    ItemRect.Top := ItemRect.Bottom;
    ItemRect.Bottom := Canvas.ClipRect.Bottom;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(ItemRect);
  end;
end;

procedure TACCustomVirtualListBox.Resize;
var
  ScrollInfo: TScrollInfo;
begin
  inherited;
  if fScrollPos > fScrollSize - ClientHeight then
  begin
    if fScrollSize > ClientHeight then
      fScrollPos := fScrollSize - ClientHeight
    else if fScrollPos > 0 then
      fScrollPos := 0;
  end;
  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_TRACKPOS or SIF_PAGE or SIF_RANGE or SIF_DISABLENOSCROLL;
  ScrollInfo.nPage := ClientHeight;
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := fScrollSize - 1;
  ScrollInfo.nPos := fScrollPos;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

procedure TACCustomVirtualListBox.SetEnabled(Value: boolean);
begin
  inherited;
  if Enabled then
    EnableScrollBar(Handle, SB_VERT, 0)
  else
    EnableScrollBar(Handle, SB_VERT, 3);
  Invalidate;
end;

procedure TACCustomVirtualListBox.SetFocus;

  function GetBaseParent(Control: TControl): TWinControl;
  begin
    while Control.Parent <> nil do
      Control := Control.Parent;
    if Control is TWinControl then
      Result := TWinControl(Control)
    else
      Result := nil;
  end;

var
  LParent: TWinControl;
begin
  LParent := GetBaseParent(Self);
  if LParent is TCustomForm then
    inherited
  // Bug in TWinControl.SetFocus (Delphi 5):
  // Control must either have a parent form or a window handle <> 0.
  // We also allow a parent (or parent of parent) with window handle <> 0
  else if Parent.ParentWindow <> 0 then
    LCLIntf.SetFocus(Handle);
  Invalidate;
end;

procedure TACCustomVirtualListBox.ItemDraw(Index: integer; ACanvas: TCanvas;
  ItemRect: TRect; State: TOwnerDrawState);
begin
  if Assigned(fOnItemDraw) then
    fOnItemDraw(Self, Index, ACanvas, ItemRect, State);
end;

procedure TACCustomVirtualListBox.ItemMeasure(Index: integer; var ItemHeight: integer);
begin
  if Assigned(fOnItemMeasure) then
    fOnItemMeasure(Self, Index, ItemHeight);
end;

procedure TACCustomVirtualListBox.ItemMoved(FromIndex, ToIndex: integer);
begin
  if Assigned(fOnItemMoved) then
    fOnItemMoved(Self, FromIndex, ToIndex);
end;

procedure TACCustomVirtualListBox.ItemMouseMove(Index: integer;
  Shift: TShiftState; X, Y: integer);
begin
  if Assigned(fOnItemMouseMove) then
    fOnItemMouseMove(Self, Index, Shift, X, Y);
end;

procedure TACCustomVirtualListBox.ItemMouseDown(Index: integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Assigned(fOnItemMouseDown) then
    fOnItemMouseDown(Self, Index, Button, Shift, X, Y);
end;

procedure TACCustomVirtualListBox.ItemMouseUp(Index: integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Assigned(fOnItemMouseUp) then
    fOnItemMouseUp(Self, Index, Button, Shift, X, Y);
end;

procedure TACCustomVirtualListBox.ItemMouseEnter(Index: integer);
begin
  if Assigned(fOnItemMouseEnter) then
    fOnItemMouseEnter(Self, Index);
end;

procedure TACCustomVirtualListBox.ItemMouseLeave(Index: integer);
begin
  if Assigned(fOnItemMouseLeave) then
    fOnItemMouseLeave(Self, Index);
end;

procedure TACCustomVirtualListBox.ItemSelected(Index: integer);
begin
  if Assigned(fOnItemSelected) then
    fOnItemSelected(Self, Index);
end;

procedure TACCustomVirtualListBox.WMVScroll(var Msg: TLMScroll);
var
  ScrollInfo: TScrollInfo;
  DeltaY: integer;
begin
  case Msg.ScrollCode of
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
    begin
      ScrollInfo.cbSize := SizeOf(TScrollInfo);
      ScrollInfo.fMask := SIF_TRACKPOS or SIF_DISABLENOSCROLL;
      GetScrollInfo(Handle, SB_VERT, ScrollInfo);
      DeltaY := fScrollPos - ScrollInfo.nTrackPos;
      fScrollPos := ScrollInfo.nTrackPos;
      if fScrollPos > (fScrollSize - ClientHeight) then
        fScrollPos := (fScrollSize - ClientHeight);
      ScrollInfo.fMask := SIF_POS or SIF_DISABLENOSCROLL;
      ScrollInfo.nPos := fScrollPos;
      SetScrollInfo(Handle, SB_VERT, ScrollInfo, False);
      ScrollWindowEx(Handle, 0, DeltaY, nil, nil, 0, nil, SW_INVALIDATE);
    end;
    SB_LINEUP: Scroll(10);
    SB_LINEDOWN: Scroll(-10);
    SB_PAGEUP: Scroll(ClientHeight div 2);
    SB_PAGEDOWN: Scroll(-ClientHeight div 2);
  end;
  Msg.Result := 0;
end;

procedure TACCustomVirtualListBox.WMEraseBkgnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

procedure TACCustomVirtualListBox.CMWantSpecialKey(var Msg: TLMessage);
begin
  inherited;
  case Msg.wParam of
    VK_UP, VK_DOWN:
      Msg.Result := 1;
  end;
end;

procedure TACCustomVirtualListBox.WMTimer(var Msg: TLMTimer);
var
  NewDropIndex: integer;
begin
  if fScrollSpeed > 0 then
  begin
    Scroll(fScrollSpeed div 10);
    if fScrollSpeed < ScrollMaxSpeed then
      Inc(fScrollSpeed, 2);
    if fScrollPos > 0 then
      SetTimer(Handle, 0, ScrollInterval, nil);
  end
  else if fScrollSpeed < 0 then
  begin
    Scroll(fScrollSpeed div 10);
    if fScrollSpeed > -ScrollMaxSpeed then
      Dec(fScrollSpeed, 2);
    if fScrollPos < fScrollSize - ClientHeight then
      SetTimer(Handle, 0, ScrollInterval, nil);
  end;
  if fDragPos >= 0 then
  begin
    NewDropIndex := SeparatorAtPos(fDragPos);
    if fDropIndex <> NewDropIndex then
    begin
      fDropIndex := NewDropIndex;
      Invalidate;
    end;
  end;
end;

procedure TACCustomVirtualListBox.CMMouseLeave(var Msg: TLMessage);
begin
  if fMouseIndex >= 0 then
  begin
    ItemMouseLeave(fMouseIndex);
    fMouseIndex := -1;
  end;
  inherited;
  if fHotTrack and fHotTracking and not fChildFocused then
  begin
    fHotTracking := False;
  end;
end;

procedure TACCustomVirtualListBox.CMMouseEnter(var Msg: TLMessage);
begin
  inherited;
  if fHotTrack and not fHotTracking then
  begin
    fHotTracking := True;
  end;
end;

procedure TACCustomVirtualListBox.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TACCustomVirtualListBox.WMSetFocus(var Msg: TLMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TACCustomVirtualListBox.WMKillFocus(var Msg: TLMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TACCustomVirtualListBox.KeyDown(var Key: word; Shift: TShiftState);
begin
  if Key = VK_UP then
  begin
    SelectItem(ItemIndex - 1, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_DOWN then
  begin
    SelectItem(ItemIndex + 1, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_PRIOR then
  begin
    SelectItem(ItemIndex - 10, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_NEXT then
  begin
    SelectItem(ItemIndex + 10, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_HOME then
  begin
    SelectItem(0, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end
  else if Key = VK_END then
  begin
    SelectItem(fCount - 1, (ssShift in Shift) and MovingAllowed);
    Key := 0;
  end;
  inherited;
end;

procedure TACCustomVirtualListBox.MouseMove(Shift: TShiftState; X, Y: integer);
var
  NewMouseIndex: integer;
begin
  if MovingAllowed and fMouseDown and ((Abs(fMousePos.X - X) > 3) or
    (Abs(fMousePos.Y - Y) > 3)) then
    BeginDrag(False, 3);
  NewMouseIndex := ItemAtPos(Y);
  if NewMouseIndex <> fMouseIndex then
  begin
    if fMouseIndex >= 0 then
      ItemMouseLeave(fMouseIndex);
    fMouseIndex := NewMouseIndex;
    if fMouseIndex >= 0 then
      ItemMouseEnter(fMouseIndex);
  end;
  if fMouseIndex >= 0 then
    ItemMouseMove(fMouseIndex, Shift, X, Y + fScrollPos - fItemList[fMouseIndex].Pos);
  inherited;
end;

procedure TACCustomVirtualListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Index: integer;
begin
  inherited;
  SetFocus;
  Index := ItemAtPos(Y);
  if Index >= 0 then
  begin
    if Button = mbLeft then
    begin
      fDragIndex := Index;
      fMouseDown := True;
      fMousePos.X := X;
      fMousePos.Y := Y;
    end;
    if avoAutoSelect in fOptions then
      SelectItem(Index);
    if Index >= 0 then
      ItemMouseDown(Index, Button, Shift, X, Y + fScrollPos - fItemList[Index].Pos);
  end;
end;

procedure TACCustomVirtualListBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Index: integer;
begin
  inherited;
  fMouseDown := False;
  Index := ItemAtPos(Y);
  if Index >= 0 then
    ItemMouseDown(Index, Button, Shift, X, Y + fScrollPos - fItemList[Index].Pos);
end;

function TACCustomVirtualListBox.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): boolean;
begin
  Scroll(-DefaultColorMapItemHeight);
  Result := True;
end;

function TACCustomVirtualListBox.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): boolean;
begin
  Scroll(DefaultColorMapItemHeight);
  Result := True;
end;

procedure TACCustomVirtualListBox.DoStartDrag(var DragObject: TDragObject);
begin
  inherited;
end;

procedure TACCustomVirtualListBox.DoEndDrag(Target: TObject; X, Y: integer);
begin
  fScrollSpeed := 0;
  fDragPos := -1;
  inherited;
  fDragIndex := -1;
end;

procedure TACCustomVirtualListBox.DragDrop(Source: TObject; X, Y: integer);
begin
  if (Source = Self) then
  begin
    if fDragIndex <> -1 then
      ItemMoved(fDragIndex, SeparatorAtPos(Y));
  end
  else
  begin
    inherited;
  end;
end;

procedure TACCustomVirtualListBox.DragOver(Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
var
  NewDropIndex: integer;
begin
  if Y < 12 then
  begin
    if fScrollSpeed = 0 then
    begin
      fScrollSpeed := 20;
      SetTimer(Handle, 0, ScrollInterval, nil);
    end;
  end
  else if Y > ClientHeight - 12 then
  begin
    if fScrollSpeed = 0 then
    begin
      fScrollSpeed := -20;
      SetTimer(Handle, 0, ScrollInterval, nil);
    end;
  end
  else
  begin
    fScrollSpeed := 0;
  end;
  if (Source = Self) then
  begin
    Accept := True;
    if State = dsDragLeave then
    begin
      NewDropIndex := -1;
      fDragPos := -1;
    end
    else
    begin
      NewDropIndex := SeparatorAtPos(Y);
      fDragPos := Y;
    end;
    if fDropIndex <> NewDropIndex then
    begin
      fDropIndex := NewDropIndex;
      Invalidate;
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TACCustomVirtualListBox.InvalidateItems(FirstIndex, LastIndex: integer);
var
  I, Pos: integer;
  ItemHeight: integer;
  LChanged: boolean;
begin
  if fCount > 0 then
  begin
    if FirstIndex < 0 then
      FirstIndex := 0
    else if FirstIndex > fCount - 1 then
      FirstIndex := fCount - 1;
    if LastIndex < 0 then
      LastIndex := 0
    else if LastIndex > fCount - 1 then
      LastIndex := fCount - 1;
    if FirstIndex > 0 then
    begin
      Pos := fItemList[FirstIndex - 1].Pos + fItemList[FirstIndex - 1].Height +
        fSeparatorSize;
    end
    else
    begin
      Pos := 0;
    end;
    LChanged := False;
    for I := FirstIndex to LastIndex do
    begin
      fItemList[I].Pos := Pos;
      ItemHeight := 20;
      ItemMeasure(I, ItemHeight);
      if fItemList[I].Height <> ItemHeight then
      begin
        fItemList[I].Height := ItemHeight;
        LChanged := True;
      end;
      Inc(Pos, ItemHeight + fSeparatorSize);
    end;
    if LChanged then
    begin
      for I := LastIndex + 1 to fCount - 1 do
      begin
        fItemList[I].Pos := Pos;
        Inc(Pos, fItemList[I].Height + fSeparatorSize);
      end;
    end;
  end
  else
  begin
    Pos := 0;
  end;
  ChangeScrollSize(Pos - fSeparatorSize);
  Invalidate;
end;

procedure TACCustomVirtualListBox.InvalidateAllItems;
begin
  InvalidateItems(0, fCount - 1);
end;

procedure TACCustomVirtualListBox.MakeVisible(Index: integer; PartialOK: boolean);
begin
  if ((fItemList[Index].Pos < fScrollPos) and not PartialOK) or
    (fItemList[Index].Pos + fItemList[Index].Height <= fScrollPos) then
  begin
    Scroll(fScrollPos - fItemList[Index].Pos);
  end
  else if ((fItemList[Index].Pos + fItemList[Index].Height > fScrollPos +
    ClientHeight) and not PartialOK) or (fItemList[Index].Pos >=
    fScrollPos + ClientHeight) then
  begin
    Scroll((fScrollPos + ClientHeight div 2) - (fItemList[Index].Pos +
      fItemList[Index].Height));
  end;
end;

procedure TACCustomVirtualListBox.SelectItem(Index: integer; MoveItem: boolean = False);
begin
  if fCount > 0 then
  begin
    if Index < 0 then
      Index := 0
    else if Index > fCount - 1 then
      Index := fCount - 1;
    if MoveItem then
    begin
      if Index <> ItemIndex then
      begin
        if Index >= ItemIndex then
          Inc(Index);
        ItemMoved(ItemIndex, Index);
      end;
    end
    else
    begin
      if ItemIndex <> Index then
      begin
        ItemIndex := Index;
        ItemSelected(ItemIndex);
      end;
      MakeVisible(ItemIndex, False);
    end;
  end;
end;

procedure TACCustomVirtualListBox.ChangeScrollSize(NewSize: integer);
var
  ScrollInfo: TScrollInfo;
begin
  if NewSize <> fScrollSize then
  begin
    fScrollSize := NewSize;
    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_PAGE or SIF_RANGE or SIF_DISABLENOSCROLL;
    ScrollInfo.nPage := ClientHeight;
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := fScrollSize - 1;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
end;

procedure TACCustomVirtualListBox.Scroll(DeltaY: integer);
var
  ScrollInfo: TScrollInfo;
begin
  if DeltaY > fScrollPos then
    DeltaY := fScrollPos
  else
  if (DeltaY < 0) and (fScrollSize < ClientHeight) then
    DeltaY := 0
  else
  if DeltaY < fScrollPos - (fScrollSize - ClientHeight) then
    DeltaY := fScrollPos - (fScrollSize - ClientHeight);
  if DeltaY <> 0 then
  begin
    fScrollPos := fScrollPos - DeltaY;
    ScrollInfo.cbSize := SizeOf(TScrollInfo);
    ScrollInfo.fMask := SIF_POS or SIF_DISABLENOSCROLL;
    ScrollInfo.nPos := fScrollPos;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    ScrollWindowEx(Handle, 0, DeltaY, nil, nil, 0, nil, SW_INVALIDATE);
  end;
end;

function TACCustomVirtualListBox.ItemAtPos(Y: integer): integer;
var
  P: integer;
  I, Min, Max: integer;
begin
  P := Y + fScrollPos;
  Result := -1;
  if (fCount > 0) then
  begin
    if P < fItemList[0].Pos then
    begin
    end
    else if P >= (fItemList[fCount - 1].Pos + fItemList[fCount - 1].Height +
      fSeparatorSize) then
    begin
    end
    else
    begin
      Min := 0;
      Max := fCount - 1;
      while (Result < 0) and (Min <= Max) do
      begin
        I := (Min + Max) div 2;
        if P < fItemList[I].Pos then
        begin
          Max := I - 1;
        end
        else if P >= (fItemList[I].Pos + fItemList[I].Height + fSeparatorSize) then
        begin
          Min := I + 1;
        end
        else
        begin
          Result := I;
        end;
      end;
    end;
  end;
end;

function TACCustomVirtualListBox.SeparatorAtPos(Y: integer): integer;
var
  P: integer;
begin
  P := Y + fScrollPos;
  Result := -1;
  if (fCount > 0) then
  begin
    if P < fItemList[0].Pos then
    begin
      Result := 0;
    end
    else if P >= (fItemList[fCount - 1].Pos + fItemList[fCount - 1].Height +
      fSeparatorSize) then
    begin
      Result := fCount;
    end
    else
    begin
      Result := ItemAtPos(Y);
      if (Result >= 0) and (Y + fScrollPos > fItemList[Result].Pos +
        fItemList[Result].Height div 2) then
        Inc(Result);
    end;
  end;
end;

procedure TACCustomVirtualListBox.AddItems(const ACount: integer = 1);
begin
  SetCount(fCount + ACount);
end;

procedure TACCustomVirtualListBox.InsertItems(const Index: integer;
  const ACount: integer = 1);
var
  I: integer;
begin
  if (Index >= 0) and (Index <= fCount) and (ACount > 0) then
  begin
    if fCount + ACount > fCapacity then
    begin
      fCapacity := (fCount + ACount) * 2;
      SetLength(fItemList, fCapacity);
    end;
    for I := fCount - 1 downto Index do
    begin
      fItemList[I + ACount].Height := fItemList[I - ACount].Height;
    end;
    Inc(fCount, ACount);
    InvalidateItems(Index, Index + ACount - 1);
    if fItemIndex >= 0 then
      Inc(fItemIndex, ACount);
    Invalidate;
  end;
end;

procedure TACCustomVirtualListBox.DeleteItem(const Index: integer);
begin
  DeleteItems(Index, Index);
end;

procedure TACCustomVirtualListBox.DeleteItems(const FromIndex, ToIndex: integer);
var
  I, P, C: integer;
  RemovedSize: integer;
begin
  if (ToIndex >= FromIndex) and (FromIndex >= 0) and (ToIndex < fCount) then
  begin
    C := ToIndex - FromIndex + 1;
    P := fItemList[FromIndex].Pos;
    RemovedSize := 0;
    for I := FromIndex to fCount - C - 1 do
    begin
      Inc(RemovedSize, fItemList[I].Height + fSeparatorSize);
      fItemList[I].Pos := P;
      fItemList[I].Height := fItemList[I + C].Height;
      Inc(P, fItemList[I].Height);
    end;
    Dec(fCount, C);
    if fCount * 2 < fCapacity then
    begin
      fCapacity := fCount;
      SetLength(fItemList, fCapacity);
    end;
    ChangeScrollSize(fScrollSize - RemovedSize);
    if fScrollSize <= ClientHeight then
      fScrollPos := 0
    else if fScrollPos > fScrollSize - ClientHeight then
      fScrollPos := fScrollSize - ClientHeight;
    if fItemIndex > fCount - 1 then
      fItemIndex := -1;
    Invalidate;
  end;
end;

procedure TACCustomVirtualListBox.ClearItems;
begin
  DeleteItems(0, fCount - 1);
end;

function TACCustomVirtualListBox.MovingAllowed: boolean;
begin
  Result := avoAllowMoving in fOptions;
end;

procedure TACCustomVirtualListBox.SetItemIndex(Value: integer);
begin
  if Value < -1 then
    Value := -1
  else if Value > fCount - 1 then
    Value := fCount - 1;
  if fItemIndex <> Value then
  begin
    fItemIndex := Value;
    Invalidate;
  end;
end;

procedure TACCustomVirtualListBox.SetCount(Value: integer);
var
  Old: integer;
begin
  if Value < 0 then
    Value := 0;
  if fCount <> Value then
  begin
    Old := fCount;
    fCount := Value;
    if fCount > fCapacity then
    begin
      fCapacity := fCount * 2;
      SetLength(fItemList, fCapacity);
    end
    else if fCount * 2 < fCapacity then
    begin
      fCapacity := fCount;
      SetLength(fItemList, fCapacity);
    end;
    InvalidateItems(Old, fCount - 1);
    if fScrollSize <= ClientHeight then
      fScrollPos := 0
    else if fScrollPos > fScrollSize - ClientHeight then
      fScrollPos := fScrollSize - ClientHeight;
    if fItemIndex > fCount - 1 then
      fItemIndex := -1;
    Invalidate;
  end;
end;

procedure TACCustomVirtualListBox.SetOptions(Value: TACVirtualListBoxOptions);
var
  Old: TACVirtualListBoxOptions;
begin
  if fOptions <> Value then
  begin
    Old := fOptions;
    fOptions := Value;
    if (fOptions * [avoSeparateLast]) <> (Old * [avoSeparateLast]) then
      Invalidate;
  end;
end;

procedure TACCustomVirtualListBox.SetSeparatorSize(Value: integer);
var
  I: integer;
begin
  if Value < 0 then
    Value := 0
  else if Value > 5 then
    Value := 5;
  if fSeparatorSize <> Value then
  begin
    if fCount > 0 then
    begin
      for I := 1 to fCount - 1 do
      begin
        fItemList[I].pos := fItemList[I].Pos + I * (Value - fSeparatorSize);
      end;
      ChangeScrollSize(fScrollSize + (fCount - 1) * (Value - fSeparatorSize));
    end;
    fSeparatorSize := Value;
    Invalidate;
  end;
end;

procedure TACCustomVirtualListBox.SetSeparatorColor(Value: TColor);
begin
  if fSeparatorColor <> Value then
  begin
    fSeparatorColor := Value;
    if Enabled then
      Invalidate;
  end;
end;

procedure TACCustomVirtualListBox.SetSeparatorDisabled(Value: TColor);
begin
  if fSeparatorDisabled <> Value then
  begin
    fSeparatorDisabled := Value;
    if not Enabled then
      Invalidate;
  end;
end;

procedure TACCustomVirtualListBox.SetOnItemDraw(Value: TACVirtualItemDrawEvent);
begin
  if (TMethod(fOnItemDraw).Code <> TMethod(Value).Code) or
    (TMethod(fOnItemDraw).Data <> TMethod(Value).Data) then
  begin
    fOnItemDraw := Value;
    if fCount > 0 then
      InvalidateAllItems;
  end;
end;

procedure TACCustomVirtualListBox.SetOnItemMeasure(Value: TACVirtualItemMeasureEvent);
begin
  if (TMethod(fOnItemMeasure).Code <> TMethod(Value).Code) or
    (TMethod(fOnItemMeasure).Data <> TMethod(Value).Data) then
  begin
    fOnItemMeasure := Value;
    if fCount > 0 then
      InvalidateAllItems;
  end;
end;

procedure TACCustomVirtualListBox.SetHotTrack(const Value: boolean);
begin
  if fHotTrack = Value then
    Exit;
  fHotTrack := Value;
end;


{ TACColorMapsListBox }

constructor TACColorMapsListBox.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  Options := Options - [avoEraseBackground];
  fSeparatorSize := 0;
  fLook := aclRoundedRect;
  fDitherBitmap := TBitmap.Create;
end;

destructor TACColorMapsListBox.Destroy;
begin
  fDitherBitmap.Free;
  SetMapsLibrary(nil);
  inherited;
end;

function TACColorMapsListBox.GetSelectedMap: TACColorMap;
begin
  Result := nil;
  if (ItemIndex < 0) or (fMapsLibrary = nil) then
    Exit;
  Result := fMapsLibrary.Items[ItemIndex].Map;
end;

procedure TACColorMapsListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = fMapsLibrary) then
    SetMapsLibrary(nil);
  inherited;
end;

procedure TACColorMapsListBox.ItemMoved(FromIndex, ToIndex: integer);
begin
  if (fMapsLibrary <> nil) then
  begin
    if ToIndex > FromIndex then
      fMapsLibrary.Items[FromIndex].Index := ToIndex - 1
    else
      fMapsLibrary.Items[FromIndex].Index := ToIndex;
  end;
end;

procedure TACColorMapsListBox.LibraryChange(Sender: TObject);
begin
  if not fMapsLibrary.Loading then
  begin
    ClearItems;
    AddItems(fMapsLibrary.Items.Count);
  end;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TACColorMapsListBox.SetMapsLibrary(const Value: TACColorMapsLibrary);
begin
  if fMapsLibrary <> Value then
  begin
    ClearItems;
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
      AddItems(fMapsLibrary.Items.Count);
    end;
  end;
end;


procedure TACColorMapsListBox.DblClick;
begin
  inherited;
  if Assigned(fOnApply) then
    fOnApply(Self);
end;

procedure TACColorMapsListBox.PopupContextMenu(const X, Y: integer);
begin

end;


procedure TACColorMapsListBox.SetLook(Value: TACColorControlLook);
begin
  if fLook <> Value then
  begin
    fLook := Value;
    Invalidate;
  end;
end;


procedure TACColorMapsListBox.SetCollection(Value: TACColorMapCollection);
begin

end;

procedure TACColorMapsListBox.ItemMeasure(Index: integer; var ItemHeight: integer);
begin
  ItemHeight := DefaultColorMapItemHeight;
  inherited;
end;

procedure TACColorMapsListBox.ItemDraw(Index: integer; ACanvas: TCanvas;
  ItemRect: TRect; State: TOwnerDrawState);
var
  Map: TACColorMap;
  TempBitmap: TBitmap;
  TextRect: TRect;
  R: TRect;
  {$ifndef MSWINDOWS}
  s: string;
  w: integer;
  {$endif}
begin
  if Assigned(fMapsLibrary) and (Index < fMapsLibrary.Items.Count) then
  begin
    Map := fMapsLibrary.Items[Index].Map;
    TempBitmap := TBitmap.Create;
    try
      TempBitmap.PixelFormat := pf32bit;
      TempBitmap.SetSize(ItemRect.Right - ItemRect.Left - 2, ItemRect.Bottom -
        ItemRect.Top);
      R := Bounds(0, 0, TempBitmap.Width, TempBitmap.Height);
      TempBitmap.Canvas.Brush.Color := Canvas.Brush.Color;
      TempBitmap.Canvas.FillRect(R);
      R.Bottom := 24;
      InflateRect(R, -4, -4);
      Map.DrawToCanvas(TempBitmap.Canvas, True, R);
      if not Enabled then
      begin
        TempBitmap.Canvas.Draw(0, 0, fDitherBitmap);
      end;

      InflateRect(R, 1, 1);
      TempBitmap.Canvas.Pen.Color := Canvas.Pen.Color;
      TempBitmap.Canvas.MoveTo(R.Left + 1, R.Top);
      TempBitmap.Canvas.LineTo(R.Right - 1, R.Top);
      TempBitmap.Canvas.LineTo(R.Right, R.Top + 1);
      TempBitmap.Canvas.LineTo(R.Right, R.Bottom - 1);
      TempBitmap.Canvas.LineTo(R.Right - 1, R.Bottom);
      TempBitmap.Canvas.LineTo(R.Left + 1, R.Bottom);
      TempBitmap.Canvas.LineTo(R.Left, R.Bottom - 1);
      TempBitmap.Canvas.LineTo(R.Left, R.Top);
      Canvas.Draw(ItemRect.Left + 1, ItemRect.Top + 1, TempBitmap);
    finally
      TempBitmap.Free;
    end;
    TextRect := Rect(R.Left, ItemRect.Top + 24, R.Right, ItemRect.Bottom);
{$ifndef MSWINDOWS}
// DT_END_ELLIPSIS not supported in Linux and Mac OS
    s := Map.MapName;
    if Canvas.TextWidth(s) > TextRect.Width then
    begin
      w := Canvas.TextWidth('...');
      repeat
        Delete(s, Length(s), 1);
      until (Length(s) = 0) or (Canvas.TextWidth(s) <= TextRect.Width - w);
      s += '...';
    end;
    DrawText(Canvas.Handle, PChar(s), Length(s),
      TextRect, DT_TOP or DT_CENTER or DT_NOPREFIX or DT_SINGLELINE);
{$else}
    DrawText(Canvas.Handle, PChar(Map.MapName), Length(Map.MapName),
      TextRect, DT_TOP or DT_CENTER or DT_END_ELLIPSIS or DT_NOPREFIX or DT_SINGLELINE);
{$endif}

  end;
  inherited;
end;


function TACColorMapsListBox.IndexOfMapName(const aName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to fMapsLibrary.Items.Count - 1 do
    if AnsiSameText(fMapsLibrary.Items[i].Map.MapName, aName) then
    begin
      Result := i;
      Break;
    end;
end;

procedure TACColorMapsListBox.SelectDefaultMap;
var
  i: integer;
begin
  i := IndexOfMapName('default');
  if i >= 0 then
  begin
    ItemIndex := i;
    MakeVisible(i, True);
    ItemSelected(i);
  end;
end;

procedure TACColorMapsListBox.SelectMapName(const aMapName: string);
var
  i: integer;
begin
  i := IndexOfMapName(aMapName);
  if i >= 0 then
  begin
    ItemIndex := i;
    MakeVisible(i, True);
  end;
end;

procedure TACColorMapsListBox.MakeDitherBitmap;
var
  x, y: integer;
  c0, c1: TColor;
begin
  c0 := $00000000;
  c1 := clBtnFace;
  fDitherBitmap.Transparent := True;
  fDitherBitmap.TransparentColor := c0;
  for x := 0 to fDitherBitmap.Width - 1 do
    for y := 0 to fDitherBitmap.Height - 1 do
    begin
      if (y and 1 = 1) then
      begin
        if (x and 1 = 1) then
          fDitherBitmap.Canvas.Pixels[x, y] := c1
        else
          fDitherBitmap.Canvas.Pixels[x, y] := c0;
      end
      else
      begin
        if (x and 1 = 1) then
          fDitherBitmap.Canvas.Pixels[x, y] := c0
        else
          fDitherBitmap.Canvas.Pixels[x, y] := c1;
      end;
    end;
end;


procedure TACColorMapsListBox.Resize;
begin
  inherited;
  fDitherBitmap.SetSize(Width, 40);
  MakeDitherBitmap;
end;

procedure TACColorMapsListBox.AddItems(const ACount: integer);
begin
  inherited AddItems(ACount);
end;

procedure TACColorMapsListBox.ClearItems;
begin
  inherited;
end;

procedure TACColorMapsListBox.DeleteItem(const Index: integer);
begin
  inherited;
end;

procedure TACColorMapsListBox.DeleteItems(const FromIndex, ToIndex: integer);
begin
  inherited;
end;

procedure TACColorMapsListBox.InsertItems(const Index: integer; const ACount: integer);
begin
  inherited;
end;

end.
