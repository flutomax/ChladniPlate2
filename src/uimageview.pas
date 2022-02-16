{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uImageView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, LCLType, Controls, Graphics, Forms, StdCtrls,
  ExtCtrls;

const
  ImageViewZooms: array[0..13] of integer = (
    6, 12, 25, 50, 75, 100, 200, 400, 500, 600, 800, 1000, 1200, 1600);

type

  TScaleMode = (smNormal, smFit, smFitWidth, smFitHeight);

  { TImageView }

  TImageView = class(TScrollBox)
  private
    fImage: TImage;
    fZoom: double;
    fScaleMode: TScaleMode;
    fImageMakePos: boolean;
    FImageFitOnlyBig: boolean;
    fInitScrollbarSize: integer;
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;
    FOldSelfW: integer;
    FOldSelfH: integer;
    FScrollSmallStep: integer;
    FScrollGapSize: integer;
    fOnZoomChange: TNotifyEvent;
    function GetBitmap: TBitmap;
    function GetImageHeight: integer;
    function GetImageWidth: integer;
    function GetPageSize(AClientSize: integer): integer;
    procedure SetZoom(AValue: double);
    procedure DoZoomChange;
    procedure UpdateImage(AReset: boolean = False);
    procedure WMGetDlgCode(var Message: TLMessage); message LM_GETDLGCODE;
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Reset;
    procedure ZoomFit;
    procedure LoadBitmap(ABitmap: TBitmap);
    property ImageWidth: integer read GetImageWidth;
    property ImageHeight: integer read GetImageHeight;
    property Zoom: double read fZoom write SetZoom;
  published
    property Bitmap: TBitmap read GetBitmap;
    property OnZoomChange: TNotifyEvent read fOnZoomChange write fOnZoomChange;
  end;

implementation


uses Math;

{ TImageView }


constructor TImageView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fScaleMode := smNormal;
  fZoom := 100;
  FImageFitOnlyBig := True;
  FScrollSmallStep := 50;
  FScrollGapSize := 20;
  AutoScroll := False;
  DoubleBuffered := True;
  HorzScrollBar.Tracking := True;
  HorzScrollBar.Smooth := True;
  VertScrollBar.Tracking := True;
  VertScrollBar.Smooth := True;

  fImage := TImage.Create(self);
  fImage.Parent := self;
  fImage.Align := alNone;
  fImage.SetBounds(0, 0, 150, 150);

  with TScrollBar.Create(Self) do
    try
      Kind := sbVertical;
      fInitScrollbarSize := Width;
    finally
      Free;
    end;

end;

procedure TImageView.Reset;
begin
  fZoom := 100;
  fImage.Visible := True;
  if Assigned(fImage.Picture) and Assigned(fImage.Picture.Graphic) then
    UpdateImage(True);
  DoZoomChange;
end;

procedure TImageView.LoadBitmap(ABitmap: TBitmap);
begin
  fImage.Picture := nil;
  fImage.Picture.Assign(ABitmap);
  if Assigned(fImage.Picture) and Assigned(fImage.Picture.Graphic) then
    UpdateImage(True);
  DoZoomChange;
end;

function TImageView.GetBitmap: TBitmap;
begin
  Result := fImage.Picture.Bitmap;
end;

function TImageView.GetImageHeight: integer;
begin
  if Assigned(fImage.Picture) then
    Result := fImage.Picture.Height
  else
    Result := 0;
end;

function TImageView.GetImageWidth: integer;
begin
  if Assigned(fImage.Picture) then
    Result := fImage.Picture.Width
  else
    Result := 0;
end;

function TImageView.GetPageSize(AClientSize: integer): integer;
begin
  Result := Max(AClientSize - FScrollGapSize, AClientSize div 3 * 2);
end;

procedure TImageView.SetZoom(AValue: double);
begin
  if AValue < ImageViewZooms[Low(ImageViewZooms)] then
    AValue := ImageViewZooms[Low(ImageViewZooms)];
  if AValue > ImageViewZooms[High(ImageViewZooms)] then
    AValue := ImageViewZooms[High(ImageViewZooms)];
  if fZoom = AValue then
    Exit;
  fZoom := AValue;
  fScaleMode := smNormal;
  HorzScrollBar.Position := 0;
  VertScrollBar.Position := 0;
  UpdateImage;
  DoZoomChange;
end;

procedure TImageView.ZoomFit;
begin
  if fScaleMode = smFit then
    exit;
  fScaleMode := smFit;
  UpdateImage;
  DoZoomChange;
end;

procedure TImageView.DoZoomChange;
begin
  if Assigned(fOnZoomChange) then
    fOnZoomChange(self);
end;

procedure TImageView.UpdateImage(AReset: boolean);
var
  MakePos: boolean;
  ImgWidth, ImgHeight, CliWidth, CliHeight, NewWidth, NewHeight,
  NewLeft, NewTop, ScrollMaxX, ScrollMaxY: integer;
  Ratio, ImgRatio, CenterRatioX, CenterRatioY: double;
  ScrollbarSize: integer;
begin
  MakePos := fImageMakePos and not AReset;

  ImgWidth := ImageWidth;
  ImgHeight := ImageHeight;
  if (ImgWidth = 0) or (ImgHeight = 0) then
    exit;

  if fScaleMode = smFit then
    ScrollbarSize := 0
  else
    ScrollbarSize := fInitScrollbarSize;

  VertScrollBar.Visible := fScaleMode <> smFit;
  HorzScrollBar.Visible := fScaleMode <> smFit;
  CliWidth := Width - ScrollbarSize;
  CliHeight := Height - ScrollbarSize;

  CenterRatioX := 0;
  CenterRatioY := 0;

  if fImage.Width > 0 then
  begin
    if fImage.Left >= 0 then
      CenterRatioX := (CliWidth div 2 - fImage.Left) / fImage.Width
    else
      CenterRatioX := (CliWidth div 2 + HorzScrollBar.Position) / fImage.Width;
  end;

  if fImage.Height > 0 then
  begin
    if fImage.Top >= 0 then
      CenterRatioY := (CliHeight div 2 - fImage.Top) / fImage.Height
    else
      CenterRatioY := (CliHeight div 2 + VertScrollBar.Position) / fImage.Height;
  end;

  if not MakePos then
  begin
    HorzScrollBar.Position := 0;
    VertScrollBar.Position := 0;
  end;

  AutoScroll := fScaleMode <> smFit;
  fImage.AutoSize := (fScaleMode <> smFit) and (fZoom = 100);
  fImage.Stretch := not fImage.AutoSize;

  if fScaleMode in [smFit..smFitHeight] then
  begin
    NewWidth := ImgWidth;
    NewHeight := ImgHeight;

    if FImageFitOnlyBig and (ImgWidth <= CliWidth) and (ImgHeight <= CliHeight) then
    begin
      fZoom := 100;
    end
    else
    begin
      if (CliWidth > 0) and (CliHeight > 0) then
      begin
        Ratio := CliWidth / CliHeight;
        ImgRatio := ImgWidth / ImgHeight;
        if ((Ratio >= ImgRatio) and (fScaleMode <> smFitWidth)) or
          (fScaleMode = smFitHeight) then
        begin
          //fit height
          if not (FImageFitOnlyBig and (CliHeight >= ImgHeight)) then
          begin
            NewHeight := CliHeight;
            NewWidth := Trunc(NewHeight * ImgRatio);
            fZoom := CliHeight * 100 / ImgHeight;
          end;
        end
        else
        begin
          //fit width
          if not (FImageFitOnlyBig and (CliWidth >= ImgWidth)) then
          begin
            NewWidth := CliWidth;
            NewHeight := Trunc(NewWidth / ImgRatio);
            fZoom := CliWidth * 100 / ImgWidth;
          end;
        end;
      end;
    end;
  end //if FImageFit
  else
  begin
    NewWidth := Round(ImgWidth * fZoom / 100);
    NewHeight := Round(ImgHeight * fZoom / 100);
  end;

  //Update image position
  NewLeft := 0;
  NewTop := 0;

  if CliWidth > NewWidth then
    NewLeft := (CliWidth - NewWidth) div 2;
  if CliHeight > NewHeight then
    NewTop := (CliHeight - NewHeight) div 2;

  if (FOldLeft <> NewLeft - HorzScrollBar.Position) or
    (FOldTop <> NewTop - VertScrollBar.Position) or (FOldWidth <> NewWidth) or
    (FOldHeight <> NewHeight) then
  begin
    FOldLeft := NewLeft - HorzScrollBar.Position;
    FOldTop := NewTop - VertScrollBar.Position;
    FOldWidth := NewWidth;
    FOldHeight := NewHeight;
    fImage.SetBounds(FOldLeft, FOldTop, FOldWidth, FOldHeight);
  end;

  if MakePos then
  begin
    if NewLeft = 0 then
    begin
      ScrollMaxX := Max(NewWidth - CliWidth, 0);
      HorzScrollBar.Position :=
        Min(ScrollMaxX, Trunc(CenterRatioX * NewWidth) - CliWidth div 2);
    end
    else
      HorzScrollBar.Position := 0;

    if NewTop = 0 then
    begin
      ScrollMaxY := Max(NewHeight - CliHeight, 0);
      VertScrollBar.Position :=
        Min(ScrollMaxY, Trunc(CenterRatioY * NewHeight) - CliHeight div 2);
    end
    else
      VertScrollBar.Position := 0;
  end;

  if HorzScrollbar.Visible then
    HorzScrollbar.Range := NewWidth;
  if VertScrollBar.Visible then
    VertScrollbar.Range := NewHeight;

end;


procedure TImageView.WMGetDlgCode(var Message: TLMessage);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TImageView.KeyDown(var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT:
    begin
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position := Position - FScrollSmallStep;
        Key := 0;
      end
      else
      if Shift = [ssModifier] then
      begin
        with HorzScrollBar do
          Position := 0;
        Key := 0;
      end;
    end;

    VK_RIGHT:
    begin
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position := Position + FScrollSmallStep;
        Key := 0;
      end
      else
      if Shift = [ssModifier] then
      begin
        with HorzScrollBar do
          Position := Range;
        Key := 0;
      end;
    end;

    VK_HOME:
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position := Position - GetPageSize(ClientWidth);
        Key := 0;
      end;

    VK_END:
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position := Position + GetPageSize(ClientWidth);
        Key := 0;
      end;

    VK_UP:
    begin
      if Shift = [] then
      begin
        with VertScrollBar do
          Position := Position - FScrollSmallStep;
        Key := 0;
      end
      else
      if Shift = [ssModifier] then
      begin
        with VertScrollBar do
          Position := 0;
        Key := 0;
      end;
    end;

    VK_DOWN:
    begin
      if Shift = [] then
      begin
        with VertScrollBar do
          Position := Position + FScrollSmallStep;
        Key := 0;
      end
      else
      if Shift = [ssModifier] then
      begin
        with VertScrollBar do
          Position := Range;
        Key := 0;
      end;
    end;

    VK_PRIOR:
      if Shift = [] then
      begin
        with VertScrollBar do
          Position := Position - GetPageSize(ClientHeight);
        Key := 0;
      end;

    VK_NEXT:
      if Shift = [] then
      begin
        with VertScrollBar do
          Position := Position + GetPageSize(ClientHeight);
        Key := 0;
      end;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TImageView.Resize;
begin
  inherited Resize;
  if Assigned(FImage) and ((FOldSelfW <> Width) or (FOldSelfH <> Height)) then
  begin
    FOldSelfW := Width;
    FOldSelfH := Height;
    UpdateImage;
  end;
end;

end.

