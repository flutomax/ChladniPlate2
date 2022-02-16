{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uFrmAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uAnimateBackgound;

type

  { TFrmAbout }

  TFrmAbout = class(TForm)
    btClose: TButton;
    DonateLink: TImage;
    Image: TImage;
    Label1: TLabel;
    lblBuildC: TLabel;
    lblBuildV: TLabel;
    lblCopy: TLabel;
    lblFreePascalVerC: TLabel;
    lblFreePascalVerV: TLabel;
    lblHomePage: TLabel;
    lblHomePageAddress: TLabel;
    lblLazarusVerC: TLabel;
    lblLazarusVerV: TLabel;
    lblOperatingSystemC: TLabel;
    lblOperatingSystemV: TLabel;
    lblPlatformC: TLabel;
    lblPlatformV: TLabel;
    lblTitle: TLabel;
    lblVersionC: TLabel;
    lblVersionV: TLabel;
    procedure DonateLinkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblHomePageAddressClick(Sender: TObject);
    procedure lblHomePageAddressMouseEnter(Sender: TObject);
    procedure lblHomePageAddressMouseLeave(Sender: TObject);
  private
    fAnimateBackgound: TAnimateBackgound;
    procedure AnimateBackgoundDraw(Sender: TObject);
  public

  end;

var
  FrmAbout: TFrmAbout;

procedure ShowAbout;

implementation

{$R *.lfm}

uses
  uSysInfo, uFrmMain;

procedure ShowAbout;
begin
  with TFrmAbout.Create(Application) do
    try
      ShowModal;
    finally
      Release;
    end;
end;

{ TFrmAbout }

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  // Windows: without DoubleBuffered - Bilnking if not theme service running
  fAnimateBackgound := TAnimateBackgound.Create(self);
  fAnimateBackgound.OnDraw := @AnimateBackgoundDraw;
  lblVersionV.Caption := GetProgramVersion;
  lblBuildV.Caption := GetBuildDate;
  lblLazarusVerV.Caption := GetLazarusVersion;
  lblFreePascalVerV.Caption := GetFPCVersion;
  lblPlatformV.Caption := GetPlatform;
  lblOperatingSystemV.Caption := GetOSVersion;
end;

procedure TFrmAbout.FormPaint(Sender: TObject);
begin
  Canvas.Draw(0, 0, fAnimateBackgound.Buffer);
end;

procedure TFrmAbout.FormShow(Sender: TObject);
begin
  fAnimateBackgound.Start;
end;

procedure TFrmAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fAnimateBackgound.Stop;
end;

procedure TFrmAbout.DonateLinkClick(Sender: TObject);
begin
  FrmMain.CmdHelpDonation.Execute;
end;

procedure TFrmAbout.lblHomePageAddressClick(Sender: TObject);
begin
  FrmMain.CmdHelpHomePage.Execute;
end;

procedure TFrmAbout.lblHomePageAddressMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold, fsUnderLine];
end;

procedure TFrmAbout.lblHomePageAddressMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TFrmAbout.AnimateBackgoundDraw(Sender: TObject);
begin
  Invalidate;
end;




end.
