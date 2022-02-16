{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

program ChladniPlate2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uFrmMain, uVirtualListBox, uColorMaps, uFunctions, uColorMapsLib,
  uImageView, uCladni, uMRUList, uRandom, uFrmProperties, uFrmAbout,
  uAnimateBackgound, uSysInfo
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.

