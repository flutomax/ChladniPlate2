{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uFrmProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  uCladni;

type

  { TFrmProperties }

  TFrmProperties = class(TForm)
    BtnOK: TButton;
    Button2: TButton;
    CkSaveLevelMapWithFile: TCheckBox;
    EdHeight: TSpinEdit;
    EdCapacity: TSpinEdit;
    GbImageSize: TGroupBox;
    LblCapacity: TLabel;
    LblWidth: TLabel;
    EdWidth: TSpinEdit;
    LblHeight: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    fCladni: TCladni;
    procedure SetCladni(AValue: TCladni);
  public
    property Cladni: TCladni read fCladni write SetCladni;
  end;

var
  FrmProperties: TFrmProperties;

implementation

{$R *.lfm}

{ TFrmProperties }

procedure TFrmProperties.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
    fCladni.Capacity := EdCapacity.Value;
    fCladni.SaveLevelMapWithFile := CkSaveLevelMapWithFile.Checked;
    fCladni.SetSize(EdWidth.Value, EdHeight.Value);
  end;
end;

procedure TFrmProperties.FormShow(Sender: TObject);
begin
  LblWidth.Height := EdWidth.Height;
  LblHeight.Height := EdHeight.Height;
  LblCapacity.Height := EdCapacity.Height;
end;

procedure TFrmProperties.SetCladni(AValue: TCladni);
begin
  fCladni := AValue;
  EdCapacity.Value := fCladni.Capacity;
  EdWidth.Value := fCladni.Width;
  EdHeight.Value := fCladni.Height;
  CkSaveLevelMapWithFile.Checked := fCladni.SaveLevelMapWithFile;
end;

end.


