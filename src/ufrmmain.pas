{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uFrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ComCtrls, ExtCtrls, Grids, StdActns, MaskEdit, StdCtrls, IniPropStorage,
  uVirtualListBox, uColorMapsLib, uImageView, uCladni, uMRUList, Types;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    BvlMainTop: TBevel;
    CmdHelpDonation: TAction;
    CmdHelpHomePage: TAction;
    CmdHelpAbout: TAction;
    CmdViewZoomFit: TAction;
    CmdViewProperties: TAction;
    CmdViewShowHitns: TAction;
    AppProp: TApplicationProperties;
    CmdViewNormalize: TAction;
    CmdCommandsRandom: TAction;
    CmdCommandsRender: TAction;
    CmdFileSave: TAction;
    CmdFileNew: TAction;
    AlMain: TActionList;
    CmdFileExit: TFileExit;
    CmdFileOpen: TFileOpen;
    CmdFileSaveAs: TFileSaveAs;
    CmdFileExport: TFileSaveAs;
    ILMain: TImageList;
    ILMainD: TImageList;
    ILStatus: TImageList;
    IniStorage: TIniPropStorage;
    LblProgress: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    MiZooms: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    msRfBot: TMenuItem;
    miFile: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    msRfTop: TMenuItem;
    MenuItem7: TMenuItem;
    PnlConteiner: TPanel;
    PnlImgView: TPanel;
    PnlRight: TPanel;
    PnlWaves: TPanel;
    PnlLeft: TPanel;
    PnlColorMaps: TPanel;
    PmImage: TPopupMenu;
    ProgressBar: TProgressBar;
    StatusBar: TStatusBar;
    GrWaves: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure AppPropDropFiles(Sender: TObject; const FileNames: array of string);
    procedure AppPropHint(Sender: TObject);
    procedure CmdCommandsRandomExecute(Sender: TObject);
    procedure CmdCommandsRenderUpdate(Sender: TObject);
    procedure CmdFileExportAccept(Sender: TObject);
    procedure CmdFileExportBeforeExecute(Sender: TObject);
    procedure CmdHelpAboutExecute(Sender: TObject);
    procedure CmdHelpDonationExecute(Sender: TObject);
    procedure CmdHelpHomePageExecute(Sender: TObject);
    procedure CmdViewPropertiesExecute(Sender: TObject);
    procedure CmdFileNewExecute(Sender: TObject);
    procedure CmdFileNewUpdate(Sender: TObject);
    procedure CmdFileOpenAccept(Sender: TObject);
    procedure CmdFileOpenBeforeExecute(Sender: TObject);
    procedure CmdFileSaveAsAccept(Sender: TObject);
    procedure CmdFileSaveAsBeforeExecute(Sender: TObject);
    procedure CmdFileSaveExecute(Sender: TObject);
    procedure CmdFileSaveUpdate(Sender: TObject);
    procedure CmdCommandsRenderExecute(Sender: TObject);
    procedure CmdViewNormalizeExecute(Sender: TObject);
    procedure CmdViewShowHitnsExecute(Sender: TObject);
    procedure CmdViewShowHitnsUpdate(Sender: TObject);
    procedure CmdViewZoomFitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GrWavesCheckboxToggled(Sender: TObject; aCol, aRow: integer;
      aState: TCheckboxState);
    procedure GrWavesDblClick(Sender: TObject);
    procedure GrWavesDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure GrWavesEditingDone(Sender: TObject);
    procedure GrWavesGetEditText(Sender: TObject; ACol, ARow: integer;
      var Value: string);
    procedure GrWavesSetEditText(Sender: TObject; ACol, ARow: integer;
      const Value: string);
    procedure StatusBarDrawPanel(AStatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    fLbMaps: TACColorMapsListBox;
    fMapsLib: TACColorMapsLibrary;
    fImgView: TImageView;
    fIniPath: string;
    fCladni: TCladni;
    fMRUList: TMRUList;
    fLoading: boolean;
    function CanCloseDoc: boolean;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure UpdateTitle;
    procedure MakeZoomMenu;
    procedure SetCapacity(AValue: integer);
    procedure DoZoomClick(Sender: TObject);
    procedure DoZoomChange(Sender: TObject);
    procedure DoCladniChanged(Sender: TObject);
    procedure DoCladniLoad(Sender: TObject);
    procedure DoCladniProgress(Sender: TObject; State: TRenderState; Value: integer);
    procedure DoRenderingDone(Sender: TObject);
    procedure RecentFileClick(Sender: TObject; const aFileName: string);
    procedure MapsListBoxItemSelected(Sender: TObject; Index: integer);
    procedure OpenDoc(const aFileName: string);
    procedure NewDoc;
    procedure SaveDoc;
  public
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses
  LCLIntf, StrUtils, Math, IniFiles, uFunctions, uFrmProperties, uFrmAbout;

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  fLoading := False;
  fIniPath := ChangeFileExt(Application.ExeName, '.ini');
  IniStorage.IniFileName := fIniPath;

  fMapsLib := TACColorMapsLibrary.Create(Self);
  fMapsLib.Name := 'MapsLib';
  fMapsLib.LoadItems;

  fCladni := TCladni.Create(self);
  fCladni.MapsLibrary := fMapsLib;
  fCladni.OnChange := @DoCladniChanged;
  fCladni.OnLoad := @DoCladniLoad;
  fCladni.OnProgress := @DoCladniProgress;
  fCladni.OnRenderingDone := @DoRenderingDone;

  fLbMaps := TACColorMapsListBox.Create(self);
  fLbMaps.Name := 'MapsListBox';
  fLbMaps.Parent := PnlRight;
  fLbMaps.Align := alClient;
  fLbMaps.MapsLibrary := fMapsLib;
  fLbMaps.OnItemSelected := @MapsListBoxItemSelected;

  fImgView := TImageView.Create(self);
  fImgView.Parent := PnlImgView;
  fImgView.Align := alClient;
  fImgView.PopupMenu := PmImage;
  fImgView.OnZoomChange := @DoZoomChange;

  fMRUList := TMRUList.Create(self);
  fMRUList.MIRecent := miFile;
  fMRUList.TopSepar := msRfTop;
  fMRUList.BotSepar := msRfBot;
  fMRUList.OnRecent := @RecentFileClick;

  DrawDisabledImagelist(ILMain, ILMainD);
  MakeZoomMenu;
  LoadSettings;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  fMRUList.UpdateRecentFiles;
  if ParamCount > 0 then
    OpenDoc(ParamStr(1))
  else
    NewDoc;
end;

procedure TFrmMain.GrWavesDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if not GrWaves.Enabled then
    GrWaves.Canvas.Font.Color := clGrayText;
  if (aCol = 0) and (aRow > 0) then
    GrWaves.Canvas.TextRect(aRect, 4, 0, IntToStr(aRow));
end;

procedure TFrmMain.GrWavesEditingDone(Sender: TObject);
var
  s: string;
begin
  if fLoading or (GrWaves.Col < 2) or (GrWaves.Row < 1) then
    exit;
  s := GrWaves.Cells[GrWaves.Col, GrWaves.Row];
  GrWavesGetEditText(GrWaves, GrWaves.Col, GrWaves.Row, s);
  GrWaves.Cells[GrWaves.Col, GrWaves.Row] := s;
end;

procedure TFrmMain.GrWavesGetEditText(Sender: TObject; ACol, ARow: integer;
  var Value: string);
var
  v: double;
  e: integer;
begin
  if fLoading then
    exit;
  Val(Value, v, e);
  if e <> 0 then
    v := 0;
  case ACol of
    2: v := EnsureRange(v, MIN_AMPLITUDE, MAX_AMPLITUDE);
    3: v := EnsureRange(v, MIN_FREQ_RATIO, MAX_FREQ_RATIO);
    4: v := EnsureRange(v, MIN_ANGLE, MAX_ANGLE);
  end;
  Value := FormatFloat('0.####', v);
end;

procedure TFrmMain.GrWavesSetEditText(Sender: TObject; ACol, ARow: integer;
  const Value: string);
begin
  if fLoading then
    exit;
  fCladni.SetParam(ACol, ARow, Value);
end;

procedure TFrmMain.StatusBarDrawPanel(AStatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  Style: TTextStyle;
begin
  ILStatus.Draw(AStatusBar.Canvas, Rect.Left + 2, Rect.Top + 2, Panel.Index);
  FillChar(Style, SizeOf(Style), 0);
  with Style do
  begin
    Clipping := True;
    SingleLine := True;
    ExpandTabs := True;
  end;
  AStatusBar.Canvas.TextRect(Rect, Rect.Left + 17, Rect.Top + 1, Panel.Text, Style);
end;

procedure TFrmMain.GrWavesCheckboxToggled(Sender: TObject; aCol, aRow: integer;
  aState: TCheckboxState);
begin
  if fLoading then
    exit;
  fCladni.SetParam(ACol, ARow, FloatToStr(Ord(aState)));
end;

procedure TFrmMain.GrWavesDblClick(Sender: TObject);
var
  p: TPoint;
  i: integer;
  s: string;
begin
  p := GrWaves.ScreenToClient(Mouse.CursorPos);
  if GrWaves.CellRect(1, 0).Contains(p) then
  begin
    s := IfThen(GrWaves.Columns[0].Tag = 0, '1', '0');
    GrWaves.Columns[0].Tag := IfThen(GrWaves.Columns[0].Tag = 0, 1, 0);
    for i := 1 to GrWaves.RowCount - 1 do
    begin
      GrWaves.Cells[1, i] := s;
      fCladni.SetParam(1, i, s);
    end;
  end;
end;

function TFrmMain.CanCloseDoc: boolean;
begin
  Result := True;
  if fCladni.Modified then
  begin
    case MessageDlg(AppTitle, 'Save changes to "' + ExtractFilename(fCladni.FileName) +
        '" before closing?', mtConfirmation, mbYesNoCancel, '') of
      mrYes: SaveDoc;
      mrCancel: Result := False;
    end;
  end;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanCloseDoc();
  SaveSettings;
  fCladni.StopCalc;
end;

procedure TFrmMain.LoadSettings;
var
  ini: TMemIniFile;
  w, h: integer;
begin
  ini := TMemIniFile.Create(fIniPath);
  try
    fMRUList.LoadFromIni(ini, 'Recent Files');
    w := ini.ReadInteger('Image', 'Width', 500);
    h := ini.ReadInteger('Image', 'Height', 500);
    fCladni.SetSize(w, h);
    SetCapacity(ini.ReadInteger('Waves', 'Capacity', 100));
    Application.ShowHint := ini.ReadBool('Settings', 'ShowHint', True);
  finally
    ini.Free;
  end;
end;

procedure TFrmMain.SaveSettings;
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(fIniPath);
  try
    ini.WriteInteger('Image', 'Width', fCladni.Width);
    ini.WriteInteger('Image', 'Height', fCladni.Height);
    ini.WriteInteger('Waves', 'Capacity', fCladni.Capacity);
    ini.WriteBool('Settings', 'ShowHint', Application.ShowHint);
    fMRUList.SaveToIni(ini, 'Recent Files');
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TFrmMain.UpdateTitle;
begin
  Caption := AppTitle + ' - ' + fCladni.Title;
end;

procedure TFrmMain.DoZoomClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  fImgView.Zoom := TComponent(Sender).Tag;
end;

procedure TFrmMain.DoZoomChange(Sender: TObject);
begin
  StatusBar.Panels[1].Text := Format('%d%%', [Round(fImgView.Zoom)]);
end;

procedure TFrmMain.MakeZoomMenu;
var
  i: integer;
  m, n: TMenuItem;
begin
  for i := Low(ImageViewZooms) to High(ImageViewZooms) do
  begin
    m := TMenuItem.Create(self);
    m.Name := Format('MiZoom%d', [ImageViewZooms[i]]);
    m.Caption := Format('%d%%', [ImageViewZooms[i]]);
    m.Tag := ImageViewZooms[i];
    m.GroupIndex := 1;
    m.RadioItem := True;
    m.Checked := ImageViewZooms[i] = 100;
    m.OnClick := @DoZoomClick;
    MiZooms.Insert(i, m);
    n := TMenuItem.Create(self);
    n.Assign(m);
    n.Name := Format('PmiZoom%d', [ImageViewZooms[i]]);
    PmImage.Items.Insert(i, n);
  end;
end;

procedure TFrmMain.CmdFileNewExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  if not CanCloseDoc then
    exit;
  NewDoc;
end;

procedure TFrmMain.CmdFileNewUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not fCladni.IsRendering;
end;

procedure TFrmMain.CmdCommandsRandomExecute(Sender: TObject);
begin
  fLoading := True;
  try
    fCladni.RandomParametrs;
  finally
    fLoading := False;
  end;
  CmdCommandsRender.Execute;
end;

procedure TFrmMain.AppPropDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if Length(FileNames) = 0 then
    exit;
  if SameText(ExtractFileExt(FileNames[0]), '.chl') then
    OpenDoc(FileNames[0]);
end;

procedure TFrmMain.AppPropHint(Sender: TObject);
begin
  StatusBar.Panels[2].Text := StringReplace(Application.Hint, LineEnding,
    ' ', [rfReplaceAll]);
end;

procedure TFrmMain.CmdCommandsRenderUpdate(Sender: TObject);
begin
  CmdCommandsRender.ImageIndex := IfThen(fCladni.IsRendering, 7, 6);
  CmdCommandsRender.Caption := IfThen(fCladni.IsRendering, 'Stop', 'Render');
  CmdCommandsRender.Hint := IfThen(fCladni.IsRendering, 'Stop Render',
    'Render|Render Chladni Plate');
end;

procedure TFrmMain.CmdFileExportAccept(Sender: TObject);
var
  ext: string;
  jpg: TJPEGImage;
  tif: TTiffImage;
  png: TPortableNetworkGraphic;
begin
  ext := ExtractFileExt(CmdFileExport.Dialog.FileName);
  if SameText(ext, '.png') then
  begin
    Screen.Cursor := crHourGlass;
    try
      png := TPortableNetworkGraphic.Create;
      try
        png.Assign(fCladni.Bitmap);
        png.SaveToFile(CmdFileExport.Dialog.FileName);
      finally
        png.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    exit;
  end;
  if SameText(ext, '.jpg') or SameText(ext, '.jpeg') then
  begin
    Screen.Cursor := crHourGlass;
    try
      jpg := TJPEGImage.Create;
      try
        jpg.Assign(fCladni.Bitmap);
        jpg.SaveToFile(CmdFileExport.Dialog.FileName);
      finally
        jpg.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    exit;
  end;
  if SameText(ext, '.tif') or SameText(ext, '.tiff') then
  begin
    Screen.Cursor := crHourGlass;
    try
      tif := TTiffImage.Create;
      try
        tif.Assign(fCladni.Bitmap);
        tif.Software := AppTitle;
        tif.SaveToFile(CmdFileExport.Dialog.FileName);
      finally
        tif.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
    exit;
  end;
  if SameText(ext, '.bmp') then
  begin
    Screen.Cursor := crHourGlass;
    try
      fCladni.Bitmap.SaveToFile(CmdFileExport.Dialog.FileName);
    finally
      Screen.Cursor := crDefault;
    end;
    exit;
  end;
  MessageDlg('Wrong image format "' + ext + '"!', mtWarning, [mbCancel], 0);
end;

procedure TFrmMain.CmdFileExportBeforeExecute(Sender: TObject);
var
  s: string;
begin
  s := ExtractFileName(fCladni.FileName);
  case CmdFileExport.Dialog.FilterIndex of
    1: s := ChangeFileExt(s, '.png');
    2: s := ChangeFileExt(s, '.jpg');
    3: s := ChangeFileExt(s, '.tif');
    4: s := ChangeFileExt(s, '.bmp');
  end;
  CmdFileExport.Dialog.FileName := s;
end;

procedure TFrmMain.CmdViewPropertiesExecute(Sender: TObject);
var
  w, h: integer;
begin
  w := fCladni.Width;
  h := fCladni.Height;
  Application.CreateForm(TFrmProperties, FrmProperties);
  try
    FrmProperties.Cladni := fCladni;
    if FrmProperties.ShowModal = mrOk then
    begin
      SetCapacity(fCladni.Capacity);
      if (w <> fCladni.Width) or (h <> fCladni.Height) then
        CmdCommandsRender.Execute;
    end;
  finally
    FrmProperties.Release;
  end;
end;

procedure TFrmMain.CmdFileOpenAccept(Sender: TObject);
begin
  OpenDoc(CmdFileOpen.Dialog.FileName);
end;

procedure TFrmMain.CmdFileOpenBeforeExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  if not CanCloseDoc() then
    Abort;
end;

procedure TFrmMain.CmdFileSaveAsAccept(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    fCladni.SaveToFile(CmdFileSaveAs.Dialog.FileName);
    fMRUList.AddToRecent(fCladni.FileName);
    UpdateTitle;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmMain.CmdFileSaveAsBeforeExecute(Sender: TObject);
begin
  CmdFileSaveAs.Dialog.FileName := ExtractFileName(fCladni.FileName);
end;

procedure TFrmMain.CmdFileSaveExecute(Sender: TObject);
begin
  SaveDoc;
end;

procedure TFrmMain.CmdFileSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := fCladni.Modified and (not fCladni.IsRendering);
end;

procedure TFrmMain.CmdCommandsRenderExecute(Sender: TObject);
begin
  fCladni.CalcLevelMap();
end;

procedure TFrmMain.CmdViewNormalizeExecute(Sender: TObject);
begin
  CmdViewNormalize.Checked := not CmdViewNormalize.Checked;
  fCladni.Normalize := CmdViewNormalize.Checked;
end;

procedure TFrmMain.CmdViewShowHitnsExecute(Sender: TObject);
begin
  Application.ShowHint := not Application.ShowHint;
end;

procedure TFrmMain.CmdViewShowHitnsUpdate(Sender: TObject);
begin
  CmdViewShowHitns.Checked := Application.ShowHint;
end;

procedure TFrmMain.CmdViewZoomFitExecute(Sender: TObject);
begin
  CmdViewZoomFit.Checked := True;
  fImgView.ZoomFit;
end;

procedure TFrmMain.SetCapacity(AValue: integer);
begin
  if fCladni.Capacity = AValue then
    Exit;
  fCladni.Capacity := AValue;
  GrWaves.RowCount := AValue + 1;
end;

procedure TFrmMain.DoCladniChanged(Sender: TObject);
begin
  UpdateTitle;
end;

procedure TFrmMain.DoCladniLoad(Sender: TObject);
var
  i: integer;
  w: TWaveInfo;
begin
  // setup GUI
  GrWaves.Clean;
  GrWaves.RowCount := fCladni.Capacity + 1;
  for i := 0 to fCladni.Capacity - 1 do
  begin
    w := fCladni.WaveInfo[i];
    if w.Frequency < MIN_FREQ_RATIO then
      continue;
    GrWaves.Cells[1, i + 1] := IntToStr(Ord(w.&On));
    GrWaves.Cells[2, i + 1] := FormatFloat('0.####', w.Amplitude);
    GrWaves.Cells[3, i + 1] := FormatFloat('0.####', w.Frequency);
    GrWaves.Cells[4, i + 1] := FormatFloat('0.####', w.Phase);
  end;
  CmdViewNormalize.Checked := fCladni.Normalize;
  fLbMaps.SelectItem(fCladni.MapIndex);
end;

procedure TFrmMain.DoCladniProgress(Sender: TObject; State: TRenderState;
  Value: integer);
begin
  case State of
    rsStart:
    begin
      LblProgress.Caption := '0%';
      ProgressBar.Position := 0;
      ProgressBar.Max := 100;
      ProgressBar.Top := StatusBar.Top;
      ProgressBar.Height := StatusBar.Height;
      if LblProgress.Height > StatusBar.Height then
        LblProgress.Visible := False;
      ProgressBar.Visible := True;
      StatusBar.Panels[0].Text := '';
      fLbMaps.Enabled := False;
      GrWaves.Enabled := False;
      UpdateActions;
    end;
    rsRun:
    begin
      ProgressBar.Position := Value;
      if LblProgress.Visible then
        LblProgress.Caption := IntToStr(Value) + '%';
    end;
    rsEnd:
    begin
      ProgressBar.Visible := False;
      StatusBar.Panels[0].Text := 'Rendering done';
      fLbMaps.Enabled := True;
      GrWaves.Enabled := True;
      UpdateActions;
    end;
  end;
end;

procedure TFrmMain.DoRenderingDone(Sender: TObject);
begin
  fImgView.LoadBitmap(fCladni.Bitmap);
end;

procedure TFrmMain.RecentFileClick(Sender: TObject; const aFileName: string);
begin
  Application.ProcessMessages;
  if not CanCloseDoc then
    exit;
  OpenDoc(aFileName);
end;

procedure TFrmMain.MapsListBoxItemSelected(Sender: TObject; Index: integer);
begin
  if fLoading then
    exit;
  fCladni.MapIndex := Index;
  fImgView.LoadBitmap(fCladni.Bitmap);
end;

procedure TFrmMain.OpenDoc(const aFileName: string);
begin
  fLoading := True;
  GrWaves.ClearSelections;
  GrWaves.ColRow := Point(1, 1);
  GrWaves.Clean;
  Screen.Cursor := crHourGlass;
  try
    fCladni.LoadFromFile(aFileName);
    fMRUList.AddToRecent(fCladni.FileName);
    UpdateTitle;
  finally
    Screen.Cursor := crDefault;
    fLoading := False;
  end;

  if fCladni.SaveLevelMapWithFile then
  begin
    fCladni.MakeBitmap;
    fImgView.LoadBitmap(fCladni.Bitmap);
  end
  else
    CmdCommandsRenderExecute(nil);
end;

procedure TFrmMain.NewDoc;
begin
  fLbMaps.SelectDefaultMap;
  fCladni.Reset;
  GrWaves.ClearSelections;
  GrWaves.ColRow := Point(1, 1);
  GrWaves.Clean;
  UpdateTitle;
  StatusBar.Panels[0].Text := 'Ready';
end;

procedure TFrmMain.SaveDoc;
begin
  if FileExists(fCladni.FileName) then
  begin
    Screen.Cursor := crHourGlass;
    try
      fCladni.SaveToFile(fCladni.FileName);
      fMRUList.AddToRecent(fCladni.FileName);
    finally
      Screen.Cursor := crDefault;
    end;
    UpdateTitle;
  end
  else
    CmdFileSaveAs.Execute;
end;

procedure TFrmMain.CmdHelpAboutExecute(Sender: TObject);
begin
  ShowAbout;
end;

procedure TFrmMain.CmdHelpDonationExecute(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru/donation?lang=en');
end;

procedure TFrmMain.CmdHelpHomePageExecute(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru/?lang=en');
end;

initialization
  DefaultFormatSettings.DecimalSeparator := '.';



end.
