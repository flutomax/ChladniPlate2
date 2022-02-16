{
    This file is part of the ChladniPlate2.
    See LICENSE.txt for more info.
    Copyright (c) 2022 by Vasily Makarov
}

unit uSysInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLVersion;

function GetProgramVersion: string;
function GetLazarusVersion: string;
function GetFPCVersion: string;
function GetBuildDate: string;
function GetPlatform: string;
function GetOSVersion: string;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows, JwaNative, JwaNtStatus, JwaWinType,
  {$ENDIF}
  {$IF DEFINED(UNIX)}
  BaseUnix,
    {$IFDEF DARWIN}
    MacOSAll,
    {$ENDIF}
  {$ENDIF}
  {$if (lcl_fullversion)>=1070000}
  LCLPlatformDef,
  {$endif}
  InterfaceBase, StrUtils, LazUTF8, fileinfo;

{$I revision.inc}

const
  BuildDate   = {$I %DATE%};
  lazVersion  = lcl_version;         // Lazarus version (major.minor.micro)
  lazRevision = RevisionStr;         // Lazarus SVN revision
  fpcVersion  = {$I %FPCVERSION%};   // FPC version (major.minor.micro)
  TargetCPU   = {$I %FPCTARGETCPU%}; // Target CPU of FPC
  TargetOS    = {$I %FPCTARGETOS%};  // Target Operating System of FPC

var

  OSVersion: string;

function GetProgramVersion: string;
var
  pv: TProgramVersion;
begin
  if fileinfo.GetProgramVersion(pv) then
    Result := ProgramversionToStr(pv);
end;

function GetLazarusVersion: string;
var
  i: integer = 1;
begin
  Result := lazVersion;
  while (i <= Length(lazRevision)) and (lazRevision[i] in ['0'..'9']) do
    Inc(i);
  if i > 1 then
    Result += '-' + Copy(lazRevision, 1, i - 1);
end;

function GetFPCVersion: string;
begin
  Result := fpcVersion;
end;

function GetBuildDate: string;
const
  dd: TSysCharSet = ['/'];
var
  s: string;
  y, m, d: word;
begin
  // in local format
  y := StrToIntDef(ExtractDelimited(1, BuildDate, dd), 2019);
  m := StrToIntDef(ExtractDelimited(2, BuildDate, dd), 0);
  d := StrToIntDef(ExtractDelimited(3, BuildDate, dd), 0);
  Result := FormatDateTime('dd.mm.yyyy', EncodeDate(y, m, d));
end;

function GetPlatform: string;
begin
  Result := Format('%s-%s', [TargetCPU, TargetOS]);
end;

function GetOSVersion: string;
begin
  Result := OSVersion;
end;

{$IF DEFINED(UNIX)}

function LoadStringList(const FileName: string; out sl: TStringList): boolean;
const
{$IF DEFINED(LINUX)}
  FD_CLOEXEC = 1;
  O_CLOEXEC  = &02000000;
{$ELSEIF DEFINED(FREEBSD)}
  O_CLOEXEC  = &04000000;
{$ELSEIF DEFINED(NETBSD)}
  O_CLOEXEC  = $00400000;
{$ELSE}
  O_CLOEXEC  = 0;
{$ENDIF}
var
  s: string;
  h: THandle;
  fs: THandleStream;
begin
  Result := False;
  sl := nil;
  s := UTF8ToSys(FileName);
  if fpAccess(s, R_OK) = 0 then
  begin
    repeat
      h := fpOpen(s, R_OK or O_CLOEXEC);
    until (h <> -1) or (fpGetErrNo <> ESysEIntr);
    fs := THandleStream.Create(h);
    try
      sl := TStringList.Create;
      try
        sl.LoadFromStream(fs);
        Result := True;
      except
        on EFilerError do ; // Bypass
      end;
    finally
      fs.Free;
    end;
  end;
end;

function LoadStringFromFile(const FileName: string; out Str: string): boolean;
var
  sl: TStringList;
begin
  str := '';
  Result := LoadStringList(FileName, sl);
  if Result then
    try
      if sl.Count > 0 then
        Str := sl.Strings[0];
    finally
      sl.Free;
    end;
end;

function FindStringInFile(const FileName, FindStr: string): boolean;
var
  s: string;
begin
  Result := LoadStringFromFile(FileName, s) and (FindStr = s);
end;

function GetOsFromLsbRelease: string;
var
  sl: TStringList;
begin
  Result := '';
  if LoadStringList('/etc/lsb-release', sl) then
    try
      if sl.Count > 0 then
      begin
        Result := sl.Values['DISTRIB_DESCRIPTION'];
        if Result <> '' then
          Result := TrimSet(Result, ['"', ''''])
        else
          Result := sl.Values['DISTRIB_ID'] + sl.Values['DISTRIB_RELEASE'] +
            sl.Values['DISTRIB_CODENAME'];
      end;
    finally
      sl.Free;
    end;
end;

function GetOsFromProcVersion: string;
var
  i: integer;
  s: string;
begin
  Result := '';
  if LoadStringFromFile('/proc/version', s) then
  begin
    // Get first three strings separated by space.
    i := Pos(' ', s);
    if i > 0 then
      Result := Result + Copy(s, 1, i);
    Delete(s, 1, i);
    i := Pos(' ', s);
    if i > 0 then
      Result := Result + Copy(s, 1, i);
    Delete(s, 1, i);
    i := Pos(' ', s);
    if i > 0 then
      Result := Result + Copy(s, 1, i - 1);
    Delete(s, 1, i);
  end;
end;

function GetOsFromIssue: string;
begin
  if not LoadStringFromFile('/etc/issue', Result) then
    Result := '';
end;

function GetDebianVersion: string;
var
  s: string;
begin
  if LoadStringFromFile('/etc/debian_version', s) then
  begin
    Result := 'Debian';
    if s <> '' then
      Result := Result + ' ' + s;
  end
  else
    Result := '';
end;

function GetSuseVersion: string;
begin
  if LoadStringFromFile('/etc/SuSE-release', Result) or
    LoadStringFromFile('/etc/suse-release', Result) then
  begin
    if Result = '' then
      Result := 'Suse';
  end
  else
    Result := '';
end;

function GetRedHatVersion: string;
begin
  if LoadStringFromFile('/etc/redhat-release', Result) then
  begin
    if Result = '' then
      Result := 'RedHat';
  end
  else
    Result := '';
end;

function GetMandrakeVersion: string;
begin
  if LoadStringFromFile('/etc/mandrake-release', Result) then
  begin
    if Result = '' then
      Result := 'Mandrake';
  end
  else
    Result := '';
end;

function GetVersionNumber: string;
var
  Info: utsname;
  i: integer = 1;
begin
  FillChar(Info, SizeOf(Info), 0);
  fpUname(Info);
  Result := Info.Release;
  while (i <= Length(Result)) and (Result[i] in ['0'..'9', '.']) do
    Inc(i);
  Result := Copy(Result, 1, i - 1);
end;

{$IFDEF DARWIN}
function GetMacOSXVersion: string;
var
  versionMajor, versionMinor, versionBugFix: SInt32;
begin
  Result := EmptyStr;
  if (Gestalt(gestaltSystemVersionMajor, versionMajor) <> noErr) then
    Exit;
  if (Gestalt(gestaltSystemVersionMinor, versionMinor) <> noErr) then
    Exit;
  if (Gestalt(gestaltSystemVersionBugFix, versionBugFix) <> noErr) then
    Exit;
  Result := Format('Mac OS X %d.%d.%d', [versionMajor, versionMinor, versionBugFix]);
end;

{$ENDIF}
{$ENDIF}


{$IF DEFINED(MSWINDOWS)}

function RegReadKey(ARoot: HKEY; const APath, AName: UnicodeString;
  out AValue: UnicodeString): boolean;
var
  AKey: HKEY = 0;
  dwSize: DWORD = MaxSmallint;
begin
  Result := RegOpenKeyExW(ARoot, PWideChar(APath), 0, KEY_READ, AKey) = ERROR_SUCCESS;
  if Result then
  begin
    SetLength(AValue, MaxSmallint);
    Result := RegQueryValueExW(AKey, PWideChar(AName), nil, nil, PByte(AValue), @dwSize) =
      ERROR_SUCCESS;
    if Result then
    begin
      dwSize := dwSize div SizeOf(widechar);
      if (dwSize > 0) and (AValue[dwSize] = #0) then
        Dec(dwSize);
      SetLength(AValue, dwSize);
    end;
    RegCloseKey(AKey);
  end;
end;

procedure TryGetNativeSystemInfo(var SystemInfo: TSystemInfo);
type
  TGetNativeSystemInfo = procedure(var lpSystemInfo: TSystemInfo); stdcall;
var
  hLib: HANDLE;
  GetNativeSystemInfoProc: TGetNativeSystemInfo;
begin
  hLib := LoadLibrary(LPCTSTR('kernel32.dll'));
  if hLib <> 0 then
  begin
    try
      GetNativeSystemInfoProc :=
        TGetNativeSystemInfo(GetProcAddress(hLib, 'GetNativeSystemInfo'));
      if Assigned(GetNativeSystemInfoProc) then
        GetNativeSystemInfoProc(SystemInfo)
      else
        GetSystemInfo(SystemInfo);
    finally
      FreeLibrary(hLib);
    end;
  end
  else
    GetSystemInfo(SystemInfo);
end;

{$ENDIF}


procedure InitializeVersionInfo;
{$IF DEFINED(MSWINDOWS)}
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  CURRENT_VERSION = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
var
  si: SYSTEM_INFO;
  osvi: TOsVersionInfoExW;
  ReleaseId: UnicodeString;
{$ENDIF}
begin
  {$IF DEFINED(MSWINDOWS)}
  OSVersion := 'Windows';

  ZeroMemory(@osvi, SizeOf(TOsVersionInfoExW));
  osvi.dwOSVersionInfoSize := SizeOf(TOsVersionInfoExW);

  if (RtlGetVersion(@osvi) = STATUS_SUCCESS) or GetVersionExW(@osvi) then
  begin
    ZeroMemory(@si, SizeOf(si));
    TryGetNativeSystemInfo(si);

    case osvi.dwPlatformId of
      VER_PLATFORM_WIN32_WINDOWS:
        case osvi.dwMajorVersion of
          4: case osvi.dwMinorVersion of
              0: OSVersion := OSVersion + ' 95';
              10: OSVersion := OSVersion + ' 98';
              90: OSVersion := OSVersion + ' ME';
            end;
        end;

      VER_PLATFORM_WIN32_NT:
      begin
        case osvi.dwMajorVersion of
          3: OSVersion := OSVersion + ' NT 3.5';
          4: OSVersion := OSVersion + ' NT 4';
          5: case osvi.dwMinorVersion of
              0: OSVersion := OSVersion + ' 2000';
              1:
              begin
                OSVersion := OSVersion + ' XP';
                if osvi.wSuiteMask = $0000 then
                  OSVersion := OSVersion + ' Home'
                else if osvi.wSuiteMask = $0200 then
                  OSVersion := OSVersion + ' Professional';
              end;
              2: if (osvi.wProductType = VER_NT_WORKSTATION) and
                  (si.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                begin
                  OSVersion := OSVersion + ' XP Professional x64';
                end
                else if (osvi.wProductType = VER_NT_SERVER) then
                begin
                  if osvi.wSuiteMask = $8000 then
                    OSVersion := OSVersion + ' Home Server'
                  else
                    OSVersion := OSVersion + ' Server 2003';
                end;
            end;
          6: case osvi.dwMinorVersion of
              0: if (osvi.wProductType = VER_NT_WORKSTATION) then
                begin
                  OSVersion := OSVersion + ' Vista';
                  if osvi.wSuiteMask = $0000 then
                    OSVersion := OSVersion + ' Ultimate'
                  else if osvi.wSuiteMask = $0200 then
                    OSVersion := OSVersion + ' Home';
                end
                else if (osvi.wProductType = VER_NT_SERVER) then
                  OSVersion := OSVersion + ' Server 2008';
              1: if (osvi.wProductType = VER_NT_WORKSTATION) then
                  OSVersion := OSVersion + ' 7'
                else if (osvi.wProductType = VER_NT_SERVER) then
                  OSVersion := OSVersion + ' Server 2008 R2';
              2: if (osvi.wProductType = VER_NT_WORKSTATION) then
                  OSVersion := OSVersion + ' 8'
                else if (osvi.wProductType = VER_NT_SERVER) then
                  OSVersion := OSVersion + ' Server 2012';
              3: if (osvi.wProductType = VER_NT_WORKSTATION) then
                  OSVersion := OSVersion + ' 8.1'
                else if (osvi.wProductType = VER_NT_SERVER) then
                  OSVersion := OSVersion + ' Server 2012 R2';
            end;
          10: case osvi.dwMinorVersion of
              0: if (osvi.wProductType = VER_NT_WORKSTATION) then
                begin
                  if osvi.dwBuildNumber >= 22000 then
                    OSVersion := OSVersion + ' 11'
                  else
                    OSVersion := OSVersion + ' 10';
                  if (osvi.wSuiteMask and VER_SUITE_PERSONAL <> 0) then
                    OSVersion := OSVersion + ' Home';
                  if RegReadKey(HKEY_LOCAL_MACHINE, CURRENT_VERSION,
                    'ReleaseId', ReleaseId) then
                    OSVersion := OSVersion + ' ' + string(ReleaseId);
                end
            end;
        end;
      end;
    end;

    // If something detected then add service pack number and architecture.
    if OSVersion <> 'Windows' then
    begin
      if osvi.wServicePackMajor > 0 then
      begin
        OSVersion := OSVersion + ' SP' + IntToStr(osvi.wServicePackMajor);
        if osvi.wServicePackMinor > 0 then
          OSVersion := OSVersion + '.' + IntToStr(osvi.wServicePackMinor);
      end;

      if si.wProcessorArchitecture in [PROCESSOR_ARCHITECTURE_AMD64] then
        OSVersion := OSVersion + ' x86_64'
      else
        OSVersion := OSVersion + ' i386';
    end
    else
      OSVersion := OSVersion + ' Build ' + IntToStr(osvi.dwBuildNumber);
  end;

  {$ELSEIF DEFINED(UNIX)}
  {$IFDEF DARWIN}
    OSVersion := GetMacOSXVersion;
  if OSVersion = '' then
     OSVersion := 'MacOS';
  exit;
  {$ENDIF}

  // Try using linux standard base.
  OSVersion := GetOsFromLsbRelease;

  // Try some distribution-specific files.
  if OSVersion = '' then
    OSVersion := GetDebianVersion;
  if OSVersion = '' then
    OSVersion := GetRedHatVersion;
  if OSVersion = '' then
    OSVersion := GetSuseVersion;
  if OSVersion = '' then
    OSVersion := GetMandrakeVersion;


  // Other methods.
  if OSVersion = '' then
    OSVersion := GetOsFromIssue;
  if OSVersion = '' then
    OSVersion := GetOsFromProcVersion;
  // Set default names.
  if OSVersion = '' then
  begin
    {$IF DEFINED(LINUX)}
    OSVersion := 'Linux';
    {$ELSEIF DEFINED(FREEBSD)}
    OSVersion := 'FreeBSD';
    {$ELSEIF DEFINED(BSD)}
    OSVersion := 'BSD';
    {$ELSE}
    OSVersion := 'Unix';
    {$ENDIF}
    OSVersion += ' ' + GetVersionNumber;
  end;
  {$ENDIF}
end;

initialization
  InitializeVersionInfo;

end.
