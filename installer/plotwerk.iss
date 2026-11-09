#ifndef Arch
  #define Arch "x64"
#endif
#ifndef Target
  #define Target "x86_64-pc-windows-msvc"
#endif
#ifndef AppVersion
  #define AppVersion "0.1.0"
#endif

[Setup]
AppId={{8C1F5A62-3B7E-4D35-9E62-6B2F0E9A4C11}
AppName=Plotwerk
AppVersion={#AppVersion}
AppVerName=Plotwerk {#AppVersion}
AppPublisher=Plotwerk
DefaultDirName={autopf}\Plotwerk
DefaultGroupName=Plotwerk
DisableProgramGroupPage=yes
OutputDir=..\dist
OutputBaseFilename=Plotwerk-Setup-{#AppVersion}-{#Arch}
Compression=lzma2
SolidCompression=yes
WizardStyle=modern
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
ChangesAssociations=yes
UninstallDisplayIcon={app}\plotwerk.exe
LicenseFile=..\LICENSE
#if Arch == "arm64"
ArchitecturesAllowed=arm64
ArchitecturesInstallIn64BitMode=arm64
#else
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
#endif

[Languages]
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\target\{#Target}\release\plotwerk.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\LICENSE"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{autoprograms}\Plotwerk"; Filename: "{app}\plotwerk.exe"
Name: "{autodesktop}\Plotwerk"; Filename: "{app}\plotwerk.exe"; Tasks: desktopicon

[Registry]
Root: HKA; Subkey: "Software\Classes\.pwk"; ValueType: string; ValueName: ""; ValueData: "Plotwerk.Datei"; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\Plotwerk.Datei"; ValueType: string; ValueName: ""; ValueData: "Plotwerk-Datei"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\Plotwerk.Datei\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\plotwerk.exe,0"
Root: HKA; Subkey: "Software\Classes\Plotwerk.Datei\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\plotwerk.exe"" ""%1"""

[Run]
Filename: "{app}\plotwerk.exe"; Description: "{cm:LaunchProgram,Plotwerk}"; Flags: nowait postinstall skipifsilent
