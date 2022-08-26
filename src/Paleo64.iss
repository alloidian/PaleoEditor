; Copyright ©2022 by Steve Garcia. All rights reserved.
;
; This file is part of the Paleo Editor project.
;
; The Paleo Editor is free software: you can redistribute it and/or modify it under the
; terms of the GNU General Public License as published by the Free Software Foundation,
; either version 3 of the License, or (at your option) any later version.
;
; The Paleo Editor project is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
; PARTICULAR PURPOSE. See the GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License along with the Paleo
; Editor project. If not, see <https://www.gnu.org/licenses/>. }

#define PaleoAppName "Paleo Editor"
#define PaleoAppVersion "0.1.0"
#define PaleoAppPublisher "Steve García"
#define PaleoAppURL "https://homebrew.computer/"
#define PaleoAppExeName "..\bin\PaleoEditor64.exe"
#define PaleoReadMeName "..\doc\Readme.txt"
#define PaleoLocalName "PaleoEditor.exe"
#define PaleoAppFolder "Paleo"

[Setup]
AppId={{F47DF585-8720-4ECD-A50F-586A7F94B0DE}
AppName={#PaleoAppName}
AppVersion={#PaleoAppVersion}
AppVerName={#PaleoAppName} {#PaleoAppVersion}
AppPublisher={#PaleoAppPublisher}
AppPublisherURL={#PaleoAppURL}
AppSupportURL={#PaleoAppURL}
AppUpdatesURL={#PaleoAppURL}
DefaultDirName={autopf}\{#PaleoAppFolder}
DefaultGroupName={#PaleoAppFolder}
AllowNoIcons=yes
LicenseFile=..\COPYING.txt
PrivilegesRequiredOverridesAllowed=dialog
OutputDir=..\bin
OutputBaseFilename=Paleo_{#PaleoAppVersion}_Win64_Setup
Compression=lzma
SolidCompression=yes
WizardStyle=modern
VersionInfoCompany=Steve García
VersionInfoCopyright=Copyright ©2022 Steve García
VersionInfoDescription=Paleo Editor Setup
VersionInfoProductName=Paleo Editor
VersionInfoVersion={#PaleoAppVersion}
VersionInfoProductVersion={#PaleoAppVersion}
VersionInfoProductTextVersion={#PaleoAppVersion} Pre-Release
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "{#PaleoAppExeName}"; DestDir: "{app}"; DestName: "{#PaleoLocalName}"; Flags: ignoreversion
Source: "{#PaleoReadMeName}"; DestDir: "{app}"; 

[Icons]
Name: "{group}\{#PaleoAppName}"; Filename: "{app}\{#PaleoLocalName}"
Name: "{group}\{cm:UninstallProgram,{#PaleoAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#PaleoAppName}"; Filename: "{app}\{#PaleoLocalName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#PaleoLocalName}"; Description: "{cm:LaunchProgram,{#StringChange(PaleoAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

