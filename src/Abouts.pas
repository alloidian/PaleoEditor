unit Abouts;

{ Copyright ©2022-2023 by Steve Garcia. All rights reserved.

  This file is part of the Paleo Editor project.

  The Paleo Editor is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software Foundation,
  either version 3 of the License, or (at your option) any later version.

  The Paleo Editor project is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with the Paleo
  Editor project. If not, see <https://www.gnu.org/licenses/>. }

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, AboutCredits;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    LicenseLabel: TLabel;
    ToolLabel: TLabel;
    Image: TImage;
    TitleLabel: TLabel;
    DescriptionLabel: TLabel;
    CopyrightLabel: TLabel;
    ThirdPartyLabel: TLabel;
    CreditBox: TScrollBox;
    CloseButton: TButton;
    VersionLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure DescriptionLabelClick(Sender: TObject);
    procedure ToolLabelClick(Sender: TObject);
    procedure CopyrightLabelClick(Sender: TObject);
    procedure LicenseLabelClick(Sender: TObject);
  private
    FCreditFrame: TAboutCreditFrame;
  end;

procedure ShowAbout;

implementation

{$R *.lfm}

uses
  Windows, Configs;

procedure ShowAbout;
var
  Dialog: TAboutForm;
begin
  Dialog := TAboutForm.Create(Application.MainForm);
  try
    Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
const
  DESC_CAPTION = 'for RomWBW Development';
  TOOL_CAPTION = 'Built with Lazarus';
  VERSION_CAPTION = 'Version: %s %s'#13#10'%s';
  COPYRIGHT_CAPTION = 'Copyright ©2023 Steve García';
  LICENSE_CAPTION = 'Licensed by GNU GPL V3';
begin
  FCreditFrame := TAboutCreditFrame.Create(Self);
  FCreditFrame.Parent := CreditBox;
  DescriptionLabel.Caption := DESC_CAPTION;
  ToolLabel.Caption := TOOL_CAPTION;
  VersionLabel.Caption := Format(VERSION_CAPTION, [Config.VersionText, Config.Platform, Config.PreRelease]).Trim;
  CopyrightLabel.Caption := COPYRIGHT_CAPTION;
  LicenseLabel.Caption := LICENSE_CAPTION;
end;

procedure TAboutForm.DescriptionLabelClick(Sender: TObject);
begin
  ShellExecute(handle, OPEN_COMMAND, PChar('https://github.com/wwarthen/RomWBW'), nil,nil, SW_SHOW);
end;

procedure TAboutForm.ToolLabelClick(Sender: TObject);
begin
  ShellExecute(handle, OPEN_COMMAND, PChar('https://www.lazarus-ide.org/'), nil,nil, SW_SHOW);
end;

procedure TAboutForm.CopyrightLabelClick(Sender: TObject);
begin
  ShellExecute(handle, OPEN_COMMAND, PChar('https://dynocomputer.com'), nil,nil, SW_SHOW);
end;

procedure TAboutForm.LicenseLabelClick(Sender: TObject);
begin
  ShellExecute(handle, OPEN_COMMAND, PChar('https://www.gnu.org/licenses/'), nil,nil, SW_SHOW);
end;

end.

