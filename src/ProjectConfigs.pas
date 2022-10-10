unit ProjectConfigs;

{ Copyright ©2022 by Steve Garcia. All rights reserved.

  This file is part of the Paleo Editor project.

  The Paleo Editor is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software Foundation,
  either version 3 of the License, or (at your option) any later version.

  The Paleo Editor project is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with the Paleo
  Editor project. If not, see <https://www.gnu.org/licenses/>. }

{$MODE DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  CustomConfigFrames, ConfigUtils;

type

  { TProjectConfigFrame }

  TProjectConfigFrame = class(TCustomConfigFrame)
    ToolFolderNameLabel: TLabel;
    ToolFolderNameEdit: TDirectoryEdit;
    AssemblerFolderNameLabel: TLabel;
    AssemblerFolderNameEdit: TEdit;
    AssemblerFileNameEdit: TEdit;
    AssemblerFileNameLabel: TLabel;
    procedure AssemblerFolderNameEditChange(Sender: TObject);
    procedure ToolFolderNameEditChange(Sender: TObject);
  private
    FToolFolderName: TFileName;
    function GetToolFolderName: TFileName;
    procedure SetToolFolderName(const Value: TFileName);
  protected
    function GetIsModified: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ReadConfig(Config: TConfig); override; overload;
    procedure WriteConfig(Config: TConfig); override; overload;
    procedure ReadConfig(Config: TCustomConfig); override; overload;
    procedure WriteConfig(Config: TCustomConfig); override; overload;
    property ToolFolderName: TFileName read GetToolFolderName write SetToolFolderName;
  end;

implementation

{$R *.lfm}

{ TProjectConfigFrame }

constructor TProjectConfigFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FToolFolderName := EmptyStr;
  AssemblerFolderNameEdit.Text := EmptyStr;
  AssemblerFileNameEdit.Text := EmptyStr;
end;

procedure TProjectConfigFrame.AssemblerFolderNameEditChange(Sender: TObject);
var
  Edit: TEdit;
begin
  Edit := Sender as TEdit;
  AssemblerFileNameEdit.Text := Format(ASSEMBLER_FILE_MASK, [ExcludeTrailingPathDelimiter(Edit.Text)]);
end;

procedure TProjectConfigFrame.ToolFolderNameEditChange(Sender: TObject);
var
  Edit: TCustomEditButton;
begin
  Edit := Sender as TCustomEditButton;
  AssemblerFolderNameEdit.Text := Format(ASSEMBLER_FOLDER_MASK, [ExcludeTrailingPathDelimiter(Edit.Text)]);
end;

function TProjectConfigFrame.GetToolFolderName: TFileName;
begin
  Result := ToolFolderNameEdit.Text;
end;

procedure TProjectConfigFrame.SetToolFolderName(const Value: TFileName);
begin
  ToolFolderNameEdit.Text := ExcludeTrailingPathDelimiter(Value);
end;

function TProjectConfigFrame.GetIsModified: Boolean;
const
  COLORS: array[Boolean] of TColor = (clRed, clDefault);
var
  Temp: String;
begin
  Temp := ToolFolderName;
  ToolFolderNameEdit.Font.Color := COLORS[DirectoryExists(Temp)];
  AssemblerFolderNameEdit.Font.Color:= COLORS[DirectoryExists(AssemblerFolderNameEdit.Text)];
  AssemblerFileNameEdit.Font.Color := COLORS[FileExists(AssemblerFileNameEdit.Text)];
  Result := not AnsiSameText(FToolFolderName, Temp) and DirectoryExists(Temp);
end;

procedure TProjectConfigFrame.ReadConfig(Config: TConfig);
begin
  // Do nothing
end;

procedure TProjectConfigFrame.WriteConfig(Config: TConfig);
begin
  // Do nothing
end;

procedure TProjectConfigFrame.ReadConfig(Config: TCustomConfig);
begin
  if Assigned(Config) then begin
    ToolFolderName := Config.ToolFolderName;
    FToolFolderName := ToolFolderName;
  end;
end;

procedure TProjectConfigFrame.WriteConfig(Config: TCustomConfig);
begin
  if Assigned(Config) then begin
    Config.ToolFolderName := ToolFolderName;
  end;
end;

end.

