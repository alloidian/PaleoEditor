unit NavigatorConfigs;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit, StdCtrls,
  CustomConfigFrames, ConfigUtils;

type
  TNavigatorConfigFrame = class(TCustomConfigFrame)
    ConfigEdit: TValueListEditor;
    SaveWorkspaceEdit: TCheckBox;
    procedure ConfigEditButtonClick(Sender: TObject; aCol, aRow: Integer);
  private
    FEditFiles: String;
    FAssemblyFiles: String;
    FExecFiles: String;
    FSearchFiles: String;
    FUneditableFiles: String;
    FSaveWorkspace: Boolean;
    FExcludeFiles: String;
    FExcludeFolders: String;
  protected
    function GetIsModified: Boolean; override;
    function GetEditFiles: String;
    procedure SetEditFiles(const Value: String);
    function GetAssemlbyFiles: String;
    procedure SetAssemblyFiles(const Value: String);
    function GetExecFiles: String;
    procedure SetExecFiles(const Value: String);
    function GetSearchFiles: String;
    procedure SetSearchFiles(const Value: String);
    function GetUneditableFiles: String;
    procedure SetUneditableFiles(const Value: String);
    function GetSaveWorkspace: Boolean;
    procedure SetSaveWorkspace(Value: Boolean);
    function GetExcludeFiles: String;
    procedure SetExcludeFiles(const Value: String);
    function GetExcludeFolders: String;
    procedure SetExcludeFolders(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ReadConfig(Config: TConfig); override;
    procedure WriteConfig(Config: TConfig); override;
    property EditFiles: String read GetEditFiles write SetEditFiles;
    property AssemblyFiles: String read GetAssemlbyFiles write SetAssemblyFiles;
    property ExecFiles: String read GetExecFiles write SetExecFiles;
    property SearchFiles: String read GetSearchFiles write SetSearchFiles;
    property UneditableFiles: String read GetUneditableFiles write SetUneditableFiles;
    property SaveWorkspace: Boolean read GetSaveWorkspace write SetSaveWorkspace;
    property ExcludeFiles: String read GetExcludeFiles write SetExcludeFiles;
    property ExcludeFolders: String read GetExcludeFolders write SetExcludeFolders;
  end;

implementation

{$R *.lfm}

uses
  FileMasks, TypInfo;

{ TNavigatorConfigFrame }

constructor TNavigatorConfigFrame.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);
  ConfigEdit.Strings.Clear;
  ConfigEdit.Strings.AddPair(ITEM_EDIT, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_ASSEMBLE, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_EXEC, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_SEARCH, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_UNEDITABLE, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_EXCLUDE_FILE, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_EXCLUDE_FOLDER, EmptyStr);
  for I := 0 to ConfigEdit.RowCount - 2 do
    ConfigEdit.ItemProps[I].EditStyle := esEllipsis;
  ConfigEdit.DefaultColWidth := 100;
end;

procedure TNavigatorConfigFrame.ConfigEditButtonClick(Sender: TObject; aCol, aRow: Integer);
var
  Editor: TValueListEditor;
  Row: Integer;
  Temp: String;
begin
  Editor := Sender as TValueListEditor;
  Row := aRow - 1;
  Temp := Editor.Strings.ValueFromIndex[Row];
  if EditFileMasks(Editor.Strings.Names[Row], Temp) then
    Editor.Strings.ValueFromIndex[Row] := Temp;
end;

function TNavigatorConfigFrame.GetIsModified: Boolean;
begin
  Result := not AnsiSameText(EditFiles, FEditFiles) or
    not AnsiSameText(AssemblyFiles, FAssemblyFiles) or
    not AnsiSameText(ExecFiles, FExecFiles) or
    not AnsiSameText(SearchFiles, FSearchFiles) or
    not AnsiSameText(UneditableFiles, FUneditableFiles) or
    (SaveWorkspace <> FSaveWorkspace) or
    not AnsiSameText(ExcludeFiles, FExcludeFiles) or
    not AnsiSameText(ExcludeFolders, FExcludeFolders);
end;

function TNavigatorConfigFrame.GetEditFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_EDIT];
end;

procedure TNavigatorConfigFrame.SetEditFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_EDIT] := Value;
  FEditFiles := Value;
end;

function TNavigatorConfigFrame.GetAssemlbyFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_ASSEMBLE];
end;

procedure TNavigatorConfigFrame.SetAssemblyFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_ASSEMBLE] := Value;
  FAssemblyFiles := Value;
end;

function TNavigatorConfigFrame.GetExecFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_EXEC];
end;

procedure TNavigatorConfigFrame.SetExecFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_EXEC] := Value;
  FExecFiles := Value;
end;

function TNavigatorConfigFrame.GetSearchFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_SEARCH];
end;

procedure TNavigatorConfigFrame.SetSearchFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_SEARCH] := Value;
  FSearchFiles := Value;
end;

function TNavigatorConfigFrame.GetUneditableFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_UNEDITABLE];
end;

procedure TNavigatorConfigFrame.SetUneditableFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_UNEDITABLE] := Value;
  FUneditableFiles := Value;
end;

function TNavigatorConfigFrame.GetSaveWorkspace: Boolean;
begin
  Result := SaveWorkspaceEdit.Checked;
end;

procedure TNavigatorConfigFrame.SetSaveWorkspace(Value: Boolean);
begin
  SaveWorkspaceEdit.Checked := Value;
  FSaveWorkspace := Value;
end;

function TNavigatorConfigFrame.GetExcludeFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_EXCLUDE_FILE];
end;

procedure TNavigatorConfigFrame.SetExcludeFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_EXCLUDE_FILE] := Value;
  FExcludeFiles := Value;
end;

function TNavigatorConfigFrame.GetExcludeFolders: String;
begin
  Result := ConfigEdit.Values[ITEM_EXCLUDE_FOLDER];
end;

procedure TNavigatorConfigFrame.SetExcludeFolders(const Value: String);
begin
  ConfigEdit.Values[ITEM_EXCLUDE_FOLDER] := Value;
  FExcludeFolders := Value;
end;

procedure TNavigatorConfigFrame.ReadConfig(Config: TConfig);
begin
  EditFiles := Config.EditFiles;
  AssemblyFiles := Config.AssemblyFiles;
  ExecFiles := Config.ExecuteFiles;
  SearchFiles := Config.SearchFiles;
  UneditableFiles := Config.UneditableFiles;
  SaveWorkspace := Config.SaveWorkspace;
  ExcludeFiles := Config.ExcludeFiles;
  ExcludeFolders := Config.ExcludeFolders;
end;

procedure TNavigatorConfigFrame.WriteConfig(Config: TConfig);
begin
  Config.EditFiles := EditFiles;
  Config.AssemblyFiles := AssemblyFiles;
  Config.ExecuteFiles := ExecFiles;
  Config.SearchFiles := SearchFiles;
  Config.UneditableFiles := UneditableFiles;
  Config.SaveWorkspace := SaveWorkspace;
  Config.ExcludeFiles := ExcludeFiles;
  Config.ExcludeFolders :=ExcludeFolders ;
end;

end.

