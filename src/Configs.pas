unit Configs;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ComCtrls,
  ExtCtrls, ActnList, ConfigUtils, NavigatorConfigs, ProjectConfigs, ColorConfigs,
  EditorConfigs, CustomWorks;

type

  { TConfigForm }

  TConfigForm = class(TForm)
    Actions: TActionList;
    SaveAction: TAction;
    Pages: TPageControl;
    NavigatorPage: TTabSheet;
    ColorPage: TTabSheet;
    EditorPage: TTabSheet;
    ButtonPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    ProjectPage: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure SaveActionUpdate(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    FConfig: TConfig;
    FPConfig: TCustomConfig;
    FNavigatorConfig: TNavigatorConfigFrame;
    FProjectConfig: TProjectConfigFrame;
    FColorConfig: TColorConfigFrame;
    FEditorConfig: TEditorConfigFrame;
    procedure SetConfig(Value: TConfig);
    procedure SetProjectConfig(Value: TCustomConfig);
    function GetIsModified: Boolean;
  public
    property Config: TConfig read FConfig write SetConfig;
    property ProjectConfig: TCustomConfig read FPConfig write SetProjectConfig;
    property IsModified: Boolean read GetIsModified;
  end;

var
  Config: TConfig;

function ShowConfig(Work: TCustomWorkForm): Boolean;

implementation

{$R *.lfm}

function ShowConfig(Work: TCustomWorkForm): Boolean;
var
  Dialog: TConfigForm;
begin
  Dialog := TConfigForm.Create(Application.MainForm);
  try
    Dialog.Config := Config;
    if Assigned(Work) then
      Dialog.ProjectConfig := Work.Configs;
    Result := Dialog.ShowModal = mrOk;
    if Result then
      Config.WriteConfig;
  finally
    Dialog.Free;
  end;
end;

{ TConfigForm }

procedure TConfigForm.FormCreate(Sender: TObject);
begin
  FNavigatorConfig := TNavigatorConfigFrame.Create(Self);
  FNavigatorConfig.Parent := NavigatorPage;
  FNavigatorConfig.Align := alClient;
  FProjectConfig := TProjectConfigFrame.Create(Self);
  FProjectConfig.Parent := ProjectPage;
  FProjectConfig.Align := alClient;
  FColorConfig := TColorConfigFrame.Create(Self);
  FColorConfig.Parent := ColorPage;
  FColorConfig.Align := alClient;
  FEditorConfig := TEditorConfigFrame.Create(Self);
  FEditorConfig.Parent := EditorPage;
  FEditorConfig.Align := alClient;
end;

procedure TConfigForm.SaveActionExecute(Sender: TObject);
begin
  if FNavigatorConfig.IsModified then
    FNavigatorConfig.WriteConfig(FConfig);
  if FProjectConfig.IsModified then begin
    FProjectConfig.WriteConfig(FConfig);
    FprojectConfig.WriteConfig(FPConfig);
  end;
  if FColorConfig.IsModified then
    FColorConfig.WriteConfig(FConfig);
  if FEditorConfig.IsModified then
    FEditorConfig.WriteConfig(FConfig);
  ModalResult := mrOk;
end;

procedure TConfigForm.SaveActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IsModified;
end;

procedure TConfigForm.SetConfig(Value: TConfig);
begin
  FConfig := Value;
  FNavigatorConfig.ReadConfig(FConfig);
  FColorConfig.ReadConfig(FConfig);
  FEditorConfig.ReadConfig(FConfig);
end;

procedure TConfigForm.SetProjectConfig(Value: TCustomConfig);
begin
  FPConfig := Value;
  ProjectPage.TabVisible := Assigned(FPConfig);
  FProjectConfig.ReadConfig(FPConfig);
end;

function TConfigForm.GetIsModified: Boolean;
begin
  Result := FNavigatorConfig.IsModified or FProjectConfig.IsModified or
    FColorConfig.IsModified or FEditorConfig.IsModified;
end;

initialization
  Config := TConfig.Create;
finalization
  Config.Free;
end.

