unit Main;

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
  Classes, SysUtils, Forms, Controls, Dialogs, ComCtrls, Menus, ActnList,
  StdActns, ExtCtrls, Actions, CustomWorks;

type
  TMainForm = class(TForm)
    Images: TImageList;
    Actions: TActionList;
    OpenFolderAction: TAction;
    OpenListingAction: TAction;
    ProjectOpenRecentMenu: TMenuItem;
    CloseProjectAction: TAction;
    CloseAllProjectAction: TAction;
    ConfigAction: TAction;
    FileExitAction: TFileExit;
    ShowAboutAction: TAction;
    MainMenu: TMainMenu;
    ProjectMenu: TMenuItem;
    ProjectOpenMenu: TMenuItem;
    ProjectOpenFolderMenu: TMenuItem;
    ProjectOpenListingMenu: TMenuItem;
    ProjectCloseMenu: TMenuItem;
    ProjectCloseAllMenu: TMenuItem;
    ConfigSeparator: TMenuItem;
    ProjectConfigMenu: TMenuItem;
    ExitSeparator: TMenuItem;
    ProjectExitMenu: TMenuItem;
    WindowMenu: TMenuItem;
    WindowTileVertialMenu: TMenuItem;
    WindowTileHorizontalMenu: TMenuItem;
    WindowCascadeMenu: TMenuItem;
    WindowSeparator: TMenuItem;
    HelpMenu: TMenuItem;
    HelpContextMenu: TMenuItem;
    HelpIndexMenu: TMenuItem;
    UpdateSeparator: TMenuItem;
    HelpOnlineMenu: TMenuItem;
    HelpCheckMenu: TMenuItem;
    AboutSeparator: TMenuItem;
    HelpAboutMenu: TMenuItem;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure HintEvent(Sender: TObject);
    procedure ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure OpenFolderActionExecute(Sender: TObject);
    procedure OpenListingActionExecute(Sender: TObject);
    procedure FileOpenRecentMenuClick(Sender: TObject);
    procedure CloseProjectActionExecute(Sender: TObject);
    procedure CloseProjectActionUpdate(Sender: TObject);
    procedure CloseAllProjectActionExecute(Sender: TObject);
    procedure CloseAllProjectActionUpdate(Sender: TObject);
    procedure ConfigActionExecute(Sender: TObject);
    procedure ShowAboutActionExecute(Sender: TObject);
    procedure UnimplementedMenuClick(Sender: TObject);
  private
    FWindowCascade: TWindowCascade;
    FWindowTileHorizontal: TWindowTileHorizontal;
    FWindowTileVertical: TWindowTileVertical;
    procedure OpenWorkspace(const FolderName: TFileName);
    procedure OpenListing(const FolderName: TFileName);
    procedure AddProject(const FolderName: TFileName);
    function GetActiveProject: TCustomWorkForm;
  protected
    property ActiveProject: TCustomWorkForm read GetActiveProject;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Utils, Configs, Abouts, FolderWorks, ProjectWorks;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
const
  MASK = 'V.%s %s %s';
begin
  Config.ReadConfig;
  Config.ReadConfig(ProjectOpenRecentMenu, FileOpenRecentMenuClick);
  StatusBar.Panels[0].Text := Format(MASK, [Config.VersionText, Config.Platform, Config.PreRelease]).Trim;
  Application.OnHint := HintEvent;
  FWindowCascade := TWindowCascade.Create(Self);
  FWindowCascade.Name := 'FWindowCascade';
  FWindowCascade.Caption := '&Cascade All';
  FWindowCascade.Hint := 'Cascade all projects.';
  WindowCascadeMenu.Action := FWindowCascade;
  FWindowTileHorizontal := TWindowTileHorizontal.Create(Self);
  FWindowTileHorizontal.Name := 'FWindowTileHorizontal';
  FWindowTileHorizontal.Caption := 'File &Horizontally';
  FWindowTileHorizontal.Hint := 'Horizontally tile all projects.';
  WindowTileHorizontalMenu.Action := FWindowTileHorizontal;
  FWindowTileVertical := TWindowTileVertical.Create(Self);
  FWindowTileVertical.Name := 'FWindowTileVertical';
  FWindowTileVertical.Caption := 'Tile &Vertically';
  FWindowTileVertical.Hint := 'Vertically tile all projects.';
  WindowTileVertialMenu.Action := FWindowTileVertical;
  Application.OnIdle := AppIdle;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Application.OnHint := nil;
  WindowCascadeMenu.Action := nil;
  WindowTileHorizontalMenu.Action := nil;
  WindowTileVertialMenu.Action := nil;
  Config.WriteConfig(Self);
  Config.WriteConfig(ProjectOpenRecentMenu);
  Config.WriteConfig;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
 Config.ReadConfig(Self);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CloseAllProjectAction.Execute;
end;

procedure TMainForm.AppIdle(Sender: TObject; var Done: Boolean);
begin
 if Assigned(ActiveProject) then
   ActiveProject.Idle;
 Done := True;
end;

procedure TMainForm.HintEvent(Sender: TObject);
begin
  StatusBar.Panels[1].Text := Application.Hint;
end;

procedure TMainForm.ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  ProjectOpenRecentMenu.Enabled := ProjectOpenRecentMenu.Count > 0;
end;

procedure TMainForm.OpenFolderActionExecute(Sender: TObject);
var
  Dialog: TSelectDirectoryDialog;
begin
  Dialog := TSelectDirectoryDialog .Create(Self);
  try
    Dialog.Title := 'Open Workspace Folder';
    if Dialog.Execute then begin
      OpenWorkspace(Dialog.FileName);
      AddProject(Dialog.FileName);
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TMainForm.OpenListingActionExecute(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog .Create(Self);
  try
    Dialog.DefaultExt := '.lst';
    Dialog.Filter := 'Listing File (*.lst)|*.lst';
    Dialog.Title := 'Open Listing File';
    if Dialog.Execute then begin
      OpenListing(Dialog.FileName);
      AddProject(Dialog.FileName);
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TMainForm.FileOpenRecentMenuClick(Sender: TObject);
var
  Item: TMenuItem;
  FolderName: TFileName;
  Found: Boolean;
  I: Integer;
  Form: TCustomForm;
begin
  Item := Sender as TMenuItem;
  FolderName := Item.Caption;
  Found := False;
  for I := 0 to MDIChildCount - 1 do begin
    Form := MDIChildren[I];
    if AnsiSameText(Form.Caption, FolderName) then begin
      Found := True;
      Item.MenuIndex := 0;
      Form.BringToFront;
      Break;
    end;
  end;
  if not Found then begin
    case Item.Tag of
      0: OpenWorkspace(FolderName);
      1: OpenListing(FolderName);
    end;
    AddProject(FolderName);
  end;
end;

procedure TMainForm.CloseProjectActionExecute(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then
    ActiveMDIChild.Free;
end;

procedure TMainForm.CloseProjectActionUpdate(Sender: TObject);
begin
 (Sender as TAction).Enabled := Assigned(ActiveMDIChild);
end;

procedure TMainForm.CloseAllProjectActionExecute(Sender: TObject);
var
  I: Integer;
begin
 for I := MDIChildCount - 1 downto 0 do
   MDIChildren[I].Free;
end;

procedure TMainForm.CloseAllProjectActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := MDIChildCount > 1;
end;

procedure TMainForm.ConfigActionExecute(Sender: TObject);
begin
  ShowConfig;
end;

procedure TMainForm.ShowAboutActionExecute(Sender: TObject);
begin
  ShowAbout;
end;

procedure TMainForm.UnimplementedMenuClick(Sender: TObject);
begin
  ShowMessage(UNIMPLEMENTED_PROMPT);
end;

procedure TMainForm.OpenWorkspace(const FolderName: TFileName);
var
  Editor: TFolderWorkForm;
begin
  Editor := TFolderWorkForm.Create(Self);
  Editor.WindowState := wsMaximized;
  Editor.Open(FolderName, WindowMenu);
  Editor.Show;
end;

procedure TMainForm.OpenListing(const FolderName: TFileName);
var
  Editor: TProjectWorkForm;
begin
  Editor := TProjectWorkForm.Create(Self);
  Editor.WindowState := wsMaximized;
  Editor.Open(FolderName, WindowMenu);
  Editor.Show;
end;

procedure TMainForm.AddProject(const FolderName: TFileName);
var
  Found: Boolean;
  IsImage: Boolean;
  Item: TMenuItem;
begin
  Found := False;
  for Item in ProjectOpenRecentMenu do
    if AnsiSameText(Item.Caption, FolderName) then begin
      Found := True;
      Item.MenuIndex := 0;
      Break;
    end;
  if not Found then begin
    IsImage := AnsiSameText(ExtractFileExt(FolderName), '.lst');
    while ProjectOpenRecentMenu.Count > 9 do
      ProjectOpenRecentMenu.Items[ProjectOpenRecentMenu.Count - 1].Free;
    Item := TMenuItem.Create(ProjectOpenRecentMenu);
    Item.Caption := FolderName;
    Item.Tag := IMAGE_INDEX[IsImage];
    Item.ImageIndex := Item.Tag;
    Item.OnClick := FileOpenRecentMenuClick;
    ProjectOpenRecentMenu.Insert(0, Item);
  end;
end;

function TMainForm.GetActiveProject: TCustomWorkForm;
begin
  if Assigned(ActiveMDIChild) and (ActiveMDIChild is TCustomWorkForm) then
    Result := ActiveMDIChild as TCustomWorkForm
  else
    Result := nil;
end;

end.

