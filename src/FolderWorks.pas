unit FolderWorks;

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
  Classes, SysUtils, Forms, Controls, Dialogs, Menus, CustomWorks, ConfigUtils, DirMonitors;

type
  TFolderWorkForm = class(TCustomWorkForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  protected
    procedure RefreshView; override;
  public
    procedure Open(const FolderName: TFileName; ParentMenu: TMenuItem); override;
  end;

implementation

{$R *.lfm}

uses
  ComCtrls, Utils, CustomEditors, Configs;

{ TFolderWorkForm }

procedure TFolderWorkForm.FormCreate(Sender: TObject);
begin
  inherited;
  FConfigs := TFolderConfig.Create;
end;

procedure TFolderWorkForm.FormDestroy(Sender: TObject);
begin
  inherited;
end;

procedure TFolderWorkForm.RefreshView;

  procedure LinkPage(Node: TTreeNode);
  var
    I: Integer;
    Page: TTabSheet;
    Editor: TCustomEditorFrame;
  begin
    for I := 0 to WorkPages.PageCount - 1 do begin
      Page := WorkPages.Pages[I];
      Editor := Page.Editor;
      if Assigned(Editor) then
        if AnsiSameText(Node.LogicalName, Editor.LogicalFileName) then begin
          Node.Page := Page;
          Editor.Node := Node;
          Break;
        end;
    end;
  end;

  procedure PopulateFolder(Parent: TTreeNode; const FolderName: TFileName);
  var
    Files: TStringList;
    Name: String;
    Attribute: TFileAttribute;
    Node: TTreeNode;
  begin
    if DirectoryExists(FolderName) then begin
      Files := GetDirectories(FolderName);
      try
        for Name in Files do begin
          Attribute := TFileAttribute.CreateFolder(Name);
          Node := Navigator.Items.AddChild(Parent, Attribute.ShortName);
          Node.ImageIndex := 0;
          Node.SelectedIndex := 4;
          Node.Data := Attribute;
          PopulateFolder(Node, Name);
        end;
      finally
        Files.Free;
      end;
      Files := GetFiles(FolderName);
      try
        for Name in Files do begin
          Attribute := TFileAttribute.CreateFile(Name, FFolderName);
          Node := Navigator.Items.AddChild(Parent, Attribute.ShortName);
          Node.ImageIndex := 2;
          Node.SelectedIndex := 5;
          Node.Data := Attribute;
          LinkPage(Node);
        end;
      finally
        Files.Free;
      end;
    end;
  end;

begin
  Navigator.Items.BeginUpdate;
  try
    Navigator.Items.Clear;
    PopulateFolder(nil, FFolderName);
  finally
    Navigator.Items.EndUpdate;
  end;
end;

procedure TFolderWorkForm.Open(const FolderName: TFileName; ParentMenu: TMenuItem);
var
  OldCursor: TCursor;
  IsImage: Boolean;
  WindowMenu: TMenuItem;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    if not DirectoryExists(FolderName) then
      ForceDirectories(FolderName);
    IsImage := AnsiSameText(ExtractFileExt(FolderName), '.lst');
    FFolderName := FolderName;
    FConfigs.ReadConfig(FFolderName);
    WindowMenu := TMenuItem.Create(Self);
    WindowMenu.Caption := FolderName;
    WindowMenu.Tag := IMAGE_INDEX[IsImage];
    WindowMenu.ImageIndex := WindowMenu.Tag;
    WindowMenu.GroupIndex := 5;
    WindowMenu.OnClick := WindowClickHandler;
    ParentMenu.Add(WindowMenu);
    Caption := FFolderName;
    RefreshView;
    if Config.MonitorFolder then begin
      FDirMonitor.Directory := FolderName;
      FDirMonitor.Start
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

end.

