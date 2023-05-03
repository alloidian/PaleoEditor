unit FolderWorks;

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
  Classes, SysUtils, Forms, Controls, Dialogs, Menus, CustomWorks, ConfigUtils, DirMonitors;

type

  { TFolderWorkForm }

  TFolderWorkForm = class(TCustomWorkForm)
    procedure FormCreate(Sender: TObject);
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

procedure TFolderWorkForm.RefreshView;

  procedure LinkPage(Node: TTreeNode);
  var
    I: Integer = 0;
    Page: TTabSheet;
    Editor: TCustomEditorFrame;
  begin
    for I := 0 to WorkPages.PageCount - 1 do begin
      Page := WorkPages.Pages[I];
      Editor := Page.Editor;
      if Assigned(Editor) then
        if AnsiSameText(Node.LogicalName, Editor.LogicalName) then begin
          Node.Page := Page;
          Editor.Node := Node;
          Break;
        end;
    end;
  end;

  procedure PopulateFolder(Parent: TTreeNode; const FolderName: TFileName);
  var
    Files: TStringList;
    Name: String = '';
    Attribute: TFileAttribute;
    Node: TTreeNode;
    List: TStringList;
  begin
    if DirectoryExists(FolderName) then begin
      Files := GetDirectories(FolderName);
      try
        for Name in Files do begin
          Attribute := TFileAttribute.CreateFolder(Name);
          Node := Navigator.Items.AddChild(Parent, Attribute.ShortName);
          Node.ImageIndex := WHITE_CLOSED_FOLDER_INDEX;
          Node.SelectedIndex := BLACK_OPENED_FOLDER_INDEX;
          Node.Data := Attribute;
          PopulateFolder(Node, Name);
        end;
      finally
        Files.Free;
      end;
      List := TStringlist.Create;
      try
        Files := GetFiles(FolderName);
        try
          for Name in Files do begin
            if List.IndexOf(Name) < 0 then begin
              Attribute := TFileAttribute.CreateDocument(Name, FFolderName);
              Node := Navigator.Items.AddChild(Parent, Attribute.ShortName);
              Node.ImageIndex := WHITE_DOCUMENT_INDEX;
              Node.SelectedIndex := BLACK_DOCUMENT_INDEX;
              Node.StateIndex := STATUS_UNATTACHED_INDEX;
              Node.Data := Attribute;
              if Node.HasExtension('.asm;.z80;.azm;.cmd') then
                PopulateChildren(Node, List);
              LinkPage(Node);
            end;
          end;
        finally
          Files.Free;
        end;
      finally
        List.Free;
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
  IsImage: Boolean = False;
  WindowMenu: TMenuItem;
begin
  Screen.BeginWaitCursor;
  try
    if not DirectoryExists(FolderName) then
      ForceDirectories(FolderName);
    inherited Open(FolderName, ParentMenu);
    FConfigs.ReadConfig(FFolderName);
  finally
    Screen.EndWaitCursor;
  end;
end;

end.

