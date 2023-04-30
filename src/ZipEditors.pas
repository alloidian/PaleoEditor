unit ZipEditors;

{ Copyright ©2023 by Steve Garcia. All rights reserved.

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
  Classes, SysUtils, Forms, Controls, PrintersDlgs, Graphics, Dialogs, ComCtrls,
  CustomEditors, Zipper;

type

  { TZipEditorFrame }

  TZipEditorFrame = class(TCustomEditorFrame)
    Editor: TTreeView;
    Images: TImageList;
  protected
    function GetIsModified: Boolean; override;
    function GetInsertMode: Boolean; override;
    procedure Populate(List: TFullZipFileEntries);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Open(Page: TTabSheet; Node: TTreeNode); override;
    procedure Save; override;
    procedure SaveAs(const FileName: TFileName); override;
    procedure ExportFile(const FileName: TFileName); override;
    procedure PrintFile(Dialog: TPrintDialog); override;
    procedure Revert; override;
    function Search(const Criteria: String; First, Backwards, MatchCase,
      MatchWholeWordOnly: Boolean): Boolean; override;
    procedure Replace(Criteria, Replacement: String; All, MatchCase,
      MatchWholeWordOnly: Boolean); override;
    procedure GotoLine(LineNumber: Integer); override;
    function LineNumber: Integer; override;
    procedure Idle; override;
    procedure RefreshConfig; override;
  end;

implementation

{$R *.lfm}

uses
  StrUtils, Types, Utils, ConfigUtils;

{ TZipEditorFrame }

constructor TZipEditorFrame.Create(AOwner: TComponent);
begin
  FSyntax := ITEM_ZIP_SYNTAX;
  inherited Create(AOwner);
  FValidActions := [];
end;

function TZipEditorFrame.GetIsModified: Boolean;
begin
  Result := False;
end;

function TZipEditorFrame.GetInsertMode: Boolean;
begin
  Result := False;
end;

procedure TZipEditorFrame.Populate(List: TFullZipFileEntries);
var
  I: Integer;

  procedure BuildFolder(Node: TTreeNode; IsDirectory: Boolean; Entries: TStringDynArray);
  const
    ICONS: array[Boolean] of Integer = (1, 0);
  var
    Caption: String;
    Child: TTreeNode;
  begin
    if Length(Entries) > 0 then begin
      Caption := Entries[0];
      if Assigned(Node) then
        Child := Node.FindNode(Caption)
      else
        Child := Editor.Items.FindNodeWithText(Caption);
      if not Assigned(Child) then begin
        if Assigned(Node) then
          Child := Node.TreeNodes.AddChild(Node, Caption)
        else
          Child := Editor.Items.AddChild(nil, Caption);
        Child.ImageIndex := ICONS[IsDirectory];
        Child.SelectedIndex := Child.ImageIndex + 2;
      end;
      Delete(Entries, 0, 1);
      BuildFolder(Child, IsDirectory, Entries);
    end;
  end;

  procedure BuildFolder(Entry: TZipFileEntry); overload;
  const
    DELIMITER = '/';
  var
    Temp: TStringDynArray;
  begin
    Temp := SplitString(Entry.ArchiveFileName, DELIMITER);
    if Entry.IsDirectory then
      Delete(Temp, Length(Temp) - 1, 1);
    BuildFolder(nil, Entry.IsDirectory, Temp);
  end;

begin
  Editor.Items.BeginUpdate;
  try
    for I := 0 to List.Count - 1 do
      BuildFolder(List.Entries[I]);
    Editor.FullExpand;
  finally
    Editor.Items.EndUpdate;
  end;
end;

procedure TZipEditorFrame.Open(Page: TTabSheet; Node: TTreeNode);
var
  Zipper: TUnZipper;
begin
  inherited Open(Page, Node);
  if FileExists(Node.FullName) then begin
    Screen.BeginWaitCursor;
    try
      Zipper := TUnZipper.Create;
      try
        Zipper.FileName := Node.FullName;
        Zipper.Examine;
        Populate(Zipper.Entries);
      finally
        Zipper.Free;
      end;
      IsModified := False;
    finally
      Screen.EndWaitCursor;
    end;
  end;
end;

procedure TZipEditorFrame.Save;
begin
  // Do nothing
end;

procedure TZipEditorFrame.SaveAs(const FileName: TFileName);
begin
  // Do nothing
end;

procedure TZipEditorFrame.ExportFile(const FileName: TFileName);
begin
  // Do nothing
end;

procedure TZipEditorFrame.PrintFile(Dialog: TPrintDialog);
begin
  // Do nothing
end;

procedure TZipEditorFrame.Revert;
begin
  // Do nothing
end;

function TZipEditorFrame.Search(const Criteria: String; First, Backwards, MatchCase,
  MatchWholeWordOnly: Boolean): Boolean;
begin
  // Do nothing
  Result := False;
end;

procedure TZipEditorFrame.Replace(Criteria, Replacement: String; All, MatchCase,
  MatchWholeWordOnly: Boolean);
begin
  // Do nothing
end;

procedure TZipEditorFrame.GotoLine(LineNumber: Integer);
begin
  // Do nothing
end;

function TZipEditorFrame.LineNumber: Integer;
begin
  Result := 0;
end;

procedure TZipEditorFrame.Idle;
begin
  // Do nothing
end;

procedure TZipEditorFrame.RefreshConfig;
begin
  // Do nothing
end;

end.

