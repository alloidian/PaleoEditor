unit FileMasks;

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
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ActnList, Buttons;

type
  TEditFileMaskForm = class(TForm)
    Images: TImageList;
    Actions: TActionList;
    MoveUpAction: TAction;
    MoveDownAction: TAction;
    ReplaceAction: TAction;
    AddAction: TAction;
    DeleteAction: TAction;
    OKAction: TAction;
    CancelAction: TAction;
    DirectoryLabel: TLabel;
    DirectoryEdit: TListBox;
    MoveUpButton: TSpeedButton;
    MoveDownButton: TSpeedButton;
    FolderEdit: TEdit;
    ReplaceButton: TButton;
    AddButton: TButton;
    DeleteButton: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure MoveUpActionExecute(Sender: TObject);
    procedure MoveUpActionUpdate(Sender: TObject);
    procedure MoveDownActionExecute(Sender: TObject);
    procedure MoveDownActionUpdate(Sender: TObject);
    procedure ReplaceActionExecute(Sender: TObject);
    procedure ReplaceActionUpdate(Sender: TObject);
    procedure AddActionExecute(Sender: TObject);
    procedure AddActionUpdate(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure DeleteActionUpdate(Sender: TObject);
    procedure OKActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure DirectoryEditClick(Sender: TObject);
  private
    function GetFolder: String;
    procedure SetFolder(const Value: String);
    function GetSelected: String;
    procedure SetSelected(const Value: String);
    function GetPath: TFileName;
    procedure SetPath(const Value: TFileName);
  protected
    property Folder: String read GetFolder write SetFolder;
    property Selected: String read GetSelected write SetSelected;
  public
    property Path: TFileName read GetPath write SetPath;
  end;

function EditFileMasks(var Path: TFileName): Boolean;

implementation

{$R *.lfm}
{$WARN SYMBOL_PLATFORM OFF}

function EditFileMasks(var Path: TFileName): Boolean;
var
  Editor: TEditFileMaskForm;
begin
  Editor := TEditFileMaskForm.Create(nil);
  try
    Editor.Path := Path;
    Result := Editor.ShowModal = mrOk;
    if Result then
      Path := Editor.Path;
  finally
    Editor.Free;
  end;
end;

{ TEditFileMaskForm }

procedure TEditFileMaskForm.FormCreate(Sender: TObject);
const
  DELIMITER = ';';
begin
  DirectoryEdit.Items.Delimiter := DELIMITER;
end;

procedure TEditFileMaskForm.MoveUpActionExecute(Sender: TObject);
var
  I: Integer;
begin
  I := DirectoryEdit.ItemIndex;
  if I > 0 then begin
    DirectoryEdit.Items.Move(I, I - 1);
    DirectoryEdit.ItemIndex := I - 1;
  end;
end;

procedure TEditFileMaskForm.MoveUpActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := DirectoryEdit.ItemIndex > 0;
end;

procedure TEditFileMaskForm.MoveDownActionExecute(Sender: TObject);
var
  I: Integer;
begin
  I := DirectoryEdit.ItemIndex;
  if (I > -1) and (I + 1 < DirectoryEdit.Items.Count) then begin
    DirectoryEdit.Items.Move(I, I + 1);
    DirectoryEdit.ItemIndex := I + 1;
  end;
end;

procedure TEditFileMaskForm.MoveDownActionUpdate(Sender: TObject);
var
  I: Integer;
begin
  I := DirectoryEdit.ItemIndex;
  (Sender as TAction).Enabled := (I > -1) and (I + 1 < DirectoryEdit.Items.Count);
end;

procedure TEditFileMaskForm.ReplaceActionExecute(Sender: TObject);
begin
  Selected := Folder;
end;

procedure TEditFileMaskForm.ReplaceActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not Selected.IsEmpty and not Folder.IsEmpty
    and not AnsiSameText(Selected, Folder) and (DirectoryEdit.Items.IndexOf(Folder) = -1);
end;

procedure TEditFileMaskForm.AddActionExecute(Sender: TObject);
begin
  DirectoryEdit.Items.Add(Folder);
  DirectoryEdit.ItemIndex := DirectoryEdit.Items.Count - 1;
end;

procedure TEditFileMaskForm.AddActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not Folder.IsEmpty and (DirectoryEdit.Items.IndexOf(Folder) = -1);
end;

procedure TEditFileMaskForm.DeleteActionExecute(Sender: TObject);
var
  I: Integer;
begin
  I := DirectoryEdit.ItemIndex;
  if I > -1 then begin
    DirectoryEdit.Items.Delete(DirectoryEdit.ItemIndex);
    if DirectoryEdit.Items.Count = 0 then
      DirectoryEdit.ItemIndex := -1
    else begin
      if I > 0 then
        Dec(I);
      DirectoryEdit.ItemIndex := I;
    end;
  end;
end;

procedure TEditFileMaskForm.DeleteActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := DirectoryEdit.ItemIndex > -1;
end;

procedure TEditFileMaskForm.OKActionExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TEditFileMaskForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TEditFileMaskForm.DirectoryEditClick(Sender: TObject);
begin
  Folder := Selected;
end;

function TEditFileMaskForm.GetFolder: String;
begin
  Result := FolderEdit.Text;
end;

procedure TEditFileMaskForm.SetFolder(const Value: String);
begin
  FolderEdit.Text := ExcludeTrailingPathDelimiter(Value);
end;

function TEditFileMaskForm.GetSelected: String;
var
  I: Integer;
begin
  I := DirectoryEdit.ItemIndex;
  if I > -1 then
    Result := DirectoryEdit.Items[I]
  else
    Result := EmptyStr;
end;

procedure TEditFileMaskForm.SetSelected(const Value: String);
var
  I: Integer;
begin
  I := DirectoryEdit.ItemIndex;
  if I > -1 then
    DirectoryEdit.Items[I] := Value;
end;

function TEditFileMaskForm.GetPath: TFileName;
begin
  Result := DirectoryEdit.Items.DelimitedText;
end;

procedure TEditFileMaskForm.SetPath(const Value: TFileName);
begin
  DirectoryEdit.Items.DelimitedText := Value;
end;

end.
