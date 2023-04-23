unit FileMasks;

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
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ActnList, Buttons;

type

  { TEditFileMaskForm }

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
    DefaultAction: TAction;
    DirectoryLabel: TLabel;
    DirectoryEdit: TListBox;
    MoveUpButton: TBitBtn;
    MoveDownButton: TBitBtn;
    FolderEdit: TEdit;
    ReplaceButton: TButton;
    AddButton: TButton;
    DeleteButton: TButton;
    DefaultButton: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure DirectoryEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DirectoryEditDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DirectoryEditDragDrop(Sender, Source: TObject; X, Y: Integer);
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
    procedure DefaultActionExecute(Sender: TObject);
    procedure DefaultActionUpdate(Sender: TObject);
    procedure OKActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure DirectoryEditClick(Sender: TObject);
  private
    FDefault: String;
    FItemIndex: Integer;
    function GetFolder: String;
    procedure SetFolder(const Value: String);
    function GetSelected: String;
    procedure SetSelected(const Value: String);
    function GetPath: String;
    procedure SetPath(const Value: String);
    procedure SetDefault(const Value: String);
  protected
    property Folder: String read GetFolder write SetFolder;
    property Selected: String read GetSelected write SetSelected;
  public
    property Path: String read GetPath write SetPath;
    property Default: String read FDefault write SetDefault;
  end;

function EditFileMasks(const Caption: String; var Path: String; Default: String): Boolean;

implementation

{$R *.lfm}
{$WARN SYMBOL_PLATFORM OFF}

function EditFileMasks(const Caption: String; var Path: String; Default: String): Boolean;
var
  Editor: TEditFileMaskForm;
begin
  Editor := TEditFileMaskForm.Create(nil);
  try
    Editor.Caption := Caption;
    Editor.Path := Path;
    Editor.Default := Default;
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

procedure TEditFileMaskForm.DirectoryEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Edit: TListBox;
begin
  Edit := Sender as TListBox;
  FItemIndex := Edit.ItemAtPos(Point(X, Y), True);
  if (FItemIndex > -1) and (FItemIndex < Edit.Count) then
    Edit.BeginDrag(True)
  else begin
    Edit.BeginDrag(False);
    FItemIndex := -1;
  end;
end;

procedure TEditFileMaskForm.DirectoryEditDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Edit: TListBox;
  ItemIndex: Integer;
begin
  Edit := Sender as TListBox;
  if Sender = Source then begin
    ItemIndex := Edit.ItemAtPos(Point(X, Y), True);
    Accept := (ItemIndex > -1) and (ItemIndex < Edit.Count);
  end;
end;

procedure TEditFileMaskForm.DirectoryEditDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Edit: TListBox;
  ItemIndex: Integer;
begin
  Edit := Sender as TListBox;
  ItemIndex := Edit.ItemAtPos(Point(X, Y), True);
  if (ItemIndex > -1) and (ItemIndex < Edit.Count) and (FItemIndex > -1)then
    Edit.Items.Move(FItemIndex, ItemIndex);
end;

procedure TEditFileMaskForm.MoveUpActionExecute(Sender: TObject);
var
  I: Integer = 0;
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
  I: Integer = 0;
begin
  I := DirectoryEdit.ItemIndex;
  if (I > -1) and (I + 1 < DirectoryEdit.Items.Count) then begin
    DirectoryEdit.Items.Move(I, I + 1);
    DirectoryEdit.ItemIndex := I + 1;
  end;
end;

procedure TEditFileMaskForm.MoveDownActionUpdate(Sender: TObject);
var
  I: Integer = 0;
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
  I: Integer = 0;
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

procedure TEditFileMaskForm.DefaultActionExecute(Sender: TObject);
const
  CAPTION = 'Revert to Default';
  PROMPT  = 'Are you sure you want to revert to default value?';
begin
  if not FDefault.IsEmpty then
    if (DirectoryEdit.Items.Count = 0) or
        (MessageDlg(CAPTION, PROMPT, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes) then
      DirectoryEdit.Items.DelimitedText := FDefault;
end;

procedure TEditFileMaskForm.DefaultActionUpdate(Sender: TObject);
var
  Action: TAction;
begin
  Action := Sender as TAction;
  Action.Visible := not Default.IsEmpty;
  Action.Enabled := not AnsiSameText(DirectoryEdit.Items.DelimitedText, FDefault);
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
  I: Integer = 0;
begin
  I := DirectoryEdit.ItemIndex;
  if I > -1 then
    Result := DirectoryEdit.Items[I]
  else
    Result := EmptyStr;
end;

procedure TEditFileMaskForm.SetSelected(const Value: String);
var
  I: Integer = 0;
begin
  I := DirectoryEdit.ItemIndex;
  if I > -1 then
    DirectoryEdit.Items[I] := Value;
end;

function TEditFileMaskForm.GetPath: String;
begin
  Result := DirectoryEdit.Items.DelimitedText;
end;

procedure TEditFileMaskForm.SetPath(const Value: String);
begin
  DirectoryEdit.Items.DelimitedText := Value;
end;

procedure TEditFileMaskForm.SetDefault(const Value: String);
begin
  FDefault := Value;
end;

end.
