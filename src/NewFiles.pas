unit NewFiles;

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
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls;

type
  TNewFileForm = class(TForm)
    FolderNameLabel: TLabel;
    FolderNameEdit: TEdit;
    FilesEdit: TListBox;
    FileNameLabel: TLabel;
    FileNameEdit: TEdit;
    FileTypeEdit: TComboBox;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FileTypeEditChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private type
    TCreationType = (ctFolder, ctFile);
  private
    FCreationType: TCreationType;
    FHomeFolder: TFileName;
    FFileName: TFileName;
    procedure PopulateFolders(List: TStrings);
    procedure RefreshFiles(List: TStrings; const Filters: String);
  protected
    function GetCreationType: TCreationType;
    procedure SetCreationType(Value: TCreationType);
    function GetHomeFolder: TFileName;
    procedure SetHomeFolder(const Value: TFileName);
  public
    procedure Populate;
    property CreationType: TCreationType read GetCreationType write SetCreationType;
    property HomeFolder: TFileName read GetHomeFolder write SetHomeFolder;
    property FileName: TFileName read FFileName;
  end;

function CreateNewFile(const HomeFolder: TFileName; var FileName: TFileName): Boolean;
function CreateNewFolder(const HomeFolder: TFileName; var FolderName: TFileName): Boolean;

implementation

{$R *.lfm}

uses
  Masks;

const
  FILTERS: array[-1..7] of String =
   ('',
    '*.asm;*.azm;*.z80',
    '*.inc;*.lib',
    '*.spin',
    '*.cmd;*.bat',
    '*.bas',
    '*.pas;*.pp',
    '*.txt',
    '*.*');

function CreateNewFile(const HomeFolder: TFileName; var FileName: TFileName): Boolean;
var
  Dialog: TNewFileForm;
begin
  Dialog := TNewFileForm.Create(nil);
  try
    Dialog.CreationType := ctFile;
    Dialog.HomeFolder := HomeFolder;
    Dialog.Populate;
    Result := Dialog.ShowModal = mrOk;
    if Result then
      FileName := Dialog.FileName
    else
      FileName := EmptyStr;
  finally
    Dialog.Free;
  end;
end;

function CreateNewFolder(const HomeFolder: TFileName; var FolderName: TFileName): Boolean;
var
  Dialog: TNewFileForm;
begin
  Dialog := TNewFileForm.Create(nil);
  try
    Dialog.CreationType := ctFolder;
    Dialog.HomeFolder := HomeFolder;
    Dialog.Populate;
    Result := Dialog.ShowModal = mrOk;
    if Result then
      FolderName := Dialog.FileName
    else
      FolderName := EmptyStr;
  finally
    Dialog.Free;
  end;
end;

{ TNewFileForm }

procedure TNewFileForm.FormCreate(Sender: TObject);
begin
  FileTypeEdit.ItemIndex := 0;
end;

procedure TNewFileForm.FileTypeEditChange(Sender: TObject);
begin
  RefreshFiles(FilesEdit.Items, FILTERS[(Sender as TComboBox).ItemIndex])
end;

procedure TNewFileForm.OKButtonClick(Sender: TObject);
const
  EXTENSIONS: array[-1..7] of String =
   ('',
    '.asm',
    '.inc',
    '.spin',
    '.cmd',
    '.bas',
    '.pas',
    '.txt',
    '');
begin
  FFileName := FHomeFolder + '\' + FileNameEdit.Text;
  if (CreationType = ctFile) and (Trim(ExtractFileExt(FFileName)) = EmptyStr) then
    FFileName := ChangeFileExt(FFileName, EXTENSIONS[FileTypeEdit.ItemIndex]);
end;

procedure TNewFileForm.PopulateFolders(List: TStrings);
var
  Rec: TSearchRec;
  Error: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    if FindFirst(FHomeFolder + '\*.*', faDirectory, Rec) = 0 then begin
      repeat
        if (Rec.Attr and faDirectory) = faDirectory then
          if not ((Rec.Name = '.') or (Rec.Name = '..')) then
            List.Add('\' + Rec.Name);
        Error := FindNext(Rec);
      until Error <> 0;
      FindClose(Rec);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TNewFileForm.RefreshFiles(List: TStrings; const Filters: String);
var
  Rec: TSearchRec;
  Masks: TStringList;
  Error: Integer;

  function Matches(const FileName: TFileName): Boolean;
  var
    Mask: String;
  begin
    Result := False;
    for Mask in Masks do
      if MatchesMask(FileName, Mask) then begin
        Result := True;
        Break;
      end;
  end;

begin
  List.BeginUpdate;
  try
    List.Clear;
    Masks := TStringList.Create;
    try
      Masks.Delimiter := ';';
      Masks.DelimitedText := Filters;
      if FindFirst(FHomeFolder + '\*.*', faAnyFile - faDirectory, Rec) = 0 then begin
        repeat
          if ((Rec.Attr and faDirectory) = 0) and Matches(Rec.Name) then
            List.Add(Rec.Name);
          Error := FindNext(Rec);
        until Error <> 0;
        FindClose(Rec);
      end;
    finally
      Masks.Free;
    end;
  finally
    List.EndUpdate;
  end;
end;

function TNewFileForm.GetCreationType: TCreationType;
begin
  Result := FCreationType;
end;

procedure TNewFileForm.SetCreationType(Value: TCreationType);
begin
  FCreationType := Value;
  case Value of
    ctFolder: begin
      Caption := 'New Folder';
      FileNameLabel.Caption := 'New Folder Name:';
      FileTypeEdit.Visible := False;
      end;
    ctFile: begin
      Caption := 'New File';
      FileNameLabel.Caption := 'New File Name:';
      FileTypeEdit.Visible := True;
      end;
  end;
end;

function TNewFileForm.GetHomeFolder: TFileName;
begin
  Result := FHomeFolder;
end;

procedure TNewFileForm.SetHomeFolder(const Value: TFileName);
begin
  FHomeFolder := Value;
  FolderNameEdit.Text := Value;
end;

procedure TNewFileForm.Populate;
begin
  case CreationType of
    ctFolder:
      PopulateFolders(FilesEdit.Items);
    ctFile:
      RefreshFiles(FilesEdit.Items, FILTERS[FileTypeEdit.ItemIndex]);
  end;
end;

end.

