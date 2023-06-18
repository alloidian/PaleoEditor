unit Uploads;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  ActnList;

type

  { TUploadForm }

  TUploadMethod = (umText, umDownload, umXModem);
  TUploadForm = class(TForm)
    ActionList: TActionList;
    UploadAction: TAction;
    CancelAction: TAction;
    FileNameLabel: TLabel;
    FileNameEdit: TFileNameEdit;
    TextFileEdit: TRadioButton;
    DownloadFileEdit: TRadioButton;
    XModemFileEdit: TRadioButton;
    UploadButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure UploadActionExecute(Sender: TObject);
    procedure UploadActionUpdate(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure CancelActionUpdate(Sender: TObject);
  private
  protected
    function GetFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
    function GetMethod: TUploadMethod;
    procedure SetMethod(Value: TUploadMethod);
  public
    property FileName: TFileName read GetFileName write SetFileName;
    property Method: TUploadMethod read GetMethod write SetMethod;
  end;

function UploadFile(var FileName: TFileName; var Method: TUploadMethod): Boolean;

implementation

{$R *.lfm}

function UploadFile(var FileName: TFileName; var Method: TUploadMethod): Boolean;
var
  Dialog: TUploadForm;
begin
  Dialog := TUploadForm.Create(nil);
  try
    Dialog.FileName := FileName;
    Dialog.Method := Method;
    Result := Dialog.ShowModal = mrOk;
    if Result then begin
      FileName := Dialog.FileName;
      Method := Dialog.Method;
    end;
  finally
    Dialog.Free;
  end;
end;

{ TUploadForm }

procedure TUploadForm.FormCreate(Sender: TObject);
begin
  // Do something
end;

procedure TUploadForm.UploadActionExecute(Sender: TObject);
begin
  // Do something
end;

procedure TUploadForm.UploadActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FileExists(FileName);
end;

procedure TUploadForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TUploadForm.CancelActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := True;
end;

function TUploadForm.GetFileName: TFileName;
begin
  Result := ExcludeTrailingPathDelimiter(FileNameEdit.FileName);
end;

procedure TUploadForm.SetFileName(const Value: TFileName);
begin
  FileNameEdit.FileName := ExcludeTrailingPathDelimiter(Value);
end;

function TUploadForm.GetMethod: TUploadMethod;
begin
  if TextFileEdit.Checked then
    Result := umText
  else
    if DownloadFileEdit.Checked then
      Result := umDownload
    else
      Result := umXModem;
end;

procedure TUploadForm.SetMethod(Value: TUploadMethod);
begin
  case Value of
    umText:
      TextFileEdit.Checked := True;
    umDownload:
      DownloadFileEdit.Checked := True;
    umXModem:
      XModemFileEdit.Checked := True;
  end;
end;

end.

