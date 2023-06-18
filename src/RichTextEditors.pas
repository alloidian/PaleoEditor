unit RichTextEditors;

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
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Dialogs, RichMemo, PrintersDlgs,
  CustomEditors;

type

  { TRichTextEditorFrame }

  TRichTextEditorFrame = class(TCustomEditorFrame)
    Editor: TRichMemo;
    procedure EditorChange(Sender: TObject);
  private
  protected
    function GetIsModified: Boolean; override;
    procedure SetIsModified(Value: Boolean); override;
    function GetInsertMode: Boolean; override;
    procedure SetInsertMode(Value: Boolean); override;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean); override;
    function GetSelectedText: String; override;
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
  Printers, Utils, Searches, ConfigUtils, Configs;

{ TRichTextEditorFrame }

constructor TRichTextEditorFrame.Create(AOwner: TComponent);
begin
  FSyntax := ITEM_RTF_SYNTAX;
  inherited Create(AOwner);
  SearchCache.SearchModes := [smSearch];
  SearchCache.ValidActions := [vaCase, vaWord, vaPrevious];
end;

procedure TRichTextEditorFrame.EditorChange(Sender: TObject);
begin
  IsModified := True;
end;

function TRichTextEditorFrame.GetIsModified: Boolean;
begin
  Result := Editor.Modified;
end;

procedure TRichTextEditorFrame.SetIsModified(Value: Boolean);
begin
  inherited SetIsModified(Value);
  if not Value then
    Editor.Modified := False;
end;

function TRichTextEditorFrame.GetInsertMode: Boolean;
begin
  Result := True;
end;

procedure TRichTextEditorFrame.SetInsertMode(Value: Boolean);
begin
  inherited SetInsertMode(Value);
end;

function TRichTextEditorFrame.GetReadOnly: Boolean;
begin
  Result := Editor.ReadOnly;
end;

procedure TRichTextEditorFrame.SetReadOnly(Value: Boolean);
begin
  inherited SetReadOnly(Value);
end;

function TRichTextEditorFrame.GetSelectedText: String;
begin
  Result := inherited GetSelectedText;
end;

procedure TRichTextEditorFrame.Open(Page: TTabSheet; Node: TTreeNode);
var
  Stream: TFileStream;
begin
  inherited Open(Page, Node);
  if FileExists(Node.FullName) then begin
    Stream := TFileStream.Create(Node.FullName, fmOpenRead);
    try
      Editor.LoadRichText(Stream);
    finally
      Stream.Free;
    end;
    IsModified := False;
  end;
end;

procedure TRichTextEditorFrame.Save;
var
  Stream: TFileStream;
begin
  if FileExists(FFileName) then
    DeleteFile(FFileName);
  Stream := TFileStream.Create(FFileName, fmOpenWrite);
  try
    Editor.SaveRichText(Stream);
  finally
    Stream.Free;
  end;
  IsModified := False;
end;

procedure TRichTextEditorFrame.SaveAs(const FileName: TFileName);
begin
  if not AnsiSameText(FFileName, FileName) then begin
    FFileName := FileName;
    Save;
  end;
end;

procedure TRichTextEditorFrame.ExportFile(const FileName: TFileName);
begin
  ShowMessage(UNIMPLEMENTED_PROMPT);
end;

procedure TRichTextEditorFrame.PrintFile(Dialog: TPrintDialog);
begin
  ShowMessage(UNIMPLEMENTED_PROMPT);
end;

procedure TRichTextEditorFrame.Revert;
var
  Stream: TFileStream;
begin
  if FileExists(FFileName) then begin
    Stream := TFileStream.Create(FFileName, fmOpenRead);
    try
      Editor.LoadRichText(Stream);
    finally
      Stream.Free;
    end;
    IsModified := False;
  end;
end;

function TRichTextEditorFrame.Search(const Criteria: String; First, Backwards,
  MatchCase, MatchWholeWordOnly: Boolean): Boolean;
var
  Options: TSearchOptions = [];
  Start: Integer;
  Length: Integer = 0;
begin
  if First then
    if Backwards then
      Start := Editor.SelStart
    else
      Start := 0
  else
    if Backwards then
      Start := Editor.SelStart - 1
    else
      Start := Editor.SelStart + Editor.SelLength;
  if Backwards then
    Include(Options, soBackward);
  if MatchCase then
    Include(Options, soMatchCase);
  if MatchWholeWordOnly then
    Include(Options, soWholeWord);
  Result :=  Editor.Search(Criteria, Start, Editor.Lines.Text.Length, Options, Start, Length);
  if Result then begin
    Editor.SelStart := Start;
    Editor.SelLength := Length;
  end;
end;

procedure TRichTextEditorFrame.Replace(Criteria, Replacement: String; All,
  MatchCase, MatchWholeWordOnly: Boolean);
begin
  // Do nothing
end;

procedure TRichTextEditorFrame.GotoLine(LineNumber: Integer);
begin
  Editor.CaretPos := Point(0, LineNumber - 1);
  if Editor.CanFocus then
    Editor.SetFocus;
end;

function TRichTextEditorFrame.LineNumber: Integer;
begin
  Result := Editor.CaretPos.Y + 1;
end;

procedure TRichTextEditorFrame.Idle;
begin
  Row := Editor.CaretPos.Y + 1;
  Column := Editor.CaretPos.X + 1;
end;

procedure TRichTextEditorFrame.RefreshConfig;
begin
  if not AnsiSameText(Editor.Font.Name, Config.FontName) then
    Editor.Font.Name := Config.FontName;
  if Editor.Font.Size <> Config.FontSize then
    Editor.Font.Size := Config.FontSize;
end;

end.

