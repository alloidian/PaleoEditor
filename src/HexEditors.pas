unit HexEditors;

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
  Classes, SysUtils, Controls, Dialogs, CustomEditors, MPHexEditorEx, ComCtrls;

type

  { THexEditorFrame }

  THexEditorFrame = class(TCustomEditorFrame)
    Editor: TMPHexEditorEx;
    procedure EditorChange(Sender: TObject);
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

var
  HexEditorFrame: THexEditorFrame;

implementation

{$R *.lfm}

uses
  MPHexEditor, Utils, Searches, Configs;

{ THexEditorFrame }

constructor THexEditorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValidActions := [vaCase, vaWord];
end;

procedure THexEditorFrame.EditorChange(Sender: TObject);
begin
  IsModified := True;
end;

function THexEditorFrame.GetIsModified: Boolean;
begin
  Result := Editor.Modified;
end;

procedure THexEditorFrame.SetIsModified(Value: Boolean);
begin
  inherited SetIsModified(Value);
  if not Value then
    Editor.Modified := False;
end;

function THexEditorFrame.GetInsertMode: Boolean;
begin
  Result := Editor.InsertMode;
end;

procedure THexEditorFrame.SetInsertMode(Value: Boolean);
begin
  inherited SetInsertMode(Value);
  Editor.InsertMode := Value;
end;

function THexEditorFrame.GetReadOnly: Boolean;
begin
  Result := Editor.ReadOnlyFile;
end;

procedure THexEditorFrame.SetReadOnly(Value: Boolean);
begin
  inherited SetReadOnly(Value);
  Editor.ReadOnlyFile := Value;
end;

function THexEditorFrame.GetSelectedText: String;
begin
  Result := Editor.SelectionAsText;
end;

procedure THexEditorFrame.Open(Page: TTabSheet; Node: TTreeNode);
begin
  inherited Open(Page, Node);
  Editor.LoadFromFile(Node.FullName);
  IsModified := False;
end;

procedure THexEditorFrame.Save;
begin
  if FileExists(FFileName) then
    DeleteFile(FFileName);
  Editor.SaveToFile(FFileName);
  IsModified := False;
end;

procedure THexEditorFrame.SaveAs(const FileName: TFileName);
begin
  if not AnsiSameText(FFileName, FileName) then begin
    FFileName := FileName;
    Save;
  end;
end;

procedure THexEditorFrame.ExportFile(const FileName: TFileName);
begin
  ShowMessage(UNIMPLEMENTED_PROMPT);
end;

procedure THexEditorFrame.Revert;
begin
  Editor.LoadFromFile(FFileName);
  IsModified := False;
end;

function THexEditorFrame.Search(const Criteria: String; First, Backwards, MatchCase,
  MatchWholeWordOnly: Boolean): Boolean;
var
  Position: Integer;
  Found: Integer;
  Text: String;
begin
  Result := False;
  if First then begin
    // Do nothing
  end;
  if not Criteria.IsEmpty then begin
    Position := Max(0, Editor.GetCursorPos);
    if (Criteria.Length = 1) and (Editor.SelCount = 1) then
      Inc(Position);
    if Position >= Editor.DataSize then
      Found := -1
    else begin
      Text := Editor.PrepareFindReplaceData(Criteria, not MatchCase, True);
      if (Text.Length mod Editor.BytesPerUnit) <> 0 then begin
        Log('Size of data to search for must be a multiple of Bytes per unit');
        Exit;
      end;
      Found := Editor.Find(PChar(Text), Text.Length, Position, Editor.DataSize - 1, not MatchCase);
    end;
    Result := Found <> -1;
    if not Result then
      Log('No match found.')
    else begin
      Editor.SelStart := Found + Text.Length - 1;
      Editor.SelEnd := Found;
    end;
  end;
end;

procedure THexEditorFrame.Replace(Criteria, Replacement: String; All, MatchCase,
  MatchWholeWordOnly: Boolean);
var
  Position: Integer;
  Count: Integer;
begin
  Criteria := Editor.PrepareFindReplaceData(Criteria, not MatchCase, True);
  Replacement := Editor.PrepareFindReplaceData(Replacement, False, True);
  if All then
    Position := 0
  else
    Position := Max(0, Editor.GetCursorPos);
  Count := 0;
  if (Length(Criteria) mod Editor.BytesPerUnit) <> 0 then begin
    Log('Size of data to search for must be a multiple of Bytes per unit');
    Exit;
  end;
  if (Length(Replacement) mod Editor.BytesPerUnit) <> 0 then begin
    Log('Size of replacement data must be a multiple of Bytes per unit');
    Exit;
  end;
  repeat
    Position := Editor.Find(PChar(Criteria), Length(Criteria), Position, Editor.DataSize -1, not MatchCase);
    if Position = -1 then
      Break;
    Inc(Count);
    Editor.SelStart := Position;
    Editor.SelEnd := Position + Length(Criteria) - 1;
    if not Replacement.IsEmpty then
      Editor.ReplaceSelection(PChar(Replacement), Length(Replacement), EmptyStr, False)
    else
      Editor.DeleteSelection;
    Position := Position + Length(Replacement);
  until not All;
  if Count = 0 then
    Log('No match found.')
  else
    if All then
      Log(Format('%d replacemets.', [Count]));
end;

procedure THexEditorFrame.GotoLine(LineNumber: Integer);
begin
  Editor.Row := LineNumber + 1;
end;

function THexEditorFrame.LineNumber: Integer;
begin
  Result := Editor.Row - 1;
end;

procedure THexEditorFrame.Idle;
var
  Pos: Integer;
begin
  InsertMode := Editor.InsertMode;
  Pos := Editor.GetCursorPos;
  Row := Pos mod 16 + 1;
  Column := Pos div 16 + 1;
end;

procedure THexEditorFrame.RefreshConfig;
begin
  if not AnsiSameText(Editor.Font.Name, Config.FontName) then
    Editor.Font.Name := Config.FontName;
  if Editor.Font.Size <> Config.FontSize then
    Editor.Font.Size := Config.FontSize;
end;

end.

