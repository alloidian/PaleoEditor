unit CustomEditors;

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
  Classes, SysUtils, Forms, Controls, ComCtrls, Searches;

type
  TGlobalSearchEvent = procedure(Sender: TObject; const Criteria, Filter: String;
    MatchCase, MatchWholeWordOnly: Boolean) of object;
  TLogMessageEvent = procedure(Sender: TObject; const Text: String) of object;
  TSearchBy = (sbNone, sbSearch, sbReplace, sbGlobal, sbGoto);

  { TCustomEditorFrame }

  TCustomEditorFrame = class(TFrame)
    StatusBar: TStatusBar;
  private
    FOnLog: TLogMessageEvent;
    FOnFindIdentifier: TGlobalSearchEvent;
    function GetRow: Integer;
    procedure SetRow(Value: Integer);
    function GetColumn: Integer;
    procedure SetColumn(Value: Integer);
  protected
    FValidActions: TValidActions;
    FFileName: TFileName;
    FPage: TTabSheet;
    FNode: TTreeNode;
    procedure Log(const Text: String);
    procedure FindIdentifier(const Token: String);
    function GetIsModified: Boolean; virtual; abstract;
    procedure SetIsModified(Value: Boolean); virtual;
    function GetLogicalName: TFileName;
    procedure SetLogicalName(const Value: TFileName);
    function GetInsertMode: Boolean; virtual; abstract;
    procedure SetInsertMode(Value: Boolean); virtual;
    function GetReadOnly: Boolean; virtual; abstract;
    procedure SetReadOnly(Value: Boolean); virtual;
    function GetSelectedText: String; virtual;
    property Row: Integer read GetRow write SetRow;
    property Column: Integer read GetColumn write SetColumn;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Open(Page: TTabSheet; Node: TTreeNode); virtual;
    procedure Save; virtual; abstract;
    procedure SaveAs(const FileName: TFileName); virtual; abstract;
    procedure ExportFile(const FileName: TFileName); virtual; abstract;
    procedure Revert; virtual; abstract;
    function Search(const Criteria: String; First, Backwards, MatchCase,
      MatchWholeWordOnly: Boolean): Boolean; virtual; abstract;
    procedure Replace(Criteria, Replacement: String; All, MatchCase,
      MatchWholeWordOnly: Boolean); virtual; abstract;
    procedure GotoLine(LineNumber: Integer); virtual; abstract;
    function LineNumber: Integer; virtual; abstract;
    procedure RetrieveLabels(List: TStrings); virtual;
    procedure Idle; virtual; abstract;
    procedure RefreshConfig; virtual; abstract;
    property IsModified: Boolean read GetIsModified write SetIsModified;
    property Node: TTreeNode read FNode write FNode;
    property LogicalName: TFileName read GetLogicalName write SetLogicalName;
    property InsertMode: Boolean read GetInsertMode write SetInsertMode;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property ValidActions: TValidActions read FValidActions;
    property SelectedText: String read GetSelectedText;
    property OnLog: TLogMessageEvent read FOnLog write FOnLog;
    property OnFindIdentifier: TGlobalSearchEvent read FOnFindIdentifier write FOnFindIdentifier;
  end;

  TTabSheetHelper = class helper for TTabSheet
  private
    function GetEditor: TCustomEditorFrame;
  public
    property Editor: TCustomEditorFrame read GetEditor;
  end;

implementation

{$R *.lfm}

uses
  StrUtils, Utils, Configs;

const
  OVERWRITE_PANEL = 0;
  READ_ONLY_PANEL = 1;
  MODIFIED_PANEL  = 2;
  ROW_PANEL       = 3;
  COL_PANEL       = 4;
  FILE_NAME_PANEL = 5;

{ TCustomEditorFrame }

constructor TCustomEditorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValidActions := [vaCase, vaWord, vaLabel, vaPrevious];
  InsertMode := True;
  ReadOnly := False;
  RefreshConfig;
end;

function TCustomEditorFrame.GetRow: Integer;
begin
  Result := StrToIntDef(StatusBar.Panels[ROW_PANEL].Text, 0);
end;

procedure TCustomEditorFrame.SetRow(Value: Integer);
begin
  StatusBar.Panels[ROW_PANEL].Text := Value.ToString;
end;

function TCustomEditorFrame.GetColumn: Integer;
begin
  Result := StrToIntDef(StatusBar.Panels[COL_PANEL].Text, 0);
end;

procedure TCustomEditorFrame.SetColumn(Value: Integer);
begin
  StatusBar.Panels[COL_PANEL].Text := Value.ToString;
end;

procedure TCustomEditorFrame.Log(const Text: String);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Text);
end;

procedure TCustomEditorFrame.FindIdentifier(const Token: String);
begin
  if Assigned(FOnFindIdentifier) then
    FOnFindIdentifier(Self, Token, Config.SearchFiles, False, True);
end;

procedure TCustomEditorFrame.SetIsModified(Value: Boolean);
const
  CAPTIONS: array[Boolean] of String = ('', 'Modified');
  TOKEN = '•';
begin
  StatusBar.Panels[MODIFIED_PANEL].Text := CAPTIONS[Value];
  if Assigned(FPage) then
    if not Value then
      FPage.Caption := AnsiReplaceText(FPage.Caption, TOKEN, EmptyStr)
    else
      if not AnsiStartsText(TOKEN, FPage.Caption) then
        FPage.Caption := TOKEN + FPage.Caption;
end;

function TCustomEditorFrame.GetLogicalName: TFileName;
begin
  Result := StatusBar.Panels[FILE_NAME_PANEL].Text;
end;

procedure TCustomEditorFrame.SetLogicalName(const Value: TFileName);
begin
  StatusBar.Panels[FILE_NAME_PANEL].Text := Value;
end;

procedure TCustomEditorFrame.SetInsertMode(Value: Boolean);
const
  CAPTIONS: array[Boolean] of String = ('Overwrite', 'Insert');
begin
  StatusBar.Panels[OVERWRITE_PANEL].Text := CAPTIONS[Value];
end;

procedure TCustomEditorFrame.SetReadOnly(Value: Boolean);
const
  CAPTIONS: array[Boolean] of String = ('', 'Read-Only');
begin
  StatusBar.Panels[READ_ONLY_PANEL].Text := CAPTIONS[Value];
end;

function TCustomEditorFrame.GetSelectedText: String;
begin
  Result := EmptyStr;
end;

procedure TCustomEditorFrame.Open(Page: TTabSheet; Node: TTreeNode);
begin
  FFileName := Node.FullName;
  FPage := Page;
  FNode := Node;
  LogicalName := Node.LogicalName;
end;

procedure TCustomEditorFrame.RetrieveLabels(List: TStrings);
begin
  List.Clear;
end;

{ TTreeNodeHelper }

function TTabSheetHelper.GetEditor: TCustomEditorFrame;
var
  I: Integer;
  Control: TControl;
begin
  Result := nil;
  for I := 0 to ControlCount - 1 do begin
    Control := Controls[I];
    if Control is TCustomEditorFrame then begin
      Result := Control as TCustomEditorFrame;
      Break;
    end;
  end;
end;

end.

