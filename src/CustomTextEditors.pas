unit CustomTextEditors;

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
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Dialogs, Menus, ActnList,
  StdActns, PrintersDlgs, CustomEditors, SynEdit, SynEditHighlighter, SynExportHTML,
  SynCompletion, SynMacroRecorder, SynEditKeyCmds, SynEditTypes;

type

  { TCustomTextEditorFrame }

  TCustomTextEditorFrame = class(TCustomEditorFrame)
    BookmarkImages: TImageList;
    EditorActions: TActionList;
    EditCutAction: TEditCut;
    EditCopyAction: TEditCopy;
    EditPasteAction: TEditPaste;
    EditDeleteAction: TEditDelete;
    EditSelectAllAction: TEditSelectAll;
    EditorMenu: TPopupMenu;
    EditorCutMenu: TMenuItem;
    EditorCopyMenu: TMenuItem;
    EditorPasteMenu: TMenuItem;
    EditorDeleteMenu: TMenuItem;
    EditorSelectAllMenu: TMenuItem;
    Editor: TSynEdit;
    ExporterHTML: TSynExporterHTML;
    Completion: TSynCompletion;
    Recorder: TSynMacroRecorder;
    procedure EditorChange(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorClickLink(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseLink(Sender: TObject; X, Y: Integer;
      var AllowMouseLink: Boolean);
    procedure CompletionExecute(Sender: TObject);
  private
    FToken: String;
  protected
    FHighlighter: TSynCustomHighlighter;
    procedure UpdateItems(Tokens: array of String);
    function GetIsModified: Boolean; override;
    procedure SetIsModified(Value: Boolean); override;
    function GetInsertMode: Boolean; override;
    procedure SetInsertMode(Value: Boolean); override;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean); override;
    function GetSelectedText: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    procedure RetrieveLabels(List: TStrings); override;
    procedure Idle; override;
    procedure RefreshConfig; override;
  end;

implementation

{$R *.lfm}

uses
  StrUtils, Printers, Utils, ConfigUtils, Configs;

{ TCustomTextEditorFrame }

constructor TCustomTextEditorFrame.Create(AOwner: TComponent);
begin
  if FSyntax.IsEmpty then
    FSyntax := ITEM_TEXT_SYNTAX;
  inherited Create(AOwner);
end;

destructor TCustomTextEditorFrame.Destroy;
begin
  inherited;
end;

procedure TCustomTextEditorFrame.EditorChange(Sender: TObject);
begin
  IsModified := Editor.Modified;
end;

procedure TCustomTextEditorFrame.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  Control: TSynEdit;
begin
  Control := Sender as TSynEdit;
  if scCaretX in Changes then
    Column := Control.CaretX;
  if scCaretY in Changes then
    Row := Control.CaretY;
  if scInsertMode in Changes then
    InsertMode := Control.InsertMode;
end;

procedure TCustomTextEditorFrame.EditorClickLink(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FindIdentifier(FToken);
end;

procedure TCustomTextEditorFrame.EditorMouseLink(Sender: TObject; X, Y: Integer;
  var AllowMouseLink: Boolean);
var
  P: TPoint;
  Attribute: TSynHighlighterAttributes;
begin
  AllowMouseLink := X > 1;
  if AllowMouseLink then begin
    P.SetLocation(X, Y);
    AllowMouseLink := Editor.GetHighlighterAttriAtRowCol(P, FToken, Attribute);
    if AllowMouseLink then
      AllowMouseLink := AnsiSameText(Attribute.StoredName, 'Identifier');
  end;
end;

procedure TCustomTextEditorFrame.CompletionExecute(Sender: TObject);
begin
  Completion.ItemList.Clear;
end;

procedure TCustomTextEditorFrame.UpdateItems(Tokens: array of String);
var
  Candidate: String = '';
  Token: String = '';
begin
  Completion.ItemList.Clear;
  Candidate := Completion.CurrentString;
  for Token in Tokens do
    if AnsiStartsText(Candidate, Token) then
      Completion.ItemList.Add(Token);
end;

function TCustomTextEditorFrame.GetIsModified: Boolean;
begin
  Result := Editor.Modified;
end;

procedure TCustomTextEditorFrame.SetIsModified(Value: Boolean);
begin
  inherited SetIsModified(Value);
  if not Value then
    Editor.Modified := False;
end;

function TCustomTextEditorFrame.GetInsertMode: Boolean;
begin
  Result := Editor.InsertMode;
end;

procedure TCustomTextEditorFrame.SetInsertMode(Value: Boolean);
begin
  inherited SetInsertMode(Value);
  Editor.InsertMode := Value;
end;

function TCustomTextEditorFrame.GetReadOnly: Boolean;
begin
  Result := Editor.ReadOnly;
end;

procedure TCustomTextEditorFrame.SetReadOnly(Value: Boolean);
begin
  inherited SetReadOnly(Value);
  Editor.ReadOnly := Value;
end;

function TCustomTextEditorFrame.GetSelectedText: String;
begin
  Result := Editor.SelText;
end;

procedure TCustomTextEditorFrame.Open(Page: TTabSheet; Node: TTreeNode);
var
  OldHandler: TStatusChangeEvent;
begin
  inherited Open(Page, Node);
  if FileExists(Node.FullName) then begin
    OldHandler := Editor.OnStatusChange;
    Editor.OnStatusChange := nil;
    try
      Editor.Lines.LoadFromFile(Node.FullName);
      IsModified := False;
    finally
      Editor.OnStatusChange := OldHandler;
    end;
    OldHandler(Editor, [scCaretX, scCaretY]);
  end;
end;

procedure TCustomTextEditorFrame.Save;
var
  OldHandler: TStatusChangeEvent;
begin
  OldHandler := Editor.OnStatusChange;
  Editor.OnStatusChange := nil;
  try
    if FileExists(FFileName) then
      DeleteFile(FFileName);
    Editor.Lines.SaveToFile(FFileName);
    IsModified := False;
  finally
    Editor.OnStatusChange := OldHandler;
  end;
end;

procedure TCustomTextEditorFrame.SaveAs(const FileName: TFileName);
begin
  if not AnsiSameText(FFileName, FileName) then begin
    FFileName := FileName;
    Save;
  end;
end;

procedure TCustomTextEditorFrame.ExportFile(const FileName: TFileName);
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
  ExporterHTML.Highlighter := Editor.Highlighter;
  ExporterHTML.ExportAll(Editor.Lines);
  ExporterHTML.SaveToFile(FileName);
end;

procedure TCustomTextEditorFrame.PrintFile(Dialog: TPrintDialog);
var
  YPos: Integer;
  LineHeight: Integer;
  HorizontalMargin: Integer;
  VerticalMargin: Integer;
  Line: string;
begin
  Printer.BeginDoc;
  try
    Printer.Canvas.Font.Name := 'Courier New';
    Printer.Canvas.Font.Size := 10;
    Printer.Canvas.Font.Color := clBlack;
    LineHeight := Round(1.2 * Abs(Printer.Canvas.TextHeight('I')));
    VerticalMargin:= 4 * LineHeight;
    HorizontalMargin := VerticalMargin;
    YPos := VerticalMargin;
    for Line in Editor.Lines do begin
      Printer.Canvas.TextOut(HorizontalMargin, YPos, Line);
      Inc(YPos, LineHeight);
      if YPos > Printer.PageHeight - VerticalMargin then begin
        YPos := VerticalMargin;
        Printer.NewPage;
      end;
    end;
  finally
    Printer.EndDoc;
  end;
end;

procedure TCustomTextEditorFrame.Revert;
begin
  Editor.Lines.LoadFromFile(FFileName);
  IsModified := False;
end;

function TCustomTextEditorFrame.Search(const Criteria: String; First, Backwards, MatchCase,
  MatchWholeWordOnly: Boolean): Boolean;
const
  DIRECTIONS: array[Boolean] of TSynSearchOptions = ([], [ssoBackwards]);
var
  Options: TSynSearchOptions;
begin
  Options := DIRECTIONS[Backwards];
  if MatchCase then
    Options := Options + [ssoMatchCase];
  if MatchWholeWordOnly then
    Options := Options + [ssoWholeWord];
  Result := Editor.SearchReplace(Criteria, EmptyStr, Options) > 0;
end;

procedure TCustomTextEditorFrame.Replace(Criteria, Replacement: String; All, MatchCase,
  MatchWholeWordOnly: Boolean);
const
  REPLACE_OPTIONS: array[Boolean] of TSynSearchOptions =
   ([ssoReplace, ssoPrompt],
    [ssoReplaceAll, ssoPrompt]);
var
  Options: TSynSearchOptions;
begin
  Options := REPLACE_OPTIONS[All];
  if MatchCase then
    Options := Options + [ssoMatchCase];
  if MatchWholeWordOnly then
    Options := Options + [ssoWholeWord];
  Editor.SearchReplace(Criteria, Replacement, Options);
end;

procedure TCustomTextEditorFrame.GotoLine(LineNumber: Integer);
begin
  Editor.CaretX := 1;
  Editor.CaretY := LineNumber;
  if Editor.CanFocus then
    Editor.SetFocus;
end;

function TCustomTextEditorFrame.LineNumber: Integer;
begin
  Result := Editor.CaretY;
end;

procedure TCustomTextEditorFrame.RetrieveLabels(List: TStrings);
var
  I: Integer = 0;
  Line: String = '';
  Token: String = '';

  function IsMethod(const Line: String; var Token: String): Boolean;
  const
    ALPHAS = ['A'..'Z','a'..'z'];
    NUMERALS = ['_', '0'..'9'];
  var
    Ch: Char = #0;
    I: Integer = 1;
    Width: Integer = 0;
  begin
    Token := EmptyStr;
    Result := Line.Trim.Length > 0;
    if Result then begin
      Ch := Line[I];
      Result := Ch in ALPHAS;
      if Result then begin
        Width := Line.Length - 1;
        repeat
          Token := Token + Ch;
          Inc(I);
          Ch := Line[I];
        until (I > Width) or not ((Ch in ALPHAS) or (Ch in NUMERALS));
      end;
    end;
  end;

begin
  inherited RetrieveLabels(List);
  for I := 0 to Editor.Lines.Count - 1 do begin
    Line := Editor.Lines[I];
    if IsMethod(Line, Token) then
      List.AddInteger(Token, I + 1);
  end;
end;

procedure TCustomTextEditorFrame.Idle;
begin
  // Do nothing
end;

procedure TCustomTextEditorFrame.RefreshConfig;
begin
  if not AnsiSameText(Editor.Font.Name, Config.FontName) then
    Editor.Font.Name := Config.FontName;
  if Editor.Font.Size <> Config.FontSize then
    Editor.Font.Size := Config.FontSize;
  if Editor.RightEdge <> Config.RightMargin then
    Editor.RightEdge := Config.RightMargin;
end;

end.

