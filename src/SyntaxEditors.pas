unit SyntaxEditors;

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
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Dialogs, Menus, ActnList, StdActns,
  CustomEditors, SynEdit, SynEditHighlighter, SynExportHTML, SynCompletion, SynMacroRecorder, SynEditKeyCmds;

type

  { TSyntaxEditorFrame }

  TSyntaxEditorFrame = class(TCustomEditorFrame)
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
  private
    FToken: String;
    procedure CompletionExecuteZ80(Sender: TObject);
    procedure CompletionExecuteSpin(Sender: TObject);
    procedure CompletionExecuteNothing(Sender: TObject);
    procedure UpdateItems(Tokens: array of String);
  protected
    FHighlighter: TSynCustomHighlighter;
    function GetIsModified: Boolean; override;
    procedure SetIsModified(Value: Boolean); override;
    function GetInsertMode: Boolean; override;
    procedure SetInsertMode(Value: Boolean); override;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean); override;
    function GetSelectedText: String; override;
  public
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
    procedure RetrieveLabels(List: TStrings); override;
    procedure Idle; override;
    procedure RefreshConfig; override;
  end;

implementation

{$R *.lfm}

uses
  StrUtils, SynEditTypes, SynHighlighterBat, SynHighlighterZ80, SynHighlighterSpin,
  SynHighlighterVB, SynHighlighterPas, Utils, Configs;

{ TSyntaxEditorFrame }

procedure TSyntaxEditorFrame.EditorChange(Sender: TObject);
begin
  IsModified := Editor.Modified;
end;

procedure TSyntaxEditorFrame.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
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

procedure TSyntaxEditorFrame.EditorClickLink(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FindIdentifier(FToken);
end;

procedure TSyntaxEditorFrame.EditorMouseLink(Sender: TObject; X, Y: Integer;
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

procedure TSyntaxEditorFrame.CompletionExecuteZ80(Sender: TObject);
const
  TOKENS : array[0..110] of string =
   ('ADC', 'ADD', 'AND', 'BIT', 'CALL', 'CCF', 'CP', 'CPD', 'CPDR', 'CPI', 'CPIR', 'CPL',
    'DAA', 'DEC', 'DI', 'DJNZ', 'EI', 'EX', 'EXX', 'HALT', 'IM', 'IN', 'IN0', 'INC', 'IND',
    'INDR', 'INI', 'INIR', 'JP', 'JR', 'LD', 'LDD', 'LDDR', 'LDI', 'LDIR', 'MULT', 'NEG',
    'NOP', 'OR', 'OTDM', 'OTDMR', 'OTDR', 'OTIM', 'OTIMR', 'OTIR', 'OUT', 'OUT0', 'OUTD',
    'OUTI', 'POP', 'PUSH', 'RES', 'RET', 'RETI', 'RETN', 'RL', 'RLA', 'RLC', 'RLCA', 'RLD',
    'RR', 'RRA', 'RRC', 'RRCA', 'RRD', 'RST', 'SBC', 'SCF', 'SET', 'SLA', 'SLP', 'SRA',
    'SRL', 'SUB', 'TST', 'TSTIO', 'XOR', 'A', 'AF', 'B', 'C', 'BC', 'D', 'E', 'DE', 'H',
    'L', 'HL', 'I', 'IX', 'IXL', 'IXH', 'IY', 'IYL', 'IYH', 'R', 'SP', '.EQU', '.ORG',
    '.DB', '.DW', '.FILL', '.ECHO', '.TEXT', 'NZ', 'NC', 'PO', 'P', 'Z', 'PE', 'M');
begin
  UpdateItems(TOKENS);
end;

procedure TSyntaxEditorFrame.CompletionExecuteSpin(Sender: TObject);
const
  TOKENS: array[0..209] of String =
   ('true', 'false', 'posx', 'negx', 'pi', 'chipver', 'clkmode', '_clkmode', 'clkfreq',
    '_clkfreq', '_xinfreq', '_stack', '_free', 'rcfast', 'rcslow', 'xinput', 'xtal1',
    'xtal2', 'xtal3', 'pll1x', 'pll2x', 'pll4x', 'pll8x', 'pll16x', 'byte', 'word', 'long',
    'if', 'elseif', 'elseifnot', 'else', 'ifnot', 'case', 'other', 'repeat', 'from', 'to',
    'step', 'until', 'while', 'next', 'quit', 'return', 'abort', 'bytefill', 'wordfill',
    'longfill', 'bytemove', 'wordmove', 'longmove', 'lookup', 'lookupz', 'lookdown',
    'lookdownz', 'strsize', 'strcomp', 'wrbyte', 'rdbyte', 'wrword', 'rdword', 'wrlong',
    'rdlong', 'hubop', 'clkset', 'cogid', 'coginit', 'cogstop', 'locknew', 'lockret',
    'lockset', 'lockclr', 'mul', 'muls', 'enc', 'ones', 'ror', 'rol', 'shr', 'shl', 'rcr',
    'rcl', 'sar', 'rev', 'mins', 'maxs', 'min', 'max', 'movs', 'movd', 'movi', 'jmpret',
    'jmp', 'call', 'ret', 'test', 'testin', 'and', 'andn', 'or', 'xor', 'muxc', 'muxnc',
    'muxz', 'muxnz', 'add', 'sub', 'cmp', 'addabs', 'subabs', 'sumc', 'sumz', 'sumnz',
    'mov', 'neg', 'abs', 'absneg', 'negc', 'negnc', 'negz', 'negnz', 'cmps', 'cmpsx',
    'addx', 'subx', 'cmpx', 'adds', 'subs', 'addsx', 'subsx', 'cmpsub', 'djnz', 'tjnz',
    'tjz', 'waitpeq', 'waitpne', 'waitcnt', 'waitvid', 'nop', 'dira', 'ina', 'dirb',
    'inb', 'outa', 'outb', 'cnt', 'ctra', 'ctrb', 'frqa', 'frqb', 'phsa', 'phsb', 'vcfg',
    'vscl', 'par', 'spr', 'CON', 'VAR', 'PUB', 'PRI', 'OBJ', 'DAT', 'cognew', 'reboot',
    'string', 'constant', 'float', 'round', 'trunc', 'file', 'result', 'not', 'if_always',
    'if_never', 'if_e', 'if_ne', 'if_a', 'if_b', 'if_ae', 'if_be', 'if_c', 'if_nc', 'if_z',
    'if_nz', 'if_c_eq_z', 'if_c_ne_z', 'if_c_and_z', 'if_c_and_nz', 'if_nc_and_z',
    'if_nc_and_nz', 'if_c_or_z', 'if_c_or_nz', 'if_nc_or_z', 'if_nc_or_nz', 'if_z_eq_c',
    'if_z_ne_c', 'if_z_and_c', 'if_z_and_nc', 'if_nz_and_c', 'if_nz_and_nc', 'if_z_or_c',
    'if_z_or_nc', 'if_nz_or_c', 'if_nz_or_nc', 'fit', 'org', 'res', 'wc', 'wz', 'wr', 'nr');
begin
  UpdateItems(TOKENS);
end;

procedure TSyntaxEditorFrame.CompletionExecuteNothing(Sender: TObject);
begin
  Completion.ItemList.Clear;
end;

procedure TSyntaxEditorFrame.UpdateItems(Tokens: array of String);
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

function TSyntaxEditorFrame.GetIsModified: Boolean;
begin
  Result := Editor.Modified;
end;

procedure TSyntaxEditorFrame.SetIsModified(Value: Boolean);
begin
  inherited SetIsModified(Value);
  if not Value then
    Editor.Modified := False;
end;

function TSyntaxEditorFrame.GetInsertMode: Boolean;
begin
  Result := Editor.InsertMode;
end;

procedure TSyntaxEditorFrame.SetInsertMode(Value: Boolean);
begin
  inherited SetInsertMode(Value);
  Editor.InsertMode := Value;
end;

function TSyntaxEditorFrame.GetReadOnly: Boolean;
begin
  Result := Editor.ReadOnly;
end;

procedure TSyntaxEditorFrame.SetReadOnly(Value: Boolean);
begin
  inherited SetReadOnly(Value);
  Editor.ReadOnly := Value;
end;

function TSyntaxEditorFrame.GetSelectedText: String;
begin
  Result := Editor.SelText;
end;

procedure TSyntaxEditorFrame.Open(Page: TTabSheet; Node: TTreeNode);
const
  EXTENSIONS: array[0..15] of String =
   ('.asm',
    '.z80',
    '.azm',
    '.inc',
    '.lib',
    '.mac',
    '.lst',
    '.ins',
    '.bat',
    '.cmd',
    '.spin',
    '.bas',
    '.pas',
    '.pp',
    '.dpr',
    '.lpr');
var
  OldHandler: TStatusChangeEvent;
  Ext: TFileName = '';
begin
  inherited Open(Page, Node);
  if FileExists(Node.FullName) then begin
    OldHandler := Editor.OnStatusChange;
    Editor.OnStatusChange := nil;
    try
      Ext := ExtractFileExt(Node.ShortName);
      case AnsiIndexText(Ext, EXTENSIONS) of
        0..7: begin
          FHighlighter := TSynZ80Syn.Create(Self);
          Completion.OnExecute := CompletionExecuteZ80;
          end;
        8..9: begin
          FHighlighter := TSynBatSyn.Create(Self);
          Completion.OnExecute := CompletionExecuteNothing;
          end;
        10: begin
          FHighlighter := TSynSpinSyn.Create(Self);
          Completion.OnExecute := CompletionExecuteSpin;
          end;
        11: begin
          FHighlighter := TSynVBSyn.Create(Self);
          Completion.OnExecute := CompletionExecuteNothing;
          end;
        12..15: begin
          FHighlighter := TSynPasSyn.Create(Self);
          Completion.OnExecute := CompletionExecuteNothing;
          end;
      else
        FHighlighter := nil;
        Completion.OnExecute := nil;
      end;
      if Assigned(FHighlighter) then begin
        Editor.Highlighter := FHighlighter;
        ExporterHTML.Highlighter := FHighlighter;
      end;
      Editor.Lines.LoadFromFile(Node.FullName);
      IsModified := False;
    finally
      Editor.OnStatusChange := OldHandler;
    end;
    OldHandler(Editor, [scCaretX, scCaretY]);
  end;
end;

procedure TSyntaxEditorFrame.Save;
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

procedure TSyntaxEditorFrame.SaveAs(const FileName: TFileName);
begin
  if not AnsiSameText(FFileName, FileName) then begin
    FFileName := FileName;
    Save;
  end;
end;

procedure TSyntaxEditorFrame.ExportFile(const FileName: TFileName);
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
  ExporterHTML.Highlighter := Editor.Highlighter;
  ExporterHTML.ExportAll(Editor.Lines);
  ExporterHTML.SaveToFile(FileName);
end;

procedure TSyntaxEditorFrame.Revert;
begin
  Editor.Lines.LoadFromFile(FFileName);
  IsModified := False;
end;

function TSyntaxEditorFrame.Search(const Criteria: String; First, Backwards, MatchCase,
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

procedure TSyntaxEditorFrame.Replace(Criteria, Replacement: String; All, MatchCase,
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

procedure TSyntaxEditorFrame.GotoLine(LineNumber: Integer);
begin
  Editor.CaretX := 1;
  Editor.CaretY := LineNumber;
end;

function TSyntaxEditorFrame.LineNumber: Integer;
begin
  Result := Editor.CaretY;
end;

procedure TSyntaxEditorFrame.RetrieveLabels(List: TStrings);
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
      I := 1;
      Ch := Line[I];
      Result := Ch in ALPHAS;
      if Result then begin
        Width := Line.Length;
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
      List.AddObject(Token, TIntegerObject.Create(I + 1));
  end;
end;

procedure TSyntaxEditorFrame.Idle;
begin
  // Do nothing
end;

procedure TSyntaxEditorFrame.RefreshConfig;
begin
  if not AnsiSameText(Editor.Font.Name, Config.FontName) then
    Editor.Font.Name := Config.FontName;
  if Editor.Font.Size <> Config.FontSize then
    Editor.Font.Size := Config.FontSize;
  if Editor.RightEdge <> Config.RightMargin then
    Editor.RightEdge := Config.RightMargin;
end;

end.

