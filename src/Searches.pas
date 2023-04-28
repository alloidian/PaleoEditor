unit Searches;

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
  Classes, SysUtils, Forms, Controls, StdCtrls, ActnList, ExtCtrls, Menus, ListFilterEdit;

type
  TSearchEvent = procedure(Sender: TObject; const Criteria: String; First, Backwards,
    MatchCase, MatchWholeWordOnly, ForFile: Boolean; var WasFound: Boolean) of object;
  TSearchAllEvent = procedure(Sender: TObject; const Criteria, Filter: String; MatchCase,
    MatchWholeWordOnly: Boolean) of object;
  TSearchDeclarationEvent = procedure(Sender: TObject; const Criteria, Filter: String) of object;
  TReplaceEvent = procedure(Sender: TObject; const Criteria, Replacement: String; All,
    MatchCase, MatchWholeWordOnly: Boolean) of object;
  TRetrieveLabelsEvent = procedure(Sender: TObject; List: TStrings) of object;
  TGotoLineEvent = procedure(Sender: TObject; LineNumber: Integer) of object;
  TSearchBy = (sbNone, sbSearch, sbReplace, sbGlobal, sbLabel, sbGoto);
  TValidAction = (vaCase, vaWord, vaLabel, vaPrevious);
  TValidActions = set of TValidAction;

  TCache = record
    Criteria: String;
    MatchCase: Boolean;
    MatchWholeWordOnly: Boolean;
    ForFile: Boolean;
    procedure Clear;
    procedure SetValue(const Criteria: String; MatchCase, MatchWholeWordOnly,
      ForFile: Boolean);
    function IsFirst(const Criteria: String; MatchCase, MatchWholeWordOnly,
      ForFile: Boolean): Boolean;
  end;

  { TSearchFrame }

  TSearchFrame = class(TFrame)
    Actions: TActionList;
    SearchPrevAction: TAction;
    SearchNextAction: TAction;
    SearchAllAction: TAction;
    ReplaceAction: TAction;
    ReplaceAllAction: TAction;
    LabelAction: TAction;
    GoToAction: TAction;
    CloseAction: TAction;
    SearchCutMenu: TMenuItem;
    SearchCopyMenu: TMenuItem;
    SearchPasteMenu: TMenuItem;
    SearchSelectAllMenu: TMenuItem;
    SearchDeleteMenu: TMenuItem;
    ScrollBox: TScrollBox;
    SearchEdit: TRadioButton;
    SearchForPanel: TPanel;
    MatchLabel: TLabel;
    ForTextEdit: TRadioButton;
    ForFileEdit: TRadioButton;
    CriteriaEdit: TComboBox;
    CaseEdit: TCheckBox;
    WordEdit: TCheckBox;
    ReplaceEdit: TRadioButton;
    ReplacementEdit: TComboBox;
    SearchAllEdit: TRadioButton;
    FilterEdit: TComboBox;
    DeclarationEdit: TCheckBox;
    SearchLabelEdit: TRadioButton;
    LabelCriteriaEdit: TListFilterEdit;
    LabelListEdit: TListBox;
    GoToEdit: TRadioButton;
    LineNumberEdit: TEdit;
    SearchPrevButton: TButton;
    SearchNextButton: TButton;
    ReplaceButton: TButton;
    ReplaceAllButton: TButton;
    CloseButton: TButton;
    procedure SearchClick(Sender: TObject);
    procedure SearchPrevActionExecute(Sender: TObject);
    procedure SearchPrevActionUpdate(Sender: TObject);
    procedure SearchNextActionExecute(Sender: TObject);
    procedure SearchNextActionUpdate(Sender: TObject);
    procedure ReplaceActionExecute(Sender: TObject);
    procedure ReplaceActionUpdate(Sender: TObject);
    procedure ReplaceAllActionExecute(Sender: TObject);
    procedure ReplaceAllActionUpdate(Sender: TObject);
    procedure SearchAllActionExecute(Sender: TObject);
    procedure SearchAllActionUpdate(Sender: TObject);
    procedure LabelActionExecute(Sender: TObject);
    procedure LabelActionUpdate(Sender: TObject);
    procedure LabelCriteriaEditAfterFilter(Sender: TObject);
    procedure GoToActionExecute(Sender: TObject);
    procedure GoToActionUpdate(Sender: TObject);
    procedure LineNumberEditKeyPress(Sender: TObject; var Key: char);
    procedure CloseActionExecute(Sender: TObject);
  private
    FValidActions: TValidActions;
    FCache: TCache;
    FOnSearch: TSearchEvent;
    FOnSearchAll: TSearchAllEvent;
    FOnSearchDeclaration: TSearchDeclarationEvent;
    FOnReplace: TReplaceEvent;
    FOnRetrieveLabels: TRetrieveLabelsEvent;
    FOnLabelLookup: TGotoLineEvent;
    FOnGotoLine: TGotoLineEvent;
  protected
    procedure UpdateHistory;
    procedure DoSearch(const Criteria: String; First, Backwards, MatchCase, MatchWholeWordOnly,
      ForFile: Boolean);
    procedure DoSearchAll(const Criteria, Filter: String; MatchCase, WholeWordsOnly: Boolean);
    procedure DoSearchDeclaration(const Criteria, Filter: String);
    procedure DoReplace(const Criteria, Replacement: String; All, MatchCase,
      MatchWholeWordOnly: Boolean);
    procedure DoRetrieveLabels(List: TStrings);
    procedure DoLabelLookup(LineNumber: Integer);
    procedure DoGoToLine(LineNumber: Integer);
    procedure SetSearchControls(Control: TControl);
    function GetSearchBy: TSearchBy;
    procedure SetSearchBy(Value: TSearchBy);
    function GetForFile: Boolean;
    procedure SetForFile(Value: Boolean);
    function GetCriteria: String;
    procedure SetCriteria(const Value: String);
    function GetReplacement: String;
    procedure SetReplacement(const Value: String);
    function GetFilter: String;
    procedure SetFilter(const Value: String);
    function GetMatchDeclaration: Boolean;
    procedure SetMatchDeclaration(Value: Boolean);
    function GetLabelNumber: Integer;
    function GetLineNumber: Integer;
    procedure SetLineNumber(Value: Integer);
    function GetMatchByCase: Boolean;
    procedure SetMatchByCase(Value: Boolean);
    function GetMatchWholeWordOnly: Boolean;
    procedure SetMatchWholeWordOnly(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    property SearchBy: TSearchBy read GetSearchBy write SetSearchBy;
    property ForFile: Boolean read GetForFile write SetForFile;
    property Criteria: String read GetCriteria write SetCriteria;
    property MatchByCase: Boolean read GetMatchByCase write SetMatchByCase;
    property MatchWholeWordOnly: Boolean read GetMatchWholeWordOnly write SetMatchWholeWordOnly;
    property Replacement: String read GetReplacement write SetReplacement;
    property Filter: String read GetFilter write SetFilter;
    property MatchDeclaration: Boolean read GetMatchDeclaration write SetMatchDeclaration;
    property LabelNumber: Integer read GetLabelNumber;
    property LineNumber: Integer read GetLineNumber write SetLineNumber;
    property ValidActions: TValidActions read FValidActions write FValidActions;
    property OnSearch: TSearchEvent read FOnSearch write FOnSearch;
    property OnSearchAll: TSearchAllEvent read FOnSearchAll write FOnSearchAll;
    property OnSearchDeclaration: TSearchDeclarationEvent read FOnSearchDeclaration write FOnSearchDeclaration;
    property OnReplace: TReplaceEvent read FOnReplace write FOnReplace;
    property OnRetrieveLabels: TRetrieveLabelsEvent read FOnRetrieveLabels write FOnRetrieveLabels;
    property OnLabelLookup: TGotoLineEvent read FOnLabelLookup write FOnLabelLookup;
    property OnGotoLine: TGotoLineEvent read FOnGotoLine write FOnGotoLine;
  end;

implementation

{$R *.lfm}

uses
  Utils, Configs, Dialogs;

{ TCache }

procedure TCache.Clear;
begin
  Self.Criteria := EmptyStr;
  Self.MatchCase := False;
  Self.MatchWholeWordOnly := False;
  Self.ForFile := False;
end;

procedure TCache.SetValue(const Criteria: String; MatchCase, MatchWholeWordOnly,
  ForFile: Boolean);
begin
  Self.Criteria := Criteria;
  Self.MatchCase := MatchCase;
  Self.MatchWholeWordOnly := MatchWholeWordOnly;
  Self.ForFile:= ForFile;
end;

function TCache.IsFirst(const Criteria: String; MatchCase, MatchWholeWordOnly,
  ForFile: Boolean): Boolean;
var
  CriteriaMatches: Boolean = False;
  CaseMatches: Boolean = False;
  WholeWordMatches: Boolean = False;
  ForFileMatches: Boolean = False;
begin
  CriteriaMatches := AnsiSameText(Self.Criteria, Criteria);
  CaseMatches := Self.MatchCase = MatchCase;
  WholeWordMatches := Self.MatchWholeWordOnly = MatchWholeWordOnly;
  ForFileMatches := Self.ForFile = ForFile;
  Result := not CriteriaMatches or not CaseMatches or not
    WholeWordMatches or not ForFileMatches;
  if Result then
    SetValue(Criteria, MatchCase, MatchWholeWordOnly, ForFile);
end;

{ TSearchFrame }

constructor TSearchFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValidActions := [vaCase, vaWord, vaPrevious];
  Filter := Config.SearchFiles;
  ForFile := False;
end;

procedure TSearchFrame.SearchClick(Sender: TObject);
begin
  SetSearchControls(Sender as TControl);
end;

procedure TSearchFrame.SearchPrevActionExecute(Sender: TObject);
var
  IsFirst: Boolean = False;
begin
  IsFirst := FCache.IsFirst(Criteria, MatchByCase, MatchWholeWordOnly, ForFile);
  DoSearch(Criteria, IsFirst, True, MatchByCase, MatchWholeWordOnly, ForFile);
end;

procedure TSearchFrame.SearchPrevActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SearchBy in [sbSearch]) and not Criteria.IsEmpty and
    (vaPrevious in FValidActions);
end;

procedure TSearchFrame.SearchNextActionExecute(Sender: TObject);
var
  IsFirst: Boolean = False;
begin
  IsFirst := FCache.IsFirst(Criteria, MatchByCase, MatchWholeWordOnly, ForFile);
  DoSearch(Criteria, IsFirst, False, MatchByCase, MatchWholeWordOnly, ForFile)
end;

procedure TSearchFrame.SearchNextActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SearchBy in [sbSearch, sbReplace]) and not Criteria.IsEmpty
end;

procedure TSearchFrame.ReplaceActionExecute(Sender: TObject);
begin
  DoReplace(Criteria, Replacement, False, MatchByCase, MatchWholeWordOnly);
end;

procedure TSearchFrame.ReplaceActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SearchBy in [sbReplace]) and not Criteria.IsEmpty and
    not Replacement.IsEmpty;
end;

procedure TSearchFrame.ReplaceAllActionExecute(Sender: TObject);
begin
  DoReplace(Criteria, Replacement, True, MatchByCase, MatchWholeWordOnly);
end;

procedure TSearchFrame.ReplaceAllActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SearchBy in [sbReplace]) and not Criteria.IsEmpty and
    not Replacement.IsEmpty;
end;

procedure TSearchFrame.SearchAllActionExecute(Sender: TObject);
begin
  Screen.BeginWaitCursor;
  try
    if MatchDeclaration then
      DoSearchDeclaration(Criteria, Filter)
    else
      DoSearchAll(Criteria, Filter, MatchByCase, MatchWholeWordOnly);
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TSearchFrame.SearchAllActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SearchBy in [sbGlobal]) and not Criteria.IsEmpty and
    not Filter.IsEmpty;
end;

procedure TSearchFrame.LabelActionExecute(Sender: TObject);
begin
  DoLabelLookup(LabelNumber);
  LabelCriteriaEdit.Text := EmptyStr;
  TIntegerObject.FreeList(LabelCriteriaEdit.Items);
  CloseAction.Execute;
end;

procedure TSearchFrame.LabelActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SearchBy in [sbLabel]
end;

procedure TSearchFrame.LabelCriteriaEditAfterFilter(Sender: TObject);
var
  Edit: TListFilterEdit;
begin
  Edit := Sender as TListFilterEdit;
  if Edit.FilteredListbox.Items.Count > 0 then
    Edit.FilteredListbox.ItemIndex := 0
  else
    Edit.FilteredListbox.ItemIndex := -1;
end;

procedure TSearchFrame.GoToActionExecute(Sender: TObject);
begin
  DoGotoLine(LineNumber);
  CloseAction.Execute;
end;

procedure TSearchFrame.GoToActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SearchBy in [sbGoTo]) and (LineNumber > 0);
end;

procedure TSearchFrame.LineNumberEditKeyPress(Sender: TObject; var Key: char);
const
  VALID_KEYS = [#8, '0'..'9'];
begin
  if not (Key in VALID_KEYS) then
    Key := #0;
end;

procedure TSearchFrame.CloseActionExecute(Sender: TObject);
begin
  SearchBy := sbNone;
end;

procedure TSearchFrame.UpdateHistory;
var
  I: Integer = 0;
  Control: TControl;

  procedure UpdateHistory(Edit: TComboBox);
  var
    Text: String = '';
    I: Integer = 0;
  begin
    Text := Edit.Text;
    I := Edit.Items.IndexOf(Text);
    if I < 0 then
      Edit.Items.Insert(0, Text)
    else
      if I > 0 then begin
        Edit.Items.Move(I, 0);
        Edit.ItemIndex := 0;
      end;
  end;

begin
  for I := 0 to ScrollBox.ControlCount - 1 do begin
    Control := ScrollBox.Controls[I];
    if Control.Enabled and (Control is TComboBox) then
      UpdateHistory(Control as TComboBox);
  end;
end;

procedure TSearchFrame.DoSearch(const Criteria: String; First, Backwards, MatchCase,
  MatchWholeWordOnly, ForFile: Boolean);
var
  WasFound: Boolean = False;
begin
  UpdateHistory;
  if Assigned(FOnSearch) then begin
    FOnSearch(Self, Criteria, First, Backwards, MatchCase, MatchWholeWordOnly, ForFile, WasFound);
    if not WasFound then
      FCache.Clear;
  end;
end;

procedure TSearchFrame.DoSearchAll(const Criteria, Filter: String; MatchCase, WholeWordsOnly: Boolean);
begin
  UpdateHistory;
  if Assigned(FOnSearchAll) then
    FOnSearchAll(Self, Criteria, Filter, MatchCase, WholeWordsOnly);
end;

procedure TSearchFrame.DoSearchDeclaration(const Criteria, Filter: String);
begin
  UpdateHistory;
  if Assigned(FOnSearchDeclaration) then
    FOnSearchDeclaration(Self, Criteria, Filter);
end;

procedure TSearchFrame.DoReplace(const Criteria, Replacement: String; All, MatchCase,
  MatchWholeWordOnly: Boolean);
begin
  UpdateHistory;
  if Assigned(FOnReplace) then
    FOnReplace(Self, Criteria, Replacement, All, MatchCase, MatchWholeWordOnly);
end;

procedure TSearchFrame.DoRetrieveLabels(List: TStrings);
begin
  if Assigned(FOnRetrieveLabels) then
    FOnRetrieveLabels(Self, List);
end;

procedure TSearchFrame.DoLabelLookup(LineNumber: Integer);
begin
  if Assigned(FOnLabelLookup) then
    FOnLabelLookup(Self, LineNumber);
end;

procedure TSearchFrame.DoGoToLine(LineNumber: Integer);
begin
  if Assigned(FOnGotoLine) then
    FOnGotoLine(Self, LineNumber);
end;

procedure TSearchFrame.SetSearchControls(Control: TControl);
var
  Method: TSearchBy;
begin
  Method := TSearchBy(Control.Tag);
  ForTextEdit.Enabled := Method in [sbSearch];
  ForFileEdit.Enabled := Method in [sbSearch];
  CriteriaEdit.Enabled := Method in [sbSearch, sbReplace, sbGlobal];
  CaseEdit.Enabled := CriteriaEdit.Enabled and (vaCase in FValidActions);
  WordEdit.Enabled := CriteriaEdit.Enabled and (vaWord in FValidActions);
  ReplacementEdit.Enabled := Method in [sbReplace];
  FilterEdit.Enabled := Method in [sbGlobal];
  DeclarationEdit.Enabled := FilterEdit.Enabled;
  LabelCriteriaEdit.Enabled := (Method in [sbLabel]) and (vaLabel in FValidActions);
  LabelListEdit.Enabled := LabelCriteriaEdit.Enabled and (vaLabel in FValidActions);
  LineNumberEdit.Enabled := Method in [sbGoTo];
  case Method of
  sbSearch: begin
    SearchNextButton.Action := SearchNextAction;
    SearchNextButton.Default := True;
    end;
  sbReplace: begin
    SearchNextButton.Action := SearchNextAction;
    ReplaceButton.Default := True;
    end;
  sbGlobal: begin
    SearchNextButton.Action := SearchAllAction;
    SearchNextButton.Default := True;
    end;
  sbLabel: begin
    SearchNextButton.Action := LabelAction;
    SearchNextButton.Default := True;
    if LabelListEdit.Items.Count = 0 then
      DoRetrieveLabels(LabelCriteriaEdit.Items);
    end;
  sbGoto: begin
    SearchNextButton.Action := GoToAction;
    SearchNextButton.Default := True;
    end;
  end;
end;

function TSearchFrame.GetSearchBy: TSearchBy;
begin
  if SearchEdit.Checked then
    Result := sbSearch
  else
    if ReplaceEdit.Checked then
      Result := sbReplace
    else
      if SearchAllEdit.Checked then
        Result := sbGlobal
      else
        if SearchLabelEdit.Checked then
          Result := sbLabel
        else
          if GotoEdit.Checked then
            Result := sbGoTo
          else
            Result := sbNone;
end;

procedure TSearchFrame.SetSearchBy(Value: TSearchBy);
begin
  case Value of
    sbNone: begin
      Parent.Visible := False;
      SearchEdit.Checked := False;
      ReplaceEdit.Checked := False;
      SearchAllEdit.Checked := False;
      SearchLabelEdit.Checked := False;
      GotoEdit.Checked := False;
      CriteriaEdit.Enabled := False;
      ReplacementEdit.Enabled := False;
      FilterEdit.Enabled := False;
      DeclarationEdit.Enabled := False;
      LabelCriteriaEdit.Enabled := False;
      LabelListEdit.Enabled := False;
      SearchNextButton.Action := SearchNextAction;
      SearchNextButton.Default := True;
      end;
    sbSearch: begin
      Parent.Visible := True;
      SearchEdit.Checked := True;
      ReplaceEdit.Checked := False;
      SearchAllEdit.Checked := False;
      SearchLabelEdit.Checked := False;
      GotoEdit.Checked := False;
      CriteriaEdit.Enabled := True;
      ReplacementEdit.Enabled := False;
      FilterEdit.Enabled := False;
      DeclarationEdit.Enabled := False;
      LabelCriteriaEdit.Enabled := False;
      LabelListEdit.Enabled := False;
      LineNumberEdit.Enabled := False;
      SearchNextButton.Action := SearchNextAction;
      SearchNextButton.Default := True;
      CriteriaEdit.SetFocus;
      end;
    sbReplace: begin
      Parent.Visible := True;
      SearchEdit.Checked := False;
      ReplaceEdit.Checked := True;
      SearchAllEdit.Checked := False;
      SearchLabelEdit.Checked := False;
      GotoEdit.Checked := False;
      CriteriaEdit.Enabled := True;
      ReplacementEdit.Enabled := True;
      FilterEdit.Enabled := False;
      DeclarationEdit.Enabled := False;
      LabelCriteriaEdit.Enabled := False;
      LabelListEdit.Enabled := False;
      LineNumberEdit.Enabled := False;
      SearchNextButton.Action := SearchNextAction;
      ReplaceButton.Default := True;
      CriteriaEdit.SetFocus;
      end;
    sbGlobal: begin
      Parent.Visible := True;
      SearchEdit.Checked := False;
      ReplaceEdit.Checked := False;
      SearchAllEdit.Checked := True;
      SearchLabelEdit.Checked := False;
      GotoEdit.Checked := False;
      CriteriaEdit.Enabled := True;
      ReplacementEdit.Enabled := False;
      FilterEdit.Enabled := True;
      DeclarationEdit.Enabled := True;
      LabelCriteriaEdit.Enabled := False;
      LabelListEdit.Enabled := False;
      LineNumberEdit.Enabled := False;
      SearchNextButton.Action := SearchAllAction;
      SearchNextButton.Default := True;
      CriteriaEdit.SetFocus;
      end;
    sbLabel: begin
      Parent.Visible := True;
      SearchEdit.Checked := False;
      ReplaceEdit.Checked := False;
      SearchAllEdit.Checked := False;
      SearchLabelEdit.Checked := True;
      GotoEdit.Checked := False;
      CriteriaEdit.Enabled := False;
      ReplacementEdit.Enabled := False;
      FilterEdit.Enabled := False;
      DeclarationEdit.Enabled := False;
      LabelCriteriaEdit.Enabled := True;
      LabelListEdit.Enabled := True;
      LineNumberEdit.Enabled := False;
      SearchNextButton.Action := LabelAction;
      SearchNextButton.Default := True;
      if LabelListEdit.Items.Count = 0 then
        DoRetrieveLabels(LabelCriteriaEdit.Items);
      LabelCriteriaEdit.SetFocus;
      end;
    sbGoTo: begin
      Parent.Visible := True;
      SearchEdit.Checked := False;
      ReplaceEdit.Checked := False;
      SearchAllEdit.Checked := False;
      SearchLabelEdit.Checked := False;
      GotoEdit.Checked := True;
      CriteriaEdit.Enabled := False;
      ReplacementEdit.Enabled := False;
      FilterEdit.Enabled := False;
      DeclarationEdit.Enabled := False;
      LabelCriteriaEdit.Enabled := False;
      LabelListEdit.Enabled := False;
      LineNumberEdit.Enabled := True;
      SearchNextButton.Action := GotoAction;
      SearchNextButton.Default := True;
      LineNumberEdit.SetFocus;
      end;
  end;
  FCache.Clear;
  ForTextEdit.Enabled := CriteriaEdit.Enabled;
  ForFileEdit.Enabled := CriteriaEdit.Enabled;
  CaseEdit.Enabled := CriteriaEdit.Enabled and (vaCase in FValidActions);
  WordEdit.Enabled := CriteriaEdit.Enabled and (vaWord in FValidActions);
end;

function TSearchFrame.GetForFile: Boolean;
begin
  Result := (SearchBy in [sbSearch]) and ForFileEdit.Checked;
end;

procedure TSearchFrame.SetForFile(Value: Boolean);
begin
  ForFileEdit.Checked := Value;
  ForTextEdit.Checked := not Value;
end;

function TSearchFrame.GetCriteria: String;
begin
  Result := CriteriaEdit.Text;
end;

procedure TSearchFrame.SetCriteria(const Value: String);
begin
  CriteriaEdit.Text := Value;
end;

function TSearchFrame.GetReplacement: String;
begin
  Result := ReplacementEdit.Text;
end;

procedure TSearchFrame.SetReplacement(const Value: String);
begin
  ReplacementEdit.Text := Value;
end;

function TSearchFrame.GetFilter: String;
begin
  Result := FilterEdit.Text;
end;

procedure TSearchFrame.SetFilter(const Value: String);
begin
  FilterEdit.Text := Value;
end;

function TSearchFrame.GetMatchDeclaration: Boolean;
begin
  Result := DeclarationEdit.Checked;
end;

procedure TSearchFrame.SetMatchDeclaration(Value: Boolean);
begin
  DeclarationEdit.Checked := Value;
end;

function TSearchFrame.GetLabelNumber: Integer;
var
  I: Integer = 0;
  Temp: TIntegerObject;
begin
  I := LabelListEdit.ItemIndex;
  if I < 0 then
    Result := -1
  else begin
    Temp := LabelListEdit.Items.Objects[I] as TIntegerObject;
    if Assigned(Temp) then
      Result := Temp.Value
    else
      Result := -1;
  end;
end;

function TSearchFrame.GetLineNumber: Integer;
begin
  Result := StrToIntDef(LineNumberEdit.Text, 0);
end;

procedure TSearchFrame.SetLineNumber(Value: Integer);
begin
  LineNumberEdit.Text := IntToStr(Value);
end;

function TSearchFrame.GetMatchByCase: Boolean;
begin
  Result := CaseEdit.Checked;
end;

procedure TSearchFrame.SetMatchByCase(Value: Boolean);
begin
  CaseEdit.Checked := Value;
end;

function TSearchFrame.GetMatchWholeWordOnly: Boolean;
begin
  Result := WordEdit.Checked;
end;

procedure TSearchFrame.SetMatchWholeWordOnly(Value: Boolean);
begin
  WordEdit.Checked := Value;
end;

end.

