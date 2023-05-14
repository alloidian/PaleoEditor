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
  TGotoLineEvent = procedure(Sender: TObject; LineNumber: Integer) of object;
  TSearchMode = (smNone, smSearch, smReplace, smGlobal, smLabel, smGoto);
  TSearchModes = set of TSearchMode;
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

  TSearchCache = class(TObject)
  private
    FSearchModes: TSearchModes;
    FValidActions: TValidActions;
    FCriteria: String;
    FMatchByCase: Boolean;
    FMatchWholeWordOnly: Boolean;
    FForFile: Boolean;
    FReplacement: String;
    FFilter: String;
    FFilters: String;
    FMatchDeclaration: Boolean;
    FLabels: TStringList;
    FLineNumber: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property SearchModes: TSearchModes read FSearchModes write FSearchModes;
    property ValidActions: TValidActions read FValidActions write FValidActions;
    property Criteria: String read FCriteria write FCriteria;
    property MatchByCase: Boolean read FMatchByCase write FMatchByCase;
    property MatchWholeWordOnly: Boolean read FMatchWholeWordOnly write FMatchWholeWordOnly;
    property ForFile: Boolean read FForFile write FForFile;
    property Replacement: String read FReplacement write FReplacement;
    property Filter: String read FFilter write FFilter;
    property Filters: String read FFilters write FFilters;
    property MatchDeclaration: Boolean read FMatchDeclaration write FMatchDeclaration;
    property Labels: TStringList read FLabels;
    property LineNumber: Integer read FLineNumber write FLineNumber;
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
    FFolderName: String;
    FOnSearch: TSearchEvent;
    FOnSearchAll: TSearchAllEvent;
    FOnSearchDeclaration: TSearchDeclarationEvent;
    FOnReplace: TReplaceEvent;
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
    procedure DoLabelLookup(LineNumber: Integer);
    procedure DoGoToLine(LineNumber: Integer);
    procedure SetSearchControls(Control: TControl);
    function GetSearchMode: TSearchMode;
    procedure SetSearchMode(Value: TSearchMode);
    function GetForFile: Boolean;
    procedure SetForFile(Value: Boolean);
    function GetCriteria: String;
    procedure SetCriteria(const Value: String);
    function GetReplacement: String;
    procedure SetReplacement(const Value: String);
    function GetFilter: String;
    procedure SetFilter(const Value: String);
    function GetFilters: String;
    procedure SetFilters(const Value: String);
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
    destructor Destroy; override;
    procedure ReadConfig(const FolderName: TFileName);
    procedure ReadCache(Cache: TSearchCache);
    procedure WriteCache(Cache: TSearchCache);
    property SearchMode: TSearchMode read GetSearchMode write SetSearchMode;
    property ForFile: Boolean read GetForFile write SetForFile;
    property Criteria: String read GetCriteria write SetCriteria;
    property MatchByCase: Boolean read GetMatchByCase write SetMatchByCase;
    property MatchWholeWordOnly: Boolean read GetMatchWholeWordOnly write SetMatchWholeWordOnly;
    property Replacement: String read GetReplacement write SetReplacement;
    property Filter: String read GetFilter write SetFilter;
    property Filters: String read GetFilters write SetFilters;
    property MatchDeclaration: Boolean read GetMatchDeclaration write SetMatchDeclaration;
    property LabelNumber: Integer read GetLabelNumber;
    property LineNumber: Integer read GetLineNumber write SetLineNumber;
    property ValidActions: TValidActions read FValidActions write FValidActions;
    property OnSearch: TSearchEvent read FOnSearch write FOnSearch;
    property OnSearchAll: TSearchAllEvent read FOnSearchAll write FOnSearchAll;
    property OnSearchDeclaration: TSearchDeclarationEvent read FOnSearchDeclaration write FOnSearchDeclaration;
    property OnReplace: TReplaceEvent read FOnReplace write FOnReplace;
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
  Self.ForFile := ForFile;
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
  Result := not CriteriaMatches or not CaseMatches or not WholeWordMatches or
    not ForFileMatches;
  if Result then
    SetValue(Criteria, MatchCase, MatchWholeWordOnly, ForFile);
end;

{ TSearchCache }

constructor TSearchCache.Create;
begin
  inherited Create;
  FLabels := TStringList.Create;
  Clear;
end;

destructor TSearchCache.Destroy;
begin
  FLabels.Free;
  inherited;
end;

procedure TSearchCache.Clear;
begin
  FValidActions := [];
  FSearchModes := [];
  FCriteria := EmptyStr;
  FMatchByCase := False;
  FMatchWholeWordOnly := False;
  FForFile := False;
  FReplacement := EmptyStr;
  FFilter := EmptyStr;
  FFilters := Emptystr;
  FMatchDeclaration := False;
  FLabels.Clear;
  FLineNumber := 0;
end;

{ TSearchFrame }

constructor TSearchFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValidActions := [vaCase, vaWord, vaPrevious];
  Filter := Config.SearchFiles;
  ForFile := False;
end;

destructor TSearchFrame.Destroy;
begin
  if not FFolderName.IsEmpty then begin
    Config.WriteConfig(CriteriaEdit, FFolderName);
    Config.WriteConfig(ReplacementEdit, FFolderName);
    Config.WriteConfig(FilterEdit, FFolderName);
  end;
  inherited;
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
  (Sender as TAction).Enabled := (SearchMode in [smSearch]) and not Criteria.IsEmpty and
    (vaPrevious in FValidActions);
end;

procedure TSearchFrame.SearchNextActionExecute(Sender: TObject);
var
  IsFirst: Boolean = False;
begin
  IsFirst := FCache.IsFirst(Criteria, MatchByCase, MatchWholeWordOnly, ForFile);
  DoSearch(Criteria, IsFirst, False, MatchByCase, MatchWholeWordOnly, ForFile);
end;

procedure TSearchFrame.SearchNextActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SearchMode in [smSearch, smReplace]) and not Criteria.IsEmpty;
end;

procedure TSearchFrame.ReplaceActionExecute(Sender: TObject);
begin
  DoReplace(Criteria, Replacement, False, MatchByCase, MatchWholeWordOnly);
end;

procedure TSearchFrame.ReplaceActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SearchMode in [smReplace]) and not Criteria.IsEmpty and
    not Replacement.IsEmpty;
end;

procedure TSearchFrame.ReplaceAllActionExecute(Sender: TObject);
begin
  DoReplace(Criteria, Replacement, True, MatchByCase, MatchWholeWordOnly);
end;

procedure TSearchFrame.ReplaceAllActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (SearchMode in [smReplace]) and not Criteria.IsEmpty and
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
  (Sender as TAction).Enabled := (SearchMode in [smGlobal]) and not Criteria.IsEmpty and
    not Filter.IsEmpty;
end;

procedure TSearchFrame.LabelActionExecute(Sender: TObject);
begin
  DoLabelLookup(LabelNumber);
  LabelCriteriaEdit.Text := EmptyStr;
end;

procedure TSearchFrame.LabelActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SearchMode in [smLabel];
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
  (Sender as TAction).Enabled := (SearchMode in [smGoTo]) and (LineNumber > 0);
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
  SearchMode := smNone;
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
    if I < 0 then begin
      Edit.Items.Insert(0, Text);
      while Edit.Items.Count > Edit.DropDownCount do
        Edit.Items.Delete(Edit.Items.Count - 1); end
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
  Mode: TSearchMode;
begin
  Mode := TSearchMode(Control.Tag);
  ForTextEdit.Enabled := Mode in [smSearch];
  ForFileEdit.Enabled := Mode in [smSearch];
  CriteriaEdit.Enabled := Mode in [smSearch, smReplace, smGlobal];
  CaseEdit.Enabled := CriteriaEdit.Enabled and (vaCase in FValidActions);
  WordEdit.Enabled := CriteriaEdit.Enabled and (vaWord in FValidActions);
  ReplacementEdit.Enabled := Mode in [smReplace];
  FilterEdit.Enabled := Mode in [smGlobal];
  DeclarationEdit.Enabled := FilterEdit.Enabled;
  LabelCriteriaEdit.Enabled := (Mode in [smLabel]) and (vaLabel in FValidActions) and
    (LabelCriteriaEdit.Items.Count > 0);
  LabelListEdit.Enabled := LabelCriteriaEdit.Enabled;
  LineNumberEdit.Enabled := Mode in [smGoTo];
  case Mode of
    smSearch: begin
      SearchNextButton.Action := SearchNextAction;
      SearchNextButton.Default := True;
    end;
    smReplace: begin
      SearchNextButton.Action := SearchNextAction;
      ReplaceButton.Default := True;
    end;
    smGlobal: begin
      SearchNextButton.Action := SearchAllAction;
      SearchNextButton.Default := True;
    end;
    smLabel: begin
      SearchNextButton.Action := LabelAction;
      SearchNextButton.Default := True;
    end;
    smGoto: begin
      SearchNextButton.Action := GoToAction;
      SearchNextButton.Default := True;
    end;
  end;
end;

function TSearchFrame.GetSearchMode: TSearchMode;
begin
  if SearchEdit.Checked then
    Result := smSearch
  else
    if ReplaceEdit.Checked then
      Result := smReplace
    else
      if SearchAllEdit.Checked then
        Result := smGlobal
      else
        if SearchLabelEdit.Checked then
          Result := smLabel
        else
          if GotoEdit.Checked then
            Result := smGoTo
          else
            Result := smNone;
end;

procedure TSearchFrame.SetSearchMode(Value: TSearchMode);
begin
  case Value of
    smNone: begin
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
    smSearch: begin
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
      if CriteriaEdit.CanFocus then
        CriteriaEdit.SetFocus;
      end;
    smReplace: begin
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
      if CriteriaEdit.CanFocus then
        CriteriaEdit.SetFocus;
      end;
    smGlobal: begin
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
      if CriteriaEdit.CanFocus then
        CriteriaEdit.SetFocus;
      end;
    smLabel: begin
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
      if LabelCriteriaEdit.CanFocus then
        LabelCriteriaEdit.SetFocus;
      end;
    smGoTo: begin
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
      if LineNumberEdit.CanFocus then
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
  Result := (SearchMode in [smSearch]) and ForFileEdit.Checked;
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

function TSearchFrame.GetFilters: String;
begin
  Result := FilterEdit.Items.Text;
end;

procedure TSearchFrame.SetFilters(const Value: String);
begin
  FilterEdit.Items.Text := Value;
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
begin
  I := LabelListEdit.ItemIndex;
  Result := LabelListEdit.Items.AsInteger[I];
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

procedure TSearchFrame.ReadConfig(const FolderName: TFileName);
begin
  FFolderName := FolderName;
  Config.ReadConfig(CriteriaEdit, FFolderName);
  Config.ReadConfig(ReplacementEdit, FFolderName);
  Config.ReadConfig(FilterEdit, FFolderName);
end;

procedure TSearchFrame.ReadCache(Cache: TSearchCache);
begin
  if Assigned(Cache) then begin
    Cache.Criteria := Criteria;
    Cache.MatchByCase := MatchByCase;
    Cache.MatchWholeWordOnly := MatchWholeWordOnly;
    Cache.ForFile := ForFile;
    Cache.Replacement := Replacement;
    Cache.Filter := Filter;
    Cache.Filters := Filters;
    Cache.MatchDeclaration := MatchDeclaration;
    Cache.LineNumber := LineNumber;
  end;
end;

procedure TSearchFrame.WriteCache(Cache: TSearchCache);
begin
  if Assigned(Cache) then begin
    ValidActions:= Cache.ValidActions;
    Criteria := Cache.Criteria;
    MatchByCase := Cache.MatchByCase;
    MatchWholeWordOnly := Cache.MatchWholeWordOnly;
    ForFile := Cache.ForFile;
    Replacement := Cache.Replacement;
    Filter := Cache.Filter;
    Filters := Cache.Filters;
    MatchDeclaration := Cache.MatchDeclaration;
    LabelCriteriaEdit.Items.Assign(Cache.Labels);
    LabelListEdit.Items.Assign(Cache.Labels);
    LineNumber := Cache.LineNumber;
    SearchEdit.Visible := smSearch in Cache.FSearchModes;
    SearchForPanel.Visible := SearchEdit.Visible;
    CriteriaEdit.Visible := SearchEdit.Visible;
    MatchLabel.Visible := SearchEdit.Visible;
    CaseEdit.Visible := SearchEdit.Visible;
    WordEdit.Visible := SearchEdit.Visible;
    ReplaceEdit.Visible := smReplace in Cache.FSearchModes;
    ReplacementEdit.Visible := ReplaceEdit.Visible;
    SearchAllEdit.Visible := smGlobal in Cache.FSearchModes;
    FilterEdit.Visible := SearchAllEdit.Visible;
    DeclarationEdit.Visible := SearchAllEdit.Visible;
    SearchLabelEdit.Visible := smLabel in Cache.FSearchModes;
    LabelCriteriaEdit.Visible := SearchLabelEdit.Visible;
    LabelListEdit.Visible := SearchLabelEdit.Visible;
    GoToEdit.Visible := smGoto in Cache.FSearchModes;
    LineNumberEdit.Visible := GoToEdit.Visible;
  end;
end;

end.
