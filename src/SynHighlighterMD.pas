unit SynHighlighterMD;

(**

  This module contains a SynEdit highlighter for the Markdown language.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 Nov 2018

**)

{ $ I SynDefines.inc}
{$IFDEF FPC}
  {$MODE OBJFPC}
  {$DEFINE SYN_LAZARUS}
{$ENDIF}
{$DEFINE SYNEDIT_INCLUDE}
{$IFdef MSWindows}
  {$DEFINE SYN_WIN32}
{$ENDIF}
{$H+}
{.$DEFINE SYN_DEVELOPMENT_CHECKS}
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  {$R+,Q+,S+,T+}
{$ENDIF}

interface

uses
  Graphics, SynEditHighlighter, SysUtils, Classes;

type
  TMDTokenKind = (tkMDLineComment, tkMDSubheading, tkMDItalic, tkMDBold, tkMDMonospace,
    tkMDBullet, tkMDNumber, tkMDCode, tkMDSpace, tkMDSymbol, tkMDText);

  TMDRangeState = (rsMDLineComment, rsMDSubHeading, rsMDItalic, rsMDBold, rsMDMonospace,
    rsMDBullet, rsMDNumber, rsMDText, rsMDCode);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function(const Index: Integer): TMDTokenKind of Object;

  { TSynMDSyn }

  TSynMDSyn = class(TSynCustomHighlighter)
  private
    FLine: PChar;
    FLineRef: String;
    FLineNumber: Integer;
    FLineLen: Integer;
    FRange: TMDRangeState;
    FTokenID: TMDTokenKind;
    FTokenPos: Integer;
    FAttributeChar: WideChar;
    FIdentFuncTable: array[0..1] of TIdentFuncTableFunc;
    FTokenAttri: array[Low(TMDTokenKind)..High(TMDTokenKind)] of TSynHighlighterAttributes;
    procedure EscapeProc;
    function IsLineEnd(_Run: Integer): Boolean;
    function MatchingAttribute(const iIndex: Integer): Boolean;
    procedure MDCodeOpenProc;
    procedure MDCodeProc;
   protected
    Run: LongInt;
    function FuncKeyword(const Index: Integer): TMDTokenKind;
    function AltFunc(const Index: Integer): TMDTokenKind;
    procedure InitIdent;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure MDCommentSubHeadingOpenProc;
    procedure MDGenericLineProc(const eTokenID: TMDTokenKind);
    procedure MDItalicOpenProc;
    procedure MDItalicProc;
    procedure MDBoldBulletOpenProc;
    procedure MDBoldProc;
    procedure MDNumberOpenProc;
    procedure MDMonospaceOpenProc;
    procedure MDMonospaceProc;
    procedure MDTextAttibuteOpenProc;
    procedure SymbolProc;
    procedure TextProc;
    function GetSampleSource: String; Override;
    function IsFilterStored: Boolean; Override;
    procedure AddAndUpdateAttributes(const _Attribute: TSynHighlighterAttributes; const ForeColour,
      BackColour: TColor; const FontStyle: TFontStyles);
    function GetAttri(const eTokenKind: TMDTokenKind): TSynHighlighterAttributes;
    procedure SetAttri(const eTokenKind: TMDTokenKind; const Attri: TSynHighlighterAttributes);
    function IsFirstCharOnLine(const iIndex: Integer): Boolean;
  public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function GetTokenPos: Integer; override;
    function GetToken: String; override;
    class function GetFriendlyLanguageName: String;
    class function GetLanguageName: String; Override;
    function GetRange: Pointer; Override;
    procedure ResetRange; Override;
    procedure SetRange(Value: Pointer); Override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; Override;
    function GetEol: Boolean; Override;
    function GetTokenID: TMDTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; Override;
    function GetTokenKind: Integer; Override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
    function IsIdentChar(AChar: WideChar): Boolean;
    procedure Next; Override;
  published
    property Comment: TSynHighlighterAttributes Index tkMDLineComment Read GetAttri Write SetAttri;
    property SubHeading: TSynHighlighterAttributes Index tkMDSubheading Read GetAttri Write SetAttri;
    property Italic: TSynHighlighterAttributes Index tkMDItalic Read GetAttri Write SetAttri;
    property Bold: TSynHighlighterAttributes Index tkMDBold Read GetAttri Write SetAttri;
    property Monospace: TSynHighlighterAttributes Index tkMDMonospace Read GetAttri Write SetAttri;
    property Bullet: TSynHighlighterAttributes Index tkMDBullet Read GetAttri Write SetAttri;
    property Number: TSynHighlighterAttributes Index tkMDNumber Read GetAttri Write SetAttri;
    property Space: TSynHighlighterAttributes Index tkMDSpace Read GetAttri Write SetAttri;
    property Symbols: TSynHighlighterAttributes Index tkMDSymbol Read GetAttri Write SetAttri;
    property Text: TSynHighlighterAttributes Index tkMDText Read GetAttri Write SetAttri;
  end;

const
  SYNS_FilterMarkdown = 'Markdown Files (*.md)|*.md';

Implementation

Uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  SYNS_LangMarkdown = 'Markdown';
  SYNS_AttrComment = 'Comment';
  SYNS_AttrSubheading = 'Subheading';
  SYNS_AttrItalic = 'Italic';
  SYNS_AttrBold = 'Bold';
  SYNS_AttrMonospace = 'Monospace';
  SYNS_AttrBullet = 'Bullet';
  SYNS_AttrNumber = 'Number';
  SYNS_AttrText = 'Text';
  SYNS_AttrCode = 'Code';

const
  KeyIndices: array[0..1] of Integer = (0, -1);
  acMDCommentChars = ['#'];
  acIdentifier = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  acSpace = [#32, #9];
  acSymbols = [#32..#128] - acSpace - acIdentifier - acMDCommentChars;
  acNumberedLists = ['.', ')'];

{$Q-}

procedure TSynMDSyn.AddAndUpdateAttributes(const _Attribute: TSynHighlighterAttributes; const ForeColour,
  BackColour: TColor; const FontStyle: TFontStyles);
begin
  _Attribute.Foreground := ForeColour;
  _Attribute.Background := BackColour;
  _Attribute.Style := FontStyle;
  AddAttribute(_Attribute);
end;

function TSynMDSyn.AltFunc(const Index: Integer): TMDTokenKind;
begin
  Result := tkMDText;
end;

constructor TSynMDSyn.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FTokenAttri[tkMDLineComment] := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_AttrComment);
  AddAndUpdateAttributes(FTokenAttri[tkMDLineComment], clNone, clNone, [fsItalic]);
  FTokenAttri[tkMDSubHeading] := TSynHighlighterAttributes.Create(SYNS_AttrSubheading, SYNS_AttrSubheading);
  AddAndUpdateAttributes(FTokenAttri[tkMDSubHeading], clNone, clNone, [fsBold, fsUnderline]);
  FTokenAttri[tkMDItalic] := TSynHighlighterAttributes.Create(SYNS_AttrItalic, SYNS_AttrItalic);
  AddAndUpdateAttributes(FTokenAttri[tkMDItalic], clNone, clNone, [fsItalic]);
  FTokenAttri[tkMDBold] := TSynHighlighterAttributes.Create(SYNS_AttrBold, SYNS_AttrBold);
  AddAndUpdateAttributes(FTokenAttri[tkMDBold], clNone, clNone, [fsBold]);
  FTokenAttri[tkMDMonospace] := TSynHighlighterAttributes.Create(SYNS_AttrMonospace, SYNS_AttrMonospace);
  AddAndUpdateAttributes(FTokenAttri[tkMDMonospace], clNone, clNone, []);
  FTokenAttri[tkMDBullet] := TSynHighlighterAttributes.Create(SYNS_AttrBullet, SYNS_AttrBullet);
  AddAndUpdateAttributes(FTokenAttri[tkMDBullet], clNone, clNone, []);
  FTokenAttri[tkMDNumber] := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_AttrNumber);
  AddAndUpdateAttributes(FTokenAttri[tkMDNumber], clNone, clNone, []);
  FTokenAttri[tkMDSymbol] := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_AttrSymbol);
  AddAndUpdateAttributes(FTokenAttri[tkMDSymbol], clMaroon, clNone, []);
  FTokenAttri[tkMDText] := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_AttrText);
  AddAndUpdateAttributes(FTokenAttri[tkMDText], clNone, clNone, []);
  FTokenAttri[tkMDSpace] := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_AttrSpace);
  AddAndUpdateAttributes(FTokenAttri[tkMDSpace], clNone, clNone, []);
  FTokenAttri[tkMDCode] := TSynHighlighterAttributes.Create(SYNS_AttrCode, SYNS_AttrCode);
  AddAndUpdateAttributes(FTokenAttri[tkMDCode], clNone, clNone, []);
  SetAttributesOnChange(@DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterMarkdown;
  FRange := rsMDText;
end;

procedure TSynMDSyn.CRProc;
begin
  if not (FRange In [rsMDItalic, rsMDBold, rsMDMonospace]) then begin
    FTokenID := tkMDSpace;
    FRange := rsMDText;
  end;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

destructor TSynMDSyn.Destroy;
begin
  Inherited Destroy;
end;

function TSynMDSyn.IsLineEnd(_Run: Integer): Boolean;
begin
  Result := (_Run >= FLineLen) or (FLine[_Run] = #10) or (FLine[_Run] = #13);
end;

function TSynMDSyn.FuncKeyword(const Index: Integer): TMDTokenKind;
begin
  Result := tkMDText;
end;

function TSynMDSyn.GetAttri(const eTokenKind: TMDTokenKind): TSynHighlighterAttributes;
begin
  Result := FTokenAttri[eTokenKind];
end;

function TSynMDSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FTokenAttri[tkMDLineComment];
    SYN_ATTR_WHITESPACE: Result := FTokenAttri[tkMDSpace];
  else
    Result := Nil;
  end;
end;

function TSynMDSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynMDSyn.GetFriendlyLanguageName: String;
begin
  Result := SYNS_LangMarkdown;
end;

class function TSynMDSyn.GetLanguageName: String;
begin
  Result := SYNS_LangMarkdown;
end;

function TSynMDSyn.GetRange: Pointer;
begin
  {$PUSH}{$HINTS OFF}
  Result := Pointer(PtrInt(FRange));
  {$POP}
end;

function TSynMDSyn.GetSampleSource: String;
const
  VALUE =
    '#'#13#10 +
    '# This is a comment header for the Markdown file'#13#10 +
    '#'#13#10 +
    ''#13#10 +
    'Heading'#13#10 +
    '======='#13#10 +
    ''#13#10 +
    '## Sub-heading'#13#10 +
    ''#13#10 +
    'Paragraphs are separated by a blank line.'#13#10 +
    ''#13#10 +
    'Two spaces at the end of a line leave a line break.'#13#10 +
    ''#13#10 +
    'Text attributes _italic_, **bold**, `monospace`.'#13#10 +
    ''#13#10 +
    'Horizontal rule:'#13#10 +
    ''#13#10 +
    '---'#13#10 +
    ''#13#10 +
    'Bullet list:'#13#10 +
    ''#13#10 +
    '  * apples'#13#10 +
    '  * oranges'#13#10 +
    '  * pears'#13#10 +
    ''#13#10 +
    'Numbered list:'#13#10 +
    ''#13#10 +
    '  1. wash'#13#10 +
    '  2. rinse'#13#10 +
    '  3. repeat'#13#10 +
    ''#13#10 +
    'A [link](http://example.com).'#13#10 +
    ''#13#10 +
    '![Image](Image_icon.png)'#13#10 +
    ''#13#10 +
    '> Markdown uses email-style > characters for blockquoting.'#13#10 +
    ''#13#10 +
    'Inline <abbr title="Hypertext Markup Language">HTML</abbr> is supported.';
begin
  Result := VALUE;
end;

function TSynMDSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FTokenAttri[GetTokenID];
end;

function TSynMDSyn.GetTokenID: TMDTokenKind;
begin
  Result := FTokenID;
end;

function TSynMDSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynMDSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart  := FLine + FTokenPos;
end;

procedure TSynMDSyn.InitIdent;

Var
  i: Integer;
begin
  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if KeyIndices[i] = - 1 then
      FIdentFuncTable[i] := @AltFunc;
  FIdentFuncTable[0] := @FuncKeyword;
end;

function TSynMDSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterMarkdown;
end;

function TSynMDSyn.IsFirstCharOnLine(const iIndex: Integer): Boolean;

Var
  iChar : Integer;
begin
  Result := True;
  for iChar := Pred(iIndex) downto 1 do
    if not CharInSet(FLine[iChar], [#32, #9]) then begin
      Result := False;
      Break;
    end;
end;

function TSynMDSyn.MatchingAttribute(const iIndex: Integer): Boolean;
Var
  iChar: Integer;
begin
  Result := False;
  iChar := iIndex;
  repeat
    Inc(iChar);
  until IsLineEnd(iChar) or ((FLine[iChar] = FAttributeChar) and (FLine[pred(iChar)] <> '\'));
  if (FLine[iChar] = FAttributeChar) then
    result := not ( (FLine[pred(iChar)] in acSpace) and not (FLine[succ(iChar)] in acSpace));
end;

function TSynMDSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '!', '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
  else
    Result := False;
  end;
end;

procedure TSynMDSyn.LFProc;
begin
  if not (FRange In [rsMDItalic, rsMDBold, rsMDMonospace]) then begin
    FTokenID := tkMDSpace;
    FRange := rsMDText;
  end;
  Inc(Run);
end;

procedure TSynMDSyn.MDTextAttibuteOpenProc;
begin
  FAttributeChar := FLine[Run];
  Inc(Run);
  if FLine[Run] = FAttributeChar then begin
    Inc(Run);
    fRange := rsMDBold;
    fTokenID := tkMDBold; end
  else
  if IsFirstCharOnLine(Pred(Run)) and (FLine[Run] in acSpace) then begin
    fTokenID := tkMDBullet;
    FRange := rsMDBullet; end
  else begin
    if MatchingAttribute(Run) then begin
    FTokenID := tkMDItalic;
    fRange := rsMDItalic; end
  else
    FTokenID := tkMDSymbol;
  end;
end;

procedure TSynMDSyn.MDCodeOpenProc;
begin
  FAttributeChar := FLine[Run];
  Inc(Run);
  if (FLine[Run] = FAttributeChar) and (FLine[Run+1] = FAttributeChar) then begin
    Inc(Run, 2);
    fRange := rsMDCode;
    fTokenID := tkMDCode;
  end;
end;

procedure TSynMDSyn.MDBoldBulletOpenProc;
begin
  Inc(Run);
  if IsFirstCharOnLine(Pred(Run)) then begin
    fTokenID := tkMDBullet;
    FRange := rsMDBullet; end
  else
    FTokenID := tkMDSymbol;
end;

procedure TSynMDSyn.MDBoldProc;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkMDBold;
    repeat
      if (FLine[Run] = FAttributeChar) and (FLine[Run - 1] = FAttributeChar) then
        begin
          inc(Run);
          fRange := rsMDText;
          Break;
        end;
      if not IsLineEnd(Run) then
        inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynMDSyn.MDCodeProc;
begin
  fTokenID := tkMDCode;
  repeat
    if (FLine[Run] = FAttributeChar) and (FLine[Run + 1] = FAttributeChar) and (FLine[Run + 2] = FAttributeChar) then begin
      inc(Run,3);
      fRange := rsMDText;
      Break; end
    else
      inc(run);
  until  (Run >= FLineLen);
end;

procedure TSynMDSyn.MDCommentSubHeadingOpenProc;
begin
  Inc(Run);
  FRange := rsMDSubHeading;
  FTokenID := tkMDSubheading;
end;

procedure TSynMDSyn.MDGenericLineProc(const eTokenID: TMDTokenKind);
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := eTokenID;
    repeat
      if not IsLineEnd(Run) then
        Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynMDSyn.MDItalicOpenProc;
begin
  Inc(Run);
  FRange := rsMDItalic;
  FTokenID := tkMDItalic;
end;

procedure TSynMDSyn.MDItalicProc;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkMDItalic;
    repeat
      Inc(Run);
    until IsLineEnd(Run) or (FLine[Run] = FAttributeChar);
    if FLine[Run] = FAttributeChar then begin
      Inc(Run);
      FRange := rsMDText;
    end;
  end;
end;

procedure TSynMDSyn.MDMonospaceOpenProc;
begin
  Inc(Run);
  FRange := rsMDMonospace;
  FTokenID := tkMDMonospace;
end;

procedure TSynMDSyn.MDMonospaceProc;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkMDMonospace;
    repeat
      Inc(Run);
    until IsLineEnd(Run) or (FLine[Run] = '`');
    if FLine[Run] = '`' then begin
      Inc(Run);
      FRange := rsMDText;
    end;
  end;
end;

procedure TSynMDSyn.MDNumberOpenProc;
begin
  Inc(Run);
  if IsFirstCharOnLine(Pred(Run)) and (FLine[run] in acNumberedLists) then begin
    FTokenID := tkMDNumber;
    FRange := rsMDNumber; end
  else
    FTokenID := tkMDText;
end;

procedure TSynMDSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsMDLineComment:
      MDGenericLineProc(tkMDLineComment);
    rsMDSubHeading:
      MDGenericLineProc(tkMDSubheading);
    rsMDItalic:
      MDItalicProc;
    rsMDBold:
      MDBoldProc;
    rsMDMonospace:
      MDMonospaceProc;
    rsMDCode:
      MDCodeProc;
  else
    case FLine[Run] of
      #00: NullProc;
      #10: LFProc;
      #13: CRProc;
      '#': MDCommentSubHeadingOpenProc;
      '\': EscapeProc;
      '_', '*': MDTextAttibuteOpenProc;
      '+': MDBoldBulletOpenProc;
      '~': MDCodeOpenProc;
      '0'..'9': MDNumberOpenProc;
      '`': MDMonospaceOpenProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
    else
      if CharInSet(FLine[Run], acSymbols) then
        SymbolProc
      else
        TextProc;
    end;
  end;
end;

procedure TSynMDSyn.NullProc;
begin
  if not (FRange In [rsMDItalic, rsMDBold, rsMDMonospace]) then begin
    FTokenID := tkMDText;
    FRange := rsMDText;
  end;
  Inc(Run);
end;

procedure TSynMDSyn.EscapeProc;
begin
  Inc(Run,2);
end;

procedure TSynMDSyn.ResetRange;
begin
  FRange := rsMDText;
end;

procedure TSynMDSyn.SetAttri(const eTokenKind: TMDTokenKind; const Attri: TSynHighlighterAttributes);
begin
  FTokenAttri[eTokenKind] := Attri;
end;

procedure TSynMDSyn.SetRange(Value: Pointer);
begin
  {$PUSH}{$HINTS OFF}
  FRange := TMDRangeState(UIntPtr(Value));
  {$POP}
end;

procedure TSynMDSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkMDSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynMDSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkMDSymbol;
  while CharInSet(FLine[Run], acSymbols) and not IsLineEnd(Run) do
    Inc(Run);
end;

procedure TSynMDSyn.TextProc;
begin
  FTokenID := tkMDText;
  FRange := rsMDText;
  Inc(Run);
end;

procedure TSynMDSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  FLineRef := NewValue;
  FLine := PChar(FLineRef);
  FLineLen :=  Length(NewValue);
  Run := 0;
  FLineNumber := LineNumber;
  Next;
end;

function TSynMDSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

function TSynMDSyn.GetToken: String;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynMDSyn);
{$ENDIF}
end.
