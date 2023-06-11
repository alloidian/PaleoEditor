unit SynHighlighterFlex;

{$MODE ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, LazFileUtils, Controls, Graphics, FGL, Registry, SynEditTypes,
  SynEditHighlighter, SynEditStrConst, Utils;

const
  SYN_ATTR_DOT      = 10;
  SYN_ATTR_ENTITY   = 11;
  SYN_ATTR_VARABLE  = 12;

type

  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkPreprocessor,
  tkSpace, tkString, tkSymbol, tkUnknown, tkEntity, tkDollarVariable, tkDot, tkLex0,
  tkLex1, tkLex2, tkLex3, tkLex4, tkLex5, tkLex6, tkLex7, tkLex8, tkLex9);

  TCommentStyle = (csAnsiStyle, csPasStyle, csCStyle, csAsmStyle, csBasStyle, csVBStyle);
  CommentStyles = set of TCommentStyle;

  TRangeState = (rsANil, rsAnsi, rsPasStyle, rsCStyle, rsUnKnown);

  TStringDelim = (sdSingleQuote, sdDoubleQuote);

  TProcTableProc = procedure of object;

{ TLexicon }

  TLexicon = class(TObject)
  private
    FIndex: Integer;
    FTokenKind: TtkTokenKind;
    FCaption: String;
    FList: TStringList;
    FAttributes: TSynHighlighterAttributes;
  public
    constructor Create(const Caption, StoreName: String; Index: Integer; TokenKind: TtkTokenKind; IsCaseSensitive: Boolean = False); virtual; overload;
    constructor Create(const Caption: String; Index: Integer; TokenKind: TtkTokenKind; IsCaseSensitive: Boolean = False); virtual; overload;
    destructor Destroy; override;
    procedure Clear;
    procedure Import(const Value: String; Delimiter: Char = ' ');
    procedure Append(const Value: String);
    function Contains(const Value: String): Boolean;
    property Index: Integer read FIndex;
    property TokenKind: TtkTokenKind read FTokenKind;
    property Caption: String read FCaption;
    property List: TStringList read FList;
    property Attributes: TSynHighlighterAttributes read FAttributes;
  end;

{ TLexicons }

  TLexicons = class(TObject)
  private type
    TLexiconList = specialize TFPGObjectList<TLexicon>;
  private
    FList: TLexiconList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const Caption, StoreName: String; Index: Integer; TokenKind: TtkTokenKind; IsCaseSensitive: Boolean = False): TSynHighlighterAttributes; overload;
    function Add(const Caption: String; Index: Integer; TokenKind: TtkTokenKind; IsCaseSensitive: Boolean = False): TSynHighlighterAttributes; overload;
    function DeriveTokenKind(const Value: String): TtkTokenKind;
    function TryFind(Index: Integer; out Lexicon: TLexicon): Boolean; overload;
    function TryFind(TokenKind: TtkTokenKind; out Lexicon: TLexicon): Boolean; overload;
    function TryFind(Index: Integer; out Attributes: TSynHighlighterAttributes): Boolean; overload;
    function TryFind(TokenKind: TtkTokenKind; out Attributes: TSynHighlighterAttributes): Boolean; overload;
    function TryFind(TokenKind: TtkTokenKind; out List: TStringList): Boolean; overload;
  end;

{ TSynFlexSyn }

  TSynFlexSyn = class(TSynPaleoHighligher)
  private
    FLexicons: TLexicons;
    FMarkupOn: Boolean;
    FRange: TRangeState;
    FLine: PChar;
    FProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FTokenPos: Integer;
    FTokenId: TtkTokenKind;
    FLineNumber : Integer;
    FComments: CommentStyles;
    FStringDelimCh: char;
    FIdentChars: TSynIdentChars;
    FDetectPreprocessor: Boolean;
    FMarkup: Boolean;
    FEntity: Boolean;
    FDollarVariables: Boolean;
    FActiveDot: Boolean;
    procedure ApostropheProc;
    procedure AmpersandProc;
    procedure AsciiCharProc;
    procedure BraceOpenProc;
    procedure BraceCloseProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure DotProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure DollarProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure RoundCloseProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure GreaterThan;
    procedure SmallerThan;
    procedure MakeMethodTables;
    procedure AnsiProc;
    procedure PasStyleProc;
    procedure CStyleProc;
    procedure SetComments(Value: CommentStyles);
    function GetStringDelim: TStringDelim;
    procedure SetStringDelim(const Value: TStringDelim);
    function GetIdentifierChars: String;
    procedure SetIdentifierChars(const Value: String);
    procedure SetDetectPreprocessor(Value: Boolean);
    procedure SetMarkup(const Value: Boolean);
    procedure SetEntity(const Value: Boolean);
    procedure SetDollarVariables(const Value: Boolean);
    procedure SetActiveDot(const Value: Boolean);
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetLanguageName: String; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    function GetTokenID: TtkTokenKind;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: String): Boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function SaveToRegistry(RootKey: HKEY; Key: String): Boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: String): Boolean; override;
    property Lexicons: TLexicons read FLexicons;

    property DotAttribute: TSynHighlighterAttributes index SYN_ATTR_DOT read GetDefaultAttribute;
    property EntityAttribute: TSynHighlighterAttributes index SYN_ATTR_ENTITY read GetDefaultAttribute;
    property PreprocessorAttribute: TSynHighlighterAttributes index SYN_ATTR_DIRECTIVE read GetDefaultAttribute;
    property NumberAttribute: TSynHighlighterAttributes index SYN_ATTR_NUMBER read GetDefaultAttribute;
    property VariableAttribute: TSynHighlighterAttributes index SYN_ATTR_VARABLE read GetDefaultAttribute;
    property IdentifierChars: String read GetIdentifierChars write SetIdentifierChars;
    property Comments: CommentStyles read FComments write SetComments;
    property DetectPreprocessor: Boolean read FDetectPreprocessor write SetDetectPreprocessor;

    property StringDelim: TStringDelim read GetStringDelim write SetStringDelim default sdSingleQuote;
    property Markup: Boolean read FMarkup write SetMarkup;
    property Entity: Boolean read FEntity write SetEntity;
    property DollarVariables: Boolean read FDollarVariables write SetDollarVariables;
    property ActiveDot: Boolean read FActiveDot write SetActiveDot;
  end;

implementation

{$WARN 6058 OFF}

uses
  StrUtils;

var
  Identifiers: array[#0..#255] of ByteBool;

procedure MakeIdentTable;
var
  I: Char;
  Idents: String;
begin
  Idents := '_0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-?!';
  for I := #0 to #255 do
    if AnsiContainsStr(Idents, I) then
      Identifiers[I] := True
    else
      Identifiers[I] := False;
end;

{ TLexicon }

constructor TLexicon.Create(const Caption, StoreName: String; Index: Integer; TokenKind: TtkTokenKind; IsCaseSensitive: Boolean = False);
begin
  inherited Create;
  FCaption := Caption;
  FIndex := Index;
  FTokenKind := TokenKind;
  FList := TStringListUTF8Fast.Create;
  FList.Sorted := True;
  FList.Duplicates := dupIgnore;
  FList.CaseSensitive := IsCaseSensitive;
  FAttributes := TSynHighlighterAttributes.Create(Caption, StoreName);
end;

constructor TLexicon.Create(const Caption: String; Index: Integer; TokenKind: TtkTokenKind; IsCaseSensitive: Boolean);
begin
  Create(Caption, EmptyStr, Index, TokenKind, IsCaseSensitive);
end;

destructor TLexicon.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TLexicon.Clear;
begin
  FList.Clear;
end;

procedure TLexicon.Import(const Value: String; Delimiter: Char);
var
  Temp: TStringList;
begin
  Temp := TStringList.Create;
  try
    Temp.Delimiter := Delimiter;
    Temp.DelimitedText := Value;
    FList.Assign(Temp);
  finally
    Temp.Free;
  end;
end;

procedure TLexicon.Append(const Value: String);
begin
  if not Contains(Value) then
    FList.Add(Value);
end;

function TLexicon.Contains(const Value: String): Boolean;
var
  I: Integer;
begin
  Result := FList.Count > 0;
  if Result then
    Result := FList.Find(Value, I);
end;

{ TLexicons }

constructor TLexicons.Create;
begin
  inherited Create;
  FList := TLexiconList.Create(True);
end;

destructor TLexicons.Destroy;
begin
  FList.Free;
  inherited;
end;

function TLexicons.Add(const Caption, StoreName: String; Index: Integer; TokenKind: TtkTokenKind; IsCaseSensitive: Boolean = False): TSynHighlighterAttributes;
var
  Temp: TLexicon;
begin
  if not TryFind(Index, Temp) then begin
    Temp := TLexicon.Create(Caption, StoreName, Index, TokenKind, IsCaseSensitive);
    FList.Add(Temp);
  end;
  Result := Temp.Attributes;
end;

function TLexicons.Add(const Caption: String; Index: Integer; TokenKind: TtkTokenKind; IsCaseSensitive: Boolean): TSynHighlighterAttributes;
var
  Temp: TLexicon;
begin
  if not TryFind(Index, Temp) then begin
    Temp := TLexicon.Create(Caption, Index, TokenKind, IsCaseSensitive);
    FList.Add(Temp);
  end;
  Result := Temp.Attributes;
end;

function TLexicons.DeriveTokenKind(const Value: String): TtkTokenKind;
var
  I: TLexicon;
begin
  Result := tkUnknown;
  for I in FList do begin
    if I.Contains(Value) then begin
      Result := I.TokenKind;
      Break;
    end;
  end;
end;

function TLexicons.TryFind(Index: Integer; out Lexicon: TLexicon): Boolean;
var
  I: TLexicon;
begin
  Result := False;
  Lexicon := nil;
  for I in FList do begin
    if I.Index = Index then begin
      Result := True;
      Lexicon := I;
      Break;
    end;
  end;
end;

function TLexicons.TryFind(TokenKind: TtkTokenKind; out Lexicon: TLexicon): Boolean;
var
  I: TLexicon;
begin
  Result := False;
  Lexicon := nil;
  for I in FList do begin
    if I.TokenKind = TokenKind then begin
      Result := True;
      Lexicon := I;
      Break;
    end;
  end;
end;

function TLexicons.TryFind(Index: Integer; out Attributes: TSynHighlighterAttributes): Boolean;
var
  I: TLexicon;
begin
  Result := False;
  Attributes := nil;
  for I in FList do begin
    if I.Index = Index then begin
      Result := True;
      Attributes := I.Attributes;
      Break;
    end;
  end;
end;

function TLexicons.TryFind(TokenKind: TtkTokenKind; out Attributes: TSynHighlighterAttributes): Boolean;
var
  I: TLexicon;
begin
  Result := False;
  Attributes := nil;
  for I in FList do begin
    if I.TokenKind = TokenKind then begin
      Result := True;
      Attributes := I.Attributes;
      Break;
    end;
  end;
end;

function TLexicons.TryFind(TokenKind: TtkTokenKind; out List: TStringList): Boolean;
var
  I: TLexicon;
begin
  Result := False;
  List := nil;
  for I in FList do begin
    if I.TokenKind = TokenKind then begin
      Result := True;
      List := I.List;
      Break;
    end;
  end;
end;

{ TSynFlexSyn }

constructor TSynFlexSyn.Create(AOwner: TComponent);
var
  Temp: TSynHighlighterAttributes;
begin
  inherited Create(AOwner);
  FLexicons := TLexicons.Create;

  Temp := FLexicons.Add(SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord, SYN_ATTR_KEYWORD, tkKey, False);
  Temp.Style := [fsBold];
  AddAttribute(Temp);

  Temp := FLexicons.Add(SYNS_AttrComment, SYNS_XML_AttrComment, SYN_ATTR_COMMENT, tkComment, False);
  Temp.Style := [fsItalic];
  AddAttribute(Temp);

  Temp := FLexicons.Add(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier, SYN_ATTR_IDENTIFIER, tkIdentifier, False);
  AddAttribute(Temp);

  Temp := FLexicons.Add('jan_entity', 'jan_entity', SYN_ATTR_ENTITY, tkEntity, False);
  Temp.Style := [fsBold];
  Temp.Foreground :=cllime;
  AddAttribute(Temp);

  Temp := FLexicons.Add('jan_dot', 'jan_dot', SYN_ATTR_DOT, tkDot, False);
  Temp.Style := [fsBold];
  Temp.Foreground :=clgreen;
  AddAttribute(Temp);

  Temp := FLexicons.Add(SYNS_AttrNumber, SYNS_XML_AttrNumber, SYN_ATTR_NUMBER, tkNumber, False);
  AddAttribute(Temp);

  Temp := FLexicons.Add(SYNS_AttrSpace, SYNS_XML_AttrSpace, SYN_ATTR_WHITESPACE, tkSpace, False);
  AddAttribute(Temp);

  Temp := FLexicons.Add(SYNS_AttrString, SYNS_XML_AttrString, SYN_ATTR_STRING, tkString, False);
  AddAttribute(Temp);

  Temp := FLexicons.Add(SYNS_AttrSymbol, SYNS_XML_AttrSymbol, SYN_ATTR_SYMBOL, tkSymbol, False);
  AddAttribute(Temp);

  Temp := FLexicons.Add('jan_Variable', 'jan_Variable', SYN_ATTR_VARIABLE, tkDollarVariable, False);
  Temp.Style := [fsBold];
  Temp.Foreground := clpurple;
  AddAttribute(Temp);

  Temp := FLexicons.Add(SYNS_AttrPreprocessor, SYNS_XML_AttrPreprocessor, SYN_ATTR_DIRECTIVE, tkPreprocessor, False);
  AddAttribute(Temp);

  SetAttributesOnChange(@DefHighlightChange);
  FStringDelimCh := '''';
  FIdentChars := inherited GetIdentChars;
  MakeMethodTables;
  FRange := rsUnknown;
end;

destructor TSynFlexSyn.Destroy;
begin
  FLexicons.Free;
  inherited;
end;

procedure TSynFlexSyn.ApostropheProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until FLine[Run] in [#0, #10, #13];
end;

procedure TSynFlexSyn.AmpersandProc;

  function TestEntity: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    I := Run;
    Inc(I);
    while FLine[I] <> #0 do
      case FLine[I] of
        ';': begin
            FRange := rsUnKnown;
            Inc(I);
            Result := True;
            Break;
          end;
        #10, #13, ' ':
          Break;
      else
        Inc(I);
      end;
    if Result then
      Run := I;
  end;

begin
  if Entity then
    if TestEntity then
      FTokenID := tkEntity
    else begin
      Inc(Run);
      FTokenID := tkSymbol;
    end
  else begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynFlexSyn.AsciiCharProc;
begin
  if FDetectPreprocessor then begin
    FTokenID := tkPreprocessor;
    repeat
      Inc(Run);
    until FLine[Run] in [#0, #9, #10, #13, #32]; end
  else begin
    FTokenID := tkString;
    repeat
      Inc(Run);
    until not (FLine[Run] in ['0'..'9']);
  end;
end;

procedure TSynFlexSyn.BraceOpenProc;
begin
  if csPasStyle in FComments then begin
    FTokenID := tkComment;
    FRange := rsPasStyle;
    Inc(Run);
    while FLine[Run] <> #0 do
      case FLine[Run] of
        '}':
          begin
            FRange := rsUnKnown;
            Inc(Run);
            Break;
          end;
        #10, #13:
          Break;
      else
        Inc(Run);
      end; end
  else begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynFlexSyn.BraceCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFlexSyn.PointCommaProc;
begin
  if (csASmStyle in FComments) or (csBasStyle in FComments) then begin
    FTokenID := tkComment;
    FRange := rsUnknown;
    Inc(Run);
    while FLine[Run] <> #0 do begin
      FTokenID := tkComment;
      Inc(Run);
    end; end
  else begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynFlexSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynFlexSyn.DotProc;

  function TestDot: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    I := Run;
    Inc(I);
    while FLine[I] in ['a'..'z','A'..'Z'] do
      Inc(I);
    if I > (Run + 1) then
      Result := True;
    if Result then
      Run := I;
  end;

begin
  if not FActiveDot then begin
    Inc(Run);
    FTokenID := tkSymbol; end
  else
    if TestDot then
      FTokenID := tkDot
    else begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
end;

procedure TSynFlexSyn.IdentProc;
var
  aToken: String;
begin
  while Identifiers[FLine[Run]] do
    Inc(Run);
  aToken := GetToken;
  if IsKeyWord(aToken) then
    if not Markup then
      FTokenId :=  tkKey
    else
      if FMarkupOn then
        FTokenId := tkKey
      else
        FTokenId := tkIdentifier
  else
    FTokenID := FLexicons.DeriveTokenKind(aToken);
end;

procedure TSynFlexSyn.IntegerProc;
begin
  Inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(Run);
end;

procedure TSynFlexSyn.DollarProc;
begin
  Inc(Run);
  FTokenID := tkDollarVariable;
  while FLine[Run] in ['0'..'9', 'A'..'Z', 'a'..'z','_'] do
    Inc(Run);
end;

procedure TSynFlexSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynFlexSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynFlexSyn.NumberProc;
begin
  Inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'a', 'A', 'b', 'B', 'c', 'C', 'd', 'D', 'e', 'E', 'f', 'F', 'h', 'H', 'x'] do begin
    case FLine[Run] of
      'x': begin { handle C style hex numbers }
        IntegerProc;
        Break;
        end;
      '.':
        if FLine[Run + 1] = '.' then
          Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynFlexSyn.RoundOpenProc;
begin
  Inc(Run);
  if csAnsiStyle in FComments then
    case FLine[Run] of
    '*': begin
      FTokenID := tkComment;
      FRange := rsAnsi;
      Inc(Run);
      while FLine[Run] <> #0 do
        case FLine[Run] of
          '*':
            if FLine[Run + 1] = ')' then begin
              FRange := rsUnKnown;
              Inc(Run, 2);
              Break; end
            else
              Inc(Run);
          #10, #13:
            Break;
        else
          Inc(Run);
        end;
      end;
    '.':
      begin
        Inc(Run);
        FTokenID := tkSymbol;
      end;
    else
      FTokenID := tkSymbol;
    end
  else
    FTokenId := tkSymbol;
end;

procedure TSynFlexSyn.RoundCloseProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFlexSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        Inc(Run, 2);
        FTokenID := tkComment;
        while FLine[Run] <> #0 do begin
          case FLine[Run] of
            #10, #13: Break;
          end;
          Inc(Run);
        end;
      end;
    '*':
      begin
        if csCStyle in FComments then begin
          FTokenID := tkComment;
          FRange := rsCStyle;
          Inc(Run);
          while FLine[Run] <> #0 do
            case FLine[Run] of
              '*':
                if FLine[Run + 1] = '/' then begin
                  FRange := rsUnKnown;
                  Inc(Run, 2);
                  Break; end
                else
                  Inc(Run);
              #10, #13:
                Break;
            else
              Inc(Run);
            end; end
        else begin
          Inc(Run);
          FTokenId := tkSymbol;
        end;
      end
  else
    begin
      Inc(Run);
      if Markup and fmarkupon then
        FTokenID := tkKey
      else
        FTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynFlexSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    Inc(Run);
end;

procedure TSynFlexSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = FStringDelimCh) and (FLine[Run + 2] = FStringDelimCh) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until FLine[Run] = FStringDelimCh;
  if FLine[Run] <> #0 then
    Inc(Run);
end;

procedure TSynFlexSyn.UnknownProc;
begin
  Inc(Run);
  while (FLine[Run] in [#128..#191]) or ((FLine[Run] <> #0) and (FProcTable[FLine[Run]] = @UnknownProc)) do
    Inc(Run);
  FTokenID := tkUnKnown;
end;

procedure TSynFlexSyn.GreaterThan;
begin
  Inc(Run);
  if Markup then begin
    FMarkupOn := False;
    FTokenId := tkKey; end
  else
    FTokenID := tkUnKnown;
end;

procedure TSynFlexSyn.SmallerThan;
begin
  Inc(Run);
  if Markup then begin
    FMarkupOn := True;
    FTokenId := tkKey; end
  else
    FTokenID := tkUnKnown;
end;

procedure TSynFlexSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #39:
        if csVBStyle in comments then begin
          FProcTable[I] := @ApostropheProc;
          fStringDelimch := #34; end
        else
          FProcTable[I] := @UnknownProc;
      '<':
        if Markup then
          FProcTable[I] := @SmallerThan
        else
          FProcTable[I] := @UnknownProc;
      '>':
        if Markup then
          FProcTable[I] := @GreaterThan
        else
          FProcTable[I] := @UnknownProc;
      '&':
        if Entity then
          FProcTable[I] := @AmpersandProc
        else
          FProcTable[I] := @UnknownProc;
      '#':
        FProcTable[I] := @AsciiCharProc;
      '{':
        FProcTable[I] := @BraceOpenProc;
      '}':
        FProcTable[I] := @BraceCloseProc;
      ';':
        FProcTable[I] := @PointCommaProc;
      #13:
        FProcTable[I] := @CRProc;
      'A'..'Z', 'a'..'z', '_':
        FProcTable[I] := @IdentProc;
      '$':
        if dollarvariables then
          FProcTable[I] := @DollarProc
        else
          FProcTable[I] := @IntegerProc;
      '.':
        FProcTable[I] := @DotProc;
      #10:
        FProcTable[I] := @LFProc;
      #0:
        FProcTable[I] := @NullProc;
      '0'..'9':
        FProcTable[I] := @NumberProc;
      '(':
        FProcTable[I] := @RoundOpenProc;
      ')':
        FProcTable[I] := @RoundCloseProc;
      '/':
        FProcTable[I] := @SlashProc;
      #1..#9, #11, #12, #14..#32:
        FProcTable[I] := @SpaceProc;
      else
        FProcTable[I] := @UnknownProc;
    end;
  FProcTable[FStringDelimCh] := @StringProc;
end;

procedure TSynFlexSyn.AnsiProc;
begin
  case FLine[Run] of
     #0:
      NullProc;
    #10:
      LFProc;
    #13:
      CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[Run] = '*') and (FLine[Run + 1] = ')') then begin
        FRange := rsUnKnown;
        Inc(Run, 2);
        Break;
      end;
      Inc(Run);
    until FLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynFlexSyn.PasStyleProc;
begin
  case FLine[Run] of
     #0:
      NullProc;
    #10:
      LFProc;
    #13:
      CRProc;
  else
    FTokenID := tkComment;
    repeat
      if FLine[Run] = '}' then begin
        FRange := rsUnKnown;
        Inc(Run);
        Break;
      end;
      Inc(Run);
    until FLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynFlexSyn.CStyleProc;
begin
  case FLine[Run] of
     #0:
      NullProc;
    #10:
      LFProc;
    #13:
      CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then begin
        FRange := rsUnKnown;
        Inc(Run, 2);
        Break;
      end;
      Inc(Run);
    until FLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynFlexSyn.SetComments(Value: CommentStyles);
begin
  if FComments <> Value then begin
    FComments := Value;
    MakeMethodTables;
    DefHighLightChange(nil);
  end;
end;

function TSynFlexSyn.GetStringDelim: TStringDelim;
begin
  if FStringDelimCh = '''' then
    Result := sdSingleQuote
  else
    Result := sdDoubleQuote;
end;

procedure TSynFlexSyn.SetStringDelim(const Value: TStringDelim);
var
  newCh: Char;
begin
  case Value of
    sdSingleQuote:
      newCh := '''';
  else
    newCh := '"';
  end;
  if newCh <> FStringDelimCh then begin
    FStringDelimCh := newCh;
    MakeMethodTables;
  end;
end;

function TSynFlexSyn.GetIdentifierChars: String;
var
  Ch: Char;
  S: ShortString;
begin
  S := EmptyStr;
  for Ch := #0 to #255 do
    if Ch in FIdentChars then
      S := S + Ch;
  Result := S;
end;

procedure TSynFlexSyn.SetIdentifierChars(const Value: String);
var
  I: Integer;
begin
  FIdentChars := [];
  for I := 1 to Length(Value) do
    FIdentChars := FIdentChars + [Value[I]];
end;

procedure TSynFlexSyn.SetDetectPreprocessor(Value: Boolean);
begin
  if Value <> FDetectPreprocessor then begin
    FDetectPreprocessor := Value;
    DefHighlightChange(Self);
  end;
end;

procedure TSynFlexSyn.SetMarkup(const Value: Boolean);
begin
  if value <> FMarkup then begin
    FMarkup := Value;
    DefHighLightChange(nil);
  end;
end;

procedure TSynFlexSyn.SetEntity(const Value: Boolean);
begin
  if value <> FEntity then begin
    FEntity := Value;
    DefHighLightChange(nil);
  end;
end;

procedure TSynFlexSyn.SetDollarVariables(const Value: Boolean);
begin
  if Value <> FDollarVariables then begin
    FDollarVariables := Value;
    MakeMethodTables;
    DefHighLightChange(nil);
  end;
end;

procedure TSynFlexSyn.SetActiveDot(const Value: Boolean);
begin
  FActiveDot := Value;
end;

function TSynFlexSyn.GetIdentChars: TSynIdentChars;
begin
  Result := FIdentChars;
end;

class function TSynFlexSyn.GetLanguageName: String;
begin
  Result := SYNS_LangGeneral;
end;

function TSynFlexSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  if not FLexicons.TryFind(Index, Result) then
    Result := nil;
end;

function TSynFlexSyn.GetEol: Boolean;
begin
  Result := FTokenId = tkNull;
end;

function TSynFlexSyn.GetRange: Pointer;
begin
  {$PUSH}{$HINTS OFF}
  Result := Pointer(PtrUInt(FRange));
  {$POP}
end;

procedure TSynFlexSyn.SetRange(Value: Pointer);
begin
  {$PUSH}{$HINTS OFF}
  FRange := TRangeState(PtrUInt(Value));
  {$POP}
end;

function TSynFlexSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenId;
end;

function TSynFlexSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - FTokenPos;
  Result := EmptyStr;
  SetString(Result, (FLine + FTokenPos), Len);
end;

procedure TSynFlexSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: Integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynFlexSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if not FLexicons.TryFind(FTokenID, Result) then
    Result := nil;
end;

function TSynFlexSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenId);
end;

function TSynFlexSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

function TSynFlexSyn.IsKeyword(const AKeyword: String): Boolean;
var
  Temp: TLexicon;
begin
  if FLexicons.TryFind(tkKey, Temp) then
    Result := Temp.Contains(AKeyword)
  else
    Result := False;
end;

procedure TSynFlexSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsAnsi:
      AnsiProc;
    rsPasStyle:
      PasStyleProc;
    rsCStyle:
      CStyleProc;
  else
    FProcTable[FLine[Run]];
  end;
end;

procedure TSynFlexSyn.ReSetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynFlexSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FLine := PChar(NewValue);
  Run := 0;
  FLineNumber := LineNumber;
  Next;
end;

function TSynFlexSyn.SaveToRegistry(RootKey: HKEY; Key: String): Boolean;
var
  r: TRegistry;
  List: TStringList;
begin
  r := TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key, True) and FLexicons.TryFind(tkKey, List) then begin
      Result := True;
      r.WriteString('KeyWords', List.Text);
      Result := inherited SaveToRegistry(RootKey, Key); end
    else
      Result := False;
  finally r.Free; end;
end;

function TSynFlexSyn.LoadFromRegistry(RootKey: HKEY; Key: String): Boolean;
var
  r: TRegistry;
  List: TStringList;
begin
  r := TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      if r.ValueExists('KeyWords') and FLexicons.TryFind(tkKey, List) then
        List.Text := r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key); end
    else
      Result := False;
  finally r.Free; end;
end;

initialization
  MakeIdentTable;
end.

