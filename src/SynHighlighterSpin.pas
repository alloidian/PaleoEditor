unit SynHighlighterSpin;

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
  SysUtils, Classes, Graphics, SynEditHighlighter, SynEditTypes, SynEditStrConst,
  SynHighlighterHashEntries, Utils;

type
  TtkTokenKind = (tkComment, tkConst, tkConfig, tkType, tkFlow, tkMemory, tkDeclaration, 
    tkDirective, tkIdentifier, tkKey, tkNull,  tkNumber,tkRegister, tkSpace, tkString, 
    tkSymbol, tkUnknown);
  TProcTableProc = procedure of object;
  TSynSpinSyn = class(TSynPaleoHighligher)
  private
    FLine: PChar;
    FLineNumber: Integer;
    FProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FStringLen: Integer;
    FToIdent: PChar;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FConstAttri: TSynHighlighterAttributes;
    FConfigAttri: TSynHighlighterAttributes;
    FTypeAttri: TSynHighlighterAttributes;
    FFlowAttri: TSynHighlighterAttributes;
    FMemoryAttri: TSynHighlighterAttributes;
    FDeclarationAttri: TSynHighlighterAttributes;
    FDirectiveAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FRegisterAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FKeywords: TSynHashEntryList;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    procedure CommentProc;
    procedure CRProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SingleQuoteStringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure DoAddKeyword(AKeyword: string; AKind: Integer);
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function IsFilterStored :boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
{$IFDEF SYN_LAZARUS}
   procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
{$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    class function GetLanguageName :string; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property ConstAttri: TSynHighlighterAttributes read FConstAttri write FConstAttri;
    property ConfigAttri: TSynHighlighterAttributes read FConfigAttri write FConfigAttri;
    property TypeAttri: TSynHighlighterAttributes read FTypeAttri write FTypeAttri;
    property FlowAttri: TSynHighlighterAttributes read FFlowAttri write FFlowAttri;
    property MemoryAttri: TSynHighlighterAttributes read FMemoryAttri write FMemoryAttri;
    property DeclarationAttri: TSynHighlighterAttributes read FDeclarationAttri write FDeclarationAttri;
    property DirectiveAttri: TSynHighlighterAttributes read FDirectiveAttri write FDirectiveAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property RegisterAttri: TSynHighlighterAttributes read FRegisterAttri write FRegisterAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

implementation

uses
  ConfigUtils, Configs;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

//Keywords
const
  ConstCodes: string =
    'true;false;posx;negx;pi;';
    
  ConfigCodes: string = 
    'chipver;clkmode;_clkmode;clkfreq;_clkfreq;_xinfreq;_stack;_free;rcfast;' +        {clkset}
    'rcslow;xinput;xtal1;xtal2;xtal3;pll1x;pll2x;pll4x;pll8x;pll16x';    
    
  TypeCodes: string = 
    'byte;word;long';
    
  FlowCodes: string =
    'if;elseif;elseifnot;else;ifnot;case;other;repeat;from;to;step;until;while;next;' +
    'quit;return;abort';
    
  MemoryCodes: string = 
    'bytefill;wordfill;longfill;bytemove;wordmove;longmove;lookup;lookupz;lookdown;' +
    'lookdownz;strsize;strcomp';

  OpCodes: string = 
    'wrbyte;rdbyte;wrword;rdword;wrlong;rdlong;hubop;clkset;cogid;coginit;cogstop;locknew;' +
    'lockret;lockset;lockclr;mul;muls;enc;ones;ror;rol;shr;shl;rcr;rcl;sar;rev;mins;maxs;' +
    'min;max;movs;movd;movi;jmpret;jmp;call;ret;test;testin;and;andn;or;xor;muxc;muxnc;muxz;' +
    'muxnz;add;sub;cmp;addabs;subabs;sumc;sumz;sumnz;mov;neg;abs;absneg;negc;negnc;negz;negnz;' +
    'cmps;cmpsx;addx;subx;cmpx;adds;subs;addsx;subsx;cmpsub;djnz;tjnz;tjz;waitpeq;waitpne;' +
    'waitcnt;waitvid;nop';

  RegCodes: string = 
    'dira;ina;dirb;inb;outa;outb;cnt;ctra;ctrb;frqa;frqb;phsa;phsb;vcfg;vscl;par;spr';
    
  DeclareCodes: string =
    'CON;VAR;PUB;PRI;OBJ;DAT';  

  DirectCodes: string = 
    'cognew;reboot;string;constant;float;round;trunc;file;result;not;if_always;if_never;if_e;' +
    'if_ne;if_a;if_b;if_ae;if_be;if_c;if_nc;if_z;if_nz;if_c_eq_z;if_c_ne_z;if_c_and_z;if_c_and_nz;' +
    'if_nc_and_z;if_nc_and_nz;if_c_or_z;if_c_or_nz;if_nc_or_z;if_nc_or_nz;if_z_eq_c;if_z_ne_c;' +
    'if_z_and_c;if_z_and_nc;if_nz_and_c;if_nz_and_nc;if_z_or_c;if_z_or_nc;if_nz_or_c;if_nz_or_nc;' +
    'fit;org;res;wc;wz;wr;nr';

    {cogid;coginit;cogstop;locknew;lockret;lockclr;lockset;waitcnt;waitpeq;waitpne;waitvid;and;or;}

procedure MakeIdentTable;
var
  Ch: Char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for Ch := 'a' to 'z' do
    Identifiers[Ch] := True;
  for Ch := 'A' to 'Z' do
    Identifiers[Ch] := True;
  for Ch := '0' to '9' do
    Identifiers[Ch] := True;
  Identifiers['_'] := True;
  FillChar(mHashTable, SizeOf(mHashTable), 0);
  for Ch := 'a' to 'z' do
    mHashTable[Ch] := 1 + Ord(Ch) - Ord('a');
  for Ch := 'A' to 'Z' do
    mHashTable[Ch] := 1 + Ord(Ch) - Ord('A');
  for Ch := '0' to '9' do
    mHashTable[Ch] := 27 + Ord(Ch) - Ord('0');
end;

{ TSynSpinSyn }

constructor TSynSpinSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeywords := TSynHashEntryList.Create;
  FCommentAttri := GenerateHighlighter(SYNS_AttrComment, atComment);
  AddAttribute(FCommentAttri);
  FConfigAttri := GenerateHighlighter(SYNS_AttrConfig, atConfig);
  AddAttribute(FConfigAttri);
  FConstAttri := GenerateHighlighter(SYNS_AttrConstant, atConstant);
  AddAttribute(FConstAttri);
  FTypeAttri := GenerateHighlighter(SYNS_AttrDataType, atDataType);
  AddAttribute(FTypeAttri);
  FDeclarationAttri := GenerateHighlighter(SYNS_AttrDeclaration, atDeclaration);
  AddAttribute(FDeclarationAttri);
  FDirectiveAttri := GenerateHighlighter(SYNS_AttrDirective, atDirective);
  AddAttribute(FDirectiveAttri);
  FFlowAttri := GenerateHighlighter(SYNS_AttrFlow, atFlow);
  AddAttribute(FFlowAttri);
  FIdentifierAttri := GenerateHighlighter(SYNS_AttrIdentifier, atIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := GenerateHighlighter(SYNS_AttrKey, atKeyword);
  AddAttribute(FKeyAttri);
  FMemoryAttri := GenerateHighlighter(SYNS_AttrMemory, atMemory);
  AddAttribute(FMemoryAttri);
  FNumberAttri := GenerateHighlighter(SYNS_AttrNumber, atNumber);
  AddAttribute(FNumberAttri);
  FRegisterAttri := GenerateHighlighter(SYNS_AttrRegister, atRegister);
  AddAttribute(FRegisterAttri);
  FStringAttri := GenerateHighlighter(SYNS_AttrString, atString);
  AddAttribute(FStringAttri);
  FSymbolAttri := GenerateHighlighter(SYNS_AttrSymbol, atSymbol);
  AddAttribute(FSymbolAttri);
  FSpaceAttri := GenerateHighlighter(SYNS_AttrWhitespace, atWhitespace);
  AddAttribute(FSpaceAttri);
  MakeMethodTables;
  EnumerateKeywords(Ord(tkKey), OpCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkRegister), RegCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkConst), ConstCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkConfig), ConfigCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkType), TypeCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkFlow), FlowCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkMemory), MemoryCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkDeclaration), DeclareCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  EnumerateKeywords(Ord(tkDirective), DirectCodes, IdentChars, {$IFDEF FPC}@{$ENDIF}DoAddKeyword);
  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  FDefaultFilter := SYNS_FilterX86Asm;
end;

destructor TSynSpinSyn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

function TSynSpinSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while Identifiers[ToHash^] do begin
{$IFOPT Q-}
    Result := 7 * Result + mHashTable[ToHash^];
{$ELSE}
    Result := (7 * Result + mHashTable[ToHash^]) and $FFFFFF;
{$ENDIF}
    Inc(ToHash);
  end;
  Result := Result and $3FF;
  FStringLen := ToHash - FToIdent;
end;

function TSynSpinSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer = 0;
  pKey1: PChar;
  pKey2: PChar;
begin
  pKey1 := FToIdent;
  // Note: FStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for I := 1 to FStringLen do begin
    if mHashTable[pKey1^] <> mHashTable[pKey2^] then begin
      Result := False;
      Exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := True;
end;

procedure TSynSpinSyn.CommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until FLine[Run] in [#0, #10, #13];
end;

procedure TSynSpinSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynSpinSyn.GreaterProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  if FLine[Run] = '=' then
    Inc(Run);
end;

procedure TSynSpinSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  Inc(Run, FStringLen);
  while Identifiers[FLine[Run]] do
    Inc(Run);
end;

procedure TSynSpinSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynSpinSyn.LowerProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynSpinSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynSpinSyn.NumberProc;
begin
  Inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['_', '0'..'9', '.', 'a'..'f', 'h', 'A'..'F', 'H'] do
    Inc(Run);
end;

procedure TSynSpinSyn.SlashProc;
begin
  Inc(Run);
  if FLine[Run] = '/' then begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until FLine[Run] in [#0, #10, #13];
  end else
    FTokenID := tkSymbol;
end;

procedure TSynSpinSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until (FLine[Run] > #32) or (FLine[Run] in [#0, #10, #13]);
end;

procedure TSynSpinSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    Inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then
    Inc(Run);
end;

procedure TSynSpinSyn.SingleQuoteStringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then
    Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then
    Inc(Run);
end;

procedure TSynSpinSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynSpinSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  Inc(Run);
  FTokenID := tkIdentifier;
end;

procedure TSynSpinSyn.DoAddKeyword(AKeyword: string; AKind: Integer);
var
  HashValue: Integer = 0;
begin
  HashValue := KeyHash(PChar(AKeyword));
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TSynSpinSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do begin
    if Entry.KeywordLen > FStringLen then
      break
    else
      if Entry.KeywordLen = FStringLen then
        if KeyComp(Entry.Keyword) then begin
          Result := TtkTokenKind(Entry.Kind);
          Exit;
        end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TSynSpinSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
       #0 : FProcTable[I] := {$IFDEF FPC}@{$ENDIF}NullProc;
      #10 : FProcTable[I] := {$IFDEF FPC}@{$ENDIF}LFProc;
      #13 : FProcTable[I] := {$IFDEF FPC}@{$ENDIF}CRProc;
      #34 : FProcTable[I] := {$IFDEF FPC}@{$ENDIF}StringProc;
      #39 : FProcTable[I] := {$IFDEF FPC}@{$ENDIF}SingleQuoteStringProc;
      '>' : FProcTable[I] := {$IFDEF FPC}@{$ENDIF}GreaterProc;
      '<' : FProcTable[I] := {$IFDEF FPC}@{$ENDIF}LowerProc;
      '/' : FProcTable[I] := {$IFDEF FPC}@{$ENDIF}SlashProc;
      'A'..'Z', 'a'..'z', '_':
        FProcTable[I] := {$IFDEF FPC}@{$ENDIF}IdentProc;
      '0'..'9':
        FProcTable[I] := {$IFDEF FPC}@{$ENDIF}NumberProc;
      #1..#9, #11, #12, #14..#32:
        FProcTable[I] := {$IFDEF FPC}@{$ENDIF}SpaceProc;
      ';':
        FProcTable[I] := {$IFDEF FPC}@{$ENDIF}CommentProc;
      '.', ':', '&', '{', '}', '=', '^', '-', '+', '(', ')', '*', '#':
        FProcTable[I] := {$IFDEF FPC}@{$ENDIF}SymbolProc;
    else
        FProcTable[I] := {$IFDEF FPC}@{$ENDIF}UnknownProc;
    end;
end;

function TSynSpinSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynSpinSyn.IsFilterStored :boolean;
begin
  Result := (FDefaultFilter <> SYNS_FilterX86Asm);
end;

function TSynSpinSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSpinSyn.GetEol: Boolean;
begin
  Result := FTokenId = tkNull;
end;

procedure TSynSpinSyn.SetLine(const NewValue :String; LineNumber :Integer);
begin
  FLine := PChar(NewValue);
  Run := 0;
  FLineNumber := LineNumber;
  Next;
end;

function TSynSpinSyn.GetToken: String;
var
  Len: LongInt = 0;
begin
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

{$IFDEF SYN_LAZARUS}
procedure TSynSpinSyn.GetTokenEx(out TokenStart :PChar; out TokenLength :Integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart  := FLine + FTokenPos;
end;
{$ENDIF}

function TSynSpinSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkConst: Result := FConstAttri;
    tkConfig: Result := FConfigAttri;
    tkType: Result := FTypeAttri;
    tkFlow: Result := FFlowAttri;
    tkMemory: Result := FMemoryAttri;
    tkDeclaration: Result := FDeclarationAttri;
    tkDirective: Result := FDirectiveAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkRegister: Result := FRegisterAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynSpinSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenId);
end;

function TSynSpinSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynSpinSyn.Next;
begin
  FTokenPos := Run;
  FProcTable[FLine[Run]];
end;

class function TSynSpinSyn.GetLanguageName :string;
begin
  Result := 'Spin';
end;

function TSynSpinSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenId;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSpinSyn);
{$ENDIF}
end.
