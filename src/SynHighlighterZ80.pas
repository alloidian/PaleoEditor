unit SynHighlighterZ80;

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

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter,    SynHighlighterFlex;

const
  SYN_ATTR_REGISTER = 13;

type
  TSynZ80Syn = class(TSynFlexSyn)
  public
    constructor Create(AOwner: TComponent); override;
    property RegisterAttribute: TSynHighlighterAttributes index SYN_ATTR_REGISTER read GetDefaultAttribute;
  end;

implementation

uses
  SynEditStrConst, ConfigUtils;

const
  OpCodes: string =
    'adc add and bit call ccf cp cpd cpdr cpi cpir cpl daa dec di djnz ei ex exx halt ' +
    'im in in0 inc ind indr ini inir jp jr ld ldd lddr ldi ldir mult neg nop or otdm ' +
    'otdmr otdr otim otimr otir out out0 outd outi pop push res ret reti retn rl rla ' +
    'rlc rlca rld rr rra rrc rrca rrd rst sbc scf set sla slp sra srl sub tst tstio xor';

  RegCodes: string =
    'a af b c bc d e de h l hl i ix ixl ixh iy iyl iyh r sp';

{ TSynZ80Syn }

constructor TSynZ80Syn.Create(AOwner: TComponent);
var
  Lexicon: TLexicon;
  Temp: TSynHighlighterAttributes;
begin
  inherited Create(AOwner);
  if Lexicons.TryFind(SYN_ATTR_KEYWORD, Lexicon) then begin
    Lexicon.Import(OpCodes);
    UpdateHighlighter(atKeyword, Lexicon.Attributes);
  end;
  Temp := Lexicons.Add(SYNS_AttrRegister, SYNS_XML_AttrRegister, SYN_ATTR_REGISTER, tkLex0, False);
  AddAttribute(Temp);
  if Lexicons.TryFind(SYN_ATTR_REGISTER, Lexicon) then begin
    Lexicon.Import(RegCodes);
    UpdateHighlighter(atRegister, Lexicon.Attributes);
  end;
  if Lexicons.TryFind(SYN_ATTR_COMMENT, Lexicon) then
    UpdateHighlighter(atComment, Lexicon.Attributes);
  if Lexicons.TryFind(SYN_ATTR_DOT, Lexicon) then begin
    Lexicon.Attributes.Foreground := clGreen;
    Lexicon.Attributes.Style := [];
  end;
  if Lexicons.TryFind(SYN_ATTR_ENTITY, Lexicon) then begin
    Lexicon.Attributes.Foreground := clLime;
    Lexicon.Attributes.Style := [fsBold];
  end;
  if Lexicons.TryFind(SYN_ATTR_IDENTIFIER, Lexicon) then
    UpdateHighlighter(atIdentifier, Lexicon.Attributes);
  if Lexicons.TryFind(SYN_ATTR_DIRECTIVE, Lexicon) then
    UpdateHighlighter(atDirective, Lexicon.Attributes);
  if Lexicons.TryFind(SYN_ATTR_NUMBER, Lexicon) then
    UpdateHighlighter(atNumber, Lexicon.Attributes);
  if Lexicons.TryFind(SYN_ATTR_WHITESPACE, Lexicon) then
    UpdateHighlighter(atWhitespace, Lexicon.Attributes);
  if Lexicons.TryFind(SYN_ATTR_STRING, Lexicon) then
    UpdateHighlighter(atString, Lexicon.Attributes);
  if Lexicons.TryFind(SYN_ATTR_SYMBOL, Lexicon) then
    UpdateHighlighter(atSymbol, Lexicon.Attributes);
  Comments := [csAsmStyle];
  DollarVariables := False;
  ActiveDot := True;
  Entity := True;
  DetectPreprocessor := True;
  StringDelim := sdDoubleQuote;
end;

end.
