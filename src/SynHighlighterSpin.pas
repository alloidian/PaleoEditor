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

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Graphics, SynEditHighlighter, SynHighlighterFlex;

const
  SYN_ATTR_CONSTANT = 13;
  SYN_ATTR_CONFIG   = 14;
  SYN_ATTR_TYPE     = 15;
  SYN_ATTR_FLOW     = 16;
  SYN_ATTR_MEMORY   = 17;
  SYN_ATTR_REGISTER = 18;
  SYN_ATTR_DECLARE  = 19;
  SYN_ATTR_DIRECT   = 20;

type
  TSynSpinSyn = class(TSynFlexSyn)
  public
    constructor Create(AOwner: TComponent); override;
    property ConstantAttribute: TSynHighlighterAttributes index SYN_ATTR_CONSTANT read GetDefaultAttribute;
    property ConfigAttribute: TSynHighlighterAttributes index SYN_ATTR_CONFIG read GetDefaultAttribute;
    property TypeAttribute: TSynHighlighterAttributes index SYN_ATTR_TYPE read GetDefaultAttribute;
    property FlowAttribute: TSynHighlighterAttributes index SYN_ATTR_FLOW read GetDefaultAttribute;
    property MemoryAttribute: TSynHighlighterAttributes index SYN_ATTR_MEMORY read GetDefaultAttribute;
    property RegisterAttribute: TSynHighlighterAttributes index SYN_ATTR_REGISTER read GetDefaultAttribute;
    property DeclarationAttribute: TSynHighlighterAttributes index SYN_ATTR_DECLARE read GetDefaultAttribute;
    property DirectAttribute: TSynHighlighterAttributes index SYN_ATTR_DIRECT read GetDefaultAttribute;
  end;

implementation

uses
  SynEditStrConst, ConfigUtils;

const
  ConstCodes: string =
    'true false posx negx pi ';

  ConfigCodes: string =
    'chipver clkmode _clkmode clkfreq _clkfreq _xinfreq _stack _free rcfast ' +
    'rcslow xinput xtal1 xtal2 xtal3 pll1x pll2x pll4x pll8x pll16x';

  TypeCodes: string =
    'byte word long';

  FlowCodes: string =
    'if elseif elseifnot else ifnot case other repeat from to step until while next ' +
    'quit return abort';

  MemoryCodes: string =
    'bytefill wordfill longfill bytemove wordmove longmove lookup lookupz lookdown ' +
    'lookdownz strsize strcomp';

  OpCodes: string =
    'wrbyte rdbyte wrword rdword wrlong rdlong hubop clkset cogid coginit cogstop locknew ' +
    'lockret lockset lockclr mul muls enc ones ror rol shr shl rcr rcl sar rev mins maxs ' +
    'min max movs movd movi jmpret jmp call ret test testin and andn or xor muxc muxnc muxz ' +
    'muxnz add sub cmp addabs subabs sumc sumz sumnz mov neg abs absneg negc negnc negz negnz ' +
    'cmps cmpsx addx subx cmpx adds subs addsx subsx cmpsub djnz tjnz tjz waitpeq waitpne ' +
    'waitcnt waitvid nop';

  RegCodes: string =
    'dira ina dirb inb outa outb cnt ctra ctrb frqa frqb phsa phsb vcfg vscl par spr';

  DeclareCodes: string =
    'CON VAR PUB PRI OBJ DAT';

  DirectCodes: string =
    'cognew reboot string constant float round trunc file result not if_always if_never if_e ' +
    'if_ne if_a if_b if_ae if_be if_c if_nc if_z if_nz if_c_eq_z if_c_ne_z if_c_and_z if_c_and_nz ' +
    'if_nc_and_z if_nc_and_nz if_c_or_z if_c_or_nz if_nc_or_z if_nc_or_nz if_z_eq_c if_z_ne_c ' +
    'if_z_and_c if_z_and_nc if_nz_and_c if_nz_and_nc if_z_or_c if_z_or_nc if_nz_or_c if_nz_or_nc ' +
    'fit org res wc wz wr nr';

    {cogid;coginit;cogstop;locknew;lockret;lockclr;lockset;waitcnt;waitpeq;waitpne;waitvid;and;or;}

{ TSynSpinSyn }

constructor TSynSpinSyn.Create(AOwner: TComponent);
var
  Lexicon: TLexicon;
  Temp: TSynHighlighterAttributes;
begin
  inherited Create(AOwner);
  Temp := Lexicons.Add(SYNS_AttrConstant, SYNS_AttrConstant, SYN_ATTR_CONSTANT, tkLex0, False);
  AddAttribute(Temp);
  if Lexicons.TryFind(SYN_ATTR_CONSTANT, Lexicon) then begin
    Lexicon.Import(ConstCodes);
    UpdateHighlighter(atConstant, Lexicon.Attributes);
  end;
  Temp := Lexicons.Add(SYNS_AttrConfig, SYNS_AttrConfig, SYN_ATTR_CONFIG, tkLex1, False);
  AddAttribute(Temp);
  if Lexicons.TryFind(SYN_ATTR_CONFIG, Lexicon) then begin
    Lexicon.Import(ConfigCodes);
    UpdateHighlighter(atConfig, Lexicon.Attributes);
  end;
  Temp := Lexicons.Add(SYNS_AttrDataType, SYNS_XML_AttrDataType, SYN_ATTR_TYPE, tkLex2, False);
  AddAttribute(Temp);
  if Lexicons.TryFind(SYN_ATTR_TYPE, Lexicon) then begin
    Lexicon.Import(TypeCodes);
    UpdateHighlighter(atDataType, Lexicon.Attributes);
  end;
  Temp := Lexicons.Add(SYNS_AttrFlow, SYNS_AttrFlow, SYN_ATTR_FLOW, tkLex3, False);
  AddAttribute(Temp);
  if Lexicons.TryFind(SYN_ATTR_FLOW, Lexicon) then begin
    Lexicon.Import(FlowCodes);
    UpdateHighlighter(atFlow, Lexicon.Attributes);
  end;
  Temp := Lexicons.Add(SYNS_AttrMemory, SYNS_AttrMemory, SYN_ATTR_MEMORY, tkLex4, False);
  AddAttribute(Temp);
  if Lexicons.TryFind(SYN_ATTR_MEMORY, Lexicon) then begin
    Lexicon.Import(MemoryCodes);
    UpdateHighlighter(atMemory, Lexicon.Attributes);
  end;
  if Lexicons.TryFind(SYN_ATTR_KEYWORD, Lexicon) then begin
    Lexicon.Import(OpCodes);
    UpdateHighlighter(atKeyword, Lexicon.Attributes);
  end;
  Temp := Lexicons.Add(SYNS_AttrRegister, SYNS_XML_AttrRegister, SYN_ATTR_REGISTER, tkLex5, False);
  AddAttribute(Temp);
  if Lexicons.TryFind(SYN_ATTR_REGISTER, Lexicon) then begin
    Lexicon.Import(RegCodes);
    UpdateHighlighter(atRegister, Lexicon.Attributes);
  end;
  Temp := Lexicons.Add(SYNS_AttrDeclaration, SYNS_AttrDeclaration, SYN_ATTR_DECLARE, tkLex6, False);
  AddAttribute(Temp);
  if Lexicons.TryFind(SYN_ATTR_DECLARE, Lexicon) then begin
    Lexicon.Import(DeclareCodes);
    UpdateHighlighter(atDeclaration, Lexicon.Attributes);
  end;
  Temp := Lexicons.Add('Direct', 'Direct', SYN_ATTR_DIRECT, tkLex7, False);
  AddAttribute(Temp);
  if Lexicons.TryFind(SYN_ATTR_DIRECT, Lexicon) then begin
    Lexicon.Import(DirectCodes);
    Lexicon.Attributes.Foreground := clBlack;
    Lexicon.Attributes.Style := [fsBold];
  end;
  if Lexicons.TryFind(SYN_ATTR_COMMENT, Lexicon) then begin
    UpdateHighlighter(atComment, Lexicon.Attributes);
  end;
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
  Comments := [csAnsiStyle, csPasStyle, csCStyle, csAsmStyle, csBasStyle, csVBStyle];
  DollarVariables := False;
  ActiveDot := False;
  Entity := True;
  DetectPreprocessor := True;
  StringDelim := sdDoubleQuote;
end;

end.
