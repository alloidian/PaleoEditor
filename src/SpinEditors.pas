unit SpinEditors;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CustomTextEditors, SynHighlighterSpin;

type
  TSpinEditorFrame = class(TCustomTextEditorFrame)
    procedure CompletionExecute(Sender: TObject);
  protected
    FHighlighter: TSynSpinSyn;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshConfig; override;
  end;

implementation

{$R *.lfm}

uses
  ConfigUtils, Configs, Searches;

{ TSpinEditorFrame }

constructor TSpinEditorFrame.Create(AOwner: TComponent);
begin
  FSyntax := ITEM_SPIN_SYNTAX;
  inherited Create(AOwner);
  FHighlighter := TSynSpinSyn.Create(Self);
  Editor.Highlighter := FHighlighter;
  ExporterHTML.Highlighter := FHighlighter;
  SearchCache.Filter := Config.SpinSyntax;
  SearchCache.Filters := SearchCache.Filter;
end;

procedure TSpinEditorFrame.CompletionExecute(Sender: TObject);
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
  inherited;
  UpdateItems(TOKENS);
end;

procedure TSpinEditorFrame.RefreshConfig;
begin
  inherited;
  UpdateHighlighter(atDirective, FHighlighter.DotAttribute);
  UpdateHighlighter(atNumber, FHighlighter.EntityAttribute);
  UpdateHighlighter(atNumber, FHighlighter.NumberAttribute);
  UpdateHighlighter(atDirective, FHighlighter.PreprocessorAttribute);
  UpdateHighlighter(atRegister, FHighlighter.RegisterAttribute);
  UpdateHighlighter(atConfig, FHighlighter.ConfigAttribute);
  UpdateHighlighter(atConstant, FHighlighter.ConstantAttribute);
  UpdateHighlighter(atDeclaration, FHighlighter.DeclarationAttribute);
  UpdateHighlighter(atDirective, FHighlighter.DirectAttribute);
  UpdateHighlighter(atFlow, FHighlighter.FlowAttribute);
  UpdateHighlighter(atMemory, FHighlighter.MemoryAttribute);
  UpdateHighlighter(atDataType, FHighlighter.TypeAttribute);
end;

end.

