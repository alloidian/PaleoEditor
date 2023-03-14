unit AssemblyEditors;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CustomTextEditors;

type

  { TAssemblyEditorFrame }

  TAssemblyEditorFrame = class(TCustomTextEditorFrame)
    procedure CompletionExecute(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  SynHighlighterZ80;

{ TAssemblyEditorFrame }

constructor TAssemblyEditorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHighlighter := TSynZ80Syn.Create(Self);
  Editor.Highlighter := FHighlighter;
  ExporterHTML.Highlighter := FHighlighter;
end;

procedure TAssemblyEditorFrame.CompletionExecute(Sender: TObject);
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
  inherited;
  UpdateItems(TOKENS);
end;

end.

