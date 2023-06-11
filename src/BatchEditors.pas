unit BatchEditors;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CustomTextEditors, SynHighlighterBat;

type
  TBatchEditorFrame = class(TCustomTextEditorFrame)
  public
    FHighlighter: TSynBatSyn;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshConfig; override;
  end;

implementation

{$R *.lfm}

uses
  ConfigUtils, Configs, Searches;

{ TBatchEditorFrame }

constructor TBatchEditorFrame.Create(AOwner: TComponent);
begin
  FSyntax := ITEM_BATCH_SYNTAX;
  inherited Create(AOwner);
  FHighlighter := TSynBatSyn.Create(Self);
  Editor.Highlighter := FHighlighter;
  ExporterHTML.Highlighter := FHighlighter;
  SearchCache.Filter := Config.BatchSyntax;
  SearchCache.Filters := SearchCache.Filter;
end;

procedure TBatchEditorFrame.RefreshConfig;
begin
  inherited;
  UpdateHighlighter(atNumber, FHighlighter.NumberAttri);
end;

end.

