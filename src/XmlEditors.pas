unit XmlEditors;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CustomTextEditors,    SynHighlighterXml;

type
  TXmlEditorFrame = class(TCustomTextEditorFrame)
  protected
    FHighlighter: TSynXmlSyn;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshConfig; override;
  end;

implementation

{$R *.lfm}

uses
  ConfigUtils, Configs, Searches;

{ TXmlEditorFrame }

constructor TXmlEditorFrame.Create(AOwner: TComponent);
begin
  FSyntax := ITEM_XML_SYNTAX;
  inherited Create(AOwner);
  FHighlighter := TSynXmlSyn.Create(Self);
  Editor.Highlighter := FHighlighter;
  ExporterHTML.Highlighter := FHighlighter;
  SearchCache.Filter := Config.XmlSyntax;
  SearchCache.Filters := SearchCache.Filter;
end;

procedure TXmlEditorFrame.RefreshConfig;
begin
  inherited;
  UpdateHighlighter(atIdentifier, FHighlighter.AttributeAttri);
  UpdateHighlighter(atIdentifier, FHighlighter.AttributeValueAttri);
  UpdateHighlighter(atIdentifier, FHighlighter.CDATAAttri);
  UpdateHighlighter(atIdentifier, FHighlighter.DocTypeAttri);
  UpdateHighlighter(atComment, FHighlighter.CommentAttri);
  UpdateHighlighter(atIdentifier, FHighlighter.ElementAttri);
  UpdateHighlighter(atIdentifier, FHighlighter.EntityRefAttri);
  UpdateHighlighter(atIdentifier, FHighlighter.NamespaceAttributeAttri);
  UpdateHighlighter(atIdentifier, FHighlighter.NamespaceAttributeValueAttri);
  UpdateHighlighter(atIdentifier, FHighlighter.ProcessingInstructionAttri);
  UpdateHighlighter(atWhitespace, FHighlighter.SpaceAttri);
  UpdateHighlighter(atSymbol, FHighlighter.SymbolAttri);
  UpdateHighlighter(atIdentifier, FHighlighter.TextAttri);
  FHighlighter.ElementAttri.Foreground:= clMaroon;
  FHighlighter.ElementAttri.Style:= [fsBold];
  FHighlighter.DocTypeAttri.Foreground:= clBlue;
  FHighlighter.DocTypeAttri.Style:= [fsItalic];
  FHighlighter.CDATAAttri.Foreground:= clOlive;
  FHighlighter.CDATAAttri.Style:= [fsItalic];
  FHighlighter.EntityRefAttri.Foreground:= clBlue;
  FHighlighter.EntityRefAttri.Style:= [fsbold];
  FHighlighter.ProcessingInstructionAttri.Foreground:= clBlue;
  FHighlighter.ProcessingInstructionAttri.Style:= [];
  FHighlighter.TextAttri.Foreground:= clBlack;
  FHighlighter.TextAttri.Style:= [fsBold];
  FHighlighter.AttributeAttri.Foreground:= clMaroon;
  FHighlighter.AttributeAttri.Style:= [];
  FHighlighter.AttributeAttri.Foreground:= clRed;
  FHighlighter.AttributeAttri.Style:= [];
  FHighlighter.AttributeValueAttri.Foreground:= clNavy;
  FHighlighter.AttributeValueAttri.Style:= [fsBold];
  FHighlighter.AttributeValueAttri.Foreground:= clRed;
  FHighlighter.AttributeValueAttri.Style:= [fsBold];
  FHighlighter.CommentAttri.Background:= clSilver;
  FHighlighter.CommentAttri.Foreground:= clGray;
  FHighlighter.CommentAttri.Style:= [fsbold, fsItalic];
  FHighlighter.SymbolAttri.Foreground:= clBlue;
  FHighlighter.SymbolAttri.Style:= [];
end;

end.

