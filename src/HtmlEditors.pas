unit HtmlEditors;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CustomPreviewEditors,
  HtmlView, SynHighlighterHtml;

type

  { THtmlEditorFrame }

  THtmlEditorFrame = class(TCustomPreviewEditorFrame)
    procedure PagesChange(Sender: TObject);
  private
    FHash: String;
  protected
    FViewer: THtmlViewer;
  protected
    FHighlighter: TSynHTMLSyn;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshConfig; override;
  end;

implementation

{$R *.lfm}

uses
  ComCtrls, HtmlBuffer, MD5, Searches, ConfigUtils, Configs;

{ THtmlEditorFrame }

constructor THtmlEditorFrame.Create(AOwner: TComponent);
begin
  FSyntax := ITEM_HTML_SYNTAX;
  inherited Create(AOwner);
  FHash := EmptyStr;
  FHighlighter := TSynHTMLSyn.Create(Self);
  Editor.Highlighter := FHighlighter;
  ExporterHTML.Highlighter := FHighlighter;
  FViewer := THtmlViewer.Create(Self);
  FViewer.Parent := PreviewPage;
  FViewer.Align := alClient;
  SearchCache.Filter := Config.HtmlSyntax;
  SearchCache.Filters := SearchCache.Filter;
end;

procedure THtmlEditorFrame.PagesChange(Sender: TObject);
var
  Value: String;
  Hash: String;
begin
  if (Sender as TPageControl).PageIndex = 1 then begin
    Value := Editor.Lines.Text;
    Hash := MD5Print(MD5String(Value));
    if not AnsiSameText(FHash, Hash) then begin
      FHash := Hash;
      FViewer.LoadFromString(TBuffer.Convert(UTF8Decode(Value), CP_UTF8));
    end;
  end;
end;

procedure THtmlEditorFrame.RefreshConfig;
begin
  inherited;
  UpdateHighlighter(atIdentifier, FHighlighter.IdentifierAttri);
  UpdateHighlighter(atKeyword,  FHighlighter.KeyAttri);
  UpdateHighlighter(atSymbol, FHighlighter.SymbolAttri);
  FHighlighter.ASPAttri.Foreground := clBlack;
  FHighlighter.ASPAttri.Background := clYellow;
  FHighlighter.CDATAAttri.Foreground := clGreen;
  FHighlighter.DOCTYPEAttri.Foreground := clBlack;
  FHighlighter.DOCTYPEAttri.Background := clYellow;
  FHighlighter.DOCTYPEAttri.Style := [fsBold];
  FHighlighter.UndefKeyAttri.Style := [fsBold];
  FHighlighter.UndefKeyAttri.Foreground := clRed;
  FHighlighter.ValueAttri.Foreground := clDarkOrange;
  FHighlighter.AndAttri.Style := [fsBold];
  FHighlighter.AndAttri.Foreground := clLime;
end;

end.

