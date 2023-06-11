unit MarkdownEditors;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CustomPreviewEditors, HtmlView,
  SynHighlighterMD;

type

  { TMarkdownEditorFrame }

  TMarkdownEditorFrame = class(TCustomPreviewEditorFrame)
    procedure PagesChange(Sender: TObject);
  private
    FHash: String;
  protected
    FViewer: THtmlViewer;
    FHighlighter: TSynMDSyn;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshConfig; override;
  end;

implementation

{$R *.lfm}

uses
  ComCtrls, HtmlBuffer, MD5, MarkdownProcessor, Searches, ConfigUtils,
  Configs;

{ TMarkdownEditorFrame }

constructor TMarkdownEditorFrame.Create(AOwner: TComponent);
begin
  FSyntax := ITEM_MARKDOWN_SYNTAX;
  inherited Create(AOwner);
  FHash := EmptyStr;
  FHighlighter := TSynMDSyn.Create(Self);
  Editor.Highlighter := FHighlighter;
  ExporterHTML.Highlighter := FHighlighter;
  FViewer := THtmlViewer.Create(Self);
  FViewer.Parent := PreviewPage;
  FViewer.Align := alClient;
  SearchCache.Filter := Config.MarkdownSyntax;
  SearchCache.Filters := SearchCache.Filter;
end;

procedure TMarkdownEditorFrame.PagesChange(Sender: TObject);
const
  DECORATION1 =
    '<!DOCTYPE html>'#13#10 +
    '<html>'#13#10 +
    '<head>'#13#10 +
    '  <meta charset="utf-8">'#13#10 +
    '  <title>Steve Garcia</title>'#13#10 +
    '  <style>'#13#10 +
    '    h1 {font-family: helvetica}'#13#10 +
    '    h2 {font-family: helvetica}'#13#10 +
    '    p {font-family: helvetica}'#13#10 +
    '    li {font-family: helvetica}'#13#10 +
    '    tr {font-family: helvetica}'#13#10 +
    '  </style>'#13#10 +
    '</head>'#13#10;
DECORATION2 =
    '</body>'#13#10 +
    '</html>';
var
  Value: String;
  Hash: String;
  MD: TMarkdownProcessor;

  procedure SaveString(InString, OutFilePath: string);
  var
    F: TextFile;
  begin
    AssignFile(F, OutFilePath);
    try
      ReWrite(F);
      Write(F, InString);
    finally
      CloseFile(F);
    end;
  end;

begin
  if (Sender as TPageControl).PageIndex = 1 then begin
    Value := Editor.Lines.Text;
    Hash := MD5Print(MD5String(Value));
    if not AnsiSameText(FHash, Hash) then begin
      FHash := Hash;
      MD := TMarkdownProcessor.CreateDialect(mdCommonMark);
      try
        Value := DECORATION1 + MD.process(Value) + DECORATION2;
      finally
        MD.Free;
      end;
      SaveString(Value, 'C:\Dev\Readme.html');
      FViewer.LoadFromString(TBuffer.Convert(UTF8Decode(Value), CP_UTF8));
    end;
  end;
end;

procedure TMarkdownEditorFrame.RefreshConfig;
begin
  inherited;
  UpdateHighlighter(atSymbol, FHighlighter.Symbols);
  UpdateHighlighter(atNumber, FHighlighter.Number);
  UpdateHighlighter(atWhitespace, FHighlighter.Space);
  UpdateHighlighter(atComment, FHighlighter.Comment);
  UpdateHighlighter(atIdentifier, FHighlighter.Text);
end;

end.

