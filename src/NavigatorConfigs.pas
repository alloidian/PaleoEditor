unit NavigatorConfigs;

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

{$MODE DELPHI}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit, StdCtrls,
  CustomConfigFrames, ConfigUtils;

type

  { TNavigatorConfigFrame }

  TNavigatorConfigFrame = class(TCustomConfigFrame)
    ConfigEdit: TValueListEditor;
    SaveWorkspaceEdit: TCheckBox;
    procedure ConfigEditButtonClick(Sender: TObject; aCol, aRow: Integer);
  private
    FEditFiles: String;
    FAssemblyFiles: String;
    FExecFiles: String;
    FSearchFiles: String;
    FUneditableFiles: String;
    FSaveWorkspace: Boolean;
    FExcludeFiles: String;
    FExcludeFolders: String;
    FAssemblySyntax: String;
    FBasicSyntax: String;
    FBatchSyntax: String;
    FHtmlSyntax: String;
    FImageSyntax: String;
    FIniSyntax: String;
    FIntelHexSyntax: String;
    FJsonSyntax: String;
    FMarkdownSyntax: String;
    FPascalSyntax: String;
    FPdfSyntax: String;
    FRtfSyntax: String;
    FSpinSyntax: String;
    FTextSyntax: String;
    FXmlSyntax: String;
    FZipSyntax: String;
  protected
    function GetIsModified: Boolean; override;
    function GetEditFiles: String;
    procedure SetEditFiles(const Value: String);
    function GetAssemlbyFiles: String;
    procedure SetAssemblyFiles(const Value: String);
    function GetExecFiles: String;
    procedure SetExecFiles(const Value: String);
    function GetSearchFiles: String;
    procedure SetSearchFiles(const Value: String);
    function GetUneditableFiles: String;
    procedure SetUneditableFiles(const Value: String);
    function GetSaveWorkspace: Boolean;
    procedure SetSaveWorkspace(Value: Boolean);
    function GetExcludeFiles: String;
    procedure SetExcludeFiles(const Value: String);
    function GetExcludeFolders: String;
    procedure SetExcludeFolders(const Value: String);
    function GetAssemblySyntax: String;
    procedure SetAssemblySyntax(const Value: String);
    function GetBasicSyntax: String;
    procedure SetBasicSyntax(const Value: String);
    function GetBatchSyntax: String;
    procedure SetBatchSyntax(const Value: String);
    function GetHtmlSyntax: String;
    procedure SetHtmlSyntax(const Value: String);
    function GetImageSyntax: String;
    procedure SetImageSyntax(const Value: String);
    function GetIniSyntax: String;
    procedure SetIniSyntax(const Value: String);
    function GetIntelHexSyntax: String;
    procedure SetIntelHexSyntax(const Value: String);
    function GetJsonSyntax: String;
    procedure SetJsonSyntax(const Value: String);
    function GetMarkdownSyntax: String;
    procedure SetMarkdownSyntax(const Value: String);
    function GetPascalSyntax: String;
    procedure SetPascalSyntax(const Value: String);
    function GetPdfSyntax: String;
    procedure SetPdfSyntax(const Value: String);
    function GetRtfSyntax: String;
    procedure SetRtfSyntax(const Value: String);
    function GetSpinSyntax: String;
    procedure SetSpinSyntax(const Value: String);
    function GetTextSyntax: String;
    procedure SetTextSyntax(const Value: String);
    function GetXmlSyntax: String;
    procedure SetXmlSyntax(const Value: String);
    function GetZipSyntax: String;
    procedure SetZipSyntax(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ReadConfig(Config: TConfig); override;
    procedure WriteConfig(Config: TConfig); override;
    property EditFiles: String read GetEditFiles write SetEditFiles;
    property AssemblyFiles: String read GetAssemlbyFiles write SetAssemblyFiles;
    property ExecFiles: String read GetExecFiles write SetExecFiles;
    property SearchFiles: String read GetSearchFiles write SetSearchFiles;
    property UneditableFiles: String read GetUneditableFiles write SetUneditableFiles;
    property SaveWorkspace: Boolean read GetSaveWorkspace write SetSaveWorkspace;
    property ExcludeFiles: String read GetExcludeFiles write SetExcludeFiles;
    property ExcludeFolders: String read GetExcludeFolders write SetExcludeFolders;
    property AssemblySyntax: String read GetAssemblySyntax write SetAssemblySyntax;
    property BasicSyntax: String read GetBasicSyntax write SetBasicSyntax;
    property BatchSyntax: String read GetBatchSyntax write SetBatchSyntax;
    property HtmlSyntax: String read GetHtmlSyntax write SetHtmlSyntax;
    property ImageSyntax: String read GetImageSyntax write SetImageSyntax;
    property IniSyntax: String read GetIniSyntax write SetIniSyntax;
    property IntelHexSyntax: String read GetIntelHexSyntax write SetIntelHexSyntax;
    property JsonSyntax: String read GetJsonSyntax write SetJsonSyntax;
    property MarkdownSyntax: String read GetMarkdownSyntax write SetMarkdownSyntax;
    property PascalSyntax: String read GetPascalSyntax write SetPascalSyntax;
    property PdfSyntax: String read GetPdfSyntax write SetPdfSyntax;
    property RtfSyntax: String read GetRtfSyntax write SetRtfSyntax;
    property SpinSyntax: String read GetSpinSyntax write SetSpinSyntax;
    property TextSyntax: String read GetTextSyntax write SetTextSyntax;
    property XmlSyntax: String read GetXmlSyntax write SetXmlSyntax;
    property ZipSyntax: String read GetZipSyntax write SetZipSyntax;
  end;

implementation

{$R *.lfm}

uses
  FileMasks, TypInfo;

{ TNavigatorConfigFrame }

constructor TNavigatorConfigFrame.Create(AOwner: TComponent);
var
  I: Integer = 0;
begin
  inherited Create(AOwner);
  ConfigEdit.Strings.Clear;
  ConfigEdit.Strings.AddPair(ITEM_EDIT, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_ASSEMBLE, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_EXEC, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_SEARCH, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_UNEDITABLE, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_EXCLUDE_FILE, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_EXCLUDE_FOLDER, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_ASSEMBLY_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_BASIC_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_BATCH_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_HTML_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_IMAGE_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_INI_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_INTEL_HEX_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_JSON_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_MARKDOWN_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_PASCAL_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_PDF_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_RTF_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_SPIN_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_TEXT_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_XML_SYNTAX, EmptyStr);
  ConfigEdit.Strings.AddPair(ITEM_ZIP_SYNTAX, EmptyStr);
  for I := 0 to ConfigEdit.RowCount - 2 do
    ConfigEdit.ItemProps[I].EditStyle := esEllipsis;
  ConfigEdit.DefaultColWidth := 100;
end;

procedure TNavigatorConfigFrame.ConfigEditButtonClick(Sender: TObject; aCol, aRow: Integer);
const
  DEFAULTS: array[0..22] of String =
   (INI_EDIT_DEF,
    INI_ASSEMBLE_DEF,
    INI_EXEC_DEF,
    INI_SEARCH_DEF,
    INI_UNEDITABLE_DEF,
    INI_EXCLUDE_FILE_DEF,
    INI_EXCLUDE_FOLDER_DEF,
    ITEM_ASSEMBLY_SYNTAX_DEF,
    ITEM_BASIC_SYNTAX_DEF,
    ITEM_BATCH_SYNTAX_DEF,
    ITEM_HTML_SYNTAX_DEF,
    ITEM_IMAGE_SYNTAX_DEF,
    ITEM_INI_SYNTAX_DEF,
    ITEM_INTEL_HEX_SYNTAX_DEF,
    ITEM_JSON_SYNTAX_DEF,
    ITEM_MARKDOWN_SYNTAX_DEF,
    ITEM_PASCAL_SYNTAX_DEF,
    ITEM_PDF_SYNTAX_DEF,
    ITEM_RTF_SYNTAX_DEF,
    ITEM_SPIN_SYNTAX_DEF,
    ITEM_TEXT_SYNTAX_DEF,
    ITEM_XML_SYNTAX_DEF,
    ITEM_ZIP_SYNTAX_DEF);
var
  Editor: TValueListEditor;
  Row: Integer = 0;
  Temp: String = '';
begin
  Editor := Sender as TValueListEditor;
  Row := aRow - 1;
  Temp := Editor.Strings.ValueFromIndex[Row];
  if EditFileMasks(Editor.Strings.Names[Row], Temp, DEFAULTS[Row]) then
    Editor.Strings.ValueFromIndex[Row] := Temp;
end;

function TNavigatorConfigFrame.GetIsModified: Boolean;
begin
  Result := not AnsiSameText(EditFiles, FEditFiles) or
    not AnsiSameText(AssemblyFiles, FAssemblyFiles) or
    not AnsiSameText(ExecFiles, FExecFiles) or
    not AnsiSameText(SearchFiles, FSearchFiles) or
    not AnsiSameText(UneditableFiles, FUneditableFiles) or
    (SaveWorkspace <> FSaveWorkspace) or
    not AnsiSameText(ExcludeFiles, FExcludeFiles) or
    not AnsiSameText(ExcludeFolders, FExcludeFolders) or
    not AnsiSameText(AssemblySyntax, FAssemblySyntax) or
    not AnsiSameText(BasicSyntax, FBasicSyntax) or
    not AnsiSameText(BatchSyntax, FBatchSyntax) or
    not AnsiSameText(HtmlSyntax, FHtmlSyntax) or
    not AnsiSameText(ImageSyntax, FImageSyntax) or
    not AnsiSameText(IniSyntax, FIniSyntax) or
    not AnsiSameText(IntelHexSyntax, FIntelHexSyntax) or
    not AnsiSameText(JsonSyntax, FJsonSyntax) or
    not AnsiSameText(MarkdownSyntax, FMarkdownSyntax) or
    not AnsiSameText(PascalSyntax, FPascalSyntax) or
    not AnsiSameText(PdfSyntax, FPdfSyntax) or
    not AnsiSameText(RtfSyntax, FRtfSyntax) or
    not AnsiSameText(SpinSyntax, FSpinSyntax) or
    not AnsiSameText(TextSyntax, FTextSyntax) or
    not AnsiSameText(XmlSyntax, FXmlSyntax) or
    not AnsiSameText(ZipSyntax, FZipSyntax);
end;

function TNavigatorConfigFrame.GetEditFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_EDIT];
end;

procedure TNavigatorConfigFrame.SetEditFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_EDIT] := Value;
  FEditFiles := Value;
end;

function TNavigatorConfigFrame.GetAssemlbyFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_ASSEMBLE];
end;

procedure TNavigatorConfigFrame.SetAssemblyFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_ASSEMBLE] := Value;
  FAssemblyFiles := Value;
end;

function TNavigatorConfigFrame.GetExecFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_EXEC];
end;

procedure TNavigatorConfigFrame.SetExecFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_EXEC] := Value;
  FExecFiles := Value;
end;

function TNavigatorConfigFrame.GetSearchFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_SEARCH];
end;

procedure TNavigatorConfigFrame.SetSearchFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_SEARCH] := Value;
  FSearchFiles := Value;
end;

function TNavigatorConfigFrame.GetUneditableFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_UNEDITABLE];
end;

procedure TNavigatorConfigFrame.SetUneditableFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_UNEDITABLE] := Value;
  FUneditableFiles := Value;
end;

function TNavigatorConfigFrame.GetSaveWorkspace: Boolean;
begin
  Result := SaveWorkspaceEdit.Checked;
end;

procedure TNavigatorConfigFrame.SetSaveWorkspace(Value: Boolean);
begin
  SaveWorkspaceEdit.Checked := Value;
  FSaveWorkspace := Value;
end;

function TNavigatorConfigFrame.GetExcludeFiles: String;
begin
  Result := ConfigEdit.Values[ITEM_EXCLUDE_FILE];
end;

procedure TNavigatorConfigFrame.SetExcludeFiles(const Value: String);
begin
  ConfigEdit.Values[ITEM_EXCLUDE_FILE] := Value;
  FExcludeFiles := Value;
end;

function TNavigatorConfigFrame.GetExcludeFolders: String;
begin
  Result := ConfigEdit.Values[ITEM_EXCLUDE_FOLDER];
end;

procedure TNavigatorConfigFrame.SetExcludeFolders(const Value: String);
begin
  ConfigEdit.Values[ITEM_EXCLUDE_FOLDER] := Value;
  FExcludeFolders := Value;
end;

function TNavigatorConfigFrame.GetAssemblySyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_ASSEMBLY_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetAssemblySyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_ASSEMBLY_SYNTAX] := Value;
  FAssemblySyntax := Value;
end;

function TNavigatorConfigFrame.GetBasicSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_BASIC_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetBasicSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_BASIC_SYNTAX] := Value;
  FBasicSyntax := Value;
end;

function TNavigatorConfigFrame.GetBatchSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_BATCH_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetBatchSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_BATCH_SYNTAX] := Value;
  FBatchSyntax := Value;
end;

function TNavigatorConfigFrame.GetHtmlSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_HTML_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetHtmlSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_HTML_SYNTAX] := Value;
  FHtmlSyntax := Value;
end;

function TNavigatorConfigFrame.GetImageSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_IMAGE_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetImageSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_IMAGE_SYNTAX] := Value;
  FImageSyntax := Value;
end;

function TNavigatorConfigFrame.GetIniSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_INI_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetIniSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_INI_SYNTAX] := Value;
  FIniSyntax := Value;
end;

function TNavigatorConfigFrame.GetIntelHexSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_INTEL_HEX_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetIntelHexSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_INTEL_HEX_SYNTAX] := Value;
  FIntelHexSyntax := Value;
end;

function TNavigatorConfigFrame.GetJsonSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_JSON_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetJsonSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_JSON_SYNTAX] := Value;
  FJsonSyntax := Value;
end;

function TNavigatorConfigFrame.GetMarkdownSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_MARKDOWN_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetMarkdownSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_MARKDOWN_SYNTAX] := Value;
  FMarkdownSyntax := Value;
end;

function TNavigatorConfigFrame.GetPascalSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_PASCAL_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetPascalSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_PASCAL_SYNTAX] := Value;
  FPascalSyntax := Value;
end;

function TNavigatorConfigFrame.GetPdfSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_PDF_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetPdfSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_PDF_SYNTAX] := Value;
  FPdfSyntax := Value;
end;

function TNavigatorConfigFrame.GetRtfSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_RTF_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetRtfSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_RTF_SYNTAX] := Value;
  FRtfSyntax := Value;
end;

function TNavigatorConfigFrame.GetSpinSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_SPIN_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetSpinSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_SPIN_SYNTAX] := Value;
  FSpinSyntax := Value;
end;

function TNavigatorConfigFrame.GetTextSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_TEXT_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetTextSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_TEXT_SYNTAX] := Value;
  FTextSyntax := Value;
end;

function TNavigatorConfigFrame.GetXmlSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_XML_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetXmlSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_XML_SYNTAX] := Value;
  FXmlSyntax := Value;
end;

function TNavigatorConfigFrame.GetZipSyntax: String;
begin
  Result := ConfigEdit.Values[ITEM_ZIP_SYNTAX];
end;

procedure TNavigatorConfigFrame.SetZipSyntax(const Value: String);
begin
  ConfigEdit.Values[ITEM_ZIP_SYNTAX] := Value;
  FZipSyntax := Value;
end;

procedure TNavigatorConfigFrame.ReadConfig(Config: TConfig);
begin
  EditFiles := Config.EditFiles;
  AssemblyFiles := Config.AssemblyFiles;
  ExecFiles := Config.ExecuteFiles;
  SearchFiles := Config.SearchFiles;
  UneditableFiles := Config.UneditableFiles;
  SaveWorkspace := Config.SaveWorkspace;
  ExcludeFiles := Config.ExcludeFiles;
  ExcludeFolders := Config.ExcludeFolders;
  AssemblySyntax := Config.AssemblySyntax;
  BasicSyntax := Config.BasicSyntax;
  BatchSyntax := Config.BatchSyntax;
  HtmlSyntax := Config.HtmlSyntax;
  ImageSyntax := Config.ImageSyntax;
  IniSyntax := Config.IniSyntax;
  IntelHexSyntax := Config.IntelHexSyntax;
  JsonSyntax := Config.JsonSyntax;
  MarkdownSyntax := Config.MarkdownSyntax;
  PascalSyntax := Config.PascalSyntax;
  PdfSyntax := Config.PdfSyntax;
  RtfSyntax := Config.RtfSyntax;
  SpinSyntax := Config.SpinSyntax;
  TextSyntax := Config.TextSyntax;
  XmlSyntax := Config.XmlSyntax;
  ZipSyntax := Config.ZipSyntax;
end;

procedure TNavigatorConfigFrame.WriteConfig(Config: TConfig);
begin
  Config.EditFiles := EditFiles;
  Config.AssemblyFiles := AssemblyFiles;
  Config.ExecuteFiles := ExecFiles;
  Config.SearchFiles := SearchFiles;
  Config.UneditableFiles := UneditableFiles;
  Config.SaveWorkspace := SaveWorkspace;
  Config.ExcludeFiles := ExcludeFiles;
  Config.ExcludeFolders := ExcludeFolders ;
  Config.AssemblySyntax := AssemblySyntax;
  Config.BasicSyntax := BasicSyntax;
  Config.BatchSyntax := BatchSyntax;
  Config.ImageSyntax := ImageSyntax;
  Config.HtmlSyntax := HtmlSyntax;
  Config.IniSyntax := IniSyntax;
  Config.IntelHexSyntax := IntelHexSyntax;
  Config.JsonSyntax := JsonSyntax;
  Config.MarkdownSyntax := MarkdownSyntax;
  Config.PascalSyntax := PascalSyntax;
  Config.PdfSyntax := PdfSyntax;
  Config.RtfSyntax := RtfSyntax;
  Config.SpinSyntax := SpinSyntax;
  Config.TextSyntax := TextSyntax;
  Config.XmlSyntax := XmlSyntax;
  Config.ZipSyntax := ZipSyntax;
end;

end.

