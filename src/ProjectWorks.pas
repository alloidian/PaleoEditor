unit ProjectWorks;

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
  Classes, SysUtils, Forms, Controls, ComCtrls, Contnrs, Dialogs, Menus, CustomWorks;

type

  { TProjectWorkForm }

  TProjectWorkForm = class(TCustomWorkForm)
    procedure FormCreate(Sender: TObject);
  private
  protected
    FProjectFolder: TFileName;
    procedure RefreshView; override;
  public
    procedure Open(const FolderName: TFileName; ParentMenu: TMenuItem); override;
  end;

  TListParser = class(TObject)
  private type
    TModules = class(TObject)
    private type
      TModule = class(TObject)
      private type
        TExtract = record
          Level: Integer;
          FileName: String;
          procedure Populate(const Text: String);
        end;
      private
        FModuleName: String;
        FLevel: Integer;
        FMayInsert: Boolean;
        FExtract: TExtract;
      public
        constructor Create(const Text: String); virtual;
        destructor Destroy; override;
        property ModuleName: String read FModuleName;
        property Level: Integer read FLevel;
        property MayInsert: Boolean read FMayInsert write FMayInsert;
      end;
    private
      FList: TObjectList;
    protected
      function GetCount: Integer;
      function GetItems(I: Integer): TModule;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Add(const Text: String);
      procedure Resolve;
      procedure SaveToFile(const FileName: TFileName);
      property Count: Integer read GetCount;
      property Items[I: Integer]:TModule read GetItems; default;
    end;
  protected
    FProjectFolder: TFileName;
    FFileName: TFileName;
    FHomeFolder: TFileName;
    FList: TModules;
    procedure Populate(const FileName: TFileName); overload;
  public
    constructor Create(const ProjectFolder: TFileName; Node: TTreeNode); virtual;
    destructor Destroy; override;
    procedure Populate(View: TTreeView); overload;
    property ProjectFolder: TFileName read FProjectFolder;
  end;

implementation

{$R *.lfm}

uses
  StrUtils, Utils, ConfigUtils, Configs;

{ TProjectWorkForm }

procedure TProjectWorkForm.FormCreate(Sender: TObject);
begin
  inherited;
  FConfigs := TProjectConfig.Create;
  NewFileAction.Visible := False;
  NewFolderAction.Visible := False;
  DeleteFolderAction.Visible := False;
  FileSeparator.Visible := False;
end;

procedure TProjectWorkForm.RefreshView;
var
  ParentNode: TTreeNode = nil;
  Parser: TListParser;
begin
  Parser := TListParser.Create(FFolderName, ParentNode);
  try
    Navigator.BeginUpdate;
    try
      Navigator.Items.Clear;
      Parser.Populate(Navigator);
    finally
      Navigator.EndUpdate;
    end;
    FProjectFolder := Parser.ProjectFolder;
  finally
    Parser.Free;
  end;
end;

procedure TProjectWorkForm.Open(const FolderName: TFileName; ParentMenu: TMenuItem);
begin
  Screen.BeginWaitCursor;
  try
    if FileExists(FolderName) then begin
      inherited Open(FolderName, ParentMenu);
      FConfigs.ReadConfig(FProjectFolder, FFolderName);
    end;
  finally
    Screen.EndWaitCursor;
  end;
end;

{ TListParser }

constructor TListParser.Create(const ProjectFolder: TFileName; Node: TTreeNode);
begin
  inherited Create;
  FProjectFolder := EmptyStr;
  FFileName := ProjectFolder;
  FHomeFolder := ExcludeTrailingPathDelimiter(ExtractFilePath(FFileName));
  FList := TModules.Create;
  Populate(FFileName);
end;

destructor TListParser.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TListParser.Populate(const FileName: TFileName);
const
  TOKEN = '#INCLUDE';
var
  List: TStringList;
  I: Integer = 0;
  Line: String = '';
begin
  if FileExists(FileName) then begin
    List := TStringList.Create;
    try
      List.LoadFromFile(FileName);
      for I := List.Count - 1 downto 0 do begin
        Line := List[I];
        if not AnsiContainsText(Line, TOKEN) then
          List.Delete(I)
        else
          if Line[12] = '~' then
            List.Delete(I);
      end;
      for I := 0 to List.Count - 1 do begin
        Line := List[I];
        FList.Add(Line);
      end;
      FList.Resolve;
    finally
      List.Free;
    end;
  end;
end;

procedure TListParser.Populate(View: TTreeView);
var
  Stack: TTreeNodeCache;
  Child: TTreeNode;
  Attribute: TFileAttribute;
  I: Integer = 0;
  Module: TModules.TModule;

  function ExtractDocFileName(const PathName: String): String;
  const
    ASM_EXTENSION = '.asm';
    DELIMITER = '_';
  var
    FolderName: TFileName = '';
    FileName: TFileName = '';
  begin
    FolderName := ExtractFilePath(PathName);
    FileName := ExtractFileName(PathName);
    if AnsiContainsStr(FileName, DELIMITER) then
      FileName := AnsiLeftStr(FileName, Pos(DELIMITER, FileName) - 1);
    FileName := ChangeFileExt(FileName, ASM_EXTENSION);
    Result := FolderName + FileName;
  end;

  function FullPath(HomeFolder: String; FileName: String): String;
  begin
    HomeFolder := ExcludeTrailingPathDelimiter(HomeFolder);
    while AnsiStartsStr('..\', FileName) do begin
      HomeFolder := ExcludeTrailingPathDelimiter(ExtractFilePath(HomeFolder));
      FileName := AnsiRightStr(FileName, FileName.Length - 3);
    end;
    Result := HomeFolder + DirectorySeparator + FileName;
  end;

  function GetBaseFolder(const HomeFolder: String; FileName: String): TFileName;
  begin
    FileName := ExcludeTrailingPathDelimiter(ExtractFilePath(FileName));
    if HomeFolder.IsEmpty then
      Result := FileName
    else
      if HomeFolder.Length > FileName.Length then
        Result := FileName
      else
        Result := HomeFolder;
  end;

begin
  if (FList.Count > 0) and Assigned(View) then begin
    FProjectFolder := GetBaseFolder(EmptyStr, FFileName);
    FProjectFolder := GetBaseFolder(FProjectFolder, ExtractDocFileName(FFileName));
    for I := 0 to FList.Count - 1 do begin
      Module := FList[I];
      FProjectFolder := GetBaseFolder(FProjectFolder, FullPath(FHomeFolder, Module.ModuleName));
    end;
    Stack := TTreeNodeCache.Create;
    try
      Attribute := TFileAttribute.CreateDocument(FFileName, FProjectFolder);
      Child := View.Items.AddChild(nil, Attribute.ShortName);
      Child.ImageIndex := WHITE_DOCUMENT_INDEX;
      Child.SelectedIndex := BLACK_DOCUMENT_INDEX ;
      Child.Data := Attribute;
      Stack.Add(Child);
      Attribute := TFileAttribute.CreateDocument(ExtractDocFileName(FFileName), FProjectFolder);
      Child := View.Items.AddChild(nil, Attribute.ShortName);
      Child.ImageIndex := WHITE_DOCUMENT_INDEX;
      Child.SelectedIndex := BLACK_DOCUMENT_INDEX;
      Child.Data := Attribute;
      Stack.Add(Child);
      for I := 0 to FList.Count - 1 do begin
        Module := FList[I];
        Attribute := TFileAttribute.CreateDocument(FullPath(FHomeFolder, Module.ModuleName), FProjectFolder);
        Child := View.Items.AddChild(Stack.Node[Module.Level + 1], Attribute.ShortName);
        Child.ImageIndex := WHITE_DOCUMENT_INDEX;
        Child.SelectedIndex := BLACK_DOCUMENT_INDEX ;
        Child.Data := Attribute;
        if Module.MayInsert then
          Stack.Add(Child);
      end;
    finally
      Stack.Free;
    end;
  end;
end;

{ TListParser.TModules }

constructor TListParser.TModules.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TListParser.TModules.Destroy;
begin
  FList.Free;
  inherited;
end;

function TListParser.TModules.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TListParser.TModules.GetItems(I: Integer): TModule;
begin
  if I < FList.Count then
    Result := TModule(FList[I])
  else
    Result := nil;
end;

procedure TListParser.TModules.Add(const Text: String);
begin
  FList.Add(TModule.Create(Text));
end;

procedure TListParser.TModules.Resolve;
var
  I: Integer = 0;
  Mod1: TModule;
  Mod2: TModule;
begin
  for I := 0 to FList.Count - 2 do begin
    Mod1 := Items[I];
    Mod2 := Items[I + 1];
    Mod1.FMayInsert := Mod2.Level - Mod1.Level > 0;
  end;
end;

procedure TListParser.TModules.SaveToFile(const FileName: TFileName);
const
  MASK = '%s'#9'%d'#9'%s';
var
  Temp: TStringList;
  I: Integer = 0;
  Module: TModule;
begin
  Temp := TStringList.Create;
  try
    for I := 0 to FList.Count - 1 do begin
      Module := TModule(FList[I]);
      Temp.Add(Format(MASK, [Module.ModuleName, Module.Level, BoolToStr(Module.MayInsert, True)]));
    end;
    Temp.SaveToFile(FileName);
  finally
    Temp.Free;
  end;
end;

{ TListParser.TModules.TModule }

constructor TListParser.TModules.TModule.Create(const Text: String);
begin
  inherited Create;
  FExtract.Populate(Text);
  FModuleName := AnsiReplaceStr(FExtract.FileName, '/', DirectorySeparator);
  FLevel := FExtract.Level;
  FMayInsert := False;
end;

destructor TListParser.TModules.TModule.Destroy;
begin
  inherited;
end;

{ TListParser.TModules.TModule.TExtract }

procedure TListParser.TModules.TModule.TExtract.Populate(const Text: String);
const
  QUOTE = '"';
var
  Temp: String = '';
  P1: Integer = 0;
  P2: Integer = 0;
begin
  Temp := Trim(Copy(Text, 5, 3));
  Level := Temp.Length;
  P1 := Pos(QUOTE, Text);
  P2 := Pos(QUOTE, Text, P1 + 1);
  FileName:= Copy(Text, P1 + 1, P2 - P1 - 1);
end;

end.

