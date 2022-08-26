unit ConfigUtils;

{ Copyright ©2022 by Steve Garcia. All rights reserved.

  This file is part of the Paleo Editor project.

  The Paleo Editor is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software Foundation,
  either version 3 of the License, or (at your option) any later version.

  The Paleo Editor project is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with the Paleo
  Editor project. If not, see <https://www.gnu.org/licenses/>. }

{$MODE DELPHI}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ComCtrls, Generics.Collections, Forms, Menus, IniFiles;

const
  ITEM_EDIT              = 'Editable File Extensions';
  ITEM_EXEC              = 'Executable File Extensions';
  ITEM_SEARCH            = 'Searchable File Extensions';
  ITEM_UNEDITABLE        = 'Uneditable File Extensions';
  ITEM_EXCLUDE_FILE      = 'Exclude File Extensions';
  ITEM_EXCLUDE_FOLDER    = 'Exclude Folders';

type
  TAttributeType = (atComment, atConfig, atConstant, atDataType, atDeclaration,
    atDirective, atFlow, atIdentifier, atKeyword, atMemory, atNumber, atRegister,
    atString, atSymbol, atWhitespace);

  TAttribute = class(TObject)
  private
    FAttr: TAttributeType;
    FStorageName: String;
    FForeground: TColor;
    FBackground: TColor;
    FStyle: TFontStyles;
  public
    constructor Create(Attr: TAttributeType; const StorageName: String); virtual;
    procedure ReadConfig(Ini: TIniFile);
    procedure WriteConfig(Ini: TIniFile);
    procedure Assign(Source: TAttribute);
    function Matches(Source: TAttribute): Boolean;
    property Attr: TAttributeType read FAttr;
    property StorageName: String read FStorageName;
    property Foreground: TColor read FForeground write FForeground;
    property Background: TColor read FBackground write FBackground;
    property Style: TFontStyles read FStyle write FStyle;
  end;

  TAttributes = class(TObject)
  private type
    TList = TDictionary<TAttributeType, TAttribute>;
  private
    FList: TList;
  protected
    function GetCount: Integer;
    function GetItems(Attr: TAttributeType): TAttribute;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    procedure Assign(Source: TAttributes);
    procedure ReadConfig(Ini: TIniFile);
    procedure WriteConfig(Ini: TIniFile);
    procedure Populate(List: TStrings);
    function Matches(Source: TAttributes): Boolean;
    property Count: Integer read GetCount;
    property Items[Attr: TAttributeType]: TAttribute read GetItems; default;
  end;

  TConfig = class(TObject)
  private type
    TParamType = TDictionary<String, String>;
    TVersion = record
      Major: Word;
      Minor: Word;
      Release: Word;
      Build: Word;
      IsDebug: Boolean;
      IsPreRelease: Boolean;
      IsPatched: Boolean;
      IsPrivateBuild: Boolean;
      IsSpecialBuild: Boolean;
      CompanyName: String;
      InternalName: String;
      ProjectName: String;
      FileVersion: String;
    end;
  private
    FConfigFileName: TFileName;
    FVersion: TVersion;
    FEditFiles: String;
    FExecuteFiles: String;
    FSearchFiles: String;
    FUneditableFiles: String;
    FSaveWorkspace: Boolean;
    FExcludeFiles: String;
    FExcludeFolders: String;
    FParams: TParamType;
    FMonitorFolder: Boolean;
    FAttributes: TAttributes;
  protected
    function IsMatch(const Value: String; const Masks: String): Boolean;
    function GetVersionText: String;
    function GetPlatform: String;
    function GetDebug: String;
    function GetPreRelease: String;
    function GetPatched: String;
    function GetPrivateBuild: String;
    function GetSpecialBuild: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ReadConfig; overload;
    procedure WriteConfig; overload;
    procedure ReadConfig(Form: TForm); overload;
    procedure WriteConfig(Form: TForm); overload;
    procedure ReadConfig(ParentMenu: TMenuItem; EventHandler: TNotifyEvent); overload;
    procedure WriteConfig(ParentMenu: TMenuItem); overload;
    procedure ReadConfig(Control: TTreeView; const FileName: TFileName); overload;
    procedure WriteConfig(Control: TTreeView; const FileName: TFileName); overload;
    procedure ReadConfig(Control: TPageControl; const FileName: TFileName); overload;
    procedure WriteConfig(Control: TPageControl; const FileName: TFileName); overload;
    procedure AddParam(const Command, Param: String);
    function GetParam(const Command: String): String;
    function ReadWorkspace(const FolderName: string): TStringList;
    procedure WriteWorkspace(const FolderName: string; List: TStringList);
    function IsEditableFile(const FileName: TFileName): Boolean;
    function IsExecutableFile(const FileName: TFileName): Boolean;
    function HasStructure(const FileName: TFileName): Boolean;
    function IsSearchableFile(const FileName: TFileName): Boolean;
    function IsReadonlyFile(const FileName: TFileName): Boolean;
    function IsExcludedFile(const FileName: TFileName): Boolean;
    function IsExcludedFolder(const FolderName: TFileName): Boolean;
    property ConfigFileName: TFileName read FConfigFileName;
    property Version: TVersion read FVersion;
    property VersionText: String read GetVersionText;
    property Platform: String read GetPlatform;
    property Debug: String read GetDebug;
    property PreRelease: String read GetPreRelease;
    property Patched: String read GetPatched;
    property PrivateBuild: String read GetPrivateBuild;
    property SpecialBuild: String read GetSpecialBuild;
    property EditFiles: String read FEditFiles write FEditFiles;
    property ExecuteFiles: String read FExecuteFiles write FExecuteFiles;
    property SearchFiles: String read FSearchFiles write FSearchFiles;
    property UneditableFiles: String read FUneditableFiles write FUneditableFiles;
    property SaveWorkspace: Boolean read FSaveWorkspace write FSaveWorkspace;
    property ExcludeFiles: String read FExcludeFiles write FExcludeFiles;
    property ExcludeFolders: String read FExcludeFolders write FExcludeFolders;
    property Params: TParamType read FParams write FParams;
    property Attributes: TAttributes read FAttributes;
    property MonitorFolder: Boolean read FMonitorFolder;
  end;

implementation

{$WARN 05044 OFF}{$WARN 06058 OFF}

uses
  StrUtils, Types, Masks, FileInfo, VersionTypes, TypInfo, SynEditStrConst, Dialogs;

type
  TAttributeConfig = record
    StorageName: String;
    Foreground: TColor;
    Background: TColor;
    Style: TFontStyles;
  end;
  TAttributeConfigs = array[TAttributeType] of TAttributeConfig;

const
  ATTRIBUTE_CONFIGS: TAttributeConfigs =
  ((StorageName: SYNS_AttrComment;     Foreground: clDkGray;     Background: clNone; Style: []),        // atComment      Z80
   (StorageName: SYNS_AttrConfig;      Foreground: clGreen;      Background: clNone; Style: []),        // atConfig
   (StorageName: SYNS_AttrConstant;    Foreground: clGreen;      Background: clNone; Style: []),        // atConstant
   (StorageName: SYNS_AttrDataType;    Foreground: clGreen;      Background: clNone; Style: []),        // atDataType
   (StorageName: SYNS_AttrDeclaration; Foreground: clRed;        Background: clNone; Style: []),        // atDeclaration
   (StorageName: SYNS_AttrDirective;   Foreground: clBlack;      Background: clNone; Style: [fsBold]),  // atDirective    Z80
   (StorageName: SYNS_AttrFlow;        Foreground: clGreen;      Background: clNone; Style: []),        // atFlow
   (StorageName: SYNS_AttrIdentifier;  Foreground: clWindowText; Background: clNone; Style: []),        // atIdentifier   Z80
   (StorageName: SYNS_AttrKey;         Foreground: clBlue;       Background: clNone; Style: [fsBold]),  // atKeyword      Z80
   (StorageName: SYNS_AttrMemory;      Foreground: clGreen;      Background: clNone; Style: []),        // atMemory
   (StorageName: SYNS_AttrNumber;      Foreground: clPurple;     Background: clNone; Style: []),        // atNumber       Z80
   (StorageName: SYNS_AttrRegister;    Foreground: clTeal;       Background: clNone; Style: [fsBold]),  // atRegister     Z80
   (StorageName: SYNS_AttrString;      Foreground: clRed;        Background: clNone; Style: []),        // atString       Z80
   (StorageName: SYNS_AttrSymbol;      Foreground: clPurple;     Background: clNone; Style: []),        // atSymbol       Z80
   (StorageName: SYNS_AttrWhitespace;  Foreground: clDefault;    Background: clNone; Style: []));       // atWhitespace   Z80

  DELIMITER                = ';';

  INI_SETTING              = 'Setting';
  INI_EDIT                 = 'EditFiles';
  INI_EDIT_DEF             = '*.asm;*.z80;*.azm;*.inc;*.lib;*.ins;*.mac;*.lst;*.bat;*.sh;'  +
                             '*.cmd;*.pas;*.dpr;*.bas;*.zex;*.txt;*.doc;*.for;*.sub;*.md;'  +
                             '*.h;*.hlp;*.not;*.inf;*.new;*.ps1;*.spin;*.msg;*.hex;'        +
                             'Makefile;ReadMe;-readme;diskdefs;readme.1st;copying;read.me;' +
                             '.gitattributes;.gitignore;readme.unix;*.log';
  INI_EXEC                 = 'ExecuteFiles';
  INI_EXEC_DEF             = '*.cmd;*.bat';
  INI_SEARCH               = 'SearchFiles';
  INI_SEARCH_DEF           = '*.asm;*.azm;*.inc;*.lib;*.lst';
  INI_UNEDITABLE           = 'Uneditable';
  INI_UNEDITABLE_DEF       = '*.lst;*.log';
  INI_SAVE_WORKSPACE       = 'SaveWorkspace';
  INI_SAVE_WORKSPACE_DEF   = False;
  INI_EXCLUDE              = 'Exclude';
  INI_EXCLUDE_FILE         = 'Files';
  INI_EXCLUDE_FILE_DEF     = '*.pdf;*.docx;*.com;*.exe;*.dll;*.zip;*.lbr;*.png;*.jpg;' +
                             '*.jpeg;*.bin;Makefile';
  INI_EXCLUDE_FOLDER       = 'Folders';
  INI_EXCLUDE_FOLDER_DEF   = '.github;Tools;Tunes';
  INI_PARAMS               = 'Params';
  INI_ATTRIBUTE_FOREGROUND = 'Foreground';
  INI_ATTRIBUTE_BACKGROUND = 'Background';
  INI_ATTRIBUTE_BOLD       = 'Bold';
  INI_ATTRIBUTE_ITALIC     = 'Italic';
  INI_ATTRIBUTE_UNDERLINE  = 'Underline';
  INI_ATTRIBUTE_STRIKEOUT  = 'StrikeOut';
  INI_MRU                  = 'MRU';
  INI_HEIGHT               = 'Height';
  INI_LEFT                 = 'Left';
  INI_LEFT_DEF             = 100;
  INI_TOP                  = 'Top';
  INI_TOP_DEF              = 100;
  INI_WIDTH                = 'Width';
  INI_WIN_STATE            = 'WindowsState';
  INI_WIN_STATE_DEF        = 'wsNormal';

  { TAttribute }

  constructor TAttribute.Create(Attr: TAttributeType; const StorageName: String);
  begin
    inherited Create;
    FAttr := Attr;
    FStorageName := StorageName;
  end;

  procedure TAttribute.ReadConfig(Ini: TIniFile);
  var
    Config: TAttributeConfig;
  begin
    Config := ATTRIBUTE_CONFIGS[Attr];
    Foreground := TColor(Ini.ReadInteger(StorageName, INI_ATTRIBUTE_FOREGROUND, Integer(Config.Foreground)));
    Background := TColor(Ini.ReadInteger(StorageName, INI_ATTRIBUTE_BACKGROUND, Integer(Config.Background)));
    Style := [];
    if Ini.ReadBool(StorageName, INI_ATTRIBUTE_BOLD, fsBold in Config.Style) then
      Style := Style + [fsBold];
    if Ini.ReadBool(StorageName, INI_ATTRIBUTE_ITALIC, fsItalic in Config.Style) then
      Style := Style + [fsItalic];
    if Ini.ReadBool(StorageName, INI_ATTRIBUTE_UNDERLINE, fsUnderline in Config.Style) then
      Style := Style + [fsUnderline];
    if Ini.ReadBool(StorageName, INI_ATTRIBUTE_STRIKEOUT, fsStrikeOut in Config.Style) then
      Style := Style + [fsStrikeOut];
  end;

  procedure TAttribute.WriteConfig(Ini: TIniFile);
  begin
    Ini.writeInteger(StorageName, INI_ATTRIBUTE_FOREGROUND, Integer(Foreground));
    Ini.WriteInteger(StorageName, INI_ATTRIBUTE_BACKGROUND, Integer(Background));
    Ini.WriteBool(StorageName, INI_ATTRIBUTE_BOLD, fsBold in Style);
    Ini.WriteBool(StorageName, INI_ATTRIBUTE_ITALIC, fsItalic in Style);
    Ini.WriteBool(StorageName, INI_ATTRIBUTE_UNDERLINE, fsUnderline in Style);
    Ini.WriteBool(StorageName, INI_ATTRIBUTE_STRIKEOUT, fsStrikeOut in Style);
  end;

  procedure TAttribute.Assign(Source: TAttribute);
  begin
    if Assigned(Source) then begin
      FStorageName := Source.StorageName;
      Foreground := Source.Foreground;
      Background := Source.Background;
      Style := Source.Style;
    end;
  end;

  function TAttribute.Matches(Source: TAttribute): Boolean;
  begin
    Result := (Foreground = Source.Foreground) and (Background = Source.Background) and (Style = Source.Style);
  end;

  { TAttributes }

  constructor TAttributes.Create;
  var
    Attr: TAttributeType;
    Config: TAttributeConfig;
    Attribute: TAttribute;
  begin
    inherited Create;
    FList := TList.Create;
    for Attr := Low(Attr) to High(Attr) do begin
      Config := ATTRIBUTE_CONFIGS[Attr];
      Attribute := TAttribute.Create(Attr, Config.StorageName);
      Attribute.Foreground := Config.Foreground;
      Attribute.Background := Config.Background;
      Attribute.Style := Config.Style;
      FList.Add(Attr, Attribute);
    end;
  end;

  destructor TAttributes.Destroy;
  begin
    FList.Free;
    inherited;
  end;

  function TAttributes.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TAttributes.GetItems(Attr: TAttributeType): TAttribute;
  begin
    if not Assigned(FList) then
      Result := nil
    else
      if not FList.TryGetValue(Attr, Result) then
        Result := nil;
  end;

  procedure TAttributes.Reset;
  var
    Attr: TAttributeType;
    Config: TAttributeConfig;
    Attribute: TAttribute;
  begin
    for Attr := Low(Attr) to High(Attr) do begin
      Config := ATTRIBUTE_CONFIGS[Attr];
      Attribute := Items[Attr];
      if Assigned(Attribute) then begin
        Attribute.Foreground := Config.Foreground;
        Attribute.Background := Config.Background;
        Attribute.Style := Config.Style;
      end;
    end;
  end;

  procedure TAttributes.Assign(Source: TAttributes);
  var
    Attr: TAttributeType;
    Attribute: TAttribute;
  begin
    for Attr := Low(Attr) to High(Attr) do begin
      Attribute := Items[Attr];
      if Assigned(Attribute) then
        Attribute.Assign(Source[Attr]);
    end;
  end;

  procedure TAttributes.ReadConfig(Ini: TIniFile);
  var
    Attr: TAttributeType;
  begin
    for Attr := Low(Attr) to High(Attr) do
      Items[Attr].ReadConfig(Ini);
  end;

  procedure TAttributes.WriteConfig(Ini: TIniFile);
  var
    Attr: TAttributeType;
  begin
    for Attr := Low(Attr) to High(Attr) do
      Items[Attr].WriteConfig(Ini);
  end;

  procedure TAttributes.Populate(List: TStrings);
  var
    Attr: TAttributeType;
    Attribute: TAttribute;
  begin
    List.BeginUpdate;
    try
      List.Clear;
      for Attr := Low(Attr) to High(Attr) do begin
        Attribute := Items[Attr];
        List.AddObject(Attribute.StorageName, Attribute);
      end;
    finally
      List.EndUpdate;
    end;
  end;

  function TAttributes.Matches(Source: TAttributes): Boolean;
  var
    Attr: TAttributeType;
  begin
    Result := True;
    for Attr := Low(Attr) to High(Attr) do
      if not Items[Attr].Matches(Source[Attr]) then begin
        Result := False;
        Break;
      end;
  end;

{ TConfig }

constructor TConfig.Create;
const
  MASK = '%s\Paleo\Editor.ini';
  DEBUG_BIT         = $01;
  PRE_RELEASE_BIT   = $02;
  PATCHED_BIT       = $04;
  PRIVATE_BUILD_BIT = $08;
  SPECIAL_BUILD_BIT = $20;
var
  VersionInfo: TVersionInfo;

  function SearchValue(Info: TVersionStringFileInfo; const Value: string): string;
  var
    S: TVersionStringTable;
    I: integer;
    J: integer;
  begin
    Result := EmptyStr;
    for I := 0 to Info.Count - 1 do begin
      S := Info.Items[I];
      for J := 0 to S.Count - 1 do
        if S.Keys[J] = Value then begin
          Result := S.Values[J];
          Break;
        end;
    end;
  end;

begin
  inherited Create;
  FParams := TParamType.Create;
  FConfigFileName := GetEnvironmentVariable('APPDATA');
  FConfigFileName := Format(MASK, [FConfigFileName]);
  VersionInfo := TVersionInfo.Create;
  try
    VersionInfo.Load(Application.Handle);
    FVersion.Major := VersionInfo.FixedInfo.FileVersion[0];
    FVersion.Minor := VersionInfo.FixedInfo.FileVersion[1];
    FVersion.Release := VersionInfo.FixedInfo.FileVersion[2];
    FVersion.Build := VersionInfo.FixedInfo.FileVersion[3];
    FVersion.IsDebug := VersionInfo.FixedInfo.FileFlags and DEBUG_BIT > 0;
    FVersion.IsPreRelease := VersionInfo.FixedInfo.FileFlags and PRE_RELEASE_BIT > 0;
    FVersion.IsPatched := VersionInfo.FixedInfo.FileFlags and PATCHED_BIT > 0;
    FVersion.IsPrivateBuild := VersionInfo.FixedInfo.FileFlags and PRIVATE_BUILD_BIT > 0;
    FVersion.IsSpecialBuild := VersionInfo.FixedInfo.FileFlags and SPECIAL_BUILD_BIT > 0;
    FVersion.CompanyName := SearchValue(VersionInfo.StringFileInfo, 'CompanyName');
    FVersion.InternalName := SearchValue(VersionInfo.StringFileInfo, 'InternalName');
    FVersion.ProjectName := SearchValue(VersionInfo.StringFileInfo, 'FileVersion');
    FVersion.FileVersion := SearchValue(VersionInfo.StringFileInfo, 'ProductName');
  finally
    VersionInfo.Free;
  end;
  FMonitorFolder := True;
  FAttributes := TAttributes.Create;
end;

destructor TConfig.Destroy;
begin
  FAttributes.Free;
  FParams.Free;
  inherited;
end;

function TConfig.IsMatch(const Value: String; const Masks: String): Boolean;
var
  List: TStringDynArray;
  Mask: String;
begin
  Result := False;
  List := SplitString(Masks, DELIMITER);
  for Mask in List do
    if MatchesMask(Value, Mask) then begin
      Result := True;
      Break;
    end;
end;

function TConfig.GetVersionText: String;
const
  MASK = '%d.%d.%d';
begin
  Result := Format(MASK, [Version.Major, Version.Minor, Version.Release]);
end;

function TConfig.GetPlatform: String;
const
{$if defined(Win32)}
  PLATFORM = 'Win32';
{$elseif defined(Win64)}
  PLATFORM = 'Win64';
{$else}
  PLATFORM = EmptyStr;
{$endif}
begin
  Result := PLATFORM;
end;

function TConfig.GetDebug: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Debug');
begin
  Result := CAPTIONS[Version.IsDebug];
end;

function TConfig.GetPreRelease: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Pre-Release');
begin
  Result := CAPTIONS[Version.IsPreRelease];
end;

function TConfig.GetPatched: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Patched');
begin
  Result := CAPTIONS[Version.IsPatched];
end;

function TConfig.GetPrivateBuild: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Private');
begin
  Result := CAPTIONS[Version.IsPrivateBuild];
end;

function TConfig.GetSpecialBuild: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Special');
begin
  Result := CAPTIONS[Version.IsSpecialBuild];
end;

procedure TConfig.ReadConfig;
var
  Ini: TIniFile;
  Temp: TStringList;
  I: Integer;
  Command: String;
  Param: String;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    FEditFiles := Ini.ReadString(INI_SETTING, INI_EDIT, INI_EDIT_DEF);
    FExecuteFiles := Ini.ReadString(INI_SETTING, INI_EXEC, INI_EXEC_DEF);
    FSearchFiles := Ini.ReadString(INI_SETTING, INI_SEARCH, INI_SEARCH_DEF);
    FUneditableFiles := Ini.ReadString(INI_SETTING, INI_UNEDITABLE, INI_UNEDITABLE_DEF);
    FSaveWorkspace := Ini.ReadBool(INI_SETTING, INI_SAVE_WORKSPACE, INI_SAVE_WORKSPACE_DEF);
    FExcludeFiles := Ini.ReadString(INI_EXCLUDE, INI_EXCLUDE_FILE, INI_EXCLUDE_FILE_DEF);
    FExcludeFolders := Ini.ReadString(INI_EXCLUDE, INI_EXCLUDE_FOLDER, INI_EXCLUDE_FOLDER_DEF);
    if Ini.SectionExists(INI_PARAMS) then begin
      FParams.Clear;
      Temp := TStringList.Create;
      try
        Ini.ReadSectionValues(INI_PARAMS, Temp);
        for I := 0 to Temp.Count - 1 do begin
          Command := Temp.Names[I];
          if not Command.IsEmpty then begin
            Param := Temp.ValueFromIndex[I];
            if not Param.IsEmpty then
              AddParam(Command, Param);
          end;
        end;
      finally
        Temp.Free
      end;
    end;
    FAttributes.ReadConfig(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TConfig.WriteConfig;
var
  Ini: TIniFile;
  Command: String;
  Param: String;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Ini.WriteString(INI_SETTING, INI_EDIT, FEditFiles);
    Ini.WriteString(INI_SETTING, INI_EXEC, FExecuteFiles);
    Ini.WriteString(INI_SETTING, INI_SEARCH, FSearchFiles);
    Ini.WriteString(INI_SETTING, INI_UNEDITABLE, FUneditableFiles);
    Ini.WriteBool(INI_SETTING, INI_SAVE_WORKSPACE, FSaveWorkspace);
    Ini.WriteString(INI_EXCLUDE, INI_EXCLUDE_FILE, FExcludeFiles);
    Ini.WriteString(INI_EXCLUDE, INI_EXCLUDE_FOLDER, FExcludeFolders);
    Ini.EraseSection(INI_PARAMS);
    for Command in FParams.Keys do begin
      Param := FParams[Command];
      if not Param.IsEmpty then
        Ini.WriteString(INI_PARAMS, Command, Param);
    end;
    FAttributes.WriteConfig(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TConfig.ReadConfig(Form: TForm);
var
  Ini: TIniFile;
  Temp: String;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Form.Height := Ini.ReadInteger(Form.ClassName, INI_HEIGHT, Form.Constraints.MinHeight);
    Form.Left := Ini.ReadInteger(Form.ClassName, INI_LEFT, INI_LEFT_DEF);
    Form.Top := Ini.ReadInteger(Form.ClassName, INI_TOP, INI_TOP_DEF);
    Form.Width := Ini.ReadInteger(Form.ClassName, INI_WIDTH, Form.Constraints.MinWidth);
    Temp := Ini.ReadString(Form.ClassName, INI_WIN_STATE, INI_WIN_STATE_DEF);
    Form.WindowState := TWindowState(GetEnumValue(TypeInfo(TWindowState), Temp));
  finally
    Ini.Free;
  end;
end;

procedure TConfig.WriteConfig(Form: TForm);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Ini.WriteInteger(Form.ClassName, INI_HEIGHT, Form.Height);
    Ini.WriteInteger(Form.ClassName, INI_LEFT, Form.Left);
    Ini.WriteInteger(Form.ClassName, INI_TOP, Form.Top);
    Ini.WriteInteger(Form.ClassName, INI_WIDTH, Form.Width);
    Ini.WriteString(Form.ClassName, INI_WIN_STATE, GetEnumName(TypeInfo(TWindowState), Ord(Form.WindowState)));
  finally
    Ini.Free;
  end;
end;

procedure TConfig.ReadConfig(ParentMenu: TMenuItem; EventHandler: TNotifyEvent);
var
  Ini: TIniFile;
  Projects: TStringList;
  Project: String;
  ImageIndex: Integer;
  Item: TMenuItem;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Projects := TStringList.Create;
    try
      Ini.ReadSection(INI_MRU, Projects);
      for Project in Projects do begin
        ImageIndex := Ini.ReadInteger(INI_MRU, Project, -1);
        Item := TMenuItem.Create(ParentMenu);
        Item.Caption := Project;
        Item.Tag := ImageIndex;
        Item.ImageIndex := Item.Tag;
        Item.OnClick := EventHandler;
        ParentMenu.Add(Item);
      end;
    finally
      Projects.Free;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TConfig.WriteConfig(ParentMenu: TMenuItem);
var
  Ini: TIniFile;
  Item: TMenuItem;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Ini.EraseSection(INI_MRU);
    for Item in ParentMenu do
      Ini.WriteInteger(INI_MRU, Item.Caption, Item.ImageIndex);
  finally
    Ini.Free;
  end;
end;

procedure TConfig.ReadConfig(Control: TTreeView; const FileName: TFileName);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Control.Width := Ini.ReadInteger(Control.ClassName, FileName, Control.Width);
  finally
    Ini.Free;
  end;
end;

procedure TConfig.WriteConfig(Control: TTreeView; const FileName: TFileName);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Ini.WriteInteger(Control.ClassName, FileName, Control.Width);
  finally
    Ini.Free;
  end;
end;

procedure TConfig.ReadConfig(Control: TPageControl; const FileName: TFileName);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Control.Height := Ini.ReadInteger(Control.ClassName, FileName, Control.Height);
  finally
    Ini.Free;
  end;
end;

procedure TConfig.WriteConfig(Control: TPageControl; const FileName: TFileName);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Ini.WriteInteger(Control.ClassName, FileName, Control.Height);
  finally
    Ini.Free;
  end;
end;

procedure TConfig.AddParam(const Command, Param: String);
begin
  FParams.AddOrSetValue(Command, Param);
end;

function TConfig.GetParam(const Command: String): String;
begin
  if not FParams.TryGetValue(Command, Result) then
    Result := EmptyStr;
end;

function TConfig.ReadWorkspace(const FolderName: string): TStringList;
var
  Ini: TIniFile;
begin
  Result := TStringList.Create;
  if SaveWorkspace then begin
    Ini := TIniFile.Create(ConfigFileName);
    try
      Ini.ReadSectionValues(FolderName, Result);
    finally
      Ini.Free;
    end;
  end;
end;

procedure TConfig.WriteWorkspace(const FolderName: string; List: TStringList);
var
  Ini: TIniFile;
  I: Integer;
begin
  Ini := TIniFile.Create(ConfigFileName);
  try
    Ini.EraseSection(FolderName);
    if SaveWorkspace then
      for I := 0 to List.Count - 1 do
        Ini.WriteString(FolderName, List.Names[I], List.ValueFromIndex[I]);
  finally
    Ini.Free;
  end;
end;

function TConfig.IsEditableFile(const FileName: TFileName): Boolean;
begin
  Result := IsMatch(FileName, EditFiles);
end;

function TConfig.IsExecutableFile(const FileName: TFileName): Boolean;
begin
  Result := IsMatch(FileName, ExecuteFiles);
end;

function TConfig.HasStructure(const FileName: TFileName): Boolean;
begin
  Result := MatchesMask(FileName, '*.lst');
end;

function TConfig.IsSearchableFile(const FileName: TFileName): Boolean;
begin
  Result := IsMatch(FileName, SearchFiles);
end;

function TConfig.IsReadonlyFile(const FileName: TFileName): Boolean;
begin
  Result := IsMatch(FileName, UneditableFiles);
end;

function TConfig.IsExcludedFile(const FileName: TFileName): Boolean;
begin
  Result := IsMatch(FileName, ExcludeFiles);
end;

function TConfig.IsExcludedFolder(const FolderName: TFileName): Boolean;
const
  INVALID_FOLDERS: array[0..1] of String = ('.', '..');
begin
  Result := AnsiMatchStr(FolderName, INVALID_FOLDERS);
  if not Result then
    Result := IsMatch(FolderName, ExcludeFolders);
end;

end.

