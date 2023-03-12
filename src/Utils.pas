unit Utils;

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
  Classes, SysUtils, ComCtrls, Generics.Collections, SynEditHighlighter, ConfigUtils;

const
  IMAGE_INDEX: array[Boolean] of Integer = (0, 1);
  UNIMPLEMENTED_PROMPT = 'This feature has not been implemented.';
  MRU_MASK = 'Reopen the ''%s'' project.';
  JUMP_MASK = 'Jump to the ''%s'' project.';

type
  TFileAttribute = class(TObject)
  public type
    TPropertyKind = (pkUnknown, pkFolder, pkFile);
  private
    FKind: TPropertyKind;
    FProjectName: TFileName;
    FShortName: TFileName;
    FLogicalName: TFileName;
    FFullName: TFileName;
    FIsFileModified: Boolean;
    FPage: TTabSheet;
  protected
    function GetImageIndex: Integer;
  public
    constructor CreateFolder(const Name: TFileName); virtual;
    constructor CreateFile(const Name: TFileName; const ProjectName: TFileName); virtual;
    procedure RenameFolder(const Name: TFileName);
    procedure RenameFile(const Name: TFileName);
    property Kind: TPropertyKind read FKind;
    property ShortName: TFileName read FShortName;
    property LogicalName: TFileName read FLogicalName;
    property FullName: TFileName read FFullName;
    property IsFileModified: Boolean read FIsFileModified write FIsFileModified;
    property ImageIndex: Integer read GetImageIndex;
    property Page: TTabSheet read FPage write Fpage;
  end;

  TTreeNodeCache = class(TObject)
  private
    FList: TList;
  protected
    function GetNode(Level: Integer): TTreeNode;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(Node: TTreeNode);
    property Node[Level: Integer]: TTreeNode read GetNode;
  end;

  TIntegerObject = class(TObject)
  private
    FValue: Integer;
  public
    constructor Create(Value: Integer); virtual;
    class procedure FreeList(List: TStrings);
    property Value: Integer read FValue;
  end;

  TStop = class(TObject)
  private
    FNode: TTreeNode;
    FLineNumber: Integer;
  public
    constructor Create(Node: TTreeNode; LineNumber: Integer); virtual;
    property Node: TTreeNode read FNode;
    property LineNumber: Integer read FLineNumber;
  end;

  TItinerary = class(TObject)
  private type
    TStopOvers = TList<TStop>;
  private
    FList: TStopOvers;
    FIndex: Integer;
  protected
    function GetCount: Integer;
    function GetItems(I: Integer): TStop;
    function GetIsFirst: Boolean;
    function GetIsLast: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Post(Node: TTreeNode; LineNumber: Integer): Boolean;
    function GoBack: TStop;
    function GoForward: TStop;
    property Count: Integer read GetCount;
    property Items[I: Integer]: TStop read GetItems;
    property IsFirst: Boolean read GetIsFirst;
    property IsLast: Boolean read GetIsLast;
  end;

{ TTreeNodeHelper }

type
  TTreeNodeHelper = class helper for TTreeNode
  private
    function GetKind: TFileAttribute.TPropertyKind;
    function GetShortName: TFileName;
    function GetLogicalName: TFileName;
    function GetFullName: TFileName;
    function GetIsFileModified: Boolean;
    procedure SetIsFileModified(Value: Boolean);
    function GetPage: TTabSheet;
    procedure SetPage(Value: TTabSheet);
    function GetIsExecutable: Boolean;
    function GetIsAssemblable: Boolean;
    function GetHasStructure: Boolean;
  public
    procedure RenameFolder(const Name: TFileName);
    procedure RenameFile(const Name: TFileName);
    property Kind: TFileAttribute.TPropertyKind read GetKind;
    property ShortName: TFileName read GetShortName;
    property LogicalName: TFileName read GetLogicalName;
    property FullName: TFileName read GetFullName;
    property IsFileModified: Boolean read GetIsFileModified write SetIsFileModified;
    property Page: TTabSheet read GetPage write SetPage;
    property IsExecutable: Boolean read GetIsExecutable;
    property IsAssemblable: Boolean read GetIsAssemblable;
    property HasStructure: Boolean read GetHasStructure;
  end;

{ TSynCustomHighlighterHelper}

  TSynPaleoHighligher = class(TSynCustomHighlighter)
  protected
    function GenerateHighlighter(const Name: String; Attr: TAttributeType): TSynHighLighterAttributes;
  public
    property DefaultHandler[Index: Integer]: TSynHighlighterAttributes read GetDefaultAttribute;
  end;

  TSynCustomHighlighterHelper = class helper for TSynCustomHighlighter
  public
    function RetrieveAttribute(const StorageName: String): TSynHighlighterAttributes;
  end;

function GetFiles(const Path: TFileName): TStringList;
function GetDirectories(const Path: TFileName): TStringList;

implementation

uses
  StrUtils, FileUtil, Configs;

function GetFiles(const Path: TFileName): TStringList;
const
  MASK = '%s\*.*';
var
  Rec: TSearchRec;
  Temp: String = '';
begin
  Result := TStringList.Create;
  if FindFirst(Format(MASK, [Path]), faAnyFile, Rec) = 0 then begin
    repeat
      if ((Rec.Attr and faDirectory) <> faDirectory) and not Config.IsExcludedFolder(Rec.Name) then begin
        Temp := Format('%s\%s', [Path, Rec.Name]);
        Result.Add(Temp);
      end;
    until FindNext(Rec) <> 0;
  end;
  FindClose(Rec);
end;

function GetDirectories(const Path: TFileName): TStringList;
const
  MASK = '%s\*.*';
var
  Rec: TSearchRec;
  Temp: String = '';
begin
  Result := TStringList.Create;
  if FindFirst(Format(MASK, [Path]), faDirectory, Rec) = 0 then begin
    repeat
      if ((Rec.Attr and faDirectory) = faDirectory) and not Config.IsExcludedFolder(Rec.Name) then begin
        Temp := Format('%s\%s', [Path, Rec.Name]);
        Result.Add(Temp);
      end;
    until FindNext(Rec) <> 0;
  end;
  FindClose(Rec);
end;

{ TFileAttribute }

function TFileAttribute.GetImageIndex: Integer;
begin
  if FKind = pkFolder then
    Result := 0
  else begin
    Result := 1;
  end;
end;

constructor TFileAttribute.CreateFolder(const Name: TFileName);
begin
  FKind := pkFolder;
  FProjectName := EmptyStr;
  FShortName := DirectorySeparator + ExtractFileName(Name);
  FLogicalName := FShortName;
  FFullName := Name;
  FIsFileModified := False;
  FPage := nil;
end;

constructor TFileAttribute.CreateFile(const Name: TFileName; const ProjectName: TFileName);
begin
  FKind := pkFile;
  FProjectName := ProjectName;
  FShortName := ExtractFileName(Name);
  if ProjectName = EmptyStr then
    FLogicalName := FShortName
  else
    FLogicalName := AnsiReplaceStr(Name, ProjectName + DirectorySeparator, EmptyStr);
  FFullName := Name;
  FIsFileModified := False;
  FPage := nil;
end;

procedure TFileAttribute.RenameFolder(const Name: TFileName);
begin
  if FKind = pkFolder then begin
    FShortName := DirectorySeparator + ExtractFileName(Name);
    FLogicalName := FShortName;
    FFullName := Name;
  end;
end;

procedure TFileAttribute.RenameFile(const Name: TFileName);
begin
  if FKind = pkFile then begin
    FShortName := ExtractFileName(Name);
    if FProjectName = EmptyStr then
      FLogicalName := FShortName
    else
      FLogicalName := AnsiReplaceStr(Name, FProjectName + DirectorySeparator, EmptyStr);
    FFullName := Name;
  end;
end;

{ TTreeNodeCache }

constructor TTreeNodeCache.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TTreeNodeCache.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTreeNodeCache.GetNode(Level: Integer): TTreeNode;
begin
  Result := TTreeNode(FList[Level]);
end;

procedure TTreeNodeCache.Add(Node: TTreeNode);
begin
  FList.Add(Pointer(Node));
end;

{ TIntegerObject }

constructor TIntegerObject.Create(Value: Integer);
begin
  inherited Create;
  FValue := Value;
end;

class procedure TIntegerObject.FreeList(List: TStrings);
var
  I: Integer = 0;
  Temp: TObject;
begin
  for I := List.Count - 1 downto 0 do begin
    Temp := List.Objects[I];
    if Assigned(Temp) then
      Temp.Free;
    List.Delete(I);
  end;
end;

{ TStop }

constructor TStop.Create(Node: TTreeNode; LineNumber: Integer);
begin
  inherited Create;
  FNode := Node;
  FLineNumber := LineNumber;
end;

{ TItinerary }

constructor TItinerary.Create;
begin
  inherited Create;
  FList := TStopOvers.Create;
  FIndex := -1;
end;

destructor TItinerary.Destroy;
begin
  FList.Free;
  inherited;
end;

function TItinerary.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TItinerary.GetItems(I: Integer): TStop;
begin
  if (I > -1) and (I < FList.Count) then
    Result := FList.Items[I]
  else
    Result := nil;
end;

function TItinerary.GetIsFirst: Boolean;
begin
  Result := FIndex < 1;
end;

function TItinerary.GetIsLast: Boolean;
begin
  Result := FIndex > FList.Count - 2;
end;

procedure TItinerary.Clear;
begin
  FList.Clear;
  FIndex := -1;
end;

function TItinerary.Post(Node: TTreeNode; LineNumber: Integer): Boolean;
var
  I: Integer = 0;
begin
  Result := FIndex < FList.Count - 2;
  if Result then
    for I := FList.Count - 1 downto FIndex + 1 do
      FList.Delete(I);
  FList.Add(TStop.Create(Node, LineNumber));
  FIndex := FList.Count - 1;
end;

function TItinerary.GoBack: TStop;
begin
  if FIndex > 0 then begin
    Dec(FIndex);
    Result := FList.Items[FIndex]; end
  else begin
    FIndex := 0;
    Result := nil;
  end;
end;

function TItinerary.GoForward: TStop;
var
  Last: Integer = 0;
begin
  Last := FList.Count - 1;
  IF FIndex < Last then begin
    Inc(FIndex);
    Result := FList[FIndex]; end
  else begin
    FIndex := Last;
    Result := nil;
  end;
end;

{ TTreeNodeHelper }

function TTreeNodeHelper.GetKind: TFileAttribute.TPropertyKind;
begin
  if Assigned(Data) then
    Result := TFileAttribute(Data).Kind
  else
    Result := pkUnknown;
end;

function TTreeNodeHelper.GetShortName: TFileName;
begin
  if Assigned(Data) then
    Result := TFileAttribute(Data).ShortName
  else
    Result := EmptyStr;
end;

function TTreeNodeHelper.GetLogicalName: TFileName;
begin
  if Assigned(Data) then
    Result := TFileAttribute(Data).LogicalName
  else
    Result := EmptyStr;
end;

function TTreeNodeHelper.GetFullName: TFileName;
begin
  if Assigned(Data) then
    Result := TFileAttribute(Data).FullName
  else
    Result := EmptyStr;
end;

function TTreeNodeHelper.GetIsFileModified: Boolean;
begin
  if Assigned(Data) then
    Result := TFileAttribute(Data).IsFileModified
  else
    Result := False;
end;

procedure TTreeNodeHelper.SetIsFileModified(Value: Boolean);
begin
  if Assigned(Data) then
    TFileAttribute(Data).IsFileModified := Value;
end;

function TTreeNodeHelper.GetPage: TTabSheet;
begin
  if Assigned(Data) then
    Result := TFileAttribute(Data).Page
  else
    Result := nil;
end;

procedure TTreeNodeHelper.SetPage(Value: TTabSheet);
begin
  if Assigned(Data) then
    TFileAttribute(Data).Page := Value;
end;

function TTreeNodeHelper.GetIsExecutable: Boolean;
var
  Attribute: TFileAttribute;
begin
  Result := Assigned(Data);
  if Result then begin
    Attribute := TFileAttribute(Data);
    Result := (Attribute.Kind = pkFile) and  Config.IsExecutableFile(Attribute.ShortName);
  end;
end;

function TTreeNodeHelper.GetIsAssemblable: Boolean;
var
  Attribute: TFileAttribute;
begin
  Result := Assigned(Data);
  if Result then begin
    Attribute := TFileAttribute(Data);
    Result := (Attribute.Kind = pkFile) and  Config.IsAssemblyFile(Attribute.ShortName);
  end;
end;

function TTreeNodeHelper.GetHasStructure: Boolean;
var
  Attribute: TFileAttribute;
begin
  Result := Assigned(Data);
  if Result then begin
    Attribute := TFileAttribute(Data);
    Result := (Attribute.Kind = pkFile) and  Config.HasStructure(Attribute.ShortName);
  end;
end;

procedure TTreeNodeHelper.RenameFolder(const Name: TFileName);
var
  Attribute: TFileAttribute;
begin
  if Assigned(Data) then begin
    Attribute := TFileAttribute(Data);
    Attribute.RenameFolder(Name);
  end;
end;

procedure TTreeNodeHelper.RenameFile(const Name: TFileName);
var
  Attribute: TFileAttribute;
begin
  if Assigned(Data) then begin
    Attribute := TFileAttribute(Data);
    Attribute.RenameFile(Name);
  end;
end;

{ TSynPaleoHighligher }

function TSynPaleoHighligher.GenerateHighlighter(const Name: String; Attr: TAttributeType): TSynHighLighterAttributes;
begin
  Result := TSynHighLighterAttributes.Create(Name);
  Result.Foreground := Config.Attributes[Attr].Foreground;
  Result.Background := Config.Attributes[Attr].Background;
  Result.Style := Config.Attributes[Attr].Style;
end;

{ TSynCustomHighlighterHelper }

function TSynCustomHighlighterHelper.RetrieveAttribute(const StorageName: String): TSynHighlighterAttributes;
var
  I: Integer = 0;
  Attr: TSynHighlighterAttributes;
begin
  Result := nil;
  for I := 0 to AttrCount - 1 do begin
    Attr := GetAttribute(I);
    if Assigned(Attr) and AnsiSameText(Attr.StoredName, StorageName) then begin
      Result := Attr;
      Break;
    end;
  end;
end;

end.

