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
  Classes, SysUtils, Controls, ComCtrls, Generics.Collections, SynEditHighlighter, ConfigUtils;

const
  IMAGE_INDEX: array[Boolean] of Integer = (0, 1);
  UNIMPLEMENTED_PROMPT      = 'This feature has not been implemented.';
  MRU_MASK                  = 'Reopen the ''%s'' project.';
  JUMP_MASK                 = 'Jump to the ''%s'' project.';
  WHITE_CLOSED_FOLDER_INDEX = 0;
  WHITE_OPENED_FOLDER_INDEX = 1;
  WHITE_DOCUMENT_INDEX      = 2;
  WHITE_FILE_INDEX          = 3;
  BLACK_CLOSED_FOLDER_INDEX = 4;
  BLACK_OPENED_FOLDER_INDEX = 5;
  BLACK_DOCUMENT_INDEX      = 6;
  BLACK_FILE_INDEX          = 7;
  LEFT_ARROW_INDEX          = 8;
  RIGHT_ARROW_INDEX         = 9;
  STATUS_UNATTACHED_INDEX   = -1;
  STATUS_UNMODIFIED_INDEX   = 10;
  STATUS_MODIFIED_INDEX     = 11;

type
  TFileAttribute = class(TObject)
  public type
    TPropertyKind = (pkUnknown, pkFolder, pkDocument, pkFile);
  private
    FKind: TPropertyKind;
    FProjectName: TFileName;
    FShortName: TFileName;
    FExtension: TFileName;
    FLogicalName: TFileName;
    FFullName: TFileName;
    FIsFileModified: Boolean;
    FPage: TTabSheet;
  protected
    function GetImageIndex: Integer;
  public
    constructor CreateFolder(const Name: TFileName); virtual;
    constructor CreateDocument(const Name: TFileName; const ProjectName: TFileName); virtual;
    constructor CreateFile(const Name: TFileName; const ProjectName: TFileName); virtual;
    procedure RenameFolder(const Name: TFileName);
    procedure RenameFile(const Name: TFileName);
    property Kind: TPropertyKind read FKind;
    property ShortName: TFileName read FShortName;
    property Extension: TFileName read FExtension;
    property LogicalName: TFileName read FLogicalName;
    property FullName: TFileName read FFullName;
    property IsFileModified: Boolean read FIsFileModified write FIsFileModified;
    property ImageIndex: Integer read GetImageIndex;
    property Page: TTabSheet read FPage write Fpage;
  end;

  TTreeNodeCache = class(TObject)
  private type
    TTreeNodeList = TObjectList<TTreeNode>;
  private
    FList: TTreeNodeList;
  protected
    function GetNode(Level: Integer): TTreeNode;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(Node: TTreeNode);
    property Node[Level: Integer]: TTreeNode read GetNode;
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
    TStopOvers = TObjectList<TStop>;
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
  TTreeNodeStatus = (tnsUnattached, tnsUnmodified, tnsModified);
  TTreeNodeHelper = class helper for TTreeNode
  private
    function GetKind: TFileAttribute.TPropertyKind;
    function GetStatus: TTreeNodeStatus;
    procedure SetStatus(Value: TTreeNodeStatus);
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
    function HasExtension(const Extensions: String): Boolean;
    function Matches(const Mask: String): Boolean;
    property Kind: TFileAttribute.TPropertyKind read GetKind;
    property Status: TTreeNodeStatus read GetStatus write SetStatus;
    property ShortName: TFileName read GetShortName;
    property LogicalName: TFileName read GetLogicalName;
    property FullName: TFileName read GetFullName;
    property IsFileModified: Boolean read GetIsFileModified write SetIsFileModified;
    property Page: TTabSheet read GetPage write SetPage;
    property IsExecutable: Boolean read GetIsExecutable;
    property IsAssemblable: Boolean read GetIsAssemblable;
    property HasStructure: Boolean read GetHasStructure;
  end;

{ TStringsHelper }

  TStringsHelper = class helper for TStrings
  private
    function GetAsInteger(I: Integer): Integer;
    procedure SetAsInteger(I: Integer; Value: Integer);
  public
    procedure AddInteger(const Text: String; Value: Integer);
    property AsInteger[I: Integer]: Integer read GetAsInteger write SetAsInteger;
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
function GetChildren(const FullFileName: TFileName): TStringList;
function FileToUploadStr(const FileName: TFileName): String;
function FileToUploadFile(const FileName: TFileName; Drive: Char = 'A'; User: Byte = 0): TStringList;
function FilesToUploadFile(FileNames: TStrings; Drive: Char = 'A'; User: Byte = 0): TStringList;
procedure Flash(Control: TControl);
function ReadStrFromFile(const FileName: TFileName): String;
function WriteStrToFile(const FileName: TFileName; const Value: String): Boolean;

implementation

uses
  Types, Forms, Graphics, StrUtils, FileUtil, Masks, Configs;

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
      if ((Rec.Attr and faDirectory) <> faDirectory) and not Config.IsExcludedFile(Rec.Name) then begin
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

function GetChildren(const FullFileName: TFileName): TStringList;
var
  Path: TFileName = '';
  FileName: TFileName = '';
  Masks: TStringDynArray;
  Mask: String;

  procedure AppendChildren(const Path, FileName: TFileName; List: TStringList; Mask: String);
  var
    Rec: TSearchRec;
    Temp: String = '';
  begin
    Mask := '%s\%s' + Mask;
    if FindFirst(Format(Mask, [Path, FileName]), faAnyFile, Rec) = 0 then begin
      repeat
        if ((Rec.Attr and faDirectory) <> faDirectory) then begin
          Temp := Format('%s\%s', [Path, Rec.Name]);
          if Result.IndexOf(Temp) < 0 then
            Result.Add(Temp);
        end;
      until FindNext(Rec) <> 0;
    end;
  end;

begin
  Result := TStringList.Create;
  Path := ExcludeTrailingPathDelimiter(ExtractFilePath(FullFileName));
  FileName := ExtractFileName(FullFileName);
  FileName := ChangeFileExt(FileName, EmptyStr);
  Masks := SplitString(Config.AuxiliaryFiles, ';');
  for Mask in Masks do
    AppendChildren(Path, FileName, Result, Mask);
end;

function FileToUploadStr(const FileName: TFileName): String;
var
  InputStream: TFileStream;
  CheckSum: Byte = 0;
  Data: Byte = 0;
begin
  Result := ':';
  if FileExists(FileName) then begin
    InputStream := TFileStream.Create(FileName, fmOpenRead);
    try
      InputStream.Position := 0;
      while InputStream.Position < InputStream.Size do begin
        Data := InputStream.ReadByte;
        Inc(CheckSum, Data);
        Result := Result + IntToHex(Data, 2)
      end;
      Result := Result + '>' + IntToHex(InputStream.Size, 2) + IntToHex(CheckSum, 2);
    finally
      InputStream.Free;
    end;
  end;
end;

function FileToUploadFile(const FileName: TFileName; Drive: Char; User: Byte): TStringList;
begin
  Result := TStringList.Create;
  Result.Add(Format('%s:DOWNLOAD %s', [Drive, AnsiUpperCase(ExtractFileName(FileName))]));
  Result.Add(Format('U%d', [User]));
  Result.Add(FileToUploadStr(FileName));
end;

function FilesToUploadFile(FileNames: TStrings; Drive: Char; User: Byte): TStringList;
var
  FileName: TFileName = '';
  Temp: TStringList;
begin
  Result := TStringList.Create;
  for FileName in FileNames do begin
    Temp := FileToUploadFile(FileName, Drive, User);
    try
      Result.AddStrings(Temp);
    finally
      Temp.Free;
    end;
  end;
end;

procedure Flash(Control: TControl);
var
  OldColor: TColor;
begin
  OldColor := Control.Color;
  Control.Color := clRed;
  try
    Beep;
    Application.ProcessMessages;
    Sleep(100);
  finally
    Control.Color := OldColor;
    Application.ProcessMessages;
  end;
end;

function ReadStrFromFile(const FileName: TFileName): String;
var
  Stream: TFileStream;
begin
  Result := EmptyStr;
  if FileExists(FileName) then begin
    Stream := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
    try
      SetLength(Result, Stream.Size);
      Stream.Read(Pointer(Result)^, Stream.Size);
    finally
      Stream.Free;
    end;
  end;
end;

function WriteStrToFile(const FileName: TFileName; const Value: String): Boolean;
var
  Stream: TFileStream;
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
  Result := not FileExists(FileName);
  if Result then begin
    Stream := TFileStream.Create(FileName, fmCreate, fmShareExclusive);
    try
      Stream.Write(Pointer(Value)^, Value.Length);
    finally
      Stream.Free;
    end;
    Result := FileExists(FileName);
  end;
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

constructor TFileAttribute.CreateDocument(const Name: TFileName; const ProjectName: TFileName);
begin
  FKind := pkDocument;
  FProjectName := ProjectName;
  FShortName := ExtractFileName(Name);
  FExtension := ExtractFileExt(FShortName);
  if ProjectName = EmptyStr then
    FLogicalName := FShortName
  else
    FLogicalName := AnsiReplaceStr(Name, ProjectName + DirectorySeparator, EmptyStr);
  FFullName := Name;
  FIsFileModified := False;
  FPage := nil;
end;

constructor TFileAttribute.CreateFile(const Name: TFileName; const ProjectName: TFileName);
begin
  CreateDocument(Name, ProjectName);
  FKind := pkFile;
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
  if FKind in [pkDocument, pkFile] then begin
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
  FList := TTreeNodeList.Create(False);
end;

destructor TTreeNodeCache.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTreeNodeCache.GetNode(Level: Integer): TTreeNode;
begin
  Result := FList[Level];
end;

procedure TTreeNodeCache.Add(Node: TTreeNode);
begin
  FList.Add(Node);
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
  FList := TStopOvers.Create(True);
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

function TTreeNodeHelper.GetStatus: TTreeNodeStatus;
begin
  if not Assigned(self.Page) then
    Result := tnsUnattached
  else
    case Self.StateIndex of
      STATUS_UNMODIFIED_INDEX:
        Result := tnsUnmodified;
      STATUS_MODIFIED_INDEX:
        Result := tnsModified;
    else
      Result := tnsUnattached;
    end;
end;

procedure TTreeNodeHelper.SetStatus(Value: TTreeNodeStatus);
begin
  if not Assigned(Page) then
    Self.StateIndex := STATUS_UNATTACHED_INDEX
  else
    case Value of
      tnsUnattached:
        Self.StateIndex := STATUS_UNATTACHED_INDEX;
      tnsUnmodified:
        Self.StateIndex := STATUS_UNMODIFIED_INDEX;
      tnsModified:
        Self.StateIndex := STATUS_MODIFIED_INDEX;
    end;
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
  if Assigned(Data) then begin
    TFileAttribute(Data).Page := Value;
    if not Assigned(Value) then
      Status := tnsUnattached;
  end;
end;

function TTreeNodeHelper.GetIsExecutable: Boolean;
var
  Attribute: TFileAttribute;
begin
  Result := Assigned(Data);
  if Result then begin
    Attribute := TFileAttribute(Data);
    Result := (Attribute.Kind in [pkDocument, pkFile]) and  Config.IsExecutableFile(Attribute.ShortName);
  end;
end;

function TTreeNodeHelper.GetIsAssemblable: Boolean;
var
  Attribute: TFileAttribute;
begin
  Result := Assigned(Data);
  if Result then begin
    Attribute := TFileAttribute(Data);
    Result := (Attribute.Kind in [pkDocument, pkFile]) and  Config.IsAssemblyFile(Attribute.ShortName);
  end;
end;

function TTreeNodeHelper.GetHasStructure: Boolean;
var
  Attribute: TFileAttribute;
begin
  Result := Assigned(Data);
  if Result then begin
    Attribute := TFileAttribute(Data);
    Result := (Attribute.Kind in [pkDocument, pkFile]) and  Config.HasStructure(Attribute.ShortName);
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

function TTreeNodeHelper.HasExtension(const Extensions: String): Boolean;
var
  Attribute: TFileAttribute;
  Temp: TStringList;
  Ext: String;
begin
  Result := False;
  if Assigned(Data) then begin
    Attribute := TFileAttribute(Data);
    Temp := TStringList.Create;
    try
      Temp.Delimiter := ';';
      Temp.DelimitedText := Extensions;
      for Ext in Temp do
        if AnsiSameText(Ext, Attribute.Extension) then begin
          Result := True;
          Break;
        end;
    finally
      Temp.Free;
    end;
  end;
end;

function TTreeNodeHelper.Matches(const Mask: String): Boolean;
begin
  if AnsiContainsStr(Mask, '*') or AnsiContainsStr(Mask, '?') then
    Result := MatchesMask(Text, Mask)
  else
    Result := AnsiContainsText(Text, Mask);
end;

{ TStringsHelper }

function TStringsHelper.GetAsInteger(I: Integer): Integer;
begin
  if (I > -1) and (I < Count) then
    Result := Integer(Objects[I])
  else
    Result := -1;
end;

procedure TStringsHelper.SetAsInteger(I: Integer; Value: Integer);
begin
  if (I > -1) and (I < Count) then
    Objects[I] := TObject(Value);
end;

procedure TStringsHelper.AddInteger(const Text: String; Value: Integer);
begin
  AddObject(Text, TObject(Value));
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

