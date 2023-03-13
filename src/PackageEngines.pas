unit PackageEngines;

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
  Classes, SysUtils, ComCtrls;

type
  TStartEvent = procedure(Sender: TObject; Max: Integer; const Text: String) of object;
  TStatusEvent = procedure(Sender: TObject; Position: Integer; const Text: String) of object;
  TErrorEvent = procedure(Sender: TObject; const Text: String) of object;

  { TPackageEngine }

  TPackageEngine = class(TComponent)
  private
    FView: TListView;
    FDrive: Char;
    FUser: Byte;
    FMayPad: Boolean;
    FPadding: Byte;
    FOnStart: TStartEvent;
    FOnStatus: TStatusEvent;
    FOnEnd: TNotifyEvent;
    FOnError: TErrorEvent;
    function FileToPackageStr(const FileName: TFileName; MayPad: Boolean; Padding: Byte): String;
    function FileToPackage(const FileName: TFileName; Drive: Char; User: Byte;
      MayPad: Boolean; Padding: Byte): TStringList;
    function FilesToPackageFile(FileNames: TListItems; Drive: Char; User: Byte;
      MayPad: Boolean; Padding: Byte): TStringList;
  protected
    procedure DoStart(Max: Integer; const Text: String);
    procedure DoStatus(Position: Integer; const Mask: String; const Args: array of const);
    procedure DoEnd;
    procedure DoError(const Mask: String; const Args: array of const);
  public
    constructor Create(AOwner: TComponent; View: TListView); reintroduce;
    destructor Destroy; override;
    function Generate: TStringList;
    procedure Extract(const FileName: TFileName; FolderName: TFileName);
    property Drive: Char read FDrive write FDrive;
    property User: Byte read FUser write FUser;
    property MayPad: Boolean read FMayPad write FMayPad;
    property Padding: Byte read FPadding write FPadding;
    property OnStart: TStartEvent read FOnStart write FOnStart;
    property OnStatus: TStatusEvent read FOnStatus write FOnStatus;
    property OnEnd: TNotifyEvent read FOnEnd write FOnEnd;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

const
  DEF_DRIVE      = 'A';
  DEF_USER       = 0;
  DEF_MAY_PAD    = False;
  DEF_PAD_CHAR   = #0;
  IDX_SIZE       = 0;
  IDX_SHORT_PATH = 1;
  IDX_SHORT_NAME = 2;


implementation

uses
  Forms, StrUtils;

{ TPackageEngine }

constructor TPackageEngine.Create(AOwner: TComponent; View: TListView);
begin
  inherited Create(AOwner);
  FView := View;
  FDrive := DEF_DRIVE;
  FUser := DEF_USER;
  FMayPad := DEF_MAY_PAD;
  FPadding := Ord(DEF_PAD_CHAR);
end;

destructor TPackageEngine.Destroy;
begin
  inherited;
end;

function TPackageEngine.FileToPackageStr(const FileName: TFileName; MayPad: Boolean; Padding: Byte): String;
var
  InputStream: TFileStream;
  CheckSum: Byte = 0;
  Data: Byte = 0;
  Size: Byte = 0;
begin
  Result := ':';
  if FileExists(FileName) then begin
    CheckSum := 0;
    InputStream := TFileStream.Create(FileName, fmOpenRead);
    try
      InputStream.Position := 0;
      while InputStream.Position < InputStream.Size do begin
        Data := InputStream.ReadByte;
        Inc(CheckSum, Data);
        Inc(Size);
        Result := Result + IntToHex(Data, 2)
      end;
      if MayPad then
        while Size mod 128 > 0 do begin
          Inc(Size);
          Result := Result + IntToHex(Padding, 2);
        end;
      Result := Result + '>' + IntToHex(Size, 2) + IntToHex(CheckSum, 2);
    finally
      InputStream.Free;
    end;
  end;
end;

function TPackageEngine.FileToPackage(const FileName: TFileName; Drive: Char; User: Byte; MayPad: Boolean; Padding: Byte): TStringList;
const
  COMMAND   = '%s:DOWNLOAD %s';
  USERSPACE = 'U%s';
begin
  Result := TStringList.Create;
  Result.Add(Format(COMMAND, [Drive, AnsiUpperCase(ExtractFileName(FileName))]));
  Result.Add(Format(USERSPACE, [IntToHex(User, 1)]));
  Result.Add(FileToPackageStr(FileName, MayPad, Padding));
end;

function TPackageEngine.FilesToPackageFile(FileNames: TListItems; Drive: Char; User: Byte;
  MayPad: Boolean; Padding: Byte): TStringList;
const
  ERROR_MISSING_FILE = 'File ''$s'' cannot be found.';
  PROMPT = 'Processing ''%s''…';
var
  OldItem: TListItem;
  FilePath: TFileName = '';
  FileName: TFileName = '';
  I: Integer = 0;
  Item: TListItem;
  Temp: TStringList;
begin
  Result := TStringList.Create;
  OldItem := FView.Selected;
  try
    for Item in FileNames do begin
      FilePath := Item.Caption;
      FileName := ExtractFileName(FilePath);
      if not FileExists(FilePath) then
        DoError(ERROR_MISSING_FILE, [FileName])
      else begin
        Inc(I);
        DoStatus(I, PROMPT, [FileName]);
        FView.Selected := Item;
        Item.MakeVisible(False);
        Application.ProcessMessages;
        Temp := FileToPackage(Item.SubItems[IDX_SHORT_PATH], Drive, User, MayPad, Padding);
        try
          Result.AddStrings(Temp);
        finally
          Temp.Free;
        end;
      end;
    end;
  finally
    FView.Selected := OldItem;
    if Assigned(OldItem) then
      OldItem.MakeVisible(False);
  end;
end;

procedure TPackageEngine.DoStart(Max: Integer; const Text: String);
begin
  if Assigned(FOnStart) then
    FOnStart(Self, Max, Text);
end;

procedure TPackageEngine.DoStatus(Position: Integer; const Mask: String; const Args: array of const);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, Position, Format(Mask, Args));
end;

procedure TPackageEngine.DoEnd;
begin
  if Assigned(FOnEnd) then
    FOnEnd(Self);
end;

procedure TPackageEngine.DoError(const Mask: String; const Args: array of const);
begin
  if Assigned(FOnError) then
    FOnError(Self, Format(Mask, Args));
end;

function TPackageEngine.Generate: TStringList;
const
  PROMPT = 'Generating Package File…';
begin
  DoStart(FView.Items.Count, PROMPT);
  try
    Result := FilesToPackageFile(FView.Items, FDrive, FUser, FMayPad, FPadding);
  finally
    DoEnd;
  end;
end;

procedure TPackageEngine.Extract(const FileName: TFileName; FolderName: TFileName);
const
  PROMPT = 'Extracting Package File…';
var
  Temp: TStringList;
  Count: Integer = 0;
  I: Integer = 0;

  procedure Extract(Index: Integer; Data: TStrings; const FolderName: TFileName);
  const
    PROMPT              = 'Extracting ''%s''…';
    ERROR_SIZE          = 'File ''%s'' has incorrect size.';
    ERROR_CHECKSUM      = 'File ''%s'' has invalid checksum.';
    ERROR_SIZE_CHECKSUM = 'File ''%s'' has incorrect size and invalid checksum.';
  var
    Datum: String = '';
    FileName: TFileName = '';
    FilePath: TFileName = '';
    P: Integer = 0;
    I: Integer = 0;
    J: Integer = 0;
    Line: String = '';
    Length: Integer = 0;
    Value: Byte = 0;
    CheckSum: Byte = 0;
    Size: Byte = 0;
    OutputStream: TFileStream;
  begin
    Datum := Data[Index];
    FileName := Copy(Datum, 12, Datum.Length - 11);
    DoStatus(Index, PROMPT, [FileName]);
    FilePath := FolderName + FileName;
    if FileExists(FilePath) then
      SysUtils.DeleteFile(FilePath);
    Datum := Data[Index + 2];
    Datum := AnsiRightStr(Datum, Datum.Length - 1);
    P := Pos('>', Datum);
    if P > 0 then begin
      Line := Copy(Datum, P + 1, Datum.Length - P);
      Size := Hex2Dec(Copy(Line, 1, 2));
      CheckSum := Hex2Dec(Copy(Line, 3, 2));
      Length := P - 1;
      OutputStream := TFileStream.Create(FilePath, fmCreate);
      try
        for I := 0 to (Length div 2) - 1 do begin
          J := (I * 2) + 1;
          Value := Hex2Dec(Copy(Datum, J, 2));
          Dec(CheckSum, Value);
          Dec(Size);
          OutputStream.WriteByte(Value);
        end;
        if Size <> 0 then
          if CheckSum <> 0 then
            DoError(ERROR_SIZE_CHECKSUM, [FileName])
          else
            DoError(ERROR_SIZE, [FileName])
        else
          if CheckSum <> 0 then
            DoError(ERROR_CHECKSUM, [FileName]);
      finally
        OutputStream.Free;
      end;
    end;
  end;

begin
  FolderName := IncludeTrailingPathDelimiter(FolderName);
  Temp := TStringList.Create;
  try
    Temp.LoadFromFile(FileName);
    DoStart(Temp.Count * 3, PROMPT);
    try
      Count := Temp.Count div 3;
      for I := 0 to Count - 1 do begin
        Extract(I * 3, Temp, FolderName);
      end;
    finally
      DoEnd;
    end;
  finally
    Temp.Free
  end;
end;

end.

