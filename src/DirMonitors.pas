unit DirMonitors;

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
  SysUtils, Classes, Windows, Contnrs, SyncObjs;

type
  TActionToWatch = (awChangeFileName, awChangeDirName, awChangeAttributes, awChangeSize,
    awChangeLastWrite, awChangeLastAccess, awChangeCreation, awChangeSecurity);
  TActionsToWatch = set of TActionToWatch;
  TDirMonitorAction = (daUnknown, daFileAdded, daFileRemoved, daFileModified,
    daFileRenamedOldName, daFileRenamedNewName);
  TDirectoryChangeEvent = procedure(Sender: TObject; Action: TDirMonitorAction; const FileName: TFileName) of object;

const
  ALL_ACTIONS = [Low(TActionToWatch) .. High(TActionToWatch)];

type
  TDirMonitorMessage = class(TObject)
  private
    FAction: TDirMonitorAction;
    FFileName: TFileName;
  public
    constructor Create(Action: TDirMonitorAction; const FileName: TFileName); virtual;
    property Action: TDirMonitorAction read FAction;
    property FileName: TFileName read FFilename;
  end;

  TDirMonitorQueue = class(TObject)
  private
    FPolice: TCriticalSection;
    FQueue: TObjectQueue;
  protected
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Push(Message: TDirMonitorMessage); overload;
    procedure Push(Action: TDirMonitorAction; const FileName: TFileName); overload;
    function Pop: TDirMonitorMessage;
    property Count: Integer read GetCount;
  end;

  TDirMonitor = class;

  TDirMonitorListenThread = class(TThread)
  private
    FOwner: TDirMonitor;
    FQueue: TDirMonitorQueue;
  protected
    procedure DoChange(Data: PtrInt);
    procedure Execute; override;
  public
    constructor Create(Owner: TDirMonitor); virtual;
    destructor Destroy; override;
  end;

  TDirMonitorWorkerThread = class;

  TDirMonitor = class(TObject)
  private
    FDirectory: TFileName;
    FActions: TActionsToWatch;
    FSubdirectories: Boolean;
    FOnChange: TDirectoryChangeEvent;
    FQueue: TDirMonitorQueue;
    FWorkerThread: TDirMonitorWorkerThread;
    FListenThread: TDirMonitorListenThread;
  protected
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetActions(const Value: TActionsToWatch);
    procedure SetDirectory(const Value: TFileName);
    procedure SetSubdirectories(const Value: Boolean);
    procedure DoChange(Action: TDirMonitorAction; const FileName: TFileName);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Resume;
    property Directory: TFileName read FDirectory write SetDirectory;
    property Actions: TActionsToWatch read FActions write SetActions default ALL_ACTIONS;
    property Subdirectories: Boolean read FSubdirectories write SetSubdirectories default False;
    property OnChange: TDirectoryChangeEvent read FOnChange write FOnChange;
    property Active: Boolean read GetActive write SetActive;
  end;

  TDirMonitorWorkerThread = class(TThread)
  private
    FDirectory: TFileName;
    FActions: TActionsToWatch;
    FPauseCount: Integer;
    FSubdirectories: Boolean;
    FOwner: TDirMonitor;
    FQueue: TDirMonitorQueue;
    FDirHandle: THandle;
    FChangeHandle: THandle;
    FShutdownHandle: THandle;
    FAction: TDirMonitorAction;
    FFileName: TFileName;
    FLastStamp: TDateTime;
    function GetNotifyMask: DWORD;
    function GetNotifyAction(SystemAction: DWORD): TDirMonitorAction;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(Owner: TDirMonitor);
    destructor Destroy; override;
    procedure Block;
    procedure Unblock;
  end;

implementation

uses
  Forms, DateUtils;

{ TDirMonitorMessage }

constructor TDirMonitorMessage.Create(Action: TDirMonitorAction; const FileName: TFileName);
begin
  inherited Create;
  FAction := Action;
  FFileName := FileName;
end;

{ TDirMonitorQueue }

constructor TDirMonitorQueue.Create;
begin
  inherited Create;
  FPolice := TCriticalSection.Create;
  FQueue := TObjectQueue.Create;
end;

destructor TDirMonitorQueue.Destroy;
begin
  FQueue.Free;
  FPolice.Free;
  inherited;
end;

function TDirMonitorQueue.GetCount: Integer;
begin
  FPolice.Enter;
  try
    Result := FQueue.Count;
  finally
    FPolice.Leave;
  end;
end;

procedure TDirMonitorQueue.Push(Message: TDirMonitorMessage);
begin
  FPolice.Enter;
  try
    FQueue.Push(TObject(Message));
  finally
    FPolice.Leave;
  end;
end;

procedure TDirMonitorQueue.Push(Action: TDirMonitorAction; const FileName: TFileName);
var
  Message: TDirMonitorMessage;
begin
  Message := TDirMonitorMessage.Create(Action, FileName);
  FPolice.Enter;
  try
    FQueue.Push(TObject(Message));
  finally
    FPolice.Leave;
  end;
end;

function TDirMonitorQueue.Pop: TDirMonitorMessage;
begin
  FPolice.Enter;
  try
    Result := TDirMonitorMessage(FQueue.Pop);
  finally
    FPolice.Leave;
  end;
end;

{ TDirMonitorListenThread }

constructor TDirMonitorListenThread.Create(Owner: TDirMonitor);
begin
  inherited Create(True);
  FOwner := Owner;
  FQueue := Owner.FQueue;
end;

destructor TDirMonitorListenThread.Destroy;
begin
  Terminate;
  inherited;
end;

procedure TDirMonitorListenThread.DoChange(Data: PtrInt);
var
  Message: TDirMonitorMessage;
begin
  Message := TDirMonitorMessage(Data);
  try
    FOwner.DoChange(Message.Action, Message.FileName);
  finally
    Message.Free;
  end;
end;

procedure TDirMonitorListenThread.Execute;
var
  Temp: TDirMonitorMessage;
begin
  while not Terminated do begin
    while FQueue.Count > 0 do begin
      Temp := FQueue.Pop;
      Application.QueueAsyncCall(DoChange, PtrInt(Temp));
    end;
    Sleep(500);
  end;
end;

{ TDirMonitorWorkerThread }

constructor TDirMonitorWorkerThread.Create(Owner: TDirMonitor);
begin
  inherited Create(False);
  FPauseCount := 0;
  FDirectory := IncludeTrailingPathDelimiter(Owner.FDirectory);
  FSubdirectories := Owner.Subdirectories;
  FActions := Owner.FActions;
  FOwner := Owner;
  FQueue := Owner.FQueue;
  FDirHandle := CreateFile(PChar(FDirectory), FILE_LIST_DIRECTORY, FILE_SHARE_READ or
    FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or
    FILE_FLAG_OVERLAPPED, 0);
  FChangeHandle := CreateEvent(nil, False, False, nil);
  FShutdownHandle := CreateEvent(nil, False, False, nil);
  FAction := daUnknown;
  FFileName := EmptyStr;
  FLastStamp := Now;
end;

destructor TDirMonitorWorkerThread.Destroy;
begin
  Terminate;
  if FDirHandle <> INVALID_HANDLE_VALUE then
    FileClose(FDirHandle);
  if FChangeHandle <> 0 then
    FileClose(FChangeHandle);
  if FShutdownHandle <> 0 then
    FileClose(FShutdownHandle);
  inherited Destroy;
end;

function TDirMonitorWorkerThread.GetNotifyAction(SystemAction: DWORD): TDirMonitorAction;
begin
  case SystemAction of
    FILE_ACTION_ADDED:
      Result := daFileAdded;
    FILE_ACTION_REMOVED:
      Result := daFileRemoved;
    FILE_ACTION_MODIFIED:
      Result := daFileModified;
    FILE_ACTION_RENAMED_OLD_NAME:
      Result := daFileRenamedOldName;
    FILE_ACTION_RENAMED_NEW_NAME:
      Result := daFileRenamedNewName;
  else
    Result := daUnknown;
  end;
end;

procedure TDirMonitorWorkerThread.Execute;
type
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array [0 .. MAX_PATH] of WCHAR;
  end;
  PFileNotifyInformation = ^TFileNotifyInformation;
const
  BUFFER_SIZE = 65536;
type
  TBuffer = array [0 .. BUFFER_SIZE - 1] of byte;
var
  WaitResult: DWORD = 0;
  BytesRead: DWORD = 0;
  Info: PFileNotifyInformation;
  Buffer: TBuffer;
  Events: array [0 .. 1] of THandle;
  Overlap: TOverlapped;
  Action: TDirMonitorAction;
  FileName: TFileName = '';

  function IsDuplicate(Action: TDirMonitorAction; const FileNam: TFileName): Boolean;
  var
    Stamp: TDateTime;
  begin
    Stamp := Now;
    Result := (FAction = Action) and AnsiSameText(FFileName, FileName) and WithinPastSeconds(Stamp, FLastStamp, 1);
    if not Result then begin
      FAction := Action;
      FFileName := FileName;
      FLastStamp := Stamp;
    end;
  end;

begin
  Buffer := Default(TBuffer);
  Overlap := Default(TOverlapped);
  if FDirHandle <> INVALID_HANDLE_VALUE then begin
    FillChar(Overlap, SizeOf(Overlap), 0);
    Overlap.hEvent := FChangeHandle;
    Events[0] := FChangeHandle;
    Events[1] := FShutdownHandle;
    while not Terminated do begin
      FillChar(Buffer[0], SizeOf(Buffer), 0);
      if ReadDirectoryChangesW(FDirHandle, @Buffer[0], BUFFER_SIZE, FSubdirectories, GetNotifyMask, @BytesRead,
          @Overlap, nil) then begin
        WaitResult := WaitForMultipleObjects(2, @Events[0], False, INFINITE);
        if WaitResult = WAIT_OBJECT_0 then begin
          Info := @Buffer[0];
          repeat
            Action := GetNotifyAction(Info^.Action);
            FileName := WideCharLenToString(@Info^.FileName[0], Info^.FileNameLength);
            FileName := FDirectory + String(PChar(FileName));
            if not IsDuplicate(Action, FileName) and (FPauseCount = 0) then
              FQueue.Push(Action, FileName);
            if Info.NextEntryOffset = 0 then
              Break
            else
              Inc(PByte(Info), Info.NextEntryOffset);
          until Terminated;
        end;
      end;
    end;
  end;
end;

function TDirMonitorWorkerThread.GetNotifyMask: DWORD;
begin
  Result := 0;
  if awChangeFileName in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
  if awChangeDirName in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
  if awChangeSize in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_SIZE;
  if awChangeAttributes in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if awChangeLastWrite in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if awChangeSecurity in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
  if awChangeLastAccess in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if awChangeCreation in FActions then
    Result := Result or FILE_NOTIFY_CHANGE_CREATION;
end;

procedure TDirMonitorWorkerThread.TerminatedSet;
begin
  if FShutdownHandle <> 0 then
    SetEvent(FShutdownHandle);
  inherited;
end;

procedure TDirMonitorWorkerThread.Block;
begin
  Inc(FPauseCount);
end;

procedure TDirMonitorWorkerThread.Unblock;
begin
  if FPauseCount > 0 then
    Dec(FPauseCount);
end;

{ TDirMonitor }

constructor TDirMonitor.Create;
begin
  inherited Create;
  FQueue := TDirMonitorQueue.Create;
  FDirectory := EmptyStr;
  FActions := ALL_ACTIONS;
  FSubdirectories := True;
  FWorkerThread := nil;
  FListenThread := TDirMonitorListenThread.Create(Self);
end;

destructor TDirMonitor.Destroy;
begin
  Stop;
  FListenThread.Free;
  FOnChange := nil;
  FQueue.Free;
  inherited;
end;

function TDirMonitor.GetActive: Boolean;
begin
  Result := FWorkerThread <> nil;
end;

procedure TDirMonitor.SetActive(const Value: Boolean);
begin
  FreeAndNil(FWorkerThread);
  if Value then
    FWorkerThread := TDirMonitorWorkerThread.Create(Self);
end;

procedure TDirMonitor.SetActions(const Value: TActionsToWatch);
begin
  if FActions <> Value then begin
    FActions := Value;
    if Active then
      Start;
  end;
end;

procedure TDirMonitor.SetDirectory(const Value: TFileName);
begin
  if FDirectory <> Value then begin
    FDirectory := Value;
    if Active then
      Start;
  end;
end;

procedure TDirMonitor.SetSubdirectories(const Value: Boolean);
begin
  if FSubdirectories <> Value then begin
    FSubdirectories := Value;
    if Active then
      Start;
  end;
end;

procedure TDirMonitor.DoChange(Action: TDirMonitorAction; const FileName: TFileName);
begin
  if Assigned(OnChange) then
    OnChange(Self, Action, FileName);
end;

procedure TDirMonitor.Start;
begin
  FListenThread.Start;
  Active := True;
end;

procedure TDirMonitor.Stop;
begin
  Active := False;
end;

procedure TDirMonitor.Pause;
begin
  FWorkerThread.Block;
end;

procedure TDirMonitor.Resume;
begin
  Application.ProcessMessages;
  Sleep(500);
  FWorkerThread.Unblock;
end;

end.
