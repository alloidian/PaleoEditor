unit Executions;

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
  Messages, Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ComCtrls;

const
  WM_EXEC_LOG = WM_USER + 1;

type
  TLogEvent = class(TObject)
  private
    FText: String;
  protected
    FStamp: TDateTime;
    function GetStampAsText: String;
    class function CleanText(const Value: String): String;
    procedure SetStamp(Value: TDateTime);
  public
    constructor Create; overload; virtual;
    constructor CreateFrom(const Text: String); virtual;
    procedure Clear; overload;
    procedure Assign(Source: TLogEvent); virtual;
    property StampAsText: String read GetStampAsText;
    property Stamp: TDateTime read FStamp write SetStamp;
    property Text: String read FText;
  end;

  TCustomExecutionEngine = class(TObject)
  private type
    TExecutionLogEvent = procedure(Sender: TObject; LogEvent: TLogEvent) of object;
  private
    FOnLog: TExecutionLogEvent;
  protected
    FProgramName: String;
    FFolderName: String;
    FParameters: String;
    FEnvironment: Pointer;
    FResultCode: Cardinal;
    function DoLog(const Text: String): String; overload; virtual;
    function DoLog(const Mask: string; const Args: array of const): String; overload;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Execute: Integer; virtual;
    property ResultCode: Cardinal read FResultCode;
    property OnLog: TExecutionLogEvent read FOnLog write FOnLog;
  end;

  TExecutionEngine = class(TCustomExecutionEngine)
  private
    FLogFile: TextFile;
  protected
    function DoLog(const Text: String): String; override;
    procedure SetProgramName(const Value: String);
    procedure SetFolderName(const Value: String);
    procedure SetParameters(const Value: String);
  public
    function Execute: Integer; override;
    property ProgramName: String read FProgramName write SetProgramName;
    property FolderName: String read FFolderName write SetFolderName;
    property Parameters: String read FParameters write SetParameters;
  end;

  TExecutionThread = class(TThread)
  private
    FEngine: TExecutionEngine;
    FRecipientHandle: THandle;
  protected
    procedure Execute; override;
    procedure DoLog(Sender: TObject; LogEvent: TLogEvent);
    function GetProgramName: TFileName;
    procedure SetProgramName(const Value: TFileName);
    function GetParameters: String;
    procedure SetParameters(const Value: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ProgramName: TFileName read GetProgramName write SetProgramName;
    property Parameters: String read GetParameters write SetParameters;
    property RecipientHandle: THandle read FRecipientHandle write FRecipientHandle;
  end;

  TExecutionForm = class(TForm)
    ExecutableNameLabel: TLabel;
    ExecutableNameEdit: TEdit;
    ParameterLabel: TLabel;
    ParameterEdit: TEdit;
    OKButton: TButton;
    CancelButton: TButton;
  private
    function GetExecutableName: String;
    procedure SetExecutableName(Value: String);
    function GetParameters: String;
    procedure SetParameters(Value: String);
  public
    property ExecutableName: String read GetExecutableName write SetExecutableName;
    property Parameters: String read GetParameters write SetParameters;
  end;

function ExecuteFile(Node: TTreeNode; var Parameters: String): Boolean;

implementation

{$R *.lfm}

uses
  Windows, StrUtils, Utils, Configs;

function ExecuteFile(Node: TTreeNode; var Parameters: String): Boolean;
var
  Command: String;
  Dialog: TExecutionForm;
begin
  Result := Assigned(Node);
  if Result then begin
    Command := Node.LogicalName;
    Dialog := TExecutionForm.Create(Application.MainForm);
    try
      Dialog.ExecutableName := Command;
      Result := Dialog.showModal = mrOk;
      if Result then begin
        Parameters := Dialog.Parameters;
        Config.AddParam(Command, Parameters);
      end;
    finally
      Dialog.Free;
    end;
  end;
end;

{ TLogEvent }

constructor TLogEvent.Create;
begin
  FStamp := Now;
end;

constructor TLogEvent.CreateFrom(const Text: String);
begin
  Create;
  FText := Text;
end;

function TLogEvent.GetStampAsText: String;
const
  MASK = 'mm/dd/yyyy hh:nn:ss';
begin
  Result := FormatDateTime(MASK, FStamp);
end;


class function TLogEvent.CleanText(const Value: String): String;
begin
  Result := AnsiReplaceText(Value, #13#10, EmptyStr);
  Result := AnsiReplaceText(Result, #9, EmptyStr);
end;

procedure TLogEvent.SetStamp(Value: TDateTime);
begin
  FStamp := Value;
end;

procedure TLogEvent.Clear;
begin
  FStamp := 0.0;
  FText := EmptyStr;
end;

procedure TLogEvent.Assign(Source: TLogEvent);
begin
  FStamp := Source.Stamp;
end;

{ TCustomExecutionEngine }

constructor TCustomExecutionEngine.Create;
begin
  inherited Create;
  FEnvironment := nil;
  Clear;
end;

destructor TCustomExecutionEngine.Destroy;
begin
  inherited;
end;

function TCustomExecutionEngine.DoLog(const Text: String): String;
begin
  Result := Text;
  if Assigned(FOnLog) then
    FOnLog(Self, TLogEvent.CreateFrom(Text));
end;

function TCustomExecutionEngine.DoLog(const Mask: string; const Args: array of const): String;
begin
  Result := DoLog(Format(Mask, Args));
end;

procedure TCustomExecutionEngine.Clear;
begin
  FProgramName := EmptyStr;
  FFolderName := EmptyStr;
  FParameters := EmptyStr;
  FResultCode := 0;
end;

function TCustomExecutionEngine.Execute: Integer;
const
  MAX_BUFFER = 2400;
  MASK = '"%s" %s';
var
  Security: TSecurityAttributes;
  ReadPipe: THandle = 0;
  WritePipe: THandle = 0;
  StartupInfo: TStartupInfo;
  CommandLine: string;
  ProcessInfo: TProcessInformation;
  Buffer: PAnsiChar;
  BytesRead: DWord;
  AppRunning: DWord;
begin
  if FFolderName.IsEmpty then
    FFolderName := ExcludeTrailingPathDelimiter(ExtractFilePath(FProgramName));
  Security.nLength := SizeOf(TSecurityAttributes);
  Security.bInheritHandle := True;
  Security.lpSecurityDescriptor := nil;
  if CreatePipe(ReadPipe, WritePipe, @Security, 0) then begin
    try
      Buffer := AllocMem(MAX_BUFFER + 1);
      try
        FillChar(StartupInfo, SizeOf(StartupInfo), 0);
        StartupInfo.cb := SizeOf(StartupInfo);
        StartupInfo.hStdInput := ReadPipe;
        StartupInfo.hStdOutput := WritePipe;
        StartupInfo.hStdError := WritePipe;
        StartupInfo.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
        StartupInfo.wShowWindow := SW_HIDE;
        StartupInfo.lpDesktop := PChar('winsta0\default');
        CommandLine := Format(MASK, [FProgramName, FParameters]);
        DoLog(CommandLine);
        if not CreateProcess(PChar(FProgramName), PChar(CommandLine), @Security, @Security,
            True, NORMAL_PRIORITY_CLASS + CREATE_UNICODE_ENVIRONMENT, FEnvironment,
            PChar(FFolderName), StartupInfo, ProcessInfo) then
          DoLog('Execution Failed. ErrorCode = %d', [GetLastError])
        else begin
          try
            repeat
              AppRunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
              repeat
                BytesRead := 0;
                if PeekNamedPipe(ReadPipe, Buffer, MAX_BUFFER, @BytesRead, nil, nil) then begin
                  if BytesRead > 0 then begin
                    BytesRead := 0;
                    ReadFile(ReadPipe, Buffer[0], MAX_BUFFER, BytesRead, nil);
                    Buffer[BytesRead] := #0;
                    OemToAnsi(Buffer, Buffer);
                    DoLog(String(Buffer));
                  end;
                end;
              until BytesRead < MAX_BUFFER;
            until AppRunning <> WAIT_TIMEOUT;
            if not GetExitCodeProcess(ProcessInfo.hProcess, FResultCode) then
              FResultCode := GetLastError;
          finally
            CloseHandle(ProcessInfo.hProcess);
            CloseHandle(ProcessInfo.hThread);
          end;
        end;
      finally
        FreeMem(Buffer);
      end;
    finally
      CloseHandle(WritePipe);
      CloseHandle(ReadPipe);
    end;
  end;
  Result := FResultCode;
end;

{ TExecutionEngine }

function TExecutionEngine.DoLog(const Text: String): String;
begin
  Result := inherited DoLog(Text);
  WriteLn(FLogFile, Result);
end;

procedure TExecutionEngine.SetProgramName(const Value: String);
begin
  FProgramName := Value;
end;

procedure TExecutionEngine.SetFolderName(const Value: String);
begin
  FFolderName := Value;
end;

procedure TExecutionEngine.SetParameters(const Value: String);
begin
  FParameters := Value;
end;

function TExecutionEngine.Execute: Integer;
var
  LogFileName: String;
begin
  LogFileName := ProgramName + '.log';
  if FileExists(LogFileName) then
    DeleteFile(PChar(LogFileName));
  if not FileExists(LogFileName) then begin
    AssignFile(FLogFile, LogFileName);
    Rewrite(FLogFile);
    try
      Result := inherited Execute;
    finally
      CloseFile(FLogFile);
    end;
  end;
end;

{ TExecutionThread }

constructor TExecutionThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FEngine := TExecutionEngine.Create;
  FEngine.OnLog := DoLog;
end;

destructor TExecutionThread.Destroy;
begin
  FEngine.Free;
  inherited;
end;

procedure TExecutionThread.Execute;
begin
  ReturnValue := FEngine.Execute;
end;

procedure TExecutionThread.DoLog(Sender: TObject; LogEvent: TLogEvent);
begin
  if FRecipientHandle <> 0 then
    PostMessage(FRecipientHandle, WM_EXEC_LOG, NativeUInt(LogEvent), 0);
end;

function TExecutionThread.GetProgramName: TFileName;
begin
  Result := FEngine.ProgramName;
end;

procedure TExecutionThread.SetProgramName(const Value: TFileName);
begin
  FEngine.ProgramName := Value;
end;

function TExecutionThread.GetParameters: String;
begin
  Result := FEngine.Parameters;
end;

procedure TExecutionThread.SetParameters(const Value: String);
begin
  FEngine.Parameters := Value;
end;

{ TExecutionForm }

function TExecutionForm.GetExecutableName: String;
begin
  Result := ExecutableNameEdit.Text;
end;

procedure TExecutionForm.SetExecutableName(Value: String);
begin
  ExecutableNameEdit.Text := Value;
  ParameterEdit.Text := Config.GetParam(Value);
end;

function TExecutionForm.GetParameters: String;
begin
  Result := ParameterEdit.Text;
end;

procedure TExecutionForm.SetParameters(Value: String);
begin
  ParameterEdit.Text := Value;
end;

end.

