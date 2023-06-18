unit TerminalFrames;

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
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, OoMisc, AdPort,
  ADTrmEmu, AdStatLt, AdProtcl, ConfigUtils;

type
{$IFDEF DEBUG}
  TTerminalSizeEvent = procedure(Owner: TObject; Height: Integer; Width: Integer; Size: Integer) of object;
{$ENDIF}

{ TTerminalFrame }

  TTerminalFrame = class(TFrame)
    FileNameLabel: TLabel;
    CtsLabel: TLabel;
    ErrorLabel: TLabel;
    RxLabel: TLabel;
    Scroller: TScrollBox;
    TerminalPanel: TPanel;
    TxLabel: TLabel;
    ProgressBar: TProgressBar;
    StatusPanel: TPanel;
    procedure FrameResize(Sender: TObject);
  private
    FTerminal: TAdTerminal;
    FComPort: TApdComPort;
    FController: TApdSLController;
    FTransmitIndicator: TApdStatusLight;
    FReceiveIndicator: TApdStatusLight;
    FCtsIndicator: TApdStatusLight;
    FErrorIndicator: TApdStatusLight;
    FEmulator: TAdVT100Emulator;
    FProtocol: TApdProtocol;
    FFlowControl: TTerminal.TFlowControl;
    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
{$IFDEF DEBUG}
    FOnSizeChange: TTerminalSizeEvent;
{$ENDIF}
    FBytesTransferred: Integer;
    FBytesRemaining: Integer;
    FOnProtocolAccept : TProtocolAcceptEvent;
    FOnProtocolError : TProtocolErrorEvent;
    FOnProtocolFinish : TProtocolFinishEvent;
    FOnProtocolLog : TProtocolLogEvent;
    FOnProtocolNextFile : TProtocolNextFileEvent;
    FOnProtocolStatus : TProtocolStatusEvent;
    function GetFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
    function GetBytesTransferred: Integer;
    procedure SetBytesTransferred(Value: Integer);
    function GetBytesRemaining: Integer;
    procedure SetBytesRemaining(Value: Integer);
  protected
    procedure DoPortOpen(Sender: TObject);
    procedure DoPortClose(Sender: TObject);
    function GetBaud: Integer;
    procedure SetBaud(Value: Integer);
    function GetDataBits: Word;
    procedure SetDataBits(Value: Word);
    function GetStopBits: Word;
    procedure SetStopBits(Value: Word);
    function GetParity: TParity;
    procedure SetParity(Value: TParity);
    function GetComPort: Word;
    procedure SetComPort(Value: Word);
    function GetFlowControl: TTerminal.TFlowControl;
    procedure SetFlowControl(Value: TTerminal.TFlowControl);
    function GetIsConnected: Boolean;
    function GetHasSelection: Boolean;
    function GetShowStatus: Boolean;
    procedure SetShowStatus(Value: Boolean);
    procedure DoProcessChar(Sender: TObject; Character: Char; var ReplaceWith: string; Commands: TAdEmuCommandList; CharSource: TAdCharSource);
    procedure DoProtocolAccept(Sender: TObject; var Accept: Boolean; var FName: TPassString);
    procedure DoProtocolError(Sender : TObject; ErrorCode : Integer);
    procedure DoProtocolFinish(Sender : TObject; ErrorCode : Integer);
    procedure DoProtocolLog(Sender : TObject; Log : Word);
    procedure DoProtocolNextFile(Sender : TObject; var FName : TPassString);
    procedure DoProtocolStatus(Sender : TObject; Options : Word);
    property FileName: TFileName read GetFileName write SetFileName;
    property BytesTransferred: Integer read GetBytesTransferred write SetBytesTransferred;
    property BytesRemaining: Integer read GetBytesRemaining write SetBytesRemaining;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure UploadText(const FileName: TFileName);
    procedure UploadFile(const FileName: TFileName);
    procedure DownloadText(const FileName: TFileName);
    procedure DownloadFile(const FileName: TFileName);
    procedure CancelTransfer;
    procedure Clear;
    procedure ClearAll;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure Paste;
    property Baud: Integer read GetBaud write SetBaud;
    property DataBits: Word read GetDataBits write SetDataBits;
    property StopBits: Word read GetStopBits write SetStopBits;
    property Parity: TParity read GetParity write SetParity;
    property ComPort: Word read GetComPort write SetComPort;
    property FlowControl: TTerminal.TFlowControl read GetFlowControl write SetFlowControl;
    property IsConnected: Boolean read GetIsConnected;
    property HasSelection: Boolean read GetHasSelection;
    property ShowStatus: Boolean read GetShowStatus write SetShowStatus;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
{$IFDEF DEBUG}
    property OnSizeChange: TTerminalSizeEvent read FOnSizeChange write FOnSizeChange;
{$ENDIF}
    property OnProtocolAccept : TProtocolAcceptEvent read FOnProtocolAccept write FOnProtocolAccept;
    property OnProtocolError : TProtocolErrorEvent read FOnProtocolError write FOnProtocolError;
    property OnProtocolFinish : TProtocolFinishEvent read FOnProtocolFinish write FOnProtocolFinish;
    property OnProtocolLog : TProtocolLogEvent read FOnProtocolLog write FOnProtocolLog;
    property OnProtocolNextFile : TProtocolNextFileEvent read FOnProtocolNextFile write FOnProtocolNextFile;
    property OnProtocolStatus : TProtocolStatusEvent read FOnProtocolStatus write FOnProtocolStatus;
  end;

implementation

{$R *.lfm}

uses
  LCLType, Graphics, Clipbrd;

{ TTerminalFrame }

constructor TTerminalFrame.Create(Owner: TComponent);
const
  DEF_PARITY   = ADPORT.pNone;
  DEF_COM_PORT = 23;
  DEF_ROWS = 25;
  DEF_COLS = 80;
begin
  inherited Create(Owner);
  TerminalPanel.Color := clBlack;
  FComPort := TApdComPort.Create(Self);
  FComPort.Baud := TTerminal.DEF_BAUD;
  FComPort.DataBits := TTerminal.DEF_DATA_BIT;
  FComPort.StopBits := TTerminal.DEF_STOP_BIT;
  FComPort.Parity := DEF_PARITY;
  FComPort.ComNumber := DEF_COM_PORT;
  FComPort.AutoOpen := False;
  FComPort.HWFlowOptions := [hwfUseRTS, hwfRequireCTS];
  FComPort.SWFlowOptions := swfNone;
  FComPort.OnPortOpen := DoPortOpen;
  FComPort.OnPortClose := DoPortClose;
  FTransmitIndicator := TApdStatusLight.Create(Self);
  FTransmitIndicator.Parent := StatusPanel;
  FTransmitIndicator.Left := 5;
  FTransmitIndicator.Top := 25;
  FTransmitIndicator.Width := 22;
  FReceiveIndicator := TApdStatusLight.Create(Self);
  FReceiveIndicator.Parent := StatusPanel;
  FReceiveIndicator.Left := 30;
  FReceiveIndicator.Top := 25;
  FReceiveIndicator.Width := 22;
  FCtsIndicator := TApdStatusLight.Create(Self);
  FCtsIndicator.Parent := StatusPanel;
  FCtsIndicator.Left := 55;
  FCtsIndicator.Top := 25;
  FCtsIndicator.Width := 22;
  FErrorIndicator := TApdStatusLight.Create(Self);
  FErrorIndicator.Parent := StatusPanel;
  FErrorIndicator.Left := 80;
  FErrorIndicator.Top := 25;
  FErrorIndicator.Width := 22;
  FController := TApdSLController.Create(Self);
  FController.ComPort := FComPort;
  FController.Lights.TXDLight := FTransmitIndicator;
  FController.Lights.RXDLight := FReceiveIndicator;
  FController.Lights.CTSLight := FCtsIndicator;
  FController.Lights.ERRORLight := FErrorIndicator;
  FTerminal := TAdTerminal.Create(Self);
  FTerminal.Name := 'DynoTerminal';
  FTerminal.Parent := TerminalPanel;
  FTerminal.BorderStyle := bsNone;
  FTerminal.BorderWidth := 0;
  FTerminal.BorderSpacing.Around := 5;
  FTerminal.Align := alClient;
  FTerminal.PasteToPort := True;
  FTerminal.PasteToScreen := False;
  FTerminal.AutoCopy := False;
  FTerminal.Columns := DEF_COLS;
  FTerminal.ScrollbackRows := DEF_ROWS;
  FTerminal.Rows := DEF_ROWS;
  FTerminal.Font.Name := 'Lucida Console';
  FTerminal.Font.Style := [];
  FTerminal.Font.Size := 11;
  FTerminal.Font.Color := clGreen; // clWhite;
  FTerminal.Font.CharSet := DEFAULT_CHARSET;
  FTerminal.Font.Pitch := fpFixed;
  FTerminal.ComPort := FComPort;
  FTerminal.Enabled := False;
  FTerminal.AutoSize := True;
  FTerminal.UseLazyDisplay := False;
  FEmulator := TAdVT100Emulator.Create(Self);
  FEmulator.Name := 'VT100';
  FEmulator.OnProcessChar := DoProcessChar;
  FProtocol := TApdProtocol.Create(Self);
  FProtocol.Name := 'Protocol';
  FProtocol.ComPort := FComPort;
  FProtocol.UpcaseFileNames := True;
  FProtocol.ProtocolType := ptXmodem;
  FProtocol.HonorDirectory := False;
  FProtocol.IncludeDirectory := False;
  FProtocol.OnProtocolAccept := DoProtocolAccept;
  FProtocol.OnProtocolError := DoProtocolError;
  FProtocol.OnProtocolFinish := DoProtocolFinish;
  FProtocol.OnProtocolLog := DoProtocolLog;
  FProtocol.OnProtocolNextFile := DoProtocolNextFile;
  FProtocol.OnProtocolStatus := DoProtocolStatus;
  FBytesTransferred := 0;
  FBytesRemaining := 0;
end;

destructor TTerminalFrame.Destroy;
begin
  if FTerminal.Active then
    FTerminal.Active := False;
  if FComPort.Open then
    FComPort.Open := False;
  inherited;
end;

procedure TTerminalFrame.FrameResize(Sender: TObject);
var
  OldColor: TColor;
begin
  OldColor := FTerminal.Font.Color;
  try
    FTerminal.Font.Size := (Width div 78) - 1;
  finally
    FTerminal.Font.Color := OldColor;
  end;
{$IFDEF DEBUG}
  if Assigned(FOnSizeChange) then
    FonSizeChange(Sender, Height, Width, FTerminal.Font.Size);
{$ENDIF}
end;

function TTerminalFrame.GetFileName: TFileName;
begin
  Result := FileNameLabel.Caption;
end;

procedure TTerminalFrame.SetFileName(const Value: TFileName);
begin
  FileNameLabel.Caption := Value;
end;

function TTerminalFrame.GetBytesTransferred: Integer;
begin
  Result := FBytesTransferred;
end;

procedure TTerminalFrame.SetBytesTransferred(Value: Integer);
begin
  FBytesTransferred := Value;
  ProgressBar.Position := Value;
end;

function TTerminalFrame.GetBytesRemaining: Integer;
begin
  Result := FBytesRemaining;
end;

procedure TTerminalFrame.SetBytesRemaining(Value: Integer);
begin
  FBytesRemaining := Value;
  ProgressBar.Max := Value + FBytesTransferred;
end;

procedure TTerminalFrame.DoPortOpen(Sender: TObject);
begin
  FTerminal.Enabled := True;
  FTerminal.Clear;
  if FTerminal.CanFocus then
    FTerminal.SetFocus;
  if Assigned(FOnOpen) then
    FOnOpen(Sender);
end;

procedure TTerminalFrame.DoPortClose(Sender: TObject);
begin
  FTerminal.Enabled := False;
  if Assigned(FOnClose) then
    FOnClose(Sender);
end;

{%REGION Parameters}
function TTerminalFrame.GetBaud: Integer;
begin
  Result := FComPort.Baud;
end;

procedure TTerminalFrame.SetBaud(Value: Integer);
begin
  FComPort.Baud := Value;
end;

function TTerminalFrame.GetDataBits: Word;
begin
  Result := FComPort.DataBits;
end;

procedure TTerminalFrame.SetDataBits(Value: Word);
begin
  FComPort.DataBits := Value;
end;

function TTerminalFrame.GetStopBits: Word;
begin
  Result := FComPort.StopBits;
end;

procedure TTerminalFrame.SetStopBits(Value: Word);
begin
  FComPort.StopBits := Value;
end;

function TTerminalFrame.GetParity: TParity;
begin
  Result := FComPort.Parity;
end;

procedure TTerminalFrame.SetParity(Value: TParity);
begin
  FComPort.Parity := Value;
end;

function TTerminalFrame.GetComPort: Word;
begin
  Result := FComPort.ComNumber;
end;

procedure TTerminalFrame.SetComPort(Value: Word);
begin
  FComPort.ComNumber := Value;
end;

function TTerminalFrame.GetFlowControl: TTerminal.TFlowControl;
begin
  Result := FFlowControl;
end;

procedure TTerminalFrame.SetFlowControl(Value: TTerminal.TFlowControl);
begin
  FFlowControl := Value;
  case FFlowControl of
    fcNone: begin
      FComPort.HWFlowOptions := [];
      FComPort.SWFlowOptions := swfNone;
      end;
    fcSoftware: begin
      FComPort.HWFlowOptions := [];
      FComPort.SWFlowOptions := swfBoth;
      end;
    fcHardward: begin
      FComPort.HWFlowOptions := [hwfUseRTS, hwfRequireCTS];
      FComPort.SWFlowOptions := swfNone;
      end;
  end;
end;

function TTerminalFrame.GetIsConnected: Boolean;
begin
  Result := FComPort.Open;
end;

function TTerminalFrame.GetHasSelection: Boolean;
begin
  Result := FTerminal.HaveSelection;
end;

function TTerminalFrame.GetShowStatus: Boolean;
begin
  Result := FileNameLabel.Visible;
end;

procedure TTerminalFrame.SetShowStatus(Value: Boolean);
begin
  FileNameLabel.Visible := Value;
  ProgressBar.Visible := Value;
  Application.ProcessMessages;
end;
{%ENDREGION}

{%REGION Event Handlers}
procedure TTerminalFrame.DoProcessChar(Sender: TObject; Character: Char; var ReplaceWith:
  string; Commands: TAdEmuCommandList; CharSource: TAdCharSource);
begin
  ReplaceWith := Character;
end;

procedure TTerminalFrame.DoProtocolAccept(Sender: TObject; var Accept: Boolean; var FName: TPassString);
begin
  if Assigned(FOnProtocolAccept) then
    FOnProtocolAccept(Sender, Accept, FName);
end;

procedure TTerminalFrame.DoProtocolError(Sender : TObject; ErrorCode : Integer);
begin
  if Assigned(FOnProtocolError) then
    FOnProtocolError(Sender, ErrorCode);
end;

procedure TTerminalFrame.DoProtocolFinish(Sender : TObject; ErrorCode : Integer);
begin
  ShowStatus := False;
  FileName := EmptyStr;
  ProgressBar.Position := 0;
  ProgressBar.Max := 100;
  if FTerminal.CanFocus then
    FTerminal.SetFocus;
  FTerminal.Active := True;
  if Assigned(FOnProtocolFinish) then
    FOnProtocolFinish(Sender, ErrorCode);
end;

procedure TTerminalFrame.DoProtocolLog(Sender : TObject; Log : Word);
begin
  if Assigned(FOnProtocolLog) then
    FOnProtocolLog(Sender, Log);
end;

procedure TTerminalFrame.DoProtocolNextFile(Sender : TObject; var FName : TPassString);
begin
  if Assigned(FOnProtocolNextFile) then
    FOnProtocolNextFile(Sender, FName);
end;

procedure TTerminalFrame.DoProtocolStatus(Sender : TObject; Options : Word);

  procedure Clear;
  begin
    FileName := EmptyStr;
    ProgressBar.Position := 0;
    ProgressBar.Max := 100;
  end;

begin
  case Options of
    apFirstCall, apLastCall:
      Clear;
  else
    FileName := FProtocol.FileName;
    BytesRemaining := FProtocol.BytesRemaining;
    BytesTransferred := FProtocol.BytesTransferred;
  end;
  if Assigned(FOnProtocolStatus) then
    FOnProtocolStatus(Sender, Options);
end;
{%ENDREGION}

{%REGION Methods}
procedure TTerminalFrame.Open;
begin
  if not FTerminal.Active then
    FTerminal.Active := True;
  if not FComPort.Open then
    FComPort.Open := True;
  FController.Monitoring := True;
end;

procedure TTerminalFrame.Close;
begin
  FController.Monitoring := False;
  if FComPort.Open then
    FComPort.Open := False;
  if FTerminal.Active then
    FTerminal.Active := False;
end;

{%REGION Transfer}
procedure TTerminalFrame.UploadText(const FileName: TFileName);
begin
  FTerminal.Active := False;
  ShowStatus := True;
  FProtocol.ProtocolType := ptAscii;
  FProtocol.FileName := FileName;
  FProtocol.StartTransmit;
end;

procedure TTerminalFrame.UploadFile(const FileName: TFileName);
begin
  FComPort.PutString('B:XM R TEST.TXT'#13#10);
  FTerminal.Active := False;
  ShowStatus := True;
  FProtocol.ProtocolType := ptXmodem;
  FProtocol.FileName := FileName;
  FProtocol.StartTransmit;
end;

procedure TTerminalFrame.DownloadText(const FileName: TFileName);
begin
  FTerminal.Active := False;
  ShowStatus := True;
  FProtocol.ProtocolType := ptAscii;
  FProtocol.FileName := FileName;
  FProtocol.StartReceive;
end;

procedure TTerminalFrame.DownloadFile(const FileName: TFileName);
begin
  FTerminal.Active := False;
  ShowStatus := True;
  FProtocol.ProtocolType := ptXmodem;
  FProtocol.FileName := FileName;
  FProtocol.StartReceive;
end;
{%ENDREGION}

procedure TTerminalFrame.CancelTransfer;
begin
  FProtocol.CancelProtocol;
  FTerminal.Active := True;
end;

procedure TTerminalFrame.Clear;
begin
  FTerminal.Clear;
end;

procedure TTerminalFrame.ClearAll;
begin
  FTerminal.ClearAll;
end;

procedure TTerminalFrame.CopyToClipboard;
begin
  if FTerminal.HaveSelection then
    FTerminal.CopyToClipboard;
end;

procedure TTerminalFrame.PasteFromClipboard;
begin
  FTerminal.PasteFromClipboard;
end;

procedure TTerminalFrame.Paste;
var
  Temp: String = '';
  Ch: Char;
begin
  Temp := Clipboard.AsText;
  for Ch in Temp do
    FComPort.PutChar(Ch);
end;
{%ENDREGION}
end.

