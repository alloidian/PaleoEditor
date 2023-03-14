unit TerminalForms;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ActnList, Menus, TerminalFrames, ConfigUtils;

type

  { TTerminalForm }

  TTerminalForm = class(TForm)
    ClearAction: TAction;
    EditClearMenu: TMenuItem;
    PasteAction: TAction;
    CopyAction: TAction;
    CancelTransferAction: TAction;
    DownloadBinaryAction: TAction;
    EditMenu: TMenuItem;
    EditCopyMenu: TMenuItem;
    EditPasteMenu: TMenuItem;
    UploadBinaryAction: TAction;
    UploadTextAction: TAction;
    DisconnectAction: TAction;
    ConnectAction: TAction;
    ActionList: TActionList;
    BaudEdit: TEdit;
    ComPortEdit: TEdit;
    ControlPanel: TPanel;
    ComPortLabel: TLabel;
    BaudLabel: TLabel;
    DataBitEdit: TEdit;
    DataBitLabel: TLabel;
    FlowControlEdit: TEdit;
    ConnectMenu: TMenuItem;
    DownloadBinaryMenu: TMenuItem;
    CancelTransferMenu: TMenuItem;
    UploadBinaryMenu: TMenuItem;
    UploadTextMenu: TMenuItem;
    TerminalMenu: TMenuItem;
    TerminalMenus: TMainMenu;
    ParityEdit: TEdit;
    StopBitEdit: TEdit;
    StopBitLabel: TLabel;
    ParityLabel: TLabel;
    FlowControlLabel: TLabel;
    CharDelayLabel: TLabel;
    CharDelayEdit: TEdit;
    LineDelayLabel: TLabel;
    LineDelayEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ClearActionExecute(Sender: TObject);
    procedure ClearActionUpdate(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure CopyActionUpdate(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure PasteActionUpdate(Sender: TObject);
    procedure ConnectActionExecute(Sender: TObject);
    procedure DisconnectActionExecute(Sender: TObject);
    procedure UploadBinaryActionExecute(Sender: TObject);
    procedure UploadBinaryActionUpdate(Sender: TObject);
    procedure UploadTextActionExecute(Sender: TObject);
    procedure UploadTextActionUpdate(Sender: TObject);
    procedure DownloadBinaryActionExecute(Sender: TObject);
    procedure DownloadBinaryActionUpdate(Sender: TObject);
    procedure CancelTransferActionExecute(Sender: TObject);
    procedure CancelTransferActionUpdate(Sender: TObject);
  private
    FTerminal: TTerminalFrame;
    FComPort: Integer;
    FBaud: Integer;
    FDataBits: TTerminal.TDataBit;
    FStopBits: TTerminal.TStopBit;
    FParity: TTerminal.TParity;
    FFlowControl: TTerminal.TFlowControl;
    FCharDelay: Integer;
    FLineDelay: Integer;
  protected
    function GetIsConnected: Boolean;
    function GetComPort: Integer;
    procedure SetComPort(Value: Integer);
    function GetBaud: Integer;
    procedure SetBaud(Value: Integer);
    function GetDataBits: TTerminal.TDatabit;
    procedure SetDataBits(Value: TTerminal.TDatabit);
    function GetStopBits: TTerminal.TStopBit;
    procedure SetStopBits(Value: TTerminal.TStopBit);
    function GetParity: TTerminal.TParity;
    procedure SetParity(Value: TTerminal.TParity);
    function GetFlowControl: TTerminal.TFlowControl;
    procedure SetFlowControl(Value: TTerminal.TFlowControl);
    function GetCharDelay: Integer;
    procedure SetCharDelay(Value: Integer);
    function GetLineDelay: Integer;
    procedure SetLineDelay(Value: Integer);
{$IFDEF DEBUG}
    procedure DoSizeChange(Owner: TObject; Height: Integer; Width: Integer; Size: Integer);
{$ENDIF}	
  public
    procedure ReadConfig(Config: TConfig);
    function UploadFile(const FileName: TFileName): Boolean;
    property IsConnected: Boolean read GetIsConnected;
    property ComPort: Integer read GetComPort write SetComPort;
    property Baud: Integer read GetBaud write SetBaud;
    property DataBits: TTerminal.TDataBit read GetDataBits write SetDataBits;
    property StopBits: TTerminal.TStopBit read GetStopBits write SetStopBits;
    property Parity: TTerminal.TParity read GetParity write SetParity;
    property FlowControl: TTerminal.TFlowControl read GetFlowControl write SetFlowControl;
    property CharDelay: Integer read GetCharDelay write SetCharDelay;
    property LineDelay: Integer read GetLineDelay write SetLineDelay;
  end;

implementation

{$R *.lfm}

{ TTerminalForm }

procedure TTerminalForm.FormCreate(Sender: TObject);
begin
  Left := 10;
  Top := 10;
  FTerminal := TTerminalFrame.Create(Self);
  FTerminal.Parent := Self;
  FTerminal.Align := alClient;
{$IFDEF DEBUG}
  FTerminal.OnSizeChange := DoSizeChange;
{$ENDIF}
  ComPort := TTerminal.DEF_COM_PORT;
  DataBits := TTerminal.DEF_DATA_BIT;
  StopBits := TTerminal.DEF_STOP_BIT;
  Parity := TTerminal.DEF_PARITY;
  FlowControl := TTerminal.DEF_FLOW_CONTROL;
  CharDelay := TTerminal.DEF_CHAR_DELAY;
  LineDelay := TTerminal.DEF_LINE_DELAY;
end;

procedure TTerminalForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
const
  PROMPT = 'The terminal is connected.'#13#10 +
           'Do you want to break the connection and close the terminal?';
begin
  CanClose := not FTerminal.IsConnected;
  if not CanClose then begin
    CanClose := MessageDlg(PROMPT, mtConfirmation, [mbYes, mbNo], 0, mbYes) = mrYes;
    if CanClose then
      FTerminal.Close;
  end;
end;

procedure TTerminalForm.ClearActionExecute(Sender: TObject);
begin
  FTerminal.ClearAll;
end;

procedure TTerminalForm.ClearActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FTerminal.IsConnected;
end;

procedure TTerminalForm.CopyActionExecute(Sender: TObject);
begin
 FTerminal.CopyToClipboard;
end;

procedure TTerminalForm.CopyActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FTerminal.IsConnected and FTerminal.HasSelection;
end;

procedure TTerminalForm.PasteActionExecute(Sender: TObject);
begin
  FTerminal.Paste;
end;

procedure TTerminalForm.PasteActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FTerminal.IsConnected;
end;

procedure TTerminalForm.ConnectActionExecute(Sender: TObject);
begin
  ConnectMenu.Action := DisconnectAction;
  FTerminal.ComPort := ComPort;
  FTerminal.Baud := Baud;
  FTerminal.DataBits := DataBits;
  FTerminal.StopBits := StopBits;
//  FTerminal.Parity := Parity;
  FTerminal.FlowControl := FlowControl;
  FTerminal.Open;
end;

procedure TTerminalForm.DisconnectActionExecute(Sender: TObject);
begin
  ConnectMenu.Action := ConnectAction;
  FTerminal.Close;
end;

procedure TTerminalForm.UploadBinaryActionExecute(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    Dialog.Title := 'Select Binary File to Upload';
    if Dialog.Execute then
      FTerminal.UploadFile(Dialog.FileName);
  finally
    Dialog.Free;
  end;
end;

procedure TTerminalForm.UploadBinaryActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FTerminal.IsConnected;
end;

procedure TTerminalForm.UploadTextActionExecute(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    Dialog.Title := 'Select Text File to Upload';
    if Dialog.Execute then
      FTerminal.UploadText(Dialog.FileName);
  finally
    Dialog.Free;
  end;
end;

procedure TTerminalForm.UploadTextActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FTerminal.IsConnected;
end;

procedure TTerminalForm.DownloadBinaryActionExecute(Sender: TObject);
var
  Dialog: TSaveDialog;
begin
  Dialog := TSaveDialog.Create(nil);
  try
    Dialog.Title := 'Select Binary File to Download';
    if Dialog.Execute then
      FTerminal.DownloadFile(Dialog.FileName);
  finally
    Dialog.Free;
  end;
end;

procedure TTerminalForm.DownloadBinaryActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FTerminal.IsConnected;
end;

procedure TTerminalForm.CancelTransferActionExecute(Sender: TObject);
begin
  FTerminal.CancelTransfer;
end;

procedure TTerminalForm.CancelTransferActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := True;
end;

function TTerminalForm.GetIsConnected: Boolean;
begin
  Result := FTerminal.IsConnected;
end;

function TTerminalForm.GetComPort: Integer;
begin
  Result := FComPort;
end;

procedure TTerminalForm.SetComPort(Value: Integer);
begin
  FComPort := Value;
  ComPortEdit.Text := TTerminal.ComPortToStr(Value);
end;

function TTerminalForm.GetBaud: Integer;
begin
  Result := FBaud;
end;

procedure TTerminalForm.SetBaud(Value: Integer);
begin
  FBaud := Value;
  BaudEdit.Text := Value.ToString;
end;

function TTerminalForm.GetDataBits: TTerminal.TDatabit;
begin
  Result := FDataBits;
end;

procedure TTerminalForm.SetDataBits(Value: TTerminal.TDatabit);
begin
  FDataBits := Value;
  DataBitEdit.Text := TTerminal.DataBitToStr(Value);
end;

function TTerminalForm.GetStopBits: TTerminal.TStopBit;
begin
  Result := FStopBits;
end;

procedure TTerminalForm.SetStopBits(Value: TTerminal.TStopBit);
begin
  FStopBits := Value;
  StopBitEdit.Text := TTerminal.StopBitToStr(Value);
end;

function TTerminalForm.GetParity: TTerminal.TParity;
begin
  Result := FParity;
end;

procedure TTerminalForm.SetParity(Value: TTerminal.TParity);
begin
  FParity := Value;
  ParityEdit.Text := TTerminal.ParityToStr(Value);
end;

function TTerminalForm.GetFlowControl: TTerminal.TFlowControl;
begin
  Result := FFlowControl;
end;

procedure TTerminalForm.SetFlowControl(Value: TTerminal.TFlowControl);
begin
  FFlowControl := Value;
  FlowControlEdit.Text := TTerminal.FlowControlToStr(Value);
end;

function TTerminalForm.GetCharDelay: Integer;
begin
  Result := FCharDelay;
end;

procedure TTerminalForm.SetCharDelay(Value: Integer);
begin
  FCharDelay := Value;
  CharDelayEdit.Text := Value.ToString;
end;

function TTerminalForm.GetLineDelay: Integer;
begin
  Result := FLineDelay;
end;

procedure TTerminalForm.SetLineDelay(Value: Integer);
begin
  FLineDelay := Value;
  LineDelayEdit.Text := Value.ToString;
end;

{$IFDEF DEBUG}
procedure TTerminalForm.DoSizeChange(Owner: TObject; Height: Integer; Width: Integer; Size: Integer);
begin
  Caption := Format('H = %d, W = %d, Size = %d', [Height, Width, Size]);
end;
{$ENDIF}

procedure TTerminalForm.ReadConfig(Config: TConfig);
begin
  ComPort := Config.Terminal.ComPort;
  Baud := Config.Terminal.Baud;
  DataBits := Config.Terminal.DataBits;
  StopBits := Config.Terminal.StopBits;
  Parity := Config.Terminal.Parity;
  FlowControl := Config.Terminal.FlowControl;
  CharDelay := Config.Terminal.CharDelay;
  LineDelay := Config.Terminal.LineDelay;
end;

function TTerminalForm.UploadFile(const FileName: TFileName): Boolean;
begin
  // Do something
  Result := True;
end;

end.

