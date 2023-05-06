unit TerminalConfigs;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  CustomConfigFrames, ConfigUtils;

type

  { TTerminalConfigFrame }

  TTerminalConfigFrame = class(TCustomConfigFrame)
    ComPortLabel: TLabel;
    ComPortEdit: TComboBox;
    BaudLabel: TLabel;
    BaudEdit: TComboBox;
    DataBitLabel: TLabel;
    DataBitEdit: TComboBox;
    CharDelayEdit: TEdit;
    CharDelayLabel: TLabel;
    UploadCommandEdit: TEdit;
    DownloadCommandEdit: TEdit;
    UploadCommandLabel: TLabel;
    DownloadCommandLabel: TLabel;
    LineDelayLabel: TLabel;
    LineDelayEdit: TEdit;
    StopBitLabel: TLabel;
    StopBitEdit: TComboBox;
    ParityLabel: TLabel;
    ParityEdit: TComboBox;
    FlowControlEdit: TComboBox;
    FlowControlLabel: TLabel;
    CharDelayUpDown: TUpDown;
    LineDelayUpDown: TUpDown;
    procedure DoKeyPress(Sender: TObject; var Key: char);
  private
    FComPort: Integer;
    FBaud: Integer;
    FDataBits: TTerminal.TDataBit;
    FStopBits: TTerminal.TStopBit;
    FParity: TTerminal.TParity;
    FFlowControl: TTerminal.TFlowControl;
    FCharDelay: Integer;
    FLineDelay: Integer;
    FUploadCommand: String;
    FDownloadCommand: String;
  protected
    function GetIsModified: Boolean; override;
(*  procedure PopulateComPort(List: TStrings);
    procedure PopulateBaud(List: TStrings);
    procedure PopulateDataBits(List: TStrings);
    procedure PopulateStopBits(List: TStrings);
    procedure PopulateParity(List: TStrings);
    procedure PopulateFlowControl(List: TStrings); *)
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
    function GetUploadCommand: String;
    procedure SetUploadCommand(const Value: String);
    function GetDownloadCommand: String;
    procedure SetDownloadCommand(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadConfig(Config: TConfig); override;
    procedure WriteConfig(Config: TConfig); override;
    property ComPort: Integer read GetComPort write SetComPort;
    property Baud: Integer read GetBaud write SetBaud;
    property DataBits: TTerminal.TDataBit read GetDataBits write SetDataBits;
    property StopBits: TTerminal.TStopBit read GetStopBits write SetStopBits;
    property Parity: TTerminal.TParity read GetParity write SetParity;
    property FlowControl: TTerminal.TFlowControl read GetFlowControl write SetFlowControl;
    property CharDelay: Integer read GetCharDelay write SetCharDelay;
    property LineDelay: Integer read GetLineDelay write SetLineDelay;
    property UploadCommand: String read GetUploadCommand write SetUploadCommand;
    property DownloadCommand: String read GetDownloadCommand write SetDownloadCommand;
  end;

implementation

{$R *.lfm}

uses
  Math, Utils;

{ TTerminalConfigFrame }

constructor TTerminalConfigFrame.Create(AOwner: TComponent);
const
  MAX_DROP = 15;
begin
  inherited Create(AOwner);
  TTerminal.PopulateComPort(ComPortEdit.Items);
  ComPortEdit.DropDownCount := Min(MAX_DROP, ComPortEdit.Items.Count);
  TTerminal.PopulateBaud(BaudEdit.Items);
  BaudEdit.DropDownCount := Min(MAX_DROP, BaudEdit.Items.Count);
  TTerminal.PopulateDataBits(DataBitEdit.Items);
  DataBitEdit.DropDownCount := Min(MAX_DROP, DataBitEdit.Items.Count);
  TTerminal.PopulateStopBits(StopBitEdit.Items);
  StopBitEdit.DropDownCount := Min(MAX_DROP, StopBitEdit.Items.Count);
  TTerminal.PopulateParity(ParityEdit.Items);
  ParityEdit.DropDownCount := Min(MAX_DROP, ParityEdit.Items.Count);
  TTerminal.PopulateFlowControl(FlowControlEdit.Items);
  FlowControlEdit.DropDownCount := Min(MAX_DROP, FlowControlEdit.Items.Count);
  ComPort := TTerminal.DEF_COM_PORT;
  DataBits := TTerminal.DEF_DATA_BIT;
  StopBits := TTerminal.DEF_STOP_BIT;
  Parity := TTerminal.DEF_PARITY;
  FlowControl := TTerminal.DEF_FLOW_CONTROL;
  CharDelay := TTerminal.DEF_CHAR_DELAY;
  LineDelay := TTerminal.DEF_LINE_DELAY;
end;

destructor TTerminalConfigFrame.Destroy;
begin
  inherited;
end;

procedure TTerminalConfigFrame.DoKeyPress(Sender: TObject; var Key: char);
const
  VALID_CHARS = [#8, '0'..'9'];
begin
  if not (Key in VALID_CHARS) then
    Key := #0;
end;

function TTerminalConfigFrame.GetIsModified: Boolean;
begin
  Result := (ComPort <> FComPort) or (Baud <> FBaud) or (DataBits <> FDataBits) or
    (StopBits <> FStopBits) or (Parity <> FParity) or (FlowControl <> FFlowControl) or
    (CharDelay <> FCharDelay) or (LineDelay <> FLineDelay) or (UploadCommand <> FUploadCommand) or
    (DownloadCommand <> FDownloadCommand);
end;

function SortByComNumber(List: TStringList; Index1, Index2: Integer): Integer;
var
  First: Integer = 0;
  Second: Integer = 0;
begin
  First := Integer(List.Objects[Index1]);
  Second := Integer(List.Objects[Index2]);
  Result := First - Second;
end;

(*sg
procedure TTerminalConfigFrame.PopulateComPort(List: TStrings);
const
  KEY = 'HARDWARE\DEVICEMAP\SERIALCOMM';
var
  Reg: TRegistry;
  Temp: TStringList;
  I: Integer;
  Name: String;
  Port: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly(KEY) then begin
        Temp := TStringList.Create;
        try
          Reg.GetValueNames(Temp);
          for I := 0 to Temp.Count - 1 do begin
            Name := Reg.ReadString(Temp[I]);
            Temp[I] := Name + ':';
            Name := Name.Substring(3, Name.Length - 3);
            Port := StrToIntDef(Name, 0);
            Temp.Objects[I] := TObject(Port);
          end;
          Temp.CustomSort(SortByComNumber);
          for I := 0 to Temp.Count - 1 do begin
            Name := Temp[I];
            Port := Integer(Temp.Objects[I]);
            List.AddObject(Name, TObject(Port));
          end;
        finally
          Temp.Free;
        end;
      end;
    finally
      Reg.Free;
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TTerminalConfigFrame.PopulateBaud(List: TStrings);
type
  TBauds = 0..12;
var
  I: TBauds;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(BAUDS[I].ToString, TObject(BAUDS[I]));
  finally
    List.EndUpdate;
  end;
end;

procedure TTerminalConfigFrame.PopulateDataBits(List: TStrings);
const
  MASK = '%d Data Bits';
var
  I: TTerminal.TDataBit;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(Format(MASK, [I]), TObject(I));
  finally
    List.EndUpdate;
  end;
end;

procedure TTerminalConfigFrame.PopulateStopBits(List: TStrings);
const
  MASK = '%d Stop Bits';
var
  I: TTerminal.TStopBit;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(Format(MASK, [I]), TObject(I));
  finally
    List.EndUpdate;
  end;
end;

procedure TTerminalConfigFrame.PopulateParity(List: TStrings);
var
  I: TTerminal.TParity;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(PARITIES[I], TObject(I));
  finally
    List.EndUpdate;
  end;
end;

procedure TTerminalConfigFrame.PopulateFlowControl(List: TStrings);
var
  I: TTerminal.TFlowControl;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(FLOW_CONTROLS[i], TObject(I));
  finally
    List.EndUpdate;
  end;
end; *)

function TTerminalConfigFrame.GetComPort: Integer;
var
  I: Integer = 0;
begin
  I := ComPortEdit.ItemIndex;
  if I < 0 then
    Result := TTerminal.DEF_COM_PORT
  else
    Result := ComPortEdit.Items.AsInteger[I];
end;

procedure TTerminalConfigFrame.SetComPort(Value: Integer);
var
  I: Integer = 0;
begin
  ComPortEdit.ItemIndex := -1;
  for I := 0 to ComPortEdit.Items.Count - 1 do
    if ComPortEdit.Items.AsInteger[I] = Value then begin
      ComPortEdit.ItemIndex := I;
      Break;
    end;
  FComPort := Value;
end;

function TTerminalConfigFrame.GetBaud: Integer;
var
  I: Integer = 0;
begin
  I := BaudEdit.ItemIndex;
  if I < 0 then
    Result := TTerminal.DEF_BAUD
  else
    Result := BaudEdit.Items.AsInteger[I];
end;

procedure TTerminalConfigFrame.SetBaud(Value: Integer);
var
  I: Integer = 0;
begin
  BaudEdit.ItemIndex := -1;
  for I := 0 to BaudEdit.Items.Count - 1 do
    if BaudEdit.Items.AsInteger[I] = Value then begin
      BaudEdit.ItemIndex := I;
      Break;
    end;
  FBaud := VAlue;
end;

function TTerminalConfigFrame.GetDataBits: TTerminal.TDatabit;
var
  I: Integer = 0;
begin
  I := DataBitEdit.ItemIndex;
  if I < 0 then
    Result := TTerminal.DEF_DATA_BIT
  else
    Result := TTerminal.TDatabit(DataBitEdit.Items.AsInteger[I]);
end;

procedure TTerminalConfigFrame.SetDataBits(Value: TTerminal.TDatabit);
var
  I: Integer = 0;
begin
  DataBitEdit.ItemIndex := -1;
  for I := 0 to DataBitEdit.Items.Count - 1 do
    if TTerminal.TDatabit(DataBitEdit.Items.AsInteger[I]) = Value then begin
      DataBitEdit.ItemIndex := I;
      Break;
    end;
  FDataBits := Value;
end;

function TTerminalConfigFrame.GetStopBits: TTerminal.TStopBit;
var
  I: Integer = 0;
begin
  I := StopBitEdit.ItemIndex;
  if I < 0 then
    Result := TTerminal.DEF_STOP_BIT
  else
    Result := TTerminal.TStopBit(StopBitEdit.Items.AsInteger[I]);
end;

procedure TTerminalConfigFrame.SetStopBits(Value: TTerminal.TStopBit);
var
  I: Integer = 0;
begin
  StopBitEdit.ItemIndex := -1;
  for I := 0 to StopBitEdit.Items.Count - 1 do
    if TTerminal.TStopBit(StopBitEdit.Items.AsInteger[I]) = Value then begin
      StopBitEdit.ItemIndex := I;
      Break;
    end;
  FStopBits := Value;
end;

function TTerminalConfigFrame.GetParity: TTerminal.TParity;
var
  I: Integer = 0;
begin
  I := ParityEdit.ItemIndex;
  if I < 0 then
    Result := TTerminal.DEF_PARITY
  else
    Result := TTerminal.TParity(ParityEdit.Items.AsInteger[I]);
end;

procedure TTerminalConfigFrame.SetParity(Value: TTerminal.TParity);
var
  I: Integer = 0;
begin
  ParityEdit.ItemIndex := -1;
  for I := 0 to ParityEdit.Items.Count - 1 do
    if TTerminal.TParity(ParityEdit.Items.AsInteger[I]) = Value then begin
      ParityEdit.ItemIndex := I;
      Break;
    end;
  FParity := Value;
end;

function TTerminalConfigFrame.GetFlowControl: TTerminal.TFlowControl;
var
  I: Integer = 0;
begin
  I := FlowControlEdit.ItemIndex;
  if I < 0 then
    Result := TTerminal.DEF_FLOW_CONTROL
  else
    Result := TTerminal.TFlowControl(FlowControlEdit.Items.AsInteger[I]);
end;

procedure TTerminalConfigFrame.SetFlowControl(Value: TTerminal.TFlowControl);
var
  I: Integer = 0;
begin
  FlowControlEdit.ItemIndex := -1;
  for I := 0 to FlowControlEdit.Items.Count - 1 do
    if TTerminal.TFlowControl(FlowControlEdit.Items.AsInteger[I]) = Value then begin
      FlowControlEdit.ItemIndex := I;
      Break;
    end;
  FFlowControl := Value;
end;

function TTerminalConfigFrame.GetCharDelay: Integer;
begin
  Result := CharDelayUpDown.Position;
end;

procedure TTerminalConfigFrame.SetCharDelay(Value: Integer);
begin
  CharDelayUpDown.Position := Value;
  FCharDelay := Value;
end;

function TTerminalConfigFrame.GetLineDelay: Integer;
begin
  Result := LineDelayUpDown.Position;
end;

procedure TTerminalConfigFrame.SetLineDelay(Value: Integer);
begin
  LineDelayUpDown.Position := Value;
  FLineDelay := Value;
end;

function TTerminalConfigFrame.GetUploadCommand: String;
begin
  Result := UploadCommandEdit.Text;
end;

procedure TTerminalConfigFrame.SetUploadCommand(const Value: String);
begin
  UploadCommandEdit.Text := Value;
  FUploadCommand := Value;
end;

function TTerminalConfigFrame.GetDownloadCommand: String;
begin
  Result := DownloadCommandEdit.Text;
end;

procedure TTerminalConfigFrame.SetDownloadCommand(const Value: String);
begin
  DownloadCommandEdit.Text := Value;
  FDownloadCommand := Value;;
end;

procedure TTerminalConfigFrame.ReadConfig(Config: TConfig);
begin
  ComPort := Config.Terminal.ComPort;
  Baud := Config.Terminal.Baud;
  DataBits := Config.Terminal.DataBits;
  StopBits := Config.Terminal.StopBits;
  Parity := Config.Terminal.Parity;
  FlowControl := Config.Terminal.FlowControl;
  CharDelay := Config.Terminal.CharDelay;
  LineDelay := Config.Terminal.LineDelay;
  UploadCommand := Config.Terminal.UploadCommand;
  DownloadCommand := Config.Terminal.DownloadCommand;
end;

procedure TTerminalConfigFrame.WriteConfig(Config: TConfig);
begin
  Config.Terminal.ComPort := ComPort;
  Config.Terminal.Baud := Baud;
  Config.Terminal.DataBits := DataBits;
  Config.Terminal.StopBits := StopBits;
  Config.Terminal.Parity := Parity;
  Config.Terminal.FlowControl := FlowControl;
  Config.Terminal.CharDelay := CharDelay;
  Config.Terminal.LineDelay := LineDelay;
  Config.Terminal.UploadCommand := UploadCommand;
  Config.Terminal.DownloadCommand := DownloadCommand;
end;
end.

