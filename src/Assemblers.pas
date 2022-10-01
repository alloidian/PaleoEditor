unit Assemblers;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ConfigUtils;

type

  { TAssemblerForm }

  TAssemblerForm = class(TForm)
    AssemblyNameEdit: TEdit;
    AssemblyNameLabel: TLabel;
    ParameterLabel: TLabel;
    ParameterEdit: TEdit;
    OKButton: TButton;
    CancelButton: TButton;
    SymbolEdit: TCheckBox;
    PlatformLabel: TLabel;
    Z80Edit: TRadioButton;
    Z180Edit: TRadioButton;
  private
  protected
    function GetAssemblyName: TFileName;
    procedure SetAssemblyName(const Value: TFileName);
    function GetPlatform: TAssemblerPlatform;
    procedure SetPlatform(Value: TAssemblerPlatform);
    function GetParameter: String;
    procedure SetParameter(const Value: String);
    function GetSupportDebugZ: Boolean;
    procedure SetSupportDebugZ(Value: Boolean);
  public
    property AssemblyName: TFileName read GetAssemblyName write SetAssemblyName;
    property Platform: TAssemblerPlatform read GetPlatform write SetPlatform;
    property Parameter: String read GetParameter write SetParameter;
    property SupportDebugZ: Boolean read GetSupportDebugZ write SetSupportDebugZ;
  end;

var
  AssemblerForm: TAssemblerForm;

function AssembleFile(Node: TTreeNode; var Parameters: String; var Platform: TAssemblerPlatform;
  var UpdateSymbols: Boolean): Boolean;

implementation

{$R *.lfm}

uses
  Utils, Configs;

function AssembleFile(Node: TTreeNode; var Parameters: String; var Platform: TAssemblerPlatform;
  var UpdateSymbols: Boolean): Boolean;
const
  PARAMS: array[TAssemblerPlatform] of String =
    ('-t80 -g3 -fFF %s -s %s.asm %s.com %s.lst %s.sym',    // apZ80
     '-t180 -g3 -fFF %s -s %s.asm %s.com %s.lst %s.sym');  // apZ180
var
  Command: String;
  Dialog: TAssemblerForm;
begin
  Result := Assigned(Node);
  if Result then begin
    Dialog := TAssemblerForm.Create(Application.MainForm);
    try
      Dialog.AssemblyName := Node.LogicalName;
      Result := Dialog.ShowModal = mrOk;
      if Result then begin
        Command := ExtractFileName(Node.LogicalName);
        Command := ChangeFileExt(Command, EmptyStr);
        Platform := Dialog.Platform;
        Parameters := Dialog.Parameter;
        UpdateSymbols := Dialog.SupportDebugZ;
        Config.AddParam(Node.LogicalName, Platform, Parameters, UpdateSymbols);
        Parameters := Format(PARAMS[Dialog.Platform], [Parameters, Command, Command, Command, Command]);
      end;
    finally
      Dialog.Free;
    end;
  end;
end;

{ TAssemblerForm }

function TAssemblerForm.GetAssemblyName: TFileName;
begin
  Result := AssemblyNameEdit.Text;
end;

procedure TAssemblerForm.SetAssemblyName(const Value: TFileName);
begin
  AssemblyNameEdit.Text := Value;
  Platform := Config.GetPlatform(Value);
  Parameter := Config.GetParameter(Value);
  SupportDebugZ := Config.GetUpdateSymbols(Value);
end;

function TAssemblerForm.GetPlatform: TAssemblerPlatform;
begin
  if Z80Edit.Checked then
    Result := apZ80
  else
    Result := apZ180;
end;

procedure TAssemblerForm.SetPlatform(Value: TAssemblerPlatform);
begin
  case Value of
    apZ80:
      Z80Edit.Checked := True;
    apZ180:
      Z180Edit.Checked := True;
  end;
end;

function TAssemblerForm.GetParameter: String;
begin
  Result := ParameterEdit.Text;
end;

procedure TAssemblerForm.SetParameter(const Value: String);
begin
  ParameterEdit.Text := Value;
end;

function TAssemblerForm.GetSupportDebugZ: Boolean;
begin
  Result := SymbolEdit.Checked;
end;

procedure TAssemblerForm.SetSupportDebugZ(Value: Boolean);
begin
  SymbolEdit.Checked := Value;
end;

end.

