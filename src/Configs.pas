unit Configs;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ComCtrls,
  ExtCtrls, ActnList, ConfigUtils, NavigatorConfigs, ColorConfigs;

type
  TConfigForm = class(TForm)
    Actions: TActionList;
    SaveAction: TAction;
    Pages: TPageControl;
    NavigatorPage: TTabSheet;
    ColorPage: TTabSheet;
    ButtonPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SaveActionUpdate(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    FConfig: TConfig;
    FNavigatorConfig: TNavigatorConfigFrame;
    FColorConfig: TColorConfigFrame;
    procedure SetConfig(Value: TConfig);
    function GetIsModified: Boolean;
  public
    property Config: TConfig read FConfig write SetConfig;
    property IsModified: Boolean read GetIsModified;
  end;

var
  Config: TConfig;

procedure ShowConfig;

implementation

{$R *.lfm}

procedure ShowConfig;
var
  Dialog: TConfigForm;
begin
  Dialog := TConfigForm.Create(Application.MainForm);
  try
    Dialog.Config := Config;
    if Dialog.ShowModal = mrOk then
      Config.WriteConfig;
  finally
    Dialog.Free;
  end;
end;

{ TConfigForm }

procedure TConfigForm.FormCreate(Sender: TObject);
begin
  FNavigatorConfig := TNavigatorConfigFrame.Create(Self);
  FNavigatorConfig.Parent := NavigatorPage;
  FNavigatorConfig.Align := alClient;
  FColorConfig := TColorConfigFrame.Create(Self);
  FColorConfig.Parent := ColorPage;
  FColorConfig.Align := alClient;
end;

procedure TConfigForm.SaveActionExecute(Sender: TObject);
begin
  if FNavigatorConfig.IsModified then
    FNavigatorConfig.WriteConfig(FConfig);
  if FColorConfig.IsModified then
    FColorConfig.WriteConfig(FConfig);
  ModalResult := mrOk;
end;

procedure TConfigForm.SaveActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IsModified;
end;

procedure TConfigForm.SetConfig(Value: TConfig);
begin
  FConfig := Value;
  FNavigatorConfig.ReadConfig(FConfig);
  FColorConfig.ReadConfig(FConfig);
end;

function TConfigForm.GetIsModified: Boolean;
begin
  Result := FNavigatorConfig.IsModified or FColorConfig.IsModified;
end;

initialization
  Config := TConfig.Create;
finalization
  Config.Free;
end.

