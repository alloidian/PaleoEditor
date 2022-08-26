unit Actions;

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
  Classes, SysUtils, Forms, ActnList, Controls;

type
  TTileMode = (tbHorizontal, tbVertical);
  TWindowAction = class(TAction)
  private
    FForm: TCustomForm;
    procedure SetForm(Value: TCustomForm);
  protected
    function GetForm(Target: TObject): TForm; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    property Form: TCustomForm read FForm write SetForm;
  end;

  TWindowClose = class(TWindowAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TWindowCascade = class(TWindowAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TWindowTileHorizontal = class(TWindowAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TWindowTileVertical = class(TWindowAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TWindowMinimizeAll = class(TWindowAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TWindowArrange = class(TWindowAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;


implementation

uses
  Windows, Messages;

const
  MDITILE_VERTICAL     = 0;
  MDITILE_HORIZONTAL   = 1;

{ TWindowAction }

function TWindowAction.GetForm(Target: TObject): TForm;
begin
  { We could hard cast Target as a TForm since HandlesTarget "should" be called
    before ExecuteTarget and UpdateTarget, however, we're being safe. }
  Result := (Target as TForm);
end;

function TWindowAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Form <> nil) and (Target = Form) or
    (Form = nil) and (Target is TForm)) and
    (TForm(Target).FormStyle = fsMDIForm);
end;

procedure TWindowAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Form) then Form := nil;
end;

procedure TWindowAction.UpdateTarget(Target: TObject);
begin
  Enabled := GetForm(Target).MDIChildCount > 0;
end;

procedure TWindowAction.SetForm(Value: TCustomForm);
begin
  if Value <> FForm then
  begin
    FForm := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

constructor TWindowAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisableIfNoHandler := False;
  Enabled := csDesigning in ComponentState;
end;

{ TWindowClose }

procedure TWindowClose.ExecuteTarget(Target: TObject);
begin
  with GetForm(Target) do
    if ActiveMDIChild <> nil then ActiveMDIChild.Close;
end;

procedure TWindowClose.UpdateTarget(Target: TObject);
begin
  Enabled := GetForm(Target).ActiveMDIChild <> nil;
end;

{ TWindowCascade }

procedure TWindowCascade.ExecuteTarget(Target: TObject);
begin
  GetForm(Target).Cascade;
end;

{ TWindowTileHorizontal }

procedure DoTile(Form: TForm; TileMode: TTileMode);
const
  TileParams: array[TTileMode] of Word = (MDITILE_HORIZONTAL, MDITILE_VERTICAL);
begin
  if (Form.FormStyle = fsMDIForm) and (Form.ClientHandle <> 0) then
    SendMessage(Form.ClientHandle, WM_MDITILE, TileParams[TileMode], 0);
end;

procedure TWindowTileHorizontal.ExecuteTarget(Target: TObject);
begin
  DoTile(GetForm(Target), tbHorizontal);
end;

{ TWindowTileVertical }

procedure TWindowTileVertical.ExecuteTarget(Target: TObject);
begin
  DoTile(GetForm(Target), tbVertical);
end;

{ TWindowMinimizeAll }

procedure TWindowMinimizeAll.ExecuteTarget(Target: TObject);
var
  I: Integer;
begin
  { Must be done backwards through the MDIChildren array }
  with GetForm(Target) do
    for I := MDIChildCount - 1 downto 0 do
      MDIChildren[I].WindowState := wsMinimized;
end;

{ TWindowArrange }

procedure TWindowArrange.ExecuteTarget(Target: TObject);
begin
  GetForm(Target).ArrangeIcons;
end;

end.

