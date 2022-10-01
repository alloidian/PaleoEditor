unit CustomConfigFrames;

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
  Classes, SysUtils, Forms, Controls, ConfigUtils;

type
  TCustomConfigFrame = class(TFrame)
  private
  protected
    function GetIsModified: Boolean; virtual; abstract;
  public
    procedure ReadConfig(Config: TConfig); virtual; abstract; overload;
    procedure WriteConfig(Config: TConfig); virtual; abstract; overload;
    procedure ReadConfig(Config: TCustomConfig); virtual; overload;
    procedure WriteConfig(Config: TCustomConfig); virtual; overload;
    property IsModified: Boolean read GetIsModified;
  end;

implementation

{$R *.lfm}

{ TCustomConfigFrame }

procedure TCustomConfigFrame.ReadConfig(Config: TCustomConfig);
begin
  // Do nothing
end;

procedure TCustomConfigFrame.WriteConfig(Config: TCustomConfig);
begin
  // Do nothing
end;

end.

