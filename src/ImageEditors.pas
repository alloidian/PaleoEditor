unit ImageEditors;

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
  Classes, SysUtils, Forms, Controls, ComCtrls, PrintersDlgs, Graphics, Dialogs, ExtCtrls,
  CustomEditors;

type

  { TImageEditorFrame }

  TImageEditorFrame = class(TCustomEditorFrame)
    ScrollBox: TScrollBox;
    Editor: TImage;
  protected
    function GetIsModified: Boolean; override;
    function GetInsertMode: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Open(Page: TTabSheet; Node: TTreeNode); override;
    procedure Save; override;
    procedure SaveAs(const FileName: TFileName); override;
    procedure ExportFile(const FileName: TFileName); override;
    procedure PrintFile(Dialog: TPrintDialog); override;
    procedure Revert; override;
    function Search(const Criteria: String; First, Backwards, MatchCase,
      MatchWholeWordOnly: Boolean): Boolean; override;
    procedure Replace(Criteria, Replacement: String; All, MatchCase,
      MatchWholeWordOnly: Boolean); override;
    procedure GotoLine(LineNumber: Integer); override;
    function LineNumber: Integer; override;
    procedure Idle; override;
    procedure RefreshConfig; override;
  end;

implementation

{$R *.lfm}

uses
  Utils;

{ TImageEditorFrame }

constructor TImageEditorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValidActions := [];
end;

function TImageEditorFrame.GetIsModified: Boolean;
begin
  Result := False;
end;

function TImageEditorFrame.GetInsertMode: Boolean;
begin
  Result := False;
end;

procedure TImageEditorFrame.Open(Page: TTabSheet; Node: TTreeNode);
begin
  inherited Open(Page, Node);
  if FileExists(Node.FullName) then begin
    Screen.BeginWaitCursor;
    try
      Editor.Picture.LoadFromFile(Node.FullName);
      IsModified := False;
    finally
      Screen.EndWaitCursor;
    end;
  end;
end;

procedure TImageEditorFrame.Save;
begin
  // Do nothing
end;

procedure TImageEditorFrame.SaveAs(const FileName: TFileName);
begin
  // Do nothing
end;

procedure TImageEditorFrame.ExportFile(const FileName: TFileName);
begin
  // Do nothing
end;

procedure TImageEditorFrame.PrintFile(Dialog: TPrintDialog);
begin
  // Do nothing
end;

procedure TImageEditorFrame.Revert;
begin
  // Do nothing
end;

function TImageEditorFrame.Search(const Criteria: String; First, Backwards, MatchCase,
  MatchWholeWordOnly: Boolean): Boolean;
begin
  // Do nothing
  Result := False;
end;

procedure TImageEditorFrame.Replace(Criteria, Replacement: String; All, MatchCase,
  MatchWholeWordOnly: Boolean);
begin
  // Do nothing
end;

procedure TImageEditorFrame.GotoLine(LineNumber: Integer);
begin
  // Do nothing
end;

function TImageEditorFrame.LineNumber: Integer;
begin
  Result := 0;
end;

procedure TImageEditorFrame.Idle;
begin
  // Do nothing
end;

procedure TImageEditorFrame.RefreshConfig;
begin
  // Do nothing
end;

end.

