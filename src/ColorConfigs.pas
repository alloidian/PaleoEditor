unit ColorConfigs;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ColorBox,
  Buttons, SynEdit, CustomConfigFrames, ConfigUtils;

type

  { TColorConfigFrame }

  TColorConfigFrame = class(TCustomConfigFrame)
    AttributeLabel: TLabel;
    AttributeEdit: TComboBox;
    ForegroundLabel: TLabel;
    ForegroundEdit: TColorBox;
    BackgroundLabel: TLabel;
    BackgroundEdit: TColorBox;
    BoldEdit: TCheckBox;
    ItalicEdit: TCheckBox;
    DefaultButton: TSpeedButton;
    UnderlineEdit: TCheckBox;
    StrikeOutEdit: TCheckBox;
    Editor: TSynEdit;
    procedure AttributeEditChange(Sender: TObject);
    procedure DefaultButtonClick(Sender: TObject);
    procedure ForegroundEditChange(Sender: TObject);
    procedure BackgroundEditChange(Sender: TObject);
    procedure BoldEditChange(Sender: TObject);
    procedure ItalicEditChange(Sender: TObject);
    procedure StrikeOutEditChange(Sender: TObject);
    procedure UnderlineEditChange(Sender: TObject);
  private
    FCache: TAttributes;
    FBackup: TAttributes;
    procedure ApplyStyle(Edit: TCheckBox; Style: TFontStyles);
  protected
    function GetIsModified: Boolean; override;
    function GetSelected: TAttribute;
    function GetAttr: TAttributeType;
    procedure SetAttr(Value: TAttributeType);
    function GetForeground: TColor;
    procedure SetForeground(Value: TColor);
    function GetBackground: TColor;
    procedure SetBackground(Value: TColor);
    function GetStyle: TFontStyles;
    procedure SetStyle(Value: TFontStyles);
    procedure Refresh;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadConfig(Config: TConfig); override;
    procedure WriteConfig(Config: TConfig); override;
    property Selected: TAttribute read GetSelected;
    property Attr: TAttributeType read GetAttr write SetAttr;
    property Foreground: TColor read GetForeground write SetForeground;
    property Background: TColor read GetBackground write SetBackground;
    property Style: TFontStyles read GetStyle write SetStyle;
  end;

implementation

{$R *.lfm}

uses
  SynEditHighlighter, SynHighlighterZ80, Utils, Configs;

{ TColorConfigFrame }

constructor TColorConfigFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCache := TAttributes.Create;
  FBackup := TAttributes.Create;
  FCache.Populate(AttributeEdit.Items);
  Editor.Highlighter := TSynZ80Syn.Create(Self);
  Attr := atComment;
  Refresh;
end;

destructor TColorConfigFrame.Destroy;
begin
  FBackup.Free;
  FCache.Free;
  inherited;
end;

procedure TColorConfigFrame.AttributeEditChange(Sender: TObject);
begin
  Foreground := Selected.Foreground;
  Background := Selected.Background;
  Style := Selected.Style;
end;

procedure TColorConfigFrame.DefaultButtonClick(Sender: TObject);
const
  TITLE = 'Reset Color';
  CAPTION = 'Are you sure you want to reset the color scheme to default values?';
begin
  if MessageDlg(TITLE, CAPTION, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    FCache.Reset;
    Refresh;
  end;
end;

procedure TColorConfigFrame.ForegroundEditChange(Sender: TObject);
var
  Attribute: TSynHighlighterAttributes;
begin
  Selected.Foreground := Foreground;
  Attribute := Editor.Highlighter.RetrieveAttribute(Selected.StorageName);
  if Assigned(Attribute) then
    Attribute.Foreground := Foreground;
end;

procedure TColorConfigFrame.BackgroundEditChange(Sender: TObject);
var
  Attribute: TSynHighlighterAttributes;
begin
  Selected.Background := Background;
  Attribute := Editor.Highlighter.RetrieveAttribute(Selected.StorageName);
  if Assigned(Attribute) then
    Attribute.Background := Background;
end;

procedure TColorConfigFrame.BoldEditChange(Sender: TObject);
begin
  ApplyStyle(Sender as TCheckBox, [fsBold]);
end;

procedure TColorConfigFrame.ItalicEditChange(Sender: TObject);
begin
  ApplyStyle(Sender as TCheckBox, [fsItalic]);
end;

procedure TColorConfigFrame.StrikeOutEditChange(Sender: TObject);
begin
  ApplyStyle(Sender as TCheckBox, [fsStrikeOut]);
end;

procedure TColorConfigFrame.UnderlineEditChange(Sender: TObject);
begin
  ApplyStyle(Sender as TCheckBox, [fsUnderline]);
end;

procedure TColorConfigFrame.ApplyStyle(Edit: TCheckBox; Style: TFontStyles);
var
  Attribute: TAttribute;
  Temp: TSynHighlighterAttributes;
begin
  Attribute := Selected;
  if Assigned(Attribute) then begin
    if Edit.Checked then
      Attribute.Style := Attribute.Style + Style
    else
      Attribute.Style := Attribute.Style - Style;
    Temp := Editor.Highlighter.RetrieveAttribute(Selected.StorageName);
    if Assigned(Temp) then
      Temp.Style := Attribute.Style;
  end;
end;

function TColorConfigFrame.GetIsModified: Boolean;
begin
  Result := not FCache.Matches(FBackup);
end;

function TColorConfigFrame.GetSelected: TAttribute;
var
  I: Integer;
begin
  I := AttributeEdit.ItemIndex;
  if I < 0 then
    Result := nil
  else
    Result := TAttribute(AttributeEdit.Items.Objects[I]);
end;

function TColorConfigFrame.GetAttr: TAttributeType;
var
  Attribute: TAttribute;
begin
  Attribute := Selected;
  if Assigned(Attribute) then
    Result := Attribute.Attr
  else
    Result := atComment
end;

procedure TColorConfigFrame.SetAttr(Value: TAttributeType);
var
  I: Integer;
begin
  AttributeEdit.ItemIndex := -1;
  for I := 0 to AttributeEdit.Items.Count - 1 do begin
    if TAttribute(AttributeEdit.Items.Objects[I]).Attr = Value then begin
      AttributeEdit.ItemIndex := I;
      Break;
    end;
  end;
end;

function TColorConfigFrame.GetForeground: TColor;
begin
  Result := ForegroundEdit.Selected;
end;

procedure TColorConfigFrame.SetForeground(Value: TColor);
begin
  ForegroundEdit.Selected := Value;
end;

function TColorConfigFrame.GetBackground: TColor;
begin
  Result := BackgroundEdit.Selected;
end;

procedure TColorConfigFrame.SetBackground(Value: TColor);
begin
  BackgroundEdit.Selected := Value;
end;

function TColorConfigFrame.GetStyle: TFontStyles;
begin
  Result := [];
  if BoldEdit.Checked then
    Result := Result + [fsBold];
  if ItalicEdit.Checked then
    Result := Result + [fsItalic];
  if UnderlineEdit.Checked then
    Result := Result + [fsUnderline];
  if StrikeOutEdit.Checked then
    Result := Result + [fsStrikeOut];
end;

procedure TColorConfigFrame.SetStyle(Value: TFontStyles);
begin
  BoldEdit.Checked := fsBold in Value;
  ItalicEdit.Checked := fsItalic in Value;
  UnderlineEdit.Checked := fsUnderline in Value;
  StrikeOutEdit.Checked := fsStrikeOut in Value;
end;

procedure TColorConfigFrame.Refresh;
var
  Source: TAttribute;
  Attribute: TSynHighlighterAttributes;
begin
  Source := Selected;
  if Assigned(Source) then begin
    Foreground := Source.Foreground;
    Background := Source.Background;
    Style := Source.Style;
    Attribute := Editor.Highlighter.RetrieveAttribute(Source.StorageName);
    if Assigned(Attribute) then begin
      Attribute.Foreground := Source.Foreground;
      Attribute.Background := Source.Background;
      if fsBold in Source.Style then
        Attribute.Style := Attribute.Style + [fsBold]
      else
        Attribute.Style := Attribute.Style - [fsBold];
      if fsItalic in Source.Style then
        Attribute.Style := Attribute.Style + [fsItalic]
      else
        Attribute.Style := Attribute.Style - [fsItalic];
      if fsUnderLine in Source.Style then
        Attribute.Style := Attribute.Style + [fsUnderLine]
      else
        Attribute.Style := Attribute.Style - [fsUnderLine];
      if fsStrikeOut in Source.Style then
        Attribute.Style := Attribute.Style + [fsStrikeOut]
      else
        Attribute.Style := Attribute.Style - [fsStrikeOut];
    end;
  end;
end;

procedure TColorConfigFrame.ReadConfig(Config: TConfig);
begin
  FCache.Assign(Config.Attributes);
  FBackup.Assign(Config.Attributes);
end;

procedure TColorConfigFrame.WriteConfig(Config: TConfig);
begin
  Config.Attributes.Assign(FCache);
end;

end.

