unit EditorConfigs;

{ Copyright ©2022-2023 by Steve Garcia. All rights reserved.

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
  CustomConfigFrames, ConfigUtils, Types;

type

  { TEditorConfigFrame }

  TEditorConfigFrame = class(TCustomConfigFrame)
    SampleGroup: TGroupBox;
    RightMarginLabel: TLabel;
    RightMarginEdit: TEdit;
    FontLabel: TLabel;
    SampleLabel: TLabel;
    SizeLabel: TLabel;
    SizeEdit: TEdit;
    FontEdit: TComboBox;
    SizeUpDown: TUpDown;
    RightMarginUpDown: TUpDown;
    procedure FontEditChange(Sender: TObject);
    procedure FontEditDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure SizeUpDownChanging(Sender: TObject; var AllowChange: Boolean);
    procedure SizeUpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
  private
    FFontName: String;
    FFontSize: Integer;
    FRightMargin: Integer;
  protected
    procedure PopulateFonts(List: TStrings);
    procedure PopulateFonts2(List: TStrings);
    function GetIsModified: Boolean; override;
    function GetFontName: String;
    procedure SetFontName(const Value: String);
    function GetFontSize: Integer;
    procedure SetFontSize(Value: Integer);
    function GetRightMargin: Integer;
    procedure SetRightMargin(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadConfig(Config: TConfig); override;
    procedure WriteConfig(Config: TConfig); override;
    property FontName: String read GetFontName write SetFontName;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property RightMargin: Integer read GetRightMargin write SetRightMargin;
  end;

implementation

{$R *.lfm}

uses
  StrUtils, LCLType, LCLIntf, LazUTF8, Configs;

const
  SAMPLE_TEXT =
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor '      +
    'incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud '  +
    'exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute '     +
    'irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla '  +
    'pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia ' +
    'deserunt mollit anim id est laborum.';

function EnumFamilyFonts(var eLogFont: TEnumLogFontEx; var Metric: TNewTextMetricEx;
  FontType: LongInt; Data: LParam): LongInt; stdcall;
var
  List: TStringList;
begin
  if FontType = TRUETYPE_FONTTYPE then begin
    List := TStringList(PtrInt(Data));
    if Assigned(List) then begin
      if (eLogFont.elfLogFont.lfPitchAndFamily and (FIXED_PITCH or MONO_FONT)) > 0 then
        if not AnsiStartsStr('@', eLogFont.elfFullName) then
          if List.IndexOf(eLogFont.elfFullName) < 0 then
            List.Add(eLogFont.elfFullName);
    end;
  end;
  Result := 1;
end;

{ TEditorConfigFrame }

constructor TEditorConfigFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PopulateFonts2(FontEdit.Items);
  SampleLabel.Caption := SAMPLE_TEXT;
end;

destructor TEditorConfigFrame.Destroy;
begin
  inherited;
end;

procedure TEditorConfigFrame.SizeUpDownChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Do nothing
end;

procedure TEditorConfigFrame.FontEditDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Edit: TComboBox;
  FontName: String = '';
begin
  if Control is TComboBox then begin
    Edit := Control as TComboBox;
    Edit.Canvas.FillRect(ARect);
    FontName := Edit.Items[Index];
    Edit.Canvas.Font.Name := FontName;
    Edit.Canvas.Font.Size := Edit.Font.Size;
    Edit.Canvas.TextOut(ARect.Left, ARect.Top, FontName);
  end;
end;

procedure TEditorConfigFrame.FontEditChange(Sender: TObject);
begin
  SampleLabel.Font.Name := (Sender as TComboBox).Text;
end;

procedure TEditorConfigFrame.SizeUpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  SampleLabel.Font.Size := NewValue;
end;

procedure TEditorConfigFrame.PopulateFonts(List: TStrings);
var
  Temp: TLabel;
  FontName: String = '';
begin
  Temp := TLabel.Create(nil);
  try
    List.BeginUpdate;
    try
      List.Clear;
      for FontName in Screen.Fonts do begin
        if not AnsiStartsStr('@', FontName) then begin
          Temp.Font.Name := FontName;
          if Temp.Font.IsMonoSpace then
            List.Add(FontName);
        end;
      end;
    finally
      List.EndUpdate;
    end;
  finally
    Temp.Free;
  end;
end;

procedure TEditorConfigFrame.PopulateFonts2(List: TStrings);
var
  LogFont: TLogFont;
begin
  LogFont.lfFaceName := EmptyStr;
  LogFont.lfCharSet := ANSI_CHARSET;
  LogFont.lfPitchAndFamily := FIXED_PITCH or MONO_FONT;
  List.BeginUpdate;
  try
    List.Clear;
    EnumFontFamiliesEx(Application.MainForm.Canvas.Handle, @LogFont, @EnumFamilyFonts, PtrInt(List), 0);
  finally
    List.EndUpdate;
  end;
end;

function TEditorConfigFrame.GetIsModified: Boolean;
begin
  Result := not AnsiSameText(FFontName, FontName) or (FFontSize <> FontSize) or
    (FRightMargin <> RightMargin);
end;

function TEditorConfigFrame.GetFontName: String;
begin
  Result := FontEdit.Text;
end;

procedure TEditorConfigFrame.SetFontName(const Value: String);
begin
  if FontEdit.Items.IndexOf(Value) < 0 then
    FontEdit.Text := INI_EDITOR_FONT_NAME_DEF
  else
    FontEdit.Text := Value;
end;

function TEditorConfigFrame.GetFontSize: Integer;
begin
  Result := StrToIntDef(SizeEdit.Text, INI_EDITOR_FONT_SIZE_DEF);
end;

procedure TEditorConfigFrame.SetFontSize(Value: Integer);
begin
  SizeEdit.Text := IntToStr(Value);
end;

function TEditorConfigFrame.GetRightMargin: Integer;
begin
  Result := StrToIntDef(RightMarginEdit.Text, INI_EDITOR_RIGHT_MARGIN_DEF);
end;

procedure TEditorConfigFrame.SetRightMargin(Value: Integer);
begin
  RightMarginEdit.Text := IntToStr(Value);
end;

procedure TEditorConfigFrame.ReadConfig(Config: TConfig);
begin
  FontName := Config.FontName;
  FontSize := Config.FontSize;
  FFontName := Config.FontName;
  FFontSize := Config.FontSize;
  RightMargin := Config.RightMargin;
  FRightMargin := Config.RightMargin;
  SampleLabel.Font.Name := Config.FontName;
  SampleLabel.Font.Size := Config.FontSize;
end;

procedure TEditorConfigFrame.WriteConfig(Config: TConfig);
begin
  Config.FontName := FontName;
  Config.FontSize := FontSize;
  Config.RightMargin := RightMargin;
end;

end.

