unit PdfEditors;

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
  Classes, SysUtils, Forms, Controls, ComCtrls, PrintersDlgs, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ActnList, Buttons, CustomEditors, PdfiumCtrl, Types;

type

  { TPdfEditorFrame }

  TPdfEditorFrame = class(TCustomEditorFrame)
    FirstPageButton: TBitBtn;
    PrevPageButton: TBitBtn;
    NextPageButton: TBitBtn;
    LastPageButton: TBitBtn;
    Images: TImageList;
    PageCountLabel: TLabel;
    OfLabel: TLabel;
    PageIndexLabel: TLabel;
    ScaleLabel: TLabel;
    ScaleEdit: TComboBox;
    LastPageAction: TAction;
    FirstPageAction: TAction;
    PageIndexEdit: TEdit;
    PageCountEdit: TEdit;
    NextPageAction: TAction;
    PrevPageAction: TAction;
    ActionList: TActionList;
    ButtonPanel: TPanel;
    ScrollBar: TScrollBar;
    ScrollBox: TScrollBox;
    procedure FirstPageActionExecute(Sender: TObject);
    procedure LastPageActionExecute(Sender: TObject);
    procedure NextPageActionExecute(Sender: TObject);
    procedure NextPageActionUpdate(Sender: TObject);
    procedure PageIndexEditKeyPress(Sender: TObject; var Key: char);
    procedure PrevPageActionExecute(Sender: TObject);
    procedure PrevPageActionUpdate(Sender: TObject);
    procedure ScaleEditChange(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    FEditor: TPdfControl;
    procedure PopulateScale(List: TStrings);
  protected
    function GetIsModified: Boolean; override;
    function GetInsertMode: Boolean; override;
    function GetScale: TPdfControlScaleMode;
    procedure SetScale(Value: TPdfControlScaleMode);
    function GetZoom: Integer;
    procedure SetZoom(Value: Integer);
    function GetPageIndex: Integer;
    procedure SetPageIndex(Value: Integer);
    function GetPageCount: Integer;
    procedure SetPageCount(Value: Integer);
    procedure DoPageChange(Sender: TObject);
    procedure DoMouseWheelEvent(Sender: TObject; Shift: TShiftState;
           WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    property Editor: TPdfControl read FEditor;
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
    property Scale: TPdfControlScaleMode read GetScale write SetScale;
    property Zoom: Integer read GetZoom write SetZoom;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property PageCount: Integer read GetPageCount write SetPageCount;
  end;

implementation

{$R *.lfm}

uses
  LCLIntf, Printers, PdfiumCore, Utils, Searches, ConfigUtils;

{ TPdfEditorFrame }

constructor TPdfEditorFrame.Create(AOwner: TComponent);
begin
  FSyntax := ITEM_PDF_SYNTAX;
  inherited Create(AOwner);
  PDFiumDllDir := ExtractFilePath(Application.ExeName);
  FEditor := TPdfControl.Create(Self);
  FEditor.Align  := alClient;
  FEditor.Parent := ScrollBox;
  FEditor.Color  := clGray;
  FEditor.AllowUserTextSelection := False;
  FEditor.OnPageChange := DoPageChange;
  FEditor.AddHandlerOnMouseWheel(DoMouseWheelEvent);
//  PopulateScaleMode(ScaleEdit.Items);
  PopulateScale(ScaleEdit.Items);
  FValidActions := [vaCase, vaWord];
  Scale := smFitHeight;
end;

procedure TPdfEditorFrame.PrevPageActionExecute(Sender: TObject);
begin
  Editor.GotoPrevPage(True);
end;

procedure TPdfEditorFrame.PrevPageActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Editor.PageIndex > 0;
end;

procedure TPdfEditorFrame.ScaleEditChange(Sender: TObject);
begin
  Editor.ScaleMode := Scale;
  Editor.ZoomPercentage := Zoom;
end;

procedure TPdfEditorFrame.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  OldOnPageChange: TNotifyEvent;
begin
  case ScrollCode of
    scTrack:
      PageIndexEdit.Text := IntToStr(ScrollPos);
    scLineUp, scLineDown, scPageUp, scPageDown, scPosition, scTop, scBottom: begin
      OldOnPageChange := Editor.OnPageChange;
      Editor.OnPageChange := nil;
      try
        Editor.PageIndex := ScrollPos - 1;
        PageIndexEdit.Text := IntToStr(ScrollPos);
      finally
        Editor.OnPageChange := OldOnPageChange;
      end;
      end;
  end;
end;

procedure TPdfEditorFrame.NextPageActionExecute(Sender: TObject);
begin
  Editor.GotoNextPage(True);
end;

procedure TPdfEditorFrame.FirstPageActionExecute(Sender: TObject);
begin
  Editor.PageIndex := 0;
end;

procedure TPdfEditorFrame.LastPageActionExecute(Sender: TObject);
begin
  Editor.PageIndex := Editor.PageCount - 1;
end;

procedure TPdfEditorFrame.NextPageActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Editor.PageIndex < Editor.PageCount - 1;
end;

procedure TPdfEditorFrame.PageIndexEditKeyPress(Sender: TObject; var Key: char);
const
  VALID_CHARS = [#8, '0'..'9'];

  procedure GotoPage(PageIndex: Integer);
  begin
    if PageIndex < 1 then
      PageIndex := 1
    else
      if PageIndex > Editor.PageCount then
        PageIndex := Editor.PageCount;
    Editor.PageIndex := PageIndex - 1;
  end;

begin
  if Key = #13 then
    GotoPage(PageIndex)
  else
    if not (Key in VALID_CHARS) then
      Key := #0;
end;

procedure TPdfEditorFrame.PopulateScale(List: TStrings);
type
  TZoom = record
    Caption: String;
    Index: Integer;
  end;
  TZoomIndex = 0..11;
const
  ZOOMS: array[TZoomIndex] of TZoom =
   ((Caption: '10%'; Index:   -10),
    (Caption: '25%'; Index:   -25),
    (Caption: '50%'; Index:   -50),
    (Caption: '75%'; Index:   -75),
    (Caption: '100%'; Index:  -100),
    (Caption: '125%'; Index:  -125),
    (Caption: '150%'; Index:  -150),
    (Caption: '200%'; Index:  -200),
    (Caption: 'Actual Size'; Index: -100),
    (Caption: 'Zoom to Page'; Index: Integer(smFitAuto)),
    (Caption: 'Fit Width'; Index: Integer(smFitWidth)),
    (Caption: 'Fit Height'; Index: Integer(smFitHeight)));
var
  I: TZoomIndex;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(ZOOMS[I].Caption, TObject(ZOOMS[I].Index));
  finally
    List.EndUpdate
  end;
end;

function TPdfEditorFrame.GetIsModified: Boolean;
begin
  Result := False;
end;

function TPdfEditorFrame.GetInsertMode: Boolean;
begin
  Result := False;
end;

function TPdfEditorFrame.GetScale: TPdfControlScaleMode;
var
  ItemIndex: Integer;
  Temp: Integer;
begin
  ItemIndex := ScaleEdit.ItemIndex;
  if (ItemIndex > -1) and (ItemIndex < ScaleEdit.Items.Count) then begin
    Temp := Integer(ScaleEdit.Items.Objects[ScaleEdit.ItemIndex]);
    if Temp >= 0 then
      Result := TPdfControlScaleMode(Temp)
    else
      Result := smZoom; end
  else
    Result := smFitHeight;
end;

procedure TPdfEditorFrame.SetScale(Value: TPdfControlScaleMode);
var
  I: Integer;
begin
  ScaleEdit.ItemIndex := -1;
  for I := 0 to ScaleEdit.Items.Count - 1 do
    if TPdfControlScaleMode(ScaleEdit.Items.Objects[I]) = Value then begin
      ScaleEdit.ItemIndex := I;
      Break;
    end;
end;

function TPdfEditorFrame.GetZoom: Integer;
var
  ItemIndex: Integer;
  Temp: Integer;
begin
  ItemIndex := ScaleEdit.ItemIndex;
  if (ItemIndex > -1) and (ItemIndex < ScaleEdit.Items.Count) then begin
    Temp := Integer(ScaleEdit.Items.Objects[ScaleEdit.ItemIndex]);
    if Temp < 0 then
      Result := - Temp
    else
      Result := 100; end
  else
    Result := 100;
end;

procedure TPdfEditorFrame.SetZoom(Value: Integer);
var
  I: Integer;
begin
  ScaleEdit.ItemIndex := -1;
  Value := - Value;
  for I := 0 to ScaleEdit.Items.Count - 1 do
    if Integer(ScaleEdit.Items.Objects[I]) = Value then begin
      ScaleEdit.ItemIndex := I;
      Break;
    end;
end;

function TPdfEditorFrame.GetPageIndex: Integer;
begin
  Result := StrToIntDef(PageIndexEdit.Text, 0);
end;

procedure TPdfEditorFrame.SetPageIndex(Value: Integer);
begin
  PageIndexEdit.Text := IntToStr(Value);
  ScrollBar.Position := Value;
end;

function TPdfEditorFrame.GetPageCount: Integer;
begin
  Result := StrToIntDef(PageCountEdit.Text, 0);
end;

procedure TPdfEditorFrame.SetPageCount(Value: Integer);
begin
  PageCountEdit.Text := IntToStr(Value);
  ScrollBar.Max := Value;
end;

procedure TPdfEditorFrame.DoPageChange(Sender: TObject);
var
  Editor: TPdfControl;
begin
  Editor := Sender as TPdfControl;
  PageIndex := Editor.PageIndex + 1;
end;

procedure TPdfEditorFrame.DoMouseWheelEvent(Sender: TObject; Shift: TShiftState;
       WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta < 0 then
    NextPageAction.Execute
  else
    if WheelDelta > 0 then
      PrevPageAction.Execute;
end;

procedure TPdfEditorFrame.Open(Page: TTabSheet; Node: TTreeNode);
begin
  inherited Open(Page, Node);
  if FileExists(Node.FullName) then begin
    Editor.LoadFromFile(Node.FullName);
    PageCount := Editor.PageCount;
    IsModified := False;
  end;
end;

procedure TPdfEditorFrame.Save;
begin
  // Do nothing
end;

procedure TPdfEditorFrame.SaveAs(const FileName: TFileName);
begin
  // Do nothing
end;

procedure TPdfEditorFrame.ExportFile(const FileName: TFileName);
begin
  // Do nothing
end;

procedure TPdfEditorFrame.PrintFile(Dialog: TPrintDialog);
begin
  Printer.BeginDoc;
  try
    Editor.CurrentPage.Draw(Printer.Canvas.Handle, 0, 0, Printer.PageWidth,
      Printer.PageHeight, prNormal, [proPrinting]);
  finally
    Printer.EndDoc;
  end;
end;

procedure TPdfEditorFrame.Revert;
begin
  // Do nothing
end;

function TPdfEditorFrame.Search(const Criteria: String; First, Backwards, MatchCase,
  MatchWholeWordOnly: Boolean): Boolean;
begin
  if First then
    Editor.HightlightText(Criteria, MatchCase, MatchWholeWordOnly);
  Result := True;
end;

procedure TPdfEditorFrame.Replace(Criteria, Replacement: String; All, MatchCase,
  MatchWholeWordOnly: Boolean);
begin
  // Do nothing
end;

procedure TPdfEditorFrame.GotoLine(LineNumber: Integer);
begin
  // Do nothing
end;

function TPdfEditorFrame.LineNumber: Integer;
begin
  Result := 0;
end;

procedure TPdfEditorFrame.Idle;
begin
  // Do nothing
end;

procedure TPdfEditorFrame.RefreshConfig;
begin
  // Do nothing
end;

end.

