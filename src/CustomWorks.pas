unit CustomWorks;

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
  Classes, Windows, SysUtils, Forms, Controls, StrUtils, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Menus, ActnList, StdActns, Types, Utils, CustomEditors, Searches,
  ConfigUtils, Executions, DirMonitors;

type
  TIterateTreeNodeProc = procedure(Node: TTreeNode; var Continue: Boolean);

  { TCustomWorkForm }

  TCustomWorkForm = class(TForm)
    Images: TImageList;
    Actions: TActionList;
    NewFolderAction: TAction;
    NewFileAction: TAction;
    OpenFileAction: TAction;
    SaveFileAction: TAction;
    SaveAsFileAction: TAction;
    RenameFileAction: TAction;
    CloseFileAction: TAction;
    CloseAllFileAction: TAction;
    CloseAllOtherFilesAction: TAction;
    CloseUnmodifiedFilesAction: TAction;
    RevertFileAction: TAction;
    DeleteFolderAction: TAction;
    AssembleAction: TAction;
    ExecuteCommandAction: TAction;
    LaunchExecuteAction: TAction;
    ExportHtmlAction: TAction;
    FilePrintSetupAction: TAction;
    FilePrintAction: TAction;
    EditUndoAction: TEditUndo;
    EditSelectAllAction: TEditSelectAll;
    EditCutAction: TEditCut;
    EditCopyAction: TEditCopy;
    EditPasteAction: TEditPaste;
    EditDeleteAction: TEditDelete;
    FindAction: TAction;
    FindAllAction: TAction;
    ReplaceAction: TAction;
    LabelLookupAction: TAction;
    GotoAction: TAction;
    ViewNavigatorAction: TAction;
    ViewStatusAction: TAction;
    ForwardAction: TAction;
    BackwardAction: TAction;
    MoveFarRightAction: TAction;
    MoveFarLeftAction: TAction;
    MoveRightAction: TAction;
    MoveLeftAction: TAction;
    RefreshAction: TAction;
    CollapseAllAction: TAction;
    ExpandAllAction: TAction;
    LaunchExplorerAction: TAction;
    LaunchConsoleAction: TAction;
    LaunchConsoleEditorAction: TAction;
    LaunchExplorerEditorAction: TAction;
    CloseStatusAction: TAction;
    ClearStatusAction: TAction;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileNewFolderMenu: TMenuItem;
    FileNewFileMenu: TMenuItem;
    FileOpenMenu: TMenuItem;
    FileSaveMenu: TMenuItem;
    FileSaveAsMenu: TMenuItem;
    FileRenameMenu: TMenuItem;
    FileCloseMenu: TMenuItem;
    FileCloseAllMenu: TMenuItem;
    FileCloseAllOtherMenu: TMenuItem;
    FileCloseUnmodifiedMenu: TMenuItem;
    FileRevertMenu: TMenuItem;
    FileDeleteFileMenu: TMenuItem;
    ExecuteSeparator: TMenuItem;
    FileAssembleMenu: TMenuItem;
    FileExecuteMenu: TMenuItem;
    FileLaunchExplorerMenu: TMenuItem;
    FileLaunchConsoleMenu: TMenuItem;
    FileLaunchExecuteMenu: TMenuItem;
    PrintSeparator: TMenuItem;
    FileExportHtmlMenu: TMenuItem;
    FilePrintMenu: TMenuItem;
    FilePrintSetupMenu: TMenuItem;
    EditMenu: TMenuItem;
    EditUndoMenu: TMenuItem;
    CopySeparator: TMenuItem;
    EditSelectAllMenu: TMenuItem;
    FileCutMenu: TMenuItem;
    FileCopyMenu: TMenuItem;
    FilePasteMenu: TMenuItem;
    EditDeleteMenu: TMenuItem;
    SearchSeparator: TMenuItem;
    EditFindMenu: TMenuItem;
    EditFindAllMenu: TMenuItem;
    FileReplaceMenu: TMenuItem;
    LabelLookupMenu: TMenuItem;
    EditGotoMenu: TMenuItem;
    ViewMenu: TMenuItem;
    ViewNavigatorMenu: TMenuItem;
    ViewStatusMenu: TMenuItem;
    ViewForwardMenu: TMenuItem;
    ViewBackwardMenu: TMenuItem;
    EditorMenu: TPopupMenu;
    EditorCloseMenu: TMenuItem;
    EditorCloseAllMenu: TMenuItem;
    EditorCloseUnchangedMenu: TMenuItem;
    EditorMoveSeparator: TMenuItem;
    EditMoveLeftMenu: TMenuItem;
    EditorModeFarLeftMenu: TMenuItem;
    EditorMoveRightMenu: TMenuItem;
    EditorMoveFarRightMenu: TMenuItem;
    EditorOpenSeparator: TMenuItem;
    EditorOpenExmplorerMenu: TMenuItem;
    EditorOpenConsoleMenu: TMenuItem;
    NavigatorPopupMenu: TPopupMenu;
    RefreshMenu: TMenuItem;
    ExpandAllMenu: TMenuItem;
    CollapseAllMenu: TMenuItem;
    FileSeparator: TMenuItem;
    NavigatorNewFolderMenu: TMenuItem;
    NavigatorNewFileMenu: TMenuItem;
    NavigatorDeleteMenu: TMenuItem;
    LaunchSeparator: TMenuItem;
    LaunchExplorerMenu: TMenuItem;
    LaunchConsoleMenu: TMenuItem;
    AssembleMenu: TMenuItem;
    LaunchExecuteMenu: TMenuItem;
    StatusPopupMenu: TPopupMenu;
    CloseStatusMenu: TMenuItem;
    ClearStatusMenu: TMenuItem;
    Navigator: TTreeView;
    NavigatorSplitter: TSplitter;
    WorkPanel: TPanel;
    SearchPanel: TPanel;
    WorkPages: TPageControl;
    StatusPages: TPageControl;
    MessagePage: TTabSheet;
    LogEdit: TMemo;
    StatusSplitter: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IsFolderUpdate(Sender: TObject);
    procedure IsFileUpdate(Sender: TObject);
    procedure NavigatorActionUpdate(Sender: TObject);
    procedure NewFolderActionExecute(Sender: TObject);
    procedure NewFileActionExecute(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure OpenFileActionUpdate(Sender: TObject);
    procedure SaveFileActionExecute(Sender: TObject);
    procedure SaveFileActionUpdate(Sender: TObject);
    procedure SaveAsFileActionExecute(Sender: TObject);
    procedure SaveAsFileActionUpdate(Sender: TObject);
    procedure RenameFileActionExecute(Sender: TObject);
    procedure RenameFileActionUpdate(Sender: TObject);
    procedure CloseFileActionExecute(Sender: TObject);
    procedure CloseFileActionUpdate(Sender: TObject);
    procedure CloseAllFilesActionExecute(Sender: TObject);
    procedure CloseAllFilesActionUpdate(Sender: TObject);
    procedure CloseAllOtherFilesActionExecute(Sender: TObject);
    procedure CloseAllOtherFilesActionUpdate(Sender: TObject);
    procedure CloseUnmodifiedFilesActionExecute(Sender: TObject);
    procedure CloseUnmodifiedFilesActionUpdate(Sender: TObject);
    procedure RevertActionExecute(Sender: TObject);
    procedure RevertActionUpdate(Sender: TObject);
    procedure DeleteFolderActionExecute(Sender: TObject);
    procedure DeleteFolderActionUpdate(Sender: TObject);
    procedure AssembleActionExecute(Sender: TObject);
    procedure AssembleActionUpdate(Sender: TObject);
    procedure ExecuteCommandActionExecute(Sender: TObject);
    procedure ExecuteCommandActionUpdate(Sender: TObject);
    procedure LaunchExplorerActionExecute(Sender: TObject);
    procedure LaunchExplorerActionUpdate(Sender: TObject);
    procedure LaunchConsoleActionExecute(Sender: TObject);
    procedure LaunchConsoleActionUpdate(Sender: TObject);
    procedure LaunchExplorerEditorActionExecute(Sender: TObject);
    procedure LaunchExplorerEditorActionUpdate(Sender: TObject);
    procedure LaunchConsoleEditorActionExecute(Sender: TObject);
    procedure LaunchConsoleEditorActionUpdate(Sender: TObject);
    procedure LaunchExecuteActionExecute(Sender: TObject);
    procedure LaunchExecuteActionUpdate(Sender: TObject);
    procedure ExportHtmlActionExecute(Sender: TObject);
    procedure ExportHtmlActionUpdate(Sender: TObject);
    procedure FilePrintSetupActionExecute(Sender: TObject);
    procedure FilePrintActionExecute(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure FindActionUpdate(Sender: TObject);
    procedure ViewNavigatorActionExecute(Sender: TObject);
    procedure ViewNavigatorActionUpdate(Sender: TObject);
    procedure ViewStatusActionExecute(Sender: TObject);
    procedure ViewStatusActionUpdate(Sender: TObject);
    procedure ForwardActionExecute(Sender: TObject);
    procedure ForwardActionUpdate(Sender: TObject);
    procedure BackwardActionExecute(Sender: TObject);
    procedure BackwardActionUpdate(Sender: TObject);
    procedure MoveLeftActionExecute(Sender: TObject);
    procedure MoveLeftActionUpdate(Sender: TObject);
    procedure MoveFarLeftActionExecute(Sender: TObject);
    procedure MoveFarLeftActionUpdate(Sender: TObject);
    procedure MoveRightActionExecute(Sender: TObject);
    procedure MoveRightActionUpdate(Sender: TObject);
    procedure MoveFarRightActionExecute(Sender: TObject);
    procedure MoveFarRightActionUpdate(Sender: TObject);
    procedure RefreshActionExecute(Sender: TObject);
    procedure CollapseAllActionExecute(Sender: TObject);
    procedure ExpandAllActionExecute(Sender: TObject);
    procedure ClearStatusActionExecute(Sender: TObject);
    procedure ClearStatusActionUpdate(Sender: TObject);
    procedure CloseStatusActionExecute(Sender: TObject);
    procedure CloseStatusActionUpdate(Sender: TObject);
    procedure NavigatorDblClick(Sender: TObject);
    procedure NavigatorDeletion(Sender: TObject; Node: TTreeNode);
    procedure NavigatorAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure NavigatorKeyPress(Sender: TObject; var Key: char);
    procedure SearchEditDblClick(Sender: TObject);
    procedure WorkPagesChange(Sender: TObject);
    procedure DoSearch(Sender: TObject; const Criteria: String; First, Backwards, MatchCase,
      MatchWholeWordOnly, ForFile: Boolean; var WasFound: Boolean);
    procedure DoSearchAll(Sender: TObject; const Criteria, Filter: String; MatchCase,
      MatchWholeWordOnly: Boolean);
    procedure DoReplace(Sender: TObject; const Criteria, Replacement: String; All, MatchCase,
      MatchWholeWordOnly: Boolean);
    procedure DoRetrieveLabels(Sender: TObject; List: TStrings);
    procedure DoGotoLine(Sender: TObject; LineNumber: Integer);
    procedure DoOriginate(Sender: TObject; const Criteria, Filter: String);
    procedure AdjustSymbolFile(Sender: TObject);
    procedure WindowClickHandler(Sender: TObject);
    procedure WorkPagesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure WorkPagesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure WorkPagesMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState;
      X, Y: Integer);
  private
    FSearchFrame: TSearchFrame;
    FFindFileName: TFileName;
    FFindNode: TTreeNode;
    FItinerary: TItinerary;
    FSymbolFileName: TFileName;
    function GetIsModified: Boolean;
    function GetIsAllModified: Boolean;
    function GetActiveEditor: TCustomEditorFrame;
    procedure SetSearchMethod(Method: TSearchBy);
    procedure SelectNode(Node: TTreeNode; LineNumber: Integer = 0);
    function OpenFile(Node: TTreeNode): TTabSheet;
    procedure CloseFile(Page: TTabSheet);
    procedure DeleteFolderNode(Node: TTreeNode);
    procedure DeleteFileNode(Node: TTreeNode);
    procedure DeleteFolder(Node: TTreeNode);
    procedure DeleteFile(Node: TTreeNode);
    procedure OpenExplorer(Node: TTreeNode);
    procedure OpenConsole(Node: TTreeNode);
    procedure ExecuteCommand(Node: TTreeNode; Parameters: String);
    function FullPath(HomeFolder: String; FileName: String): String;
    function SearchNode(const FileName: TFileName): TTreeNode; overload;
    function SearchNode(Node: TTreeNode; const FileName: TFileName): TTreeNode; overload;
    function FindFirstFile(const FileName: TFileName; Backwards: Boolean): Boolean;
    function FindNextFile(Backwards: Boolean): Boolean;
    procedure SetNavigatorVisible(Value: Boolean);
    procedure SetStatusVisible(Value: Boolean);
    procedure DoLog(var Msg: TMessage); message WM_EXEC_LOG;
    procedure DoGlobalSearch(Sender: TObject; const Criteria, Filter: String;
      MatchCase, MatchWholeWordOnly: Boolean);
  protected
    FFolderName: TFileName;
    FConfigs: TCustomConfig;
    FDirMonitor: TDirMonitor;
    procedure RefreshView; virtual; abstract;
    procedure Log(const Text: String); overload;
    procedure Log(const Mask: String; Args: array of const); overload;
    procedure LogHandler(Sender: TObject; const Text: String);
    procedure LogEventHandler(Sender: TObject; LogEvent: TLogEvent);
    procedure DoFileEvent(Sender: TObject; Action: TDirMonitorAction;
      const FileName: TFileName); virtual;
    procedure DoExecuteComplete(Sender: TObject);
    procedure FindIdentifier(Sender: TObject; const Criteria, Filter: String;
      MatchCase, MatchWholeWordOnly: Boolean);
    procedure RetrieveLabels(List: TStrings);
    procedure CheckIfModified(Node: TTreeNode);
  public
    procedure Open(const FolderName: TFileName; ParentMenu: TMenuItem); virtual; abstract;
    procedure Idle;
    procedure RefreshConfig;
    property ActiveEditor: TCustomEditorFrame read GetActiveEditor;
    property Configs: TCustomConfig read FConfigs;
    property IsModified: Boolean read GetIsModified;
    property IsAllModified: Boolean read GetIsAllModified;
  end;

  TNavigatorIterator = class(TObject)
  private type
    TIterationEvent = procedure(Sender: TObject; Node: TTreeNode; var MayContinue: Boolean) of object;
  private
    FNavigator: TTreeView;
    FOnIterate: TIterationEvent;
  protected
    procedure DoIterate(Node: TTreeNode; var MayContinue: Boolean);
    property Navigator: TTreeView read FNavigator;
  public
    constructor Create(Navigator: TTreeView); virtual;
    destructor Destroy; override;
    procedure Execute;
    property OnIterate: TIterationEvent read FOnIterate write FOnIterate;
  end;

  TGlobalSearchEngine = class(TObject)
  private
    FEngine: TNavigatorIterator;
    FProject: TCustomWorkForm;
    FPages: TPageControl;
    FImages: TImageList;
    FReport: TListView;
    FBuffer: TStringList;
    FCriteria: String;
    FExtensions: TStringDynArray;
    FFilter: String;
    FMatchCase: Boolean;
    FMatchWholeWordOnly: Boolean;
    FOptions: TStringSearchOptions;
  protected
    procedure DoIterate(Sender: TObject; Node: TTreeNode; var MayContinue: Boolean);
    function GeneratePage: TListView;
  public
    constructor Create(Project: TCustomWorkForm; Navigator: TTreeView; Pages: TPageControl;
      Images: TImageList); virtual;
    destructor Destroy; override;
    procedure Execute;
    property Criteria: String read FCriteria write FCriteria;
    property Filter: String read FFilter write FFilter;
    property MatchCase: Boolean read FMatchCase write FMatchCase;
    property MatchWholeWordOnly: Boolean read FMatchWholeWordOnly write FMatchWholeWordOnly;
  end;

  TGlobalOriginateEngine = class(TObject)
  private
    FEngine: TNavigatorIterator;
    FProject: TCustomWorkForm;
    FPages: TPageControl;
    FImages: TImageList;
    FReport: TListView;
    FBuffer: TStringList;
    FCriteria: String;
    FExtensions: TStringDynArray;
    FFilter: String;
  protected
    procedure DoIterate(Sender: TObject; Node: TTreeNode; var MayContinue: Boolean);
    function GeneratePage: TListView;
  public
    constructor Create(Project: TCustomWorkForm; Navigator: TTreeView; Pages: TPageControl;
      Images: TImageList); virtual;
    destructor Destroy; override;
    procedure Execute;
    property Criteria: String read FCriteria write FCriteria;
    property Filter: String read FFilter write FFilter;
  end;

  TFileRefreshEngine = class(TObject)
  private
    FEngine: TNavigatorIterator;
    FForm: TCustomWorkForm;
    FAction: TDirMonitorAction;
    FFileName: TFileName;
  protected
    procedure DoIterate(Sender: TObject; Node: TTreeNode; var MayContinue: Boolean);
  public
    constructor Create(Form: TCustomWorkForm); virtual;
    destructor Destroy; override;
    procedure Execute;
    property Action: TDirMonitorAction read FAction write FAction;
    property FileName: TFileName read FFileName write FFileName;
  end;

implementation

{$R *.lfm}

uses
  System.UITypes, Masks, FileUtil, PrintersDlgs, LCLIntf, Generics.Collections,
  SyntaxEditors, HexEditors, Configs, NewFiles, Assemblers;

var
  Search_Index: Integer;

{ TCustomWorkForm }

procedure TCustomWorkForm.FormCreate(Sender: TObject);
begin
  FDirMonitor := TDirMonitor.Create;
  FDirMonitor.Actions := ALL_ACTIONS;
  FDirMonitor.Subdirectories := True;
  FDirMonitor.OnChange := DoFileEvent;
  FSearchFrame := TSearchFrame.Create(Self);
  FSearchFrame.Parent := SearchPanel;
  FSearchFrame.Align := alClient;
  FSearchFrame.OnSearch := DoSearch;
  FSearchFrame.OnSearchAll := DoSearchAll;
  FSearchFrame.OnSearchDeclaration := DoOriginate;
  FSearchFrame.OnReplace := DoReplace;
  FSearchFrame.OnRetrieveLabels := DoRetrieveLabels;
  FSearchFrame.OnLabelLookup := DoGotoLine;
  FSearchFrame.OnGotoLine := DoGotoLine;
  FSearchFrame.SearchBy := sbNone;
  FItinerary := TItinerary.Create;
  FSymbolFileName := EmptyStr;
end;

procedure TCustomWorkForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TCustomWorkForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
const
  PRMOPT = 'Do you want to close project with modified files?';
begin
  CanClose := not IsModified;
  if not CanClose then
    CanClose := MessageDlg(PRMOPT, mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

procedure TCustomWorkForm.FormDestroy(Sender: TObject);
var
  Temp: TStringList;
  I: Integer = 0;
  Page: TTabSheet;
  Editor: TCustomEditorFrame;
  Node: TTreeNode;
begin
  if FDirMonitor.Active then
    FDirMonitor.Stop;
  FDirMonitor.Free;
  FConfigs.WriteConfig;
  FConfigs.Free;
  FItinerary.Free;
  Config.WriteConfig(Navigator, FFolderName);
  Config.WriteConfig(StatusPages, FFolderName);
  if Config.SaveWorkspace then begin
    Temp := TStringList.Create;
    try
      for I := 0 to WorkPages.PageCount - 1 do begin
        Page := WorkPages.Pages[I];
        Editor := Page.Editor;
        if Assigned(Editor) then begin
          Node := Editor.Node;
          Temp.Values[Node.LogicalName] := EmptyStr;
        end;
      end;
      Config.WriteWorkspace(FFolderName, Temp);
    finally
      Temp.Free;
    end;
  end;
end;

procedure TCustomWorkForm.FormShow(Sender: TObject);
var
  Temp: TStringList;
  I: Integer = 0;
  ProjectName: String = '';
  Node: TTreeNode = nil;
begin
  Config.ReadConfig(Navigator, FFolderName);
  Config.ReadConfig(StatusPages, FFolderName);
  Temp := Config.ReadWorkspace(FFolderName);
  try
    for I := 0 to Temp.Count - 1 do begin
      ProjectName := Temp.Names[I];
      Node := SearchNode(ProjectName);
      if Assigned(Node) then
        Node.Page := OpenFile(Node);
    end;
  finally
    Temp.Free;
  end;
  if Assigned(Node) then
    Navigator.Selected := Node;
end;

function GetTabIndex(APageControl: TPageControl; X, Y: Integer): Integer;
var
  TabRect: TRect;
begin
  Result := APageControl.IndexOfTabAt(X, Y);
  if Result > -1 then begin
    TabRect := APageControl.TabRect(Result);
    if X > ((TabRect.Left + TabRect.Right) div 2) then
      Inc(Result);
  end;
end;

procedure TCustomWorkForm.IsFolderUpdate(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := Navigator.Selected;
  (Sender as TAction).Enabled := Assigned(Node) and (Node.Kind = pkFolder);
end;

procedure TCustomWorkForm.IsFileUpdate(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := Navigator.Selected;
  (Sender as TAction).Enabled := Assigned(Node) and (Node.Kind = pkFile);
end;

procedure TCustomWorkForm.NavigatorActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Navigator.Items.Count > 0;
end;

procedure TCustomWorkForm.NewFolderActionExecute(Sender: TObject);
var
  Node: TTreeNode;
  FolderName: TFileName = '';
  Attribute: TFileAttribute;
  Child: TTreeNode;
begin
  Node := Navigator.Selected;
  if Assigned(Node) then begin
    if Node.Kind  = pkFolder then begin
      FolderName := EmptyStr;
      if CreateNewFolder(Node.FullName, FolderName) then begin
        if not DirectoryExists(FolderName) then
          ForceDirectories(FolderName);
        Attribute := TFileAttribute.CreateFolder(FolderName);
        Child := Navigator.Items.AddChildFirst(Node, Attribute.ShortName);
        Child.ImageIndex := 0;
        Child.SelectedIndex := 4;
        Child.Data := Attribute;
      end;
      Node.Expand(False);
    end;
  end;
end;

procedure TCustomWorkForm.NewFileActionExecute(Sender: TObject);
var
  Node: TTreeNode;
  FileName: TFileName = '';
  Attribute: TFileAttribute;
  Child: TTreeNode;
begin
  Node := Navigator.Selected;
  if Assigned(Node) and (Node.Kind = pkFolder) then begin
    FileName := EmptyStr;
    if CreateNewFile(Node.FullName, FileName) then begin
      if not FileExists(FileName) then
        CloseHandle(FileCreate(FileName));
      Attribute := TFileAttribute.CreateFile(FileName, FFolderName);
      Child := Navigator.Items.AddChildFirst(Node, Attribute.ShortName);
      Child.ImageIndex := 2;
      Child.SelectedIndex := 5;
      Child.Data := Attribute;
    end;
    Node.Expand(False);
  end;
end;

procedure TCustomWorkForm.OpenFileActionExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := Navigator.Selected;
  if Assigned(Node) then begin
    Node.IsFileModified := False;
    case Node.Kind of
      pkFolder:
        Node.Expand(False);
      pkFile:
        if Assigned(Node.Page) then
          WorkPages.ActivePage := Node.Page
        else begin
          Node.Page := OpenFile(Node);
          FItinerary.Post(Node, 1);
        end;
    end;
  end;
end;

procedure TCustomWorkForm.OpenFileActionUpdate(Sender: TObject);
var
  Node: TTreeNode;
  Action: TAction;
begin
  Node := Navigator.Selected;
  Action := Sender as TAction;
  Action.Enabled := Assigned(Node);
  if Action.Enabled then
    case Node.Kind of
      pkFolder:
        Action.Hint := Format('Open the ''%s'' folder.', [Node.LogicalName]);
      pkFile:
        if Assigned(Node.Page) then
          Action.Hint := Format('Select the ''%s'' file.', [Node.LogicalName])
        else
          Action.Hint := Format('Open the ''%s'' file.', [Node.LogicalName]);
    end
  else
    Action.Hint := 'Cannot open an invalid node';
end;

procedure TCustomWorkForm.SaveFileActionExecute(Sender: TObject);
var
  Editor: TCustomEditorFrame;
begin
  Editor := ActiveEditor;
  if Assigned(Editor) then begin
    FDirMonitor.Pause;
    try
      Editor.Save;
    finally
      FDirMonitor.Resume;
    end;
  end;
end;

procedure TCustomWorkForm.SaveFileActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  Action.Enabled := Assigned(Node) and Assigned(Node.Page) and Assigned(Node.Page.Editor);
  if Action.Enabled then
    Action.Enabled := Node.Page.Editor.IsModified;
  if not Assigned(Node) then
    Action.Hint := 'Cannot save an invalid file.'
  else
    if Action.Enabled then
      Action.Hint := Format('Save the ''%s'' file.', [Node.LogicalName])
    else
      Action.Hint := Format('Save the ''%s'' file has not been modified.', [Node.LogicalName])
end;

procedure TCustomWorkForm.SaveAsFileActionExecute(Sender: TObject);
var
  Node: TTreeNode;
  Dialog: TSaveDialog;
begin
  Node := Navigator.Selected;
  if Assigned(Node) then begin
    Dialog := TSaveDialog.Create(nil);
    try
      Dialog.DefaultExt := ExtractFileExt(Node.ShortName);
      Dialog.InitialDir := ExtractFilePath(Node.FullName);
      Dialog.Filter := 'Listing File (*.lst)|*.lst';
      Dialog.Title := 'Save File As';
      if Dialog.Execute then
        ActiveEditor.SaveAs(Dialog.FileName);
    finally
      Dialog.Free;
    end;
  end;
end;

procedure TCustomWorkForm.SaveAsFileActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  Action.Enabled := Assigned(Node);
  if Action.Enabled then
      Action.Hint := Format('Save the ''%s'' file with a new filename.', [Node.LogicalName])
  else
    Action.Hint := 'Cannot save an invalid file.';
end;

procedure TCustomWorkForm.RenameFileActionExecute(Sender: TObject);
const
  PROMPT = 'Rename the file ''%s'' to:';
  CAPTION = 'Rename File';
  FAILURE = 'Cannot rename ''%s'' to ''%s''.';
var
  Node: TTreeNode;
  OldFileName: String = '';
  NewFileName: String = '';
  Temp: String = '';

  function Rename(const OldFileName, NewFileName: TFileName): Boolean;
  begin
    FDirMonitor.Pause;
    try
      Result := RenameFile(OldFileName, NewFileName);
    finally
      FDirMonitor.Resume;
    end;
  end;

begin
  Node := Navigator.Selected;
  if Assigned(Node) then begin
    OldFileName := Node.FullName;
    Temp := Node.ShortName;
    NewFileName := Format(PROMPT, [Temp]);
    if InputQuery(CAPTION, NewFileName, Temp) then begin
      NewFileName := ExtractFilePath(OldFileName);
      NewFileName := IncludeTrailingPathDelimiter(NewFileName);
      NewFileName := NewFileName + Temp;
      if not Rename(OldFileName, NewFileName) then
        ShowMessage(Format(FAILURE, [OldFileName, NewFileName]))
      else begin
        Node.RenameFile(NewFileName);
        Node.Text := Node.ShortName;
        if Assigned(Node.Page) then begin
          Node.Page.Caption := Node.ShortName;
          Node.Page.Editor.LogicalName := Node.LogicalName;
        end;
      end;
    end;
  end;
end;

procedure TCustomWorkForm.RenameFileActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  Action.Enabled := Assigned(Node) and (Node.Kind = pkFile);
  if not Assigned(Node) then
    Action.Hint := 'Cannot rename an invalid file.'
  else
    if Action.Enabled then
      Action.Hint := Format('Rename the ''%s'' file.', [Node.LogicalName])
    else
      Action.Hint := 'Cannot rename folders.';
end;

procedure TCustomWorkForm.CloseFileActionExecute(Sender: TObject);
begin
  CloseFile(WorkPages.ActivePage);
end;

procedure TCustomWorkForm.CloseFileActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  Action.Enabled := Assigned(WorkPages.ActivePage);
  if Assigned(Node) then
    if Action.Enabled then
      Action.Hint := Format('Close the ''%s'' file.', [Node.LogicalName])
    else
      Action.Hint := Format('The ''%s'' file is not opened.', [Node.LogicalName])
  else
    Action.Hint := 'The selected file is not opened.';
end;

procedure TCustomWorkForm.CloseAllFilesActionExecute(Sender: TObject);
var
  I: Integer = 0;
begin
  for I := WorkPages.PageCount - 1 downto 0 do
    CloseFile(WorkPages.Pages[I]);
  FItinerary.Clear;
end;

procedure TCustomWorkForm.CloseAllFilesActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := WorkPages.PageCount > 1;
end;

procedure TCustomWorkForm.CloseAllOtherFilesActionExecute(Sender: TObject);
type
  TIntList = TList<Integer>;
var
  List: TIntList;
  I: Integer = 0;
begin
  List := TIntList.Create;
  try
    for I := 0 to WorkPages.PageCount - 1 do begin
      if WorkPages.Pages[I].PageIndex <> WorkPages.ActivePageIndex then
        List.Add(I);
    end;
    for I := List.Count - 1 downto 0 do
      CloseFile(WorkPages.Pages[List[I]]);
  finally
    List.Free;
  end;
end;

procedure TCustomWorkForm.CloseAllOtherFilesActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := WorkPages.PageCount > 1;
end;

procedure TCustomWorkForm.CloseUnmodifiedFilesActionExecute(Sender: TObject);
type
  TIntList = TList<Integer>;
var
  List: TIntList;
  I: Integer = 0;
begin
  List := TIntList.Create;
  try
    for I := 0 to WorkPages.PageCount - 1 do begin
      if not WorkPages.Pages[I].Editor.IsModified then
        List.Add(I);
    end;
    for I := List.Count - 1 downto 0 do
      CloseFile(WorkPages.Pages[List[I]]);
  finally
    List.Free;
  end;
  if Assigned(ActiveEditor) then
    Navigator.Selected := ActiveEditor.Node;
end;

procedure TCustomWorkForm.CloseUnmodifiedFilesActionUpdate(Sender: TObject);
var
  Temp: Boolean = False;
  I: Integer = 0;
begin
  Temp := False;
  for I := 0 to WorkPages.PageCount - 1 do
    if not WorkPages.Pages[I].Editor.IsModified then begin
      Temp := True;
      Break;
    end;
  (Sender as TAction).Enabled := Temp;
end;

procedure TCustomWorkForm.RevertActionExecute(Sender: TObject);
const
  PROMPT = 'Are you sure you want to revert your changes?';
var
  Editor: TCustomEditorFrame;
  CurrentLine: Integer = 0;
begin
  Editor := ActiveEditor;
  if Assigned(Editor) then
    if MessageDlg(PROMPT, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      CurrentLine := Editor.LineNumber;
      try
        Editor.Revert;
      finally
        Editor.GotoLine(CurrentLine);
      end;
    end;
end;

procedure TCustomWorkForm.RevertActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  Action.Enabled := IsModified;
  if Assigned(Node) then
    if Action.Enabled then
      Action.Hint := Format('Revert the ''%s'' file.', [Node.LogicalName])
    else
      Action.Hint := Format('The ''%s'' file is not modified.', [Node.LogicalName])
  else
    Action.Hint := 'Cannot revert an invalid file.';

end;

procedure TCustomWorkForm.DeleteFolderActionExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := Navigator.Selected;
  if Assigned(Node) then begin
    case Node.Kind of
      pkFolder : DeleteFolder(Node);
      pkFile: DeleteFile(Node);
    end;
  end;
end;

procedure TCustomWorkForm.DeleteFolderActionUpdate(Sender: TObject);
const
  NAMES: array[TFileAttribute.TPropertyKind] of String =
   ('node',    // pkUnknown
    'folder',  // pkFolder
    'file');   // pkFile
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  Action.Enabled := Assigned(Node);
  if Action.Enabled then
    Action.Hint := Format('Delete the ''%s'' %s.', [Node.LogicalName, NAMES[Node.Kind]])
  else
    Action.Hint := 'Cannot delete an invalid node.';
end;

procedure TCustomWorkForm.AssembleActionExecute(Sender: TObject);
const
  MASK = 'TASMTABS=%s';
var
  Node: TTreeNode;
  Parameters: String = '';
  Platform: TAssemblerPlatform;
  UpdateSymbols: Boolean = False;
  Engine: TExecutionThread;
begin
  if FConfigs.HasAssembler then begin
    Node := Navigator.Selected;
    if Assigned(Node) then begin
      Parameters := EmptyStr;
      Platform := apZ80;
      UpdateSymbols := False;
      if AssembleFile(Node, Parameters, Platform, UpdateSymbols) then begin
        if not StatusPages.Visible then
          SetStatusVisible(True);
        StatusPages.ActivePage := MessagePage;
        LogEdit.Lines.Clear;
        Engine := TExecutionThread.Create;
        Engine.ProgramName := FConfigs.AssemblerNameFile;
        Engine.FolderName := ExtractFilePath(Node.FullName);
        Engine.Parameters := Parameters;
        Engine.Environment.Text := Format(MASK, [FConfigs.AssemblerFolderName]);
        Engine.OnLog := LogEventHandler;
        if not UpdateSymbols then
          Engine.OnTerminate := DoExecuteComplete
        else begin
          FSymbolFileName := Node.FullName;
          Engine.OnTerminate := AdjustSymbolFile;
        end;
        FDirMonitor.Pause;
        Engine.Start;
      end;
    end;
  end;
end;

procedure TCustomWorkForm.AssembleActionUpdate(Sender: TObject);
const
  NO_ASSEMBLER = 'The ''TASM32'' assembler is not found. ';
  NO_ASSEMBLE  = 'Cannot assemble the ''%s'' file within the editor. ';
  YES_ASSEMBLE = 'Assemble the ''%s'' assembly file within the editor.';
  NO_FILE      = 'Cannot assemble.';
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Action.Enabled := FConfigs.HasAssembler;
  if not Action.Enabled then
    Action.Hint := NO_ASSEMBLER
  else begin
    Node := Navigator.Selected;
    if not Assigned(Node) then begin
      Action.Enabled := False;
      Action.Hint := NO_FILE; end
    else begin
      Action.Enabled := Node.IsAssemblable;
      if Action.Enabled then
        Action.Hint := Format(YES_ASSEMBLE, [Node.LogicalName])
      else
        Action.Hint := Format(NO_ASSEMBLE, [Node.LogicalName]);
    end;
  end;
end;

procedure TCustomWorkForm.ExecuteCommandActionExecute(Sender: TObject);
var
  Node: TTreeNode;
  Parameters: String = '';
  Engine: TExecutionThread;
begin
  Node := Navigator.Selected;
  if Assigned(Node) then
    Parameters := EmptyStr;
    if ExecuteFile(Node, Parameters) then begin
      if not StatusPages.Visible then
        SetStatusVisible(True);
      StatusPages.ActivePage := MessagePage;
      LogEdit.Lines.Clear;
      Engine := TExecutionThread.Create;
      Engine.ProgramName := Node.FullName;
      Engine.Parameters := Parameters;
      Engine.OnLog := LogEventHandler;
      Engine.OnTerminate := DoExecuteComplete;
      FDirMonitor.Pause;
      Engine.Start;
    end;
end;

procedure TCustomWorkForm.ExecuteCommandActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  if Assigned(Node) then
    Action.Enabled := Node.IsExecutable
  else
    Action.Enabled := False;
  if Action.Enabled then
    Action.Hint := Format('Execute the ''%s'' command file within the editor.', [Node.LogicalName])
  else
    Action.Hint := 'Execute the selected command file within the editor.';
end;

procedure TCustomWorkForm.LaunchExplorerActionExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := Navigator.Selected;
  if Assigned(Node) then
    OpenExplorer(Node);
end;

procedure TCustomWorkForm.LaunchExplorerActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  Action.Enabled := Assigned(Node) and (Node.Kind in [pkFolder, pkFile]);
  if Action.Enabled then
    case Node.Kind of
      pkFolder: Action.Hint := Format('Open the ''%s'' folder in Windows Explorer.', [Node.LogicalName]);
      pkFile: Action.Hint := Format('Open the folder containing the ''%s'' file in Windows Explorer.', [Node.LogicalName]);
    end
  else
    Action.Hint := 'Cannot open Windows Explorer.';
end;

procedure TCustomWorkForm.LaunchConsoleActionExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := Navigator.Selected;
  if Assigned(Node) then begin
    case Node.Kind of
      pkFolder: OpenConsole(Node);
      pkFile: OpenConsole(Node);
    end;
  end;
end;

procedure TCustomWorkForm.LaunchConsoleActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  Action.Enabled := Assigned(Node) and (Node.Kind in [pkFolder, pkFile]);
  if Action.Enabled then
    case Node.Kind of
      pkFolder: Action.Hint := Format('Open the ''%s'' folder in console.', [Node.LogicalName]);
      pkFile: Action.Hint := Format('Open the folder containing the ''%s'' file in console.', [Node.LogicalName]);
    end
  else
    Action.Hint := 'Cannot open the console.';
end;

procedure TCustomWorkForm.LaunchExplorerEditorActionExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  if Assigned(ActiveEditor) then begin
    Node := ActiveEditor.Node;
    if Assigned(Node) and (Node.Kind = pkFile) then
      OpenExplorer(Node);
  end;
end;

procedure TCustomWorkForm.LaunchExplorerEditorActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  if Assigned(ActiveEditor) then begin
    Action := Sender as TAction;
    Node := ActiveEditor.Node;
    Action.Enabled := Assigned(Node) and (Node.Kind in [pkFile]);
    if Action.Enabled then
      case Node.Kind of
        pkFolder: Action.Hint := Format('Open the ''%s'' editro in Windows Explorer.', [Node.LogicalName]);
        pkFile: Action.Hint := Format('Open the folder containing the ''%s'' editor in Windows Explorer.', [Node.LogicalName]);
      end
    else
      Action.Hint := 'Cannot open Windows Explorer.';
  end;
end;

procedure TCustomWorkForm.LaunchConsoleEditorActionExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  if Assigned(ActiveEditor) then begin
    Node := ActiveEditor.Node;
    if Assigned(Node) and (Node.Kind = pkFile) then
      OpenConsole(Node);
  end;
end;

procedure TCustomWorkForm.LaunchConsoleEditorActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  if Assigned(ActiveEditor) then begin
    Action := Sender as TAction;
    Node := ActiveEditor.Node;
    Action.Enabled := Assigned(Node) and (Node.Kind in [pkFile]);
    if Action.Enabled then
      case Node.Kind of
        pkFolder: Action.Hint := Format('Open the ''%s'' editor in console.', [Node.LogicalName]);
        pkFile: Action.Hint := Format('Open the folder containing the ''%s'' editor in console.', [Node.LogicalName]);
      end
    else
      Action.Hint := 'Cannot open the console.';
  end;
end;

procedure TCustomWorkForm.LaunchExecuteActionExecute(Sender: TObject);
var
  Node: TTreeNode;
  Parameters: String = '';
begin
  Node := Navigator.Selected;
  if Assigned(Node) then begin
    Parameters := EmptyStr;
    if ExecuteFile(Node, Parameters) then begin
      FDirMonitor.Pause;
      try
        ExecuteCommand(Node, Parameters);
      finally
        FDirMonitor.Resume;
      end;
    end;
  end;
end;

procedure TCustomWorkForm.LaunchExecuteActionUpdate(Sender: TObject);
var
  Action: TAction;
  Node: TTreeNode;
begin
  Action := Sender as TAction;
  Node := Navigator.Selected;
  Action.Enabled := Assigned(Node) and Node.IsExecutable;
  if Action.Enabled then
    Action.Hint := Format('Execute the ''%s'' command file within console.', [Node.LogicalName])
  else
    Action.Hint := 'Execute selected command file within console.';
end;

procedure TCustomWorkForm.ExportHtmlActionExecute(Sender: TObject);
var
  Editor: TCustomEditorFrame;
  FileName: TFileName = '';
begin
  Editor := ActiveEditor;
  if Assigned(Editor) then begin
    FileName := ChangeFileExt(Editor.Node.FullName, '.html');
    Editor.ExportFile(FileName);
    if FileExists(FileName) then
      OpenURL(FileName);
  end;
end;

procedure TCustomWorkForm.ExportHtmlActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(ActiveEditor);
end;

procedure TCustomWorkForm.FilePrintSetupActionExecute(Sender: TObject);
var
  Dialog: TPrinterSetupDialog;
begin
  Dialog := TPrinterSetupDialog.Create(nil);
  try
    Dialog.Title := 'Print File';
    if Dialog.Execute then
      ShowMessage(UNIMPLEMENTED_PROMPT);
  finally
    Dialog.Free;
  end;
end;

procedure TCustomWorkForm.FilePrintActionExecute(Sender: TObject);
var
  Dialog: TPrintDialog;
begin
  Dialog := TPrintDialog.Create(nil);
  try
    Dialog.Title := 'Print File';
    if Dialog.Execute then
      ShowMessage(UNIMPLEMENTED_PROMPT);
  finally
    Dialog.Free;
  end;
end;

procedure TCustomWorkForm.FindActionExecute(Sender: TObject);
begin
  SetSearchMethod(TSearchBy((Sender as TAction).Tag));
  if Assigned(ActiveEditor) then begin
    FSearchFrame.ValidActions := ActiveEditor.ValidActions;
    if FSearchFrame.Criteria.IsEmpty then
      FSearchFrame.Criteria := ActiveEditor.SelectedText;
  end;
end;

procedure TCustomWorkForm.FindActionUpdate(Sender: TObject);
var
  Action: TAction;
begin
  Action := Sender as TAction;
  Action.Enabled := Assigned(ActiveEditor);
  if Action.Enabled then
    Action.Checked := TSearchBy(Action.Tag) = FSearchFrame.SearchBy
  else
    Action.Checked := False;
end;

procedure TCustomWorkForm.ViewNavigatorActionExecute(Sender: TObject);
begin
  SetNavigatorVisible(not Navigator.Visible);
end;

procedure TCustomWorkForm.ViewNavigatorActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := Navigator.Visible;
end;

procedure TCustomWorkForm.ViewStatusActionExecute(Sender: TObject);
begin
  SetStatusVisible(not StatusPages.Visible);
end;

procedure TCustomWorkForm.ViewStatusActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := StatusPages.Visible;
end;

procedure TCustomWorkForm.ForwardActionExecute(Sender: TObject);
var
  Stop: TStop;
begin
  Stop := FItinerary.GoForward;
  if Assigned(Stop) then
    SelectNode(Stop.Node, Stop.LineNumber);
end;

procedure TCustomWorkForm.ForwardActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not FItinerary.IsLast;
end;

procedure TCustomWorkForm.BackwardActionExecute(Sender: TObject);
var
  Stop: TStop;
begin
  Stop := FItinerary.GoBack;
  if Assigned(Stop) then
    SelectNode(Stop.Node, Stop.LineNumber);
end;

procedure TCustomWorkForm.BackwardActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not FItinerary.IsFirst;
end;

procedure TCustomWorkForm.MoveLeftActionExecute(Sender: TObject);
begin
  WorkPages.ActivePage.PageIndex := WorkPages.ActivePage.TabIndex - 1;
end;

procedure TCustomWorkForm.MoveLeftActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := WorkPages.ActivePageIndex > 0;
end;

procedure TCustomWorkForm.MoveFarLeftActionExecute(Sender: TObject);
begin
  WorkPages.ActivePage.PageIndex := 0;
end;

procedure TCustomWorkForm.MoveFarLeftActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := WorkPages.ActivePageIndex > 1;
end;

procedure TCustomWorkForm.MoveRightActionExecute(Sender: TObject);
begin
  WorkPages.ActivePage.PageIndex := WorkPages.ActivePage.TabIndex + 1;
end;

procedure TCustomWorkForm.MoveRightActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (WorkPages.ActivePageIndex > -1) and (WorkPages.ActivePageIndex < WorkPages.PageCount - 1);
end;

procedure TCustomWorkForm.MoveFarRightActionExecute(Sender: TObject);
begin
  WorkPages.ActivePage.PageIndex := WorkPages.PageCount - 1;
end;

procedure TCustomWorkForm.MoveFarRightActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (WorkPages.ActivePageIndex > -1) and (WorkPages.ActivePageIndex < WorkPages.PageCount - 2);
end;

procedure TCustomWorkForm.RefreshActionExecute(Sender: TObject);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    RefreshView;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TCustomWorkForm.CollapseAllActionExecute(Sender: TObject);
begin
  Navigator.FullCollapse;
end;

procedure TCustomWorkForm.ExpandAllActionExecute(Sender: TObject);
begin
  Navigator.FullExpand;
end;

procedure TCustomWorkForm.ClearStatusActionExecute(Sender: TObject);
begin
  LogEdit.Lines.Clear;
end;

procedure TCustomWorkForm.ClearStatusActionUpdate(Sender: TObject);
var
  Page: TTabSheet;
begin
  Page := StatusPages.ActivePage;
  (Sender as TAction).Enabled := Assigned(Page) and (Page.Tag = 0) and (LogEdit.Lines.Count > 0);
end;

procedure TCustomWorkForm.CloseStatusActionExecute(Sender: TObject);
var
  Page: TTabSheet;
begin
  Page := StatusPages.ActivePage;
  if Assigned(Page) then
    Page.Free;
end;

procedure TCustomWorkForm.CloseStatusActionUpdate(Sender: TObject);
var
  Page: TTabSheet;
begin
  Page := StatusPages.ActivePage;
  (Sender as TAction).Enabled := Assigned(Page) and (Page.Tag > 0);
end;

procedure TCustomWorkForm.NavigatorDblClick(Sender: TObject);
begin
  OpenFileAction.Execute;
end;

procedure TCustomWorkForm.NavigatorDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then begin
    TFileAttribute(Node.Data).Free;
    Node.Data := nil;
  end;
end;

procedure TCustomWorkForm.NavigatorAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
const
  COLORS: array[Boolean] of TColor = (clDefault, clTeal);
  STYLES: array[Boolean] of TFontStyles = ([], [fsBold]);
var
  Font: TFont;
  IsFolder: Boolean = False;
begin
  Font := Sender.Canvas.Font;
  IsFolder := Node.Kind = pkFolder;
  Font.Color := COLORS[IsFolder];
  Font.Style := STYLES[IsFolder];
end;

procedure TCustomWorkForm.NavigatorKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    OpenFileAction.Execute;
end;

procedure TCustomWorkForm.SearchEditDblClick(Sender: TObject);
{ This event handler is attached to the Search Result list view and allows the associated
  file and line to be accessed (brought to front and selected) when the list view row is
  double-clicked. }
var
  Page: TTabSheet;
  View: TListView;
  Item: TListItem;
  Node: TTreeNode;
  LineNumber: Integer = 0;
begin
  Page := StatusPages.ActivePage;
  if Assigned(Page) and (Page.Tag > 0) then begin
    View := TListView(Page.Tag);
    if Assigned(View) then begin
      Item := View.Selected;
      if Assigned(Item) then begin
        Node := TTreeNode(Item.Data);
        if Assigned(Node) then begin
          Navigator.Selected := Node;
          if Assigned(Node.Page) then
            WorkPages.ActivePage := Node.Page
          else
            Node.Page := OpenFile(Node);
          LineNumber := Item.SubItems[0].ToInteger + 1;
          FItinerary.Post(Node, LineNumber);
          ActiveEditor.GotoLine(LineNumber);
        end;
      end;
    end;
  end;
end;

procedure TCustomWorkForm.WorkPagesChange(Sender: TObject);
{ This event handler is attached to the WorkPages that selects the Navigator node
  associated with the selected page, thus keeping the Navigator and WorkPages in sync. }
var
  Page: TTabSheet;
  Editor: TCustomEditorFrame;
begin
  Page := WorkPages.ActivePage;
  if Assigned(Page) then begin
    Editor := Page.Editor;
    if Assigned(Editor) then begin
      CheckIfModified(Editor.Node);
      Navigator.Selected := Editor.Node;
      FSearchFrame.ValidActions := Editor.ValidActions;
    end;
  end;
end;

procedure TCustomWorkForm.DoSearch(Sender: TObject; const Criteria: String; First, Backwards,
    MatchCase, MatchWholeWordOnly, ForFile: Boolean; var WasFound: Boolean);
var
  Editor: TCustomEditorFrame;
begin
  if ForFile then
    if First then
      WasFound := FindFirstFile(Criteria, Backwards)
    else
      WasFound := FindNextFile(Backwards)
  else begin
    Editor := ActiveEditor;
    if Assigned(Editor) then begin
      WasFound := Editor.Search(Criteria, First, Backwards, MatchCase, MatchWholeWordOnly);
    end;
  end;
end;

procedure TCustomWorkForm.DoSearchAll(Sender: TObject; const Criteria, Filter: String;
    MatchCase, MatchWholeWordOnly: Boolean);
var
  Engine: TGlobalSearchEngine;
begin
  if not StatusPages.Visible then
    SetStatusVisible(True);
  Engine := TGlobalSearchEngine.Create(Self, Navigator, StatusPages, Images);
  try
    Engine.Criteria := Criteria;
    Engine.Filter := Filter;
    Engine.MatchCase := MatchCase;
    Engine.MatchWholeWordOnly := MatchWholeWordOnly;
    Engine.Execute;
  finally
    Engine.Free;
  end;
end;

procedure TCustomWorkForm.DoReplace(Sender: TObject; const Criteria, Replacement: String;
  All, MatchCase, MatchWholeWordOnly: Boolean);
var
  Editor: TCustomEditorFrame;
begin
  Editor := ActiveEditor;
  if Assigned(Editor) then begin
    Editor.Replace(Criteria, Replacement, All, MatchCase, MatchWholeWordOnly);
  end;
end;

procedure TCustomWorkForm.DoRetrieveLabels(Sender: TObject; List: TStrings);
begin
  RetrieveLabels(List);
end;

procedure TCustomWorkForm.DoGotoLine(Sender: TObject; LineNumber: Integer);
var
  Editor: TCustomEditorFrame;
begin
  Editor := ActiveEditor;
  if Assigned(Editor) then begin
    Editor.GotoLine(LineNumber);
  end;
end;

procedure TCustomWorkForm.DoOriginate(Sender: TObject; const Criteria, Filter: String);
var
  Engine: TGlobalOriginateEngine;
begin
  if not StatusPages.Visible then
    SetStatusVisible(True);
  Engine := TGlobalOriginateEngine.Create(Self, Navigator, StatusPages, Images);
  try
    Engine.Criteria := Criteria;
    Engine.Filter := Filter;
    Engine.Execute;
  finally
    Engine.Free;
  end;
end;

procedure TCustomWorkForm.AdjustSymbolFile(Sender: TObject);
var
  Temp: TStringList;
  I: Integer = 0;
  L: String = '';
begin
  FDirMonitor.Resume;
  Temp := TStringList.Create;
  try
    FSymbolFileName := ChangeFileExt(FSymbolFileName, '.sym');
    if FileExists(FSymbolFileName) then begin
      Temp.LoadFromFile(FSymbolFileName);
      for I := 0 to Temp.Count - 1 do begin
        L := Temp[I];
        L := Trim(Copy(L, 19, Length(L))) + ' ' + Trim(Copy(L, 1, 18));
        Temp[I] := L;
      end;
      Temp.SaveToFile(FSymbolFileName);
    end;
  finally
    Temp.Free;
    FSymbolFileName := EmptyStr;
  end;
end;

procedure TCustomWorkForm.WindowClickHandler(Sender: TObject);
begin
  BringToFront;
end;

procedure TCustomWorkForm.WorkPagesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SenderPages: TPageControl;
  SourcePages: TPageControl;

  procedure DoTabDragDrop(Sender, Source: TPageControl; X, Y: Integer);
  var
    SourceIndex: Integer = 0;
    TargetIndex: Integer = 0;
    ATabSheet: TTabSheet;
  begin
     if Sender = Source then begin
       TargetIndex := GetTabIndex(Sender, X, Y);
       if TargetIndex > -1  then begin
         if TargetIndex > Sender.ActivePage.PageIndex then
           Dec(TargetIndex);
         Sender.ActivePage.PageIndex := TargetIndex;
       end; end
     else begin
       SourceIndex := Source.ActivePage.PageIndex;
       TargetIndex := GetTabIndex(Sender, X, Y);
       if TargetIndex > -1 then begin
         ATabSheet := Source.Pages[SourceIndex];
         ATabSheet.PageControl := Sender;
         ATabSheet.PageIndex := TargetIndex;
       end;
    end;
  end;

begin
  Exit;
  if (Sender is TPageControl) and (Source is TPageControl) then begin
    SenderPages := Sender as TPageControl;
    SourcePages := Source as TPageControl;
    DoTabDragDrop(SenderPages, SourcePages, X, Y);
    SourcePages.EndDrag(True);
  end;
end;

procedure TCustomWorkForm.WorkPagesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  SenderPages: TPageControl;
begin
  Exit;
  if Sender is TPageControl then begin
    SenderPages := Sender as TPageControl;
    SenderPages.Repaint;
    if State <> dsDragLeave then
      Accept := True;
  end;
end;

procedure TCustomWorkForm.WorkPagesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Exit;
  if (Sender is TPageControl) then
    (Sender as TPageControl).BeginDrag(False);
end;

function TCustomWorkForm.GetIsModified: Boolean;
var
  Editor: TCustomEditorFrame;
begin
  Editor := ActiveEditor;
  if Assigned(Editor) then
    Result := Editor.IsModified
  else
    Result := False;
end;

function TCustomWorkForm.GetIsAllModified: Boolean;
begin
  Result := False;
end;

function TCustomWorkForm.GetActiveEditor: TCustomEditorFrame;
begin
  if Assigned(WorkPages.ActivePage) then
    Result := WorkPages.ActivePage.Editor
  else
    Result := nil;
end;

procedure TCustomWorkForm.SetSearchMethod(Method: TSearchBy);
begin
  if FSearchFrame.SearchBy = Method then
    FSearchFrame.SearchBy := sbNone
  else
    FSearchFrame.SearchBy := Method;
end;

procedure TCustomWorkForm.SelectNode(Node: TTreeNode; LineNumber: Integer);
begin
  if Assigned(Node) then
    case Node.Kind of
      pkFolder:
        Node.Expand(False);
      pkFile:
        if Assigned(Node.Page) then begin
          WorkPages.ActivePage := Node.Page;
          WorkPages.ActivePage.Editor.GotoLine(LineNumber);
        end
        else begin
          Node.Page := OpenFile(Node);
          FItinerary.Post(Node, 1);
        end;
    end;
end;

function TCustomWorkForm.OpenFile(Node: TTreeNode): TTabSheet;
var
  Editor: TCustomEditorFrame;
begin
  Result := TTabSheet.Create(WorkPages);
  Result.Caption := Node.ShortName;
  Result.PageControl := WorkPages;
  Result.ImageIndex := 2;
  if Config.IsEditableFile(Node.ShortName) then
    Editor := TSyntaxEditorFrame.Create(Result)
  else
    Editor := THexEditorFrame.Create(Result);
  Editor.Parent := Result;
  Editor.Align := alClient;
  Editor.Open(Result, Node);
  Editor.ReadOnly := Config.IsReadonlyFile(Node.ShortName);
  Editor.OnLog := LogHandler;
  Editor.OnFindIdentifier := FindIdentifier;
  WorkPages.ActivePage := Result;
end;

procedure TCustomWorkForm.CloseFile(Page: TTabSheet);
const
  PROMPT = 'The file:'#13#10#13#10'''%s'''#13#10#13#10'has been modified. Do you want to save it before closing?';
var
  Editor: TCustomEditorFrame;
begin
  if Assigned(Page) then begin
    Editor := Page.Editor;
    if Assigned(Editor) then
      if Editor.IsModified then
        if MessageDlg(Format(PROMPT, [Editor.LogicalName]), mtWarning, [mbYes, mbNo], 0) = mrYes then
          Editor.Save;
    Editor.Node.Page := nil;
    Page.Free;
  end;
end;

procedure TCustomWorkForm.DeleteFolderNode(Node: TTreeNode);
var
  I: Integer = 0;
  Child: TTreeNode;
begin
  for I := Node.Count - 1 downto 0 do begin
    Child := Node.Items[I];
    case Child.Kind of
      pkFolder: DeleteFolderNode(Child);
      pkFile: DeleteFileNode(Child);
    end;
  end;
  if DeleteDirectory(Node.FullName, False) then
    Node.Delete;
end;

procedure TCustomWorkForm.DeleteFileNode(Node: TTreeNode);
begin
  if Assigned(Node.Page) then
    CloseFile(Node.Page);
  if SysUtils.DeleteFile(Node.FullName) then
    Node.Delete;
end;

procedure TCustomWorkForm.DeleteFolder(Node: TTreeNode);
const
  TITLE = 'Delete Folder';
  CAPTION = 'Are you sure you want to recursively delete the ''%s'' folder?';
begin
  if Assigned(Node) then
    if MessageDlg(TITLE, Format(CAPTION, [Node.LogicalName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      DeleteFolderNode(Node);
end;

procedure TCustomWorkForm.DeleteFile(Node: TTreeNode);
const
  TITLE = 'Delete File';
  CAPTION = 'Are you sure you want to delete the ''%s'' file?';
begin
  if Assigned(Node) then
    if MessageDlg(TITLE, Format(CAPTION, [Node.LogicalName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      DeleteFileNode(Node);
end;

procedure TCustomWorkForm.OpenExplorer(Node: TTreeNode);
const
  SHELL = 'explorer.exe';
  PARAM = '/select,"%s"';
var
  FolderName: TFileName = '';
  Parameters: String = '';
begin
  if Assigned(Node) and (Node.Kind in [pkFolder, pkFile]) then begin
    case Node.Kind of
      pkFolder: begin
        FolderName := Node.FullName;
        Parameters := Node.FullName;
        end;
      pkFile: begin
        FolderName := ExtractFilePath(Node.FullName);
        Parameters := Format(PARAM, [Node.FullName]);
        end;
    end;
    ShellExecute(0, nil, PChar(SHELL), PChar(Parameters), PChar(FolderName), SW_NORMAL);
  end;
end;

procedure TCustomWorkForm.OpenConsole(Node: TTreeNode);
const
  SHELL = 'cmd.exe';
  MASK = '/K "cd /d "%s""';
var
  FolderName: TFileName = '';
begin
  if Assigned(Node) and (Node.Kind in [pkFolder, pkFile]) then begin
    case Node.Kind of
      pkFolder:
        FolderName := Node.FullName;
      pkFile:
        FolderName := ExtractFilePath(Node.FullName);
    end;
    ShellExecute(0, nil, PChar(SHELL), PChar(Format(MASK, [FolderName])), PChar(FolderName), SW_NORMAL);
  end;
end;

procedure TCustomWorkForm.ExecuteCommand(Node: TTreeNode; Parameters: String);
const
  MASK = '/K ""%s" %s"';
var
  FolderName: TFileName;
begin
  Node.Kind;
  if Assigned(Node) and Node.IsExecutable then begin
    FolderName := ExtractFilePath(Node.FullName);
    ShellExecute(0, nil, PChar('cmd.exe'), PChar(Format(MASK, [Node.FullName, Parameters])),
      PChar(FolderName), SW_NORMAL);
  end;
end;

function TCustomWorkForm.FullPath(HomeFolder: String; FileName: String): String;
begin
  HomeFolder := ExcludeTrailingPathDelimiter(HomeFolder);
  while AnsiStartsStr('../', FileName) do begin
    HomeFolder := ExcludeTrailingPathDelimiter(ExtractFilePath(HomeFolder));
    FileName := AnsiRightStr(FileName, FileName.Length - 3);
  end;
  Result := AnsiReplaceStr(HomeFolder + '\' + FileName, '/', '\');
end;

function TCustomWorkForm.SearchNode(const FileName: TFileName): TTreeNode;
begin
  Result := SearchNode(Navigator.TopItem, FileName);
end;

function TCustomWorkForm.SearchNode(Node: TTreeNode; const FileName: TFileName): TTreeNode;
begin
  if not Assigned(Node) then
    Result := nil
  else
    if AnsiSameText(Node.LogicalName, FileName) then
      Result := Node
    else
      Result := SearchNode(Node.GetNext, FileName);
end;

function TCustomWorkForm.FindFirstFile(const FileName: TFileName; Backwards: Boolean): Boolean;
begin
  FFindFileName := FileName;
  FFindNode := Navigator.TopItem;
  Result := AnsiSameText(FFindNode.ShortName, FileName);
  if Result then
    Navigator.Selected := FFindNode
  else
    Result := FindNextFile(Backwards);
end;

function TCustomWorkForm.FindNextFile(Backwards: Boolean): Boolean;
var
  OldColor: TColor;
begin
  Result := False;
  repeat
    if Backwards then
      FFindNode := FFindNode.GetPrev
    else
      FFindNode := FFindNode.GetNext;
    if not Assigned(FFindNode) then begin
      FFindFileName :=  EmptyStr;
      OldColor := Navigator.Color;
      Navigator.Color := clRed;
      try
        Application.ProcessMessages;
        Beep;
      finally
        Navigator.Color := OldColor;
      end;
      Break;
    end;
    Result := AnsiSameText(FFindNode.ShortName, FFindFileName);
    if Result then
      Navigator.Selected := FFindNode
  until Result or not Assigned(FFindNode);
end;

procedure TCustomWorkForm.SetNavigatorVisible(Value: Boolean);
begin
  Navigator.Visible := Value;
  NavigatorSplitter.Visible := Value;
end;

procedure TCustomWorkForm.SetStatusVisible(Value: Boolean);
begin
  StatusPages.Visible := Value;
  StatusSplitter.Visible := Value;
end;

procedure TCustomWorkForm.DoLog(var Msg: TMessage);
var
  LogEvent: TLogEvent;
begin
  LogEvent := TLogEvent(Msg.WParam);
  Log(LogEvent.Text);
  LogEvent.Free;
end;

procedure TCustomWorkForm.DoGlobalSearch(Sender: TObject; const Criteria, Filter: String;
  MatchCase, MatchWholeWordOnly: Boolean);
var
  Engine: TGlobalOriginateEngine;
begin
  Engine := TGlobalOriginateEngine.Create(Self, Navigator, StatusPages, Images);
  try
    Engine.Criteria := Criteria;
    Engine.Filter := Filter;
    Engine.Execute;
  finally
    Engine.Free;
  end;
end;

procedure TCustomWorkForm.Log(const Text: String);
begin
  if not StatusPages.Visible then
    SetStatusVisible(True);
  LogEdit.Lines.Add(Text);
end;

procedure TCustomWorkForm.Log(const Mask: String; Args: array of const);
begin
  Log(Format(MASK, Args));
end;

procedure TCustomWorkForm.LogHandler(Sender: TObject; const Text: String);
begin
  Log(Text);
end;

procedure TCustomWorkForm.LogEventHandler(Sender: TObject; LogEvent: TLogEvent);
begin
  Log(LogEvent.Text);
  LogEvent.Free;
end;

procedure TCustomWorkForm.DoFileEvent(Sender: TObject; Action: TDirMonitorAction;
  const FileName: TFileName);
const
  MASK = '%s: %s';
  EVENTS: array[TDirMonitorAction] of String =
   ('Unknown',        // daUnknown
    'Added',          // daFileAdded
    'Deleted',        // daFileRemoved
    'Modified',       // daFileModified
    'Rename (Old)',   // daFileRenamedOldName
    'Rename (New)');  // daFileRenamedNewName
var
  Engine: TFileRefreshEngine;
begin
  Engine := TFileRefreshEngine.Create(Self);
  try
    Engine.Action := Action;
    Engine.FileName := FileName;
    Engine.Execute;
  finally
    Engine.Free;
  end;
  Log(MASK, [EVENTS[Action], FileName]);
end;

procedure TCustomWorkForm.DoExecuteComplete(Sender: TObject);
begin
  FDirMonitor.Resume;
end;

procedure TCustomWorkForm.FindIdentifier(Sender: TObject; const Criteria, Filter: String;
  MatchCase, MatchWholeWordOnly: Boolean);
begin
  DoOriginate(Sender, Criteria, Filter);
end;

procedure TCustomWorkForm.RetrieveLabels(List: TStrings);
begin
  if Assigned(ActiveEditor) then
    ActiveEditor.RetrieveLabels(List)
  else
    List.Clear;
end;

procedure TCustomWorkForm.CheckIfModified(Node: TTreeNode);
const
  PROMPT = 'File has been modified. Do you want to refresh from disk?';
begin
  if Node.IsFileModified then begin
    if Assigned(Node.Page) and (Node.Page = WorkPages.ActivePage)  then
      if MessageDlg(PROMPT, mtWarning, [mbYes, mbNo], 0) = mrYes then begin
        Node.IsFileModified := False;
        ActiveEditor.Revert;
      end;
    Node.IsFileModified := False;
  end;
end;

procedure TCustomWorkForm.Idle;
begin
  if Assigned(ActiveEditor) then
    ActiveEditor.Idle;
end;

procedure TCustomWorkForm.RefreshConfig;
var
  I: Integer = 0;
  Page: TTabSheet;
begin
  for I := 0 to WorkPages.PageCount - 1 do begin
    Page := WorkPages.Pages[I];
    Page.Editor.RefreshConfig;
  end;
end;

{ TNavigatorIterator }

constructor TNavigatorIterator.Create(Navigator: TTreeView);
begin
  inherited Create;
  FNavigator := Navigator;
end;

destructor TNavigatorIterator.Destroy;
begin
  inherited;
end;

procedure TNavigatorIterator.DoIterate(Node: TTreeNode; var MayContinue: Boolean);
begin
  if Assigned(FOnIterate) then
    FOnIterate(Self, Node, MayContinue);
end;

procedure TNavigatorIterator.Execute;
var
  MayContinue: Boolean = True;
  Node: TTreeNode;
begin
  MayContinue := True;
  Node := Navigator.TopItem;
  while Assigned(Node) and MayContinue do begin
    if Node.Kind = pkFile then
      DoIterate(Node, MayContinue);
    Node := Node.GetNext;
  end;
end;

{ TGlobalSearchEngine }

constructor TGlobalSearchEngine.Create(Project: TCustomWorkForm; Navigator: TTreeView;
  Pages: TPageControl; Images: TImageList);
begin
  inherited Create;
  FEngine := TNavigatorIterator.Create(Navigator);
  FEngine.OnIterate := DoIterate;
  FBuffer := TStringList.Create;
  FProject := Project;
  FPages := Pages;
  FImages := Images;
end;

destructor TGlobalSearchEngine.Destroy;
begin
  FBuffer.Free;
  FEngine.Free;
  inherited;
end;

procedure TGlobalSearchEngine.DoIterate(Sender: TObject; Node: TTreeNode; var MayContinue: Boolean);
var
  I: Integer = 0;
  L: String = '';
  Item: TListItem;

  function IsValidNode(const FileName: TFileName): Boolean;
  var
    Extention: String = '';
  begin
    Result := False;
    for Extention in FExtensions do
      if MatchesMask(FileName, Extention) then begin
        Result := True;
        Break;
      end;
  end;

  function WordExists(const Value: String; const Criteria:string; Options: TStringSearchOptions): Boolean;
  var
    Buffer: PChar;
    Size : Integer = 0;
  begin
    Result := not Value.IsEmpty;
    if Result then begin
      Buffer := @Value[1];
      Size := StrLen(Buffer);
      Result := Assigned(SearchBuf(Buffer, Size, 0, 0, Criteria, Options));
    end;
  end;

begin
  if IsValidNode(Node.ShortName) then begin
    FBuffer.LoadFromFile(Node.FullName);
    try
      for I := 0 to FBuffer.Count - 1 do begin
        L := FBuffer[I];
        if WordExists(L, Criteria, FOptions) then begin
          Item := FReport.Items.Add;
          Item.Caption := Node.LogicalName;
          Item.Data := Node;
          Item.SubItems.Add(I.ToString);
          Item.SubItems.Add(L);
          Item.ImageIndex := 2;
        end;
      end;
    finally
      FBuffer.Clear;
    end;
    Application.ProcessMessages;
  end;
end;

function TGlobalSearchEngine.GeneratePage: TListView;
const
  NAME_MASK = 'SearchPage_%d';
  CAPTION_MASK = 'Results: ''%s''';
  HINT_MASK = 'Criteria = ''%s'', Case = %s, Whole Words = %s';
var
  Page: TTabSheet;

  procedure AddColumn(View: TListView; const Caption: String; Width: Integer; Alignment: TAlignment; AutoSize: Boolean);
  var
    Column: TListColumn;
  begin
    Column := View.Columns.Add;
    Column.Caption := Caption;
    Column.Alignment := Alignment;
    Column.Width := Width;
    Column.AutoSize := AutoSize;
  end;

begin
  Inc(Search_Index);
  Page := TTabSheet.Create(FPages);
  Page.PageControl := FPages;
  Page.Name := Format(NAME_MASK, [Search_Index]);
  Page.Caption := Format(CAPTION_MASK, [Criteria]);
  Page.Hint := Format(HINT_MASK, [Criteria, BoolToStr(MatchCase, True), BoolToStr(MatchWholeWordOnly, True)]);
  Page.Visible := True;
  Result := TListView.Create(Page);
  Result.Parent := Page;
  Result.Align := alClient;
  Result.ReadOnly := True;
  Result.RowSelect := True;
  Result.ViewStyle := vsReport;
  Result.SmallImages := FImages;
  Result.OnDblClick := FProject.SearchEditDblClick;
  AddColumn(Result, 'File', 200, taLeftJustify, False);
  AddColumn(Result, 'Line', 50, taRightJustify, False);
  AddColumn(Result, 'Text', 362, taLeftJustify, True);
  Page.Tag := Integer(Result);
  FPages.ActivePage := Page;
end;

procedure TGlobalSearchEngine.Execute;
begin
  FOptions := [soDown];
  if FMatchCase then
    FOptions := FOptions + [soMatchCase];
  if FMatchWholeWordOnly then
    FOptions := FOptions + [soWholeWord];
  FExtensions := SplitString(Filter, ';');
  FReport := GeneratePage;
  FReport.Items.BeginUpdate;
  try
    FEngine.Execute;
  finally
    FReport.Items.EndUpdate;
  end;
end;

{ TGlobalOriginateEngine }

constructor TGlobalOriginateEngine.Create(Project: TCustomWorkForm; Navigator: TTreeView;
  Pages: TPageControl; Images: TImageList);
begin
  inherited Create;
  FEngine := TNavigatorIterator.Create(Navigator);
  FEngine.OnIterate := DoIterate;
  FBuffer := TStringList.Create;
  FProject := Project;
  FPages := Pages;
  FImages := Images;
end;

destructor TGlobalOriginateEngine.Destroy;
begin
  FBuffer.Free;
  FEngine.Free;
  inherited;
end;

procedure TGlobalOriginateEngine.DoIterate(Sender: TObject; Node: TTreeNode; var MayContinue: Boolean);
var
  I: Integer = 0;
  L: String = '';
  Item: TListItem;

  function IsValidNode(const FileName: TFileName): Boolean;
  var
    Extention: String;
  begin
    Result := False;
    for Extention in FExtensions do
      if MatchesMask(FileName, Extention) then begin
        Result := True;
        Break;
      end;
  end;

begin
  if IsValidNode(Node.ShortName) then begin
    FBuffer.LoadFromFile(Node.FullName);
    try
      for I := 0 to FBuffer.Count - 1 do begin
        L := FBuffer[I];
        if AnsiStartsText(Criteria, L) then begin
          Item := FReport.Items.Add;
          Item.Caption := Node.LogicalName;
          Item.Data := Node;
          Item.SubItems.Add(I.ToString);
          Item.SubItems.Add(L);
          Item.ImageIndex := 2;
          MayContinue := False;
          Break;
        end;
      end;
    finally
      FBuffer.Clear;
    end;
    Application.ProcessMessages;
  end;
end;

function TGlobalOriginateEngine.GeneratePage: TListView;
const
  NAME_MASK = 'SearchPage_%d';
  CAPTION_MASK = 'Results: ''%s''';
  HINT_MASK = 'Criteria = ''%s''';
var
  Page: TTabSheet;

  procedure AddColumn(View: TListView; const Caption: String; Width: Integer; Alignment: TAlignment; AutoSize: Boolean);
  var
    Column: TListColumn;
  begin
    Column := View.Columns.Add;
    Column.Caption := Caption;
    Column.Alignment := Alignment;
    Column.Width := Width;
    Column.AutoSize := AutoSize;
  end;

begin
  Inc(Search_Index);
  Page := TTabSheet.Create(FPages);
  Page.PageControl := FPages;
  Page.Name := Format(NAME_MASK, [Search_Index]);
  Page.Caption := Format(CAPTION_MASK, [Criteria]);
  Page.Hint := Format(HINT_MASK, [Criteria]);
  Page.Visible := True;
  Result := TListView.Create(Page);
  Result.Parent := Page;
  Result.Align := alClient;
  Result.ReadOnly := True;
  Result.RowSelect := True;
  Result.ViewStyle := vsReport;
  Result.SmallImages := FImages;
  Result.OnDblClick := FProject.SearchEditDblClick;
  AddColumn(Result, 'File', 200, taLeftJustify, False);
  AddColumn(Result, 'Line', 50, taRightJustify, False);
  AddColumn(Result, 'Text', 362, taLeftJustify, True);
  Page.Tag := Integer(Result);
  FPages.ActivePage := Page;
end;

procedure TGlobalOriginateEngine.Execute;
begin
  FExtensions := SplitString(Filter, ';');
  FReport := GeneratePage;
  FReport.Items.BeginUpdate;
  try
    FEngine.Execute;
  finally
    FReport.Items.EndUpdate;
  end;
end;

{ TFileRefreshEngine }

constructor TFileRefreshEngine.Create(Form: TCustomWorkForm);
begin
  inherited Create;
  FForm := Form;
  FEngine := TNavigatorIterator.Create(Form.Navigator);
  FEngine.OnIterate := DoIterate;
end;

destructor TFileRefreshEngine.Destroy;
begin
  FEngine.Free;
  inherited;
end;

procedure TFileRefreshEngine.DoIterate(Sender: TObject; Node: TTreeNode; var MayContinue: Boolean);
begin
  MayContinue := not AnsiSameText(Node.FullName, FFileName);
  if not MayContinue then begin
    Node.IsFileModified := True;
    if Assigned(Node.Page) and (Node.Page = FForm.WorkPages.ActivePage) then
      FForm.CheckIfModified(Node);
  end;
end;

procedure TFileRefreshEngine.Execute;
begin
  if not (FAction in [daFileRenamedOldName, daFileRenamedNewName]) then
    FEngine.Execute;
end;

initialization
  Search_Index := 0;
end.

