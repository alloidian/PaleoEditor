program Paleo;

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

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  {$IFDEF DEBUG} SysUtils, {$ENDIF}
  Forms, printer4lazarus, lazcontrols, Main, Configs, CustomWorks, Searches,
  CustomEditors, Utils, CustomTextEditors, Executions, HexEditors, Actions, Abouts,
  SynHighlighterSpin, SynHighlighterZ80, FileMasks, FolderWorks, ProjectWorks,
  NewFiles, CustomConfigFrames, ConfigUtils, NavigatorConfigs, ColorConfigs,
  DirMonitors, EditorConfigs, Assemblers, ProjectConfigs, UnTerminal,
  {$IFDEF TERMINAL} TerminalFrames, TerminalForms, TerminalConfigs, {$ENDIF}
  Uploads, PackageEngines, AssemblyEditors, BatchEditors, SpinEditors,
  BasicEditors, PascalEditors, CustomPreviewEditors, HtmlEditors, MarkdownEditors,
  SynHighlighterMD, IntelHexEditors, SynHighlighterIntelHex, XmlEditors, 
  JsonEditors, IniEditors, RichTextEditors;

{$R *.res}

{$IFDEF DEBUG}
const
  MEMORY_LOG_FILENAME = 'heap.trc';
{$ENDIF}

begin
{$IFDEF DEBUG}
  if FileExists(MEMORY_LOG_FILENAME) then
    DeleteFile(MEMORY_LOG_FILENAME);
  SetHeapTraceOutput(MEMORY_LOG_FILENAME);
{$ENDIF}
  RequireDerivedFormResource := True;
  Application.Title:='Paleo Editor';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

