unit AboutCredits;

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
  Classes, SysUtils, Forms, Controls, StdCtrls;

const
  OPEN_COMMAND = 'open';

type

  { TAboutCreditFrame }

  TAboutCreditFrame = class(TFrame)
    HexEditTitleLabel: TLabel;
    AsyncProTitleLabel: TLabel;
    HtmlViewerTitleLabel: TLabel;
    MarkdownTitleLabel: TLabel;
    PdfiumLibTitleLabel: TLabel;
    IconTitleLabel: TLabel;
    SilkTitleLabel: TLabel;
    procedure HexEditTitleLabelClick(Sender: TObject);
    procedure AsyncProTitleLabelClick(Sender: TObject);
    procedure HtmlViewerTitleLabelClick(Sender: TObject);
    procedure MarkdownTitleLabelClick(Sender: TObject);
    procedure PdfiumLibTitleLabelClick(Sender: TObject);
    procedure IconTitleLabelClick(Sender: TObject);
    procedure SilkTitleLabelClick(Sender: TObject);
  end;

implementation

{$R *.lfm}

uses
  Windows;

{ TAboutCreditFrame }

procedure TAboutCreditFrame.HexEditTitleLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, OPEN_COMMAND, PChar('https://github.com/michalgw/mphexeditor'), nil,nil, SW_SHOW);
end;

procedure TAboutCreditFrame.AsyncProTitleLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, OPEN_COMMAND, PChar('https://sourceforge.net/projects/tpapro'), nil,nil, SW_SHOW);
end;

procedure TAboutCreditFrame.HtmlViewerTitleLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, OPEN_COMMAND, PChar('https://github.com/BerndGabriel/HtmlViewer'), nil,nil, SW_SHOW);
end;

procedure TAboutCreditFrame.MarkdownTitleLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, OPEN_COMMAND, PChar('https://github.com/grahamegrieve/delphi-markdown'), nil,nil, SW_SHOW);
end;

procedure TAboutCreditFrame.PdfiumLibTitleLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, OPEN_COMMAND, PChar('https://github.com/ahausladen/PdfiumLib'), nil,nil, SW_SHOW);
end;

procedure TAboutCreditFrame.IconTitleLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, OPEN_COMMAND, PChar('http://www.fasticon.com'), nil,nil, SW_SHOW);
end;

procedure TAboutCreditFrame.SilkTitleLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, OPEN_COMMAND, PChar('http://www.famfamfam.com/lab/icons/silk'), nil,nil, SW_SHOW);
end;

end.

