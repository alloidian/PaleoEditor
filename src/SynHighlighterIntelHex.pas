unit SynHighlighterIntelHex;

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
  Classes, SysUtils, SynEditHighlighter;

type
  TtkTokenKind = (tkNone, tkStart, tkByteCount, tkAddress, tkRecordType, tkData,
    tkCheckSum);
  TRangeState = (rsUnknown, rsStart, rsByteCount, rsAddress, rsRecordtype, rsData,
    rsChecksum);

  { TSynIntelHexSyn }

  TSynIntelHexSyn = class(TSynCustomHighlighter)
  private
    FLine: String;
    FLineLength: Integer;
    FLineNumber: Integer;
    FCursor: Integer;
    FCurrentToken: TtkTokenKind;
    FRange: TRangeState;
    FStartCode: String;
    FByteCount: String;
    FAddress: String;
    FRecordType: String;
    FData: String;
    FDataLength: Integer;
    FCheckSum: String;
    FStartCodeAttr: TSynHighlighterAttributes;
    FByteCountAttr: TSynHighlighterAttributes;
    FAddressAttr: TSynHighlighterAttributes;
    FRecordTypeAttr: TSynHighlighterAttributes;
    FDataAttr: TSynHighlighterAttributes;
    FCheckSumAttr: TSynHighlighterAttributes;
    FBlankAttr: TSynHighlighterAttributes;
  protected
    function GetSampleSource: string; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetEol: Boolean; override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  end;


implementation

uses
  Graphics;

{ TSynIntelHexSyn }

constructor TSynIntelHexSyn.Create(AOwner: TComponent);
begin
  inherited;
  FRange := rsUnknown;
  FStartCode := EmptyStr;
  FByteCount := EmptyStr;
  FAddress := EmptyStr;
  FRecordType := EmptyStr;
  FData := EmptyStr;
  FDataLength := 0;
  FCheckSum := EmptyStr;
  FStartCodeAttr := TSynHighlighterAttributes.Create('Start');
  FStartCodeAttr.Style := [];
  FStartCodeAttr.Foreground := clBlack;
  AddAttribute(FStartCodeAttr);
  FByteCountAttr := TSynHighlighterAttributes.Create('Count');
  FByteCountAttr.Style := [fsBold];
  FByteCountAttr.Foreground := clBlue;
  AddAttribute(FByteCountAttr);
  FAddressAttr := TSynHighlighterAttributes.Create('Address');
  FAddressAttr.Style := [fsBold];
  FAddressAttr.Foreground := clBlack;
  AddAttribute(FAddressAttr);
  FRecordTypeAttr := TSynHighlighterAttributes.Create('Type');
  FRecordTypeAttr.Style := [fsBold];
  FRecordTypeAttr.Foreground := clRed;
  AddAttribute(FRecordTypeAttr);
  FDataAttr := TSynHighlighterAttributes.Create('Data');
  FDataAttr.Style := [];
  FDataAttr.Foreground := clBlack;
  AddAttribute(FDataAttr);
  FCheckSumAttr := TSynHighlighterAttributes.Create('Checksum');
  FCheckSumAttr.Style := [fsBold];
  FCheckSumAttr.Foreground := clGreen;
  AddAttribute(FCheckSumAttr);
  FBlankAttr := TSynHighlighterAttributes.Create('Blank');
  AddAttribute(FBlankAttr);
end;

destructor TSynIntelHexSyn.Destroy;
begin
  Inherited;
end;

function TSynIntelHexSyn.GetSampleSource: string;
begin
  Result :=
    ':10010000214601360121470136007EFE09D2190140'#13#10 +
    ':100110002146017E17C20001FF5F16002148011928'#13#10 +
    ':10012000194E79234623965778239EDA3F01B2CAA7'#13#10 +
    ':100130003F0156702B5E712B722B732146013421C7'#13#10 +
    ':00000001FF';
end;

function TSynIntelHexSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  Result := FDataAttr;
end;

class function TSynIntelHexSyn.GetLanguageName: string;
begin
  Result := 'Intel Hex';
end;

class function TSynIntelHexSyn.GetFriendlyLanguageName: string;
begin
  Result := 'Intel Hex';
end;

procedure TSynIntelHexSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  FLine := NewValue;
  FLineLength := NewValue.Length;
  FLineNumber := LineNumber;
  FCursor := 1;
  FCurrentToken := tkNone;
  FRange := rsUnknown;
  if FLineLength > 0 then begin
    FStartCode := Copy(FLine, 1, 1);
    if FLineLength > 2 then begin
      FByteCount := Copy(FLine, 2, 2);
      if FLineLength > 6 then begin
        FAddress := Copy(FLine, 4, 4);
        if FLineLength > 8 then begin
          FRecordType := Copy(FLine, 8, 2);
          if FLineLength > 10 then begin
            FDataLength := FLine.Length - 11;
            FData := Copy(FLine, 10, FDataLength);
            if FLineLength > 10 then
              FCheckSum := Copy(FLine, FLine.Length - 1, 2)
            else
              FCheckSum := EmptyStr; end
          else begin
            FDataLength := 0;
            FData := EmptyStr;
          end; end
        else
          FRecordType := EmptyStr; end
      else
        FAddress := EmptyStr; end
    else
      FByteCount := EmptyStr; end
  else
    FStartCode := EmptyStr;
  Inherited SetLine(NewValue, LineNumber);
end;

function TSynIntelHexSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynIntelHexSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynIntelHexSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynIntelHexSyn.GetEol: Boolean;
begin
  Result := FCursor > FLineLength;
end;

function TSynIntelHexSyn.GetToken: String;
begin
  case FCurrentToken of
    tkStart:
      Result :=FStartCode;
    tkByteCount:
      Result := FByteCount;
    tkAddress:
      Result := FAddress;
    tkRecordType:
      Result := FRecordType;
    tkData:
      Result := FData;
    tkCheckSum:
      Result := FCheckSum;
  else
    Result := EmptyStr;
  end;
end;

procedure TSynIntelHexSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
begin
  case FCurrentToken of
    tkStart: begin
      TokenStart := @FStartCode[1];
      TokenLength :=1;
      end;
    tkByteCount: begin
      TokenStart := @FByteCount[1];
      TokenLength := 2;
      end;
    tkAddress: begin
      TokenStart := @FAddress[1];
      TokenLength := 4;
      end;
    tkRecordType: begin
      TokenStart := @FRecordType[1];
      TokenLength := 2;
      end;
    tkData :begin
      TokenStart := @FData[1];
      TokenLength := FDataLength;
      end;
    tkCheckSum: begin
      TokenStart := @FCheckSum[1];
      TokenLength := 2;
      end;
  else
      TokenStart := @FStartCode[1];
    TokenLength := 0;
  end;
end;

function TSynIntelHexSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FCurrentToken of
    tkStart:
      Result :=FStartCodeAttr;
    tkByteCount:
      Result := FByteCountAttr;
    tkAddress:
      Result := FAddressAttr;
    tkRecordType:
      Result := FRecordTypeAttr;
    tkData:
      Result := FDataAttr;
    tkCheckSum:
      Result := FCheckSumAttr;
  else
    Result := FBlankAttr;
  end;
end;

function TSynIntelHexSyn.GetTokenKind: Integer;
begin
  Result := Ord(FCurrentToken);
end;

function TSynIntelHexSyn.GetTokenPos: Integer;
begin
  Result := FCursor;
end;

procedure TSynIntelHexSyn.Next;
begin
  case FCursor of
    1: begin
      FCurrentToken := tkStart;
      FRange := rsStart;
      Inc(FCursor);
    end;
    2..3: begin
      FCurrentToken := tkByteCount;
      FRange := rsByteCount;
      Inc(FCursor, 2);
    end;
    4..7: begin
      FCurrentToken := tkAddress;
      FRange := rsAddress;
      Inc(FCursor, 4);
    end;
    8..9: begin
      FCurrentToken := tkRecordType;
      FRange := rsRecordtype;
      Inc(FCursor, 2);
    end
  else
    if FCursor >= FLineLength - 1 then begin
      FCurrentToken := tkCheckSum;
      FRange := rsChecksum;
      Inc(FCursor, 1); end
    else
      if FCursor > FLineLength then begin
        FCurrentToken := tkStart;
        FRange := rsUnknown;
        Inc(FCursor); end
      else begin
        FCurrentToken := tkData;
        FRange := rsData;
        Inc(FCursor, FDataLength);
      end;
  end;
end;

end.

