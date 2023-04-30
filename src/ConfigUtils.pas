unit ConfigUtils;

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
  Classes, SysUtils, Graphics, ComCtrls, ExtCtrls, Generics.Collections, Forms, Menus,
  FpJson;

const
  ITEM_EDIT                   = 'Editable Files';
  ITEM_EXEC                   = 'Executable Files';
  ITEM_ASSEMBLE               = 'Assembly Files';
  ITEM_SEARCH                 = 'Searchable Files';
  ITEM_UNEDITABLE             = 'Uneditable Files';
  ITEM_EXCLUDE_FILE           = 'Excluded Files';
  ITEM_EXCLUDE_FOLDER         = 'Excluded Folders';
  ITEM_BINARY_SYNTAX          = 'Binary Syntax';
  ITEM_ASSEMBLY_SYNTAX        = 'Assembly Syntax';
  ITEM_BASIC_SYNTAX           = 'BASIC Syntax';
  ITEM_BATCH_SYNTAX           = 'Batch Syntax';
  ITEM_HTML_SYNTAX            = 'HTML Syntax';
  ITEM_IMAGE_SYNTAX           = 'Image Syntax';
  ITEM_INI_SYNTAX             = 'INI Syntax';
  ITEM_INTEL_HEX_SYNTAX       = 'Intel Hex Syntax';
  ITEM_JSON_SYNTAX            = 'JSON Syntax';
  ITEM_MARKDOWN_SYNTAX        = 'Markdown Syntax';
  ITEM_PASCAL_SYNTAX          = 'Pascal Syntax';
  ITEM_PDF_SYNTAX             = 'PDF Syntax';
  ITEM_RTF_SYNTAX             = 'RTF Syntax';
  ITEM_SPIN_SYNTAX            = 'Spin Syntax';
  ITEM_TEXT_SYNTAX            = 'Text Syntax';
  ITEM_XML_SYNTAX             = 'XML Syntax';
  ITEM_ZIP_SYNTAX             = 'ZIP Syntax';
  INI_EDITOR_FONT_NAME_DEF    = 'Courier New';
  INI_EDITOR_FONT_SIZE_DEF    = 10;
  INI_EDITOR_RIGHT_MARGIN_DEF = 90;
  INI_EDIT_DEF                = '*.asm;*.z80;*.azm;*.180;*.inc;*.lib;*.ins;*.mac;*.lst;'    +
                                '*.bat;*.sh;*.cmd;*.pas;*.dpr;*.bas;*.zex;*.txt;*.doc;'     +
                                '*.for;*.sub;*.md;*.h;*.hlp;*.not;*.inf;*.new;*.ps1;'       +
                                '*.spin;*.msg;*.hex;Makefile;ReadMe;-readme;diskdefs;'      +
                                'readme.1st;copying;read.me;.gitattributes;.gitignore;'     +
                                'readme.unix;*.log;*.sym;*.kvset';
  INI_EXEC_DEF                = '*.cmd;*.bat';
  INI_ASSEMBLE_DEF            = 'Assign.asm;bcd.asm;BDOS.ASM;BDOS22.ASM;cbios.asm;cls.asm;' +
                                'cpmldr.asm;Decode.asm;Encode.asm;FDU.asm;Format.asm;'      +
                                'Halt.asm;hbios.asm;IntTest.asm;loader.asm;Mode.asm;'       +
                                'Reboot.asm;RTC.asm;Startup.asm;Survey.asm;SysCopy.asm;'    +
                                'Talk.asm;TESTZ80.asm;Time.asm;Timer.asm;TimeUtil.asm;'     +
                                'tune.asm;zcpr.asm';
  INI_HTML_DEF                = '*.html;*.htm';
  INI_SEARCH_DEF              = '*.asm;*.z80;*.azm;*.180;*.inc;*.lib;*.lst';
  INI_UNEDITABLE_DEF          = '*.list;*.lst;*.log;*.sym';
  INI_EXCLUDE_FILE_DEF        = '*.docx;*.exe;*.dll;*.lbr';
  INI_EXCLUDE_FOLDER_DEF      = '.github;Tools;Tunes';
  ITEM_ASSEMBLY_SYNTAX_DEF    = '*.asm;*.z80;*.z80.sav;*.azm;*.180;*.inc;*.lib;*.lib.sav;'  +
                                '*.mac;*.lst;*.ins';
  ITEM_BASIC_SYNTAX_DEF       = '*.bas';
  ITEM_BATCH_SYNTAX_DEF       = '*.bat;*.cmd;*.ps1';
  ITEM_HTML_SYNTAX_DEF        = '*.html;*.htm';
  ITEM_IMAGE_SYNTAX_DEF       = '*.png;*.xpm;*.bmp;*.cur;*.ico;*.icns;*.jpeg;*.jpg;*.jpe;'  +
                                '*.jfif;*.tif;*.tiff;*.gif;*.pgm;*.pgm;*.ppm';
  ITEM_INI_SYNTAX_DEF         = '*.ini;*.prj;*.proj';
  ITEM_INTEL_HEX_SYNTAX_DEF   = '*.hex;*.h86;*.hxl;*.hxh;*.obl;*.obh;*.mcs;*.ihex;*.ihe;'   +
                                '*.ihx;*.a43;*.a90;*.p00;*.pff';
  ITEM_JSON_SYNTAX_DEF        = '*.json';
  ITEM_MARKDOWN_SYNTAX_DEF    = '*.md';
  ITEM_PASCAL_SYNTAX_DEF      = '*.pas;*.pp;*.dpr;*.lpr';
  ITEM_PDF_SYNTAX_DEF         = '*.pdf';
  ITEM_RTF_SYNTAX_DEF         = '*.rtf';
  ITEM_SPIN_SYNTAX_DEF        = '*.spin';
  ITEM_TEXT_SYNTAX_DEF        = '*.txt;Makefile;copying;*.doc;*.not;*.hlp;*.msg;*.prn;'     +
                                '*.sym;*.for;*.log;readme;readme.*;read.me;.git*;defboot;'  +
                                'defbank;diskdefs;*.cnf';
  ITEM_XML_SYNTAX_DEF         = '*.xml;*.kvset';
  ITEM_ZIP_SYNTAX_DEF         = '*.zip';
  ASSEMBLER_FOLDER_MASK       = '%s\tasm32';
  ASSEMBLER_FILE_MASK         = '%s\TASM.EXE';

type
  TAssemblerPlatform = (apZ80, apZ180);

  TAttributeType = (atComment, atConfig, atConstant, atDataType, atDeclaration,
    atDirective, atFlow, atIdentifier, atKeyword, atMemory, atNumber, atRegister,
    atString, atSymbol, atWhitespace);

  TAttribute = class(TObject)
  private
    FAttr: TAttributeType;
    FStorageName: String;
    FForeground: TColor;
    FBackground: TColor;
    FStyle: TFontStyles;
  public
    constructor Create(Attr: TAttributeType; const StorageName: String); virtual;
    procedure Reset;
    procedure ReadConfig(Parent: TJsonObject); overload;
    function WriteConfig: TJsonObject; overload;
    procedure Assign(Source: TAttribute);
    function Matches(Source: TAttribute): Boolean;
    property Attr: TAttributeType read FAttr;
    property StorageName: String read FStorageName;
    property Foreground: TColor read FForeground write FForeground;
    property Background: TColor read FBackground write FBackground;
    property Style: TFontStyles read FStyle write FStyle;
  end;

  TAttributes = class(TObject)
  private type
    TAttributeList = TObjectDictionary<TAttributeType, TAttribute>;
  private
    FList: TAttributeList;
  protected
    function GetCount: Integer;
    function GetItems(Attr: TAttributeType): TAttribute;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    procedure Assign(Source: TAttributes);
    function ReadConfig(Parent: TJsonObject): TJsonObject; overload;
    function WriteConfig(Parent: TJsonObject; const Name: String): TJsonObject; overload;
    procedure Populate(List: TStrings);
    function Matches(Source: TAttributes): Boolean;
    property Count: Integer read GetCount;
    property Items[Attr: TAttributeType]: TAttribute read GetItems; default;
  end;

  TTerminal = class(TObject)
  public type
    TBauds = 0..12;
    TDataBit = 5..8;
    TStopBit = 0..2;
    TParity = (pNone, pOdd, pEven, sMark, pSpace);
    TFlowControl = (fcNone, fcSoftware, fcHardward);
  private const
    INI_TERMINAL     = 'Terminal';
    INI_COM_PORT     = 'ComPort';
    INI_BAUD         = 'Baud';
    INI_DATA_BIT     = 'DataBit';
    INI_STOP_BIT     = 'StopBit';
    INI_PARITY       = 'Parity';
    INI_FLOW_CONTROL = 'FlowControl';
    INI_CHAR_DELAY   = 'CharDelay';
    INI_LINE_DELAY   = 'LineDelay';
    INI_UPLOAD       = 'Upload';
    INI_DOWNLOAD     = 'Download';
  public const
    DEF_COM_PORT     = -1;
    DEF_BAUD         = 19200;
    DEF_DATA_BIT     = 8;
    DEF_STOP_BIT     = 1;
    DEF_PARITY       = pNone;
    DEF_FLOW_CONTROL = fcNone;
    DEF_CHAR_DELAY   = 20;
    DEF_LINE_DELAY   = 20;
    DEF_UPLOAD       = 'B:XM R A0:%s';
    DEF_DOWNLOAD     = 'B:XM S A0:%s';
  private
    FComPort: Integer;
    FBaud: Integer;
    FDataBits: TDataBit;
    FStopBits: TStopBit;
    FParity: TParity;
    FFlowControl: TFlowControl;
    FCharDelay: Integer;
    FLineDelay: Integer;
    FUploadCommand: String;
    FDownloadCommand: String;
  protected
  public
    function ReadConfig(Parent: TJsonObject): TJsonObject; overload;
    function WriteConfig(Parent: TJsonObject; const Name: String): TJsonObject; overload;
    class procedure PopulateComPort(List: TStrings);
    class procedure PopulateBaud(List: TStrings);
    class procedure PopulateDataBits(List: TStrings);
    class procedure PopulateStopBits(List: TStrings);
    class procedure PopulateParity(List: TStrings);
    class procedure PopulateFlowControl(List: TStrings);
    class function ComPortToStr(Value: Integer): String;
    class function DataBitToStr(Value: TDataBit): String;
    class function StopBitToStr(Value: TStopBit): String;
    class function ParityToStr(Value: TParity): String;
    class function FlowControlToStr(Value: TFlowControl): String;
    property ComPort: Integer read FComPort write FComPort;
    property Baud: Integer read FBaud write FBaud;
    property DataBits: TDataBit read FDataBits write FDataBits;
    property StopBits: TStopBit read FStopBits write FStopBits;
    property Parity: TParity read FParity write FParity;
    property FlowControl: TFlowControl read FFlowControl write FFlowControl;
    property CharDelay: Integer read FCharDelay write FCharDelay;
    property LineDelay: Integer read FLineDelay write FLineDelay;
    property UploadCommand: String read FUploadCommand write FUploadCommand;
    property DownloadCommand: String read FDownloadCommand write FDownloadCommand;
  end;

  TCustomConfig = class(TObject)
  private
    FConfigFileName: TFileName;
  protected
    function GetDocument: TJsonObject;
    procedure SaveDocument(Document: TJsonObject);
    property ConfigFileName: TFileName read FConfigFileName;
  public
    constructor Create; virtual;
  end;

  TConfig = class(TCustomConfig)
  private type
    TParamType = TDictionary<String, String>;
    TVersion = record
      Major: Word;
      Minor: Word;
      Release: Word;
      Build: Word;
      IsDebug: Boolean;
      IsPreRelease: Boolean;
      IsPatched: Boolean;
      IsPrivateBuild: Boolean;
      IsSpecialBuild: Boolean;
      CompanyName: String;
      InternalName: String;
      ProjectName: String;
      FileVersion: String;
    end;
  public type
    TSyntax = (synAssembly, synBasic, synBatch, synHex, synHtml, synImage, synIni,
      synIntelHex, synJson, synMarkdown, synPascal, synPdf, synRtf, synSpin, synText,
      synXml, synZip);
    TSyntaxes = array[TSyntax] of String;
  private
    FVersion: TVersion;
    FTerminal: TTerminal;
    FEditFiles: String;
    FExecuteFiles: String;
    FAssemblyFiles: String;
    FHtmlFiles: String;
    FSearchFiles: String;
    FUneditableFiles: String;
    FSaveWorkspace: Boolean;
    FExcludeFiles: String;
    FExcludeFolders: String;
    FSyntax: TSyntaxes;
    FParams: TParamType;
    FMonitorFolder: Boolean;
    FAttributes: TAttributes;
    FFontName: String;
    FFontSize: Integer;
    FRightMargin: Integer;
  protected
    function GetVersionText: String;
    function GetPlatform: String; overload;
    function GetDebug: String;
    function GetPreRelease: String;
    function GetPatched: String;
    function GetPrivateBuild: String;
    function GetSpecialBuild: String;
    function GetSyntax(Syntax: TSyntax): String; overload;
    procedure SetSyntax(Syntax: TSyntax; const Value: String);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadConfig; overload;
    procedure WriteConfig; overload;
    procedure ReadConfig(Form: TForm); overload;
    procedure WriteConfig(Form: TForm); overload;
    procedure ReadConfig(ParentMenu: TMenuItem; EventHandler: TNotifyEvent); overload;
    procedure WriteConfig(ParentMenu: TMenuItem); overload;
    procedure ReadConfig(Panel: TPanel; PageControl: TPageControl; const FolderName: TFileName); overload;
    procedure WriteConfig(Panel: TPanel; PageControl: TPageControl; const FolderName: TFileName); overload;
    procedure AddParam(const Command, Param: String); overload;
    procedure AddParam(const Assembly: String; Platform: TAssemblerPlatform;
      Parameter: String; UpdateSymbols: Boolean); overload;
    function GetParam(const Command: String): String; overload;
    function GetPlatform(const FileName: TFileName): TAssemblerPlatform; overload;
    function GetParameter(const FileName: TFileName): String;
    function GetUpdateSymbols(const FileName: TFileName): Boolean;
    function ReadWorkspace(const FolderName: string): TStringList;
    procedure WriteWorkspace(const FolderName: string; List: TStringList);
    function IsEditableFile(const FileName: TFileName): Boolean;
    function IsExecutableFile(const FileName: TFileName): Boolean;
    function IsAssemblyFile(const FileName: TFileName): Boolean;
    function IsHtmlFile(const FileName: TFileName): Boolean;
    function HasStructure(const FileName: TFileName): Boolean;
    function IsSearchableFile(const FileName: TFileName): Boolean;
    function IsReadonlyFile(const FileName: TFileName): Boolean;
    function IsExcludedFile(const FileName: TFileName): Boolean;
    function IsExcludedFolder(const FolderName: TFileName): Boolean;
    function GetSyntax(const FileName: TFileName): TSyntax; overload;
    property Version: TVersion read FVersion;
    property VersionText: String read GetVersionText;
    property Terminal: TTerminal read FTerminal;
    property Platform: String read GetPlatform;
    property Debug: String read GetDebug;
    property PreRelease: String read GetPreRelease;
    property Patched: String read GetPatched;
    property PrivateBuild: String read GetPrivateBuild;
    property SpecialBuild: String read GetSpecialBuild;
    property EditFiles: String read FEditFiles write FEditFiles;
    property ExecuteFiles: String read FExecuteFiles write FExecuteFiles;
    property AssemblyFiles: String read FAssemblyFiles write FAssemblyFiles;
    property HtmlFiles: String read FHtmlFiles write FHtmlFiles;
    property SearchFiles: String read FSearchFiles write FSearchFiles;
    property UneditableFiles: String read FUneditableFiles write FUneditableFiles;
    property SaveWorkspace: Boolean read FSaveWorkspace write FSaveWorkspace;
    property ExcludeFiles: String read FExcludeFiles write FExcludeFiles;
    property ExcludeFolders: String read FExcludeFolders write FExcludeFolders;
    property AssemblySyntax: String index synAssembly read GetSyntax write SetSyntax;
    property BasicSyntax: String index synBasic read GetSyntax write SetSyntax;
    property BatchSyntax: String index synBatch read GetSyntax write SetSyntax;
    property HtmlSyntax: String index synHtml read GetSyntax write SetSyntax;
    property ImageSyntax: String index synImage read GetSyntax write SetSyntax;
    property IniSyntax: String index synIni read GetSyntax write SetSyntax;
    property IntelHexSyntax: String index synIntelHex read GetSyntax write SetSyntax;
    property JsonSyntax: String index synJson read GetSyntax write SetSyntax;
    property MarkdownSyntax: String index synMarkdown read GetSyntax write SetSyntax;
    property PascalSyntax: String index synPascal read GetSyntax write SetSyntax;
    property PdfSyntax: String index synPdf read GetSyntax write SetSyntax;
    property RtfSyntax: String index synRtf read GetSyntax write SetSyntax;
    property SpinSyntax: String index synSpin read GetSyntax write SetSyntax;
    property TextSyntax: String index synText read GetSyntax write SetSyntax;
    property XmlSyntax: String index synXml read GetSyntax write SetSyntax;
    property ZipSyntax: String index synZip read GetSyntax write SetSyntax;
    property Params: TParamType read FParams write FParams;
    property Attributes: TAttributes read FAttributes;
    property MonitorFolder: Boolean read FMonitorFolder;
    property FontName: String read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property RightMargin: Integer read FRightMargin write FRightMargin;
  end;

  TBaseConfig = class(TCustomConfig)
  private
  protected
    FFileName: TFileName;
    FHomeFolder: TFileName;
    FToolFolderName: TFileName;
    FHasTools: Boolean;
    FAssemblerFolderName: TFileName;
    FHasAssembler: Boolean;
    FAssemblerFileName: TFileName;
    procedure SetToolFolderName(const Value: TFileName);
  public
    constructor Create; override;
    procedure ReadConfig(const FolderName : TFileName; const FileName: TFileName = ''); virtual;
    procedure WriteConfig; virtual; abstract;
    property ToolFolderName: TFileName read FToolFolderName write SetToolFolderName;
    property HasAssembler: Boolean read FHasAssembler;
    property AssemblerFolderName: TFileName read FAssemblerFolderName;
    property AssemblerNameFile: TFileName read FAssemblerFileName;
  end;

  TProjectConfig = class(TBaseConfig)
  private
  public
    procedure ReadConfig(const FolderName : TFileName; const FileName: TFileName = ''); override;
    procedure WriteConfig; override;
  end;

  TFolderConfig = class(TBaseConfig)
  private
  public
    procedure ReadConfig(const FolderName : TFileName; const FileName: TFileName = ''); override;
    procedure WriteConfig; override;
  end;

const
  BAUDS: array[TTerminal.TBauds] of Integer =
   (   110,          // CBR_110
       300,          // CBR_300
       600,          // CBR_600
      1200,          // CBR_1200
      2400,          // CBR_2400
      4800,          // CBR_4800
      9600,          // CBR_9600
     14400,          // CBR_14400
     19200,          // CBR_19200
     38400,          // CBR_38400
     56000,          // CBR_56000
     57600,          // CBR_57600
    115200);         // CBR_115200

  PARITIES: array[TTerminal.TParity] of String =
   ('No Parity',     // NOPARITY
    'Odd Parity',    // ODDPARITY
    'Even Parity',   // EVENPARITY
    'Mark Parity',   // MARKPARITY
    'Space Parity'); // SPACEPARITY

  FLOW_CONTROLS: array[TTerminal.TFlowControl] of String =
   ('None',          //
    'Software',      //
    'Hardware');     //

implementation

{$WARN 05044 OFF}{$WARN 06058 OFF}

uses
  StrUtils, Masks, FileInfo, VersionTypes, TypInfo, Dialogs, Registry, SynEditStrConst,
  Utils, JsonParser;

type
  TAttributeConfig = record
    StorageName: String;
    Foreground: TColor;
    Background: TColor;
    Style: TFontStyles;
  end;
  TAttributeConfigs = array[TAttributeType] of TAttributeConfig;

const
  ATTRIBUTE_CONFIGS: TAttributeConfigs =
  ((StorageName: SYNS_AttrComment;     Foreground: clDkGray;     Background: clNone; Style: []),        // atComment      Z80
   (StorageName: SYNS_AttrConfig;      Foreground: clGreen;      Background: clNone; Style: []),        // atConfig
   (StorageName: SYNS_AttrConstant;    Foreground: clGreen;      Background: clNone; Style: []),        // atConstant
   (StorageName: SYNS_AttrDataType;    Foreground: clGreen;      Background: clNone; Style: []),        // atDataType
   (StorageName: SYNS_AttrDeclaration; Foreground: clRed;        Background: clNone; Style: []),        // atDeclaration
   (StorageName: SYNS_AttrDirective;   Foreground: clBlack;      Background: clNone; Style: [fsBold]),  // atDirective    Z80
   (StorageName: SYNS_AttrFlow;        Foreground: clGreen;      Background: clNone; Style: []),        // atFlow
   (StorageName: SYNS_AttrIdentifier;  Foreground: clWindowText; Background: clNone; Style: []),        // atIdentifier   Z80
   (StorageName: SYNS_AttrKey;         Foreground: clBlue;       Background: clNone; Style: [fsBold]),  // atKeyword      Z80
   (StorageName: SYNS_AttrMemory;      Foreground: clGreen;      Background: clNone; Style: []),        // atMemory
   (StorageName: SYNS_AttrNumber;      Foreground: clPurple;     Background: clNone; Style: []),        // atNumber       Z80
   (StorageName: SYNS_AttrRegister;    Foreground: clTeal;       Background: clNone; Style: [fsBold]),  // atRegister     Z80
   (StorageName: SYNS_AttrString;      Foreground: clRed;        Background: clNone; Style: []),        // atString       Z80
   (StorageName: SYNS_AttrSymbol;      Foreground: clPurple;     Background: clNone; Style: []),        // atSymbol       Z80
   (StorageName: SYNS_AttrWhitespace;  Foreground: clDefault;    Background: clNone; Style: []));       // atWhitespace   Z80

  DELIMITER                = ';';
  INI_CONFIG               = '%s\Paleo\Editor.json';
  INI_SETTING              = 'Setting';
  INI_EDIT                 = 'EditFiles';
  INI_EXEC                 = 'ExecuteFiles';
  INI_ASSEMBLE             = 'AssemblyFiles';
  INI_HTML                 = 'HTML';
  INI_SEARCH               = 'SearchFiles';
  INI_UNEDITABLE           = 'Uneditable';
  INI_SAVE_WORKSPACE       = 'SaveWorkspace';
  INI_SAVE_WORKSPACE_DEF   = False;
  INI_EXCLUDE              = 'Exclude';
  INI_EXCLUDE_FILE         = 'Files';
  INI_EXCLUDE_FOLDER       = 'Folders';
  INI_SYNTAX               = 'Syntax';
  INI_ASSEMBLY_SYNTAX      = 'Assembly';
  INI_BASIC_SYNTAX         = 'BASIC';
  INI_BATCH_SYNTAX         = 'Batch';
  INI_HTML_SYNTAX          = 'HTML';
  INI_IMAGE_SYNTAX         = 'Image';
  INI_INI_SYNTAX           = 'INI';
  INI_INTEL_HEX_SYNTAX     = 'IntelHex';
  INI_JSON_SYNTAX          = 'JSON';
  INI_MARKDOWN_SYNTAX      = 'Markdown';
  INI_PASCAL_SYNTAX        = 'Pascal';
  INI_PDF_SYNTAX           = 'PDF';
  INI_RTF_SYNTAX           = 'RTF';
  INI_SPIN_SYNTAX          = 'Spin';
  INI_TEXT_SYNTAX          = 'Text';
  INI_XML_SYNTAX           = 'XML';
  INI_ZIP_SYNTAX           = 'ZIP';
  INI_PARAMS               = 'Params';
  INI_TERMINAL             = 'Terminal';
  INI_ATTRIBUTE            = 'Attribute';
  INI_TOOLS                = 'Tools';
  INI_COMMAND              = 'Command';
  INI_PARAM                = 'Param';
  INI_ATTRIBUTE_FOREGROUND = 'Foreground';
  INI_ATTRIBUTE_BACKGROUND = 'Background';
  INI_ATTRIBUTE_BOLD       = 'Bold';
  INI_ATTRIBUTE_ITALIC     = 'Italic';
  INI_ATTRIBUTE_UNDERLINE  = 'Underline';
  INI_ATTRIBUTE_STRIKEOUT  = 'StrikeOut';
  INI_PROJECTS             = 'Projects';
  INI_MRU                  = 'MRU';
  INI_HEIGHT               = 'Height';
  INI_LEFT                 = 'Left';
  INI_LEFT_DEF             = 100;
  INI_TOP                  = 'Top';
  INI_TOP_DEF              = 100;
  INI_WIDTH                = 'Width';
  INI_WIN_STATE            = 'WindowsState';
  INI_WIN_STATE_DEF        = 'wsNormal';
  INI_EDITOR               = 'Editor';
  INI_EDITOR_FONT_NAME     = 'Font';
  INI_EDITOR_FONT_SIZE     = 'Size';
  INI_EDITOR_RIGHT_MARGIN  = 'RightMargin';

{ TJsonObjectHelper }

type
  TJsonObjectHelper = class helper for TJsonObject
  public
    function GetObject(const Name: String; Clear: Boolean = False): TJsonObject;
    function GetArray(const Name: String; Clear: Boolean = False): TJsonArray;
    function GetProject(const Name: String): TJsonObject;
    function GetForm(Form: TForm): TJsonObject;
    function Read(const Name: String; const Default: String): String; overload;
    procedure Write(const Name: String; const Value: String); overload;
    function Read(const Name: String; const Default: Integer): Integer; overload;
    procedure Write(const Name: String; const Value: Integer); overload;
    function Read(const Name: String; const Default: Boolean): Boolean; overload;
    procedure Write(const Name: String; const Value: Boolean); overload;
    procedure SaveToFile(const FileName: TFileName);
  end;

function TJsonObjectHelper.GetObject(const Name: String; Clear: Boolean): TJsonObject;
begin
  if Self.Find(Name, Result) then begin
    if Clear then
      Result.Clear; end
  else begin
    Result := TJsonObject.Create;
    Self.Add(Name, Result);
  end;
end;

function TJsonObjectHelper.GetArray(const Name: String; Clear: Boolean = False): TJsonArray;
begin
  if Self.Find(Name, Result) then begin
    if Clear then
      Result.Clear; end
  else begin
    Result := TJsonArray.Create;
    Self.Add(Name, Result);
  end;
end;

function TJsonObjectHelper.GetProject(const Name: String): TJsonObject;
const
  INI_PROJECT = 'Project';
var
  Temp: TJsonObject;
begin
  Temp := Self.GetObject(INI_PROJECT);
  Result := Temp.GetObject(Name);
end;

function TJsonObjectHelper.GetForm(Form: TForm): TJsonObject;
const
  INI_FORMS = 'Forms';
var
  Temp: TJsonObject;
begin
  Temp := Self.GetObject(INI_FORMS);
  Result := Temp.GetObject(Form.ClassName);
end;

function TJsonObjectHelper.Read(const Name: String; const Default: String): String;
var
  Node: TJsonData;
begin
  if Self.Find(Name, Node) then
    Result := Node.AsString
  else begin
    Self.Add(Name, Default);
    if Self.Find(Name, Node) then
      Result := Node.AsString
    else
      Result := Default;
  end;
end;

procedure TJsonObjectHelper.Write(const Name: String; const Value: String);
var
  Node: TJsonData;
begin
  if Self.Find(Name, Node) then
    Node.AsString := Value
  else
    Self.Add(Name, Value);
end;

function TJsonObjectHelper.Read(const Name: String; const Default: Integer): Integer;
var
  Node: TJsonData;
begin
  if Self.Find(Name, Node) then
    Result := Node.AsInteger
  else begin
    Self.Add(Name, Default);
    if Self.Find(Name, Node) then
      Result := Node.AsInteger
    else
      Result := Default;
  end;
end;

procedure TJsonObjectHelper.Write(const Name: String; const Value: Integer);
var
  Node: TJsonData;
begin
  if Self.Find(Name, Node) then
    Node.AsInteger := Value
  else
    Self.Add(Name, Value);
end;

function TJsonObjectHelper.Read(const Name: String; const Default: Boolean): Boolean;
var
  Node: TJsonData;
begin
  if Self.Find(Name, Node) then
    Result := Node.AsBoolean
  else begin
    Self.Add(Name, Default);
    if Self.Find(Name, Node) then
      Result := Node.AsBoolean
    else
      Result := Default;
  end;
end;

procedure TJsonObjectHelper.Write(const Name: String; const Value: Boolean);
var
  Node: TJsonData;
begin
  if Self.Find(Name, Node) then
    Node.AsBoolean := Value
  else
    Self.Add(Name, Value);
end;

procedure TJsonObjectHelper.SaveToFile(const FileName: TFileName);
begin
  WriteStrToFile(FileName, Self.FormatJSON);
end;

{ TAttribute }

constructor TAttribute.Create(Attr: TAttributeType; const StorageName: String);
begin
  inherited Create;
  FAttr := Attr;
  FStorageName := StorageName;
end;

procedure TAttribute.Reset;
var
  Config: TAttributeConfig;
begin
  Config := ATTRIBUTE_CONFIGS[Attr];
  Foreground := Config.Foreground;
  Background := Config.Background;
  Style := Config.Style;
end;

procedure TAttribute.ReadConfig(Parent: TJsonObject);
var
  Config: TAttributeConfig;
begin
  Config := ATTRIBUTE_CONFIGS[Attr];
  Foreground := TColor(Parent.Read(INI_ATTRIBUTE_FOREGROUND, Integer(Config.Foreground)));
  Background := TColor(Parent.Read(INI_ATTRIBUTE_BACKGROUND, Integer(Config.Background)));
  Style := [];
  if Parent.Read(INI_ATTRIBUTE_BOLD, fsBold in Config.Style) then
    Style := Style + [fsBold];
  if Parent.Read(INI_ATTRIBUTE_ITALIC, fsItalic in Config.Style) then
    Style := Style + [fsItalic];
  if Parent.Read(INI_ATTRIBUTE_UNDERLINE, fsUnderline in Config.Style) then
    Style := Style + [fsUnderline];
  if Parent.Read(INI_ATTRIBUTE_STRIKEOUT, fsStrikeOut in Config.Style) then
    Style := Style + [fsStrikeOut];
end;

function TAttribute.WriteConfig: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.Write(INI_ATTRIBUTE_FOREGROUND, Integer(Foreground));
  Result.Write(INI_ATTRIBUTE_BACKGROUND, Integer(Background));
  Result.Write(INI_ATTRIBUTE_BOLD, fsBold in Style);
  Result.Write(INI_ATTRIBUTE_ITALIC, fsItalic in Style);
  Result.Write(INI_ATTRIBUTE_UNDERLINE, fsUnderline in Style);
  Result.Write(INI_ATTRIBUTE_STRIKEOUT, fsStrikeOut in Style);
end;

procedure TAttribute.Assign(Source: TAttribute);
begin
  if Assigned(Source) then begin
    FStorageName := Source.StorageName;
    Foreground := Source.Foreground;
    Background := Source.Background;
    Style := Source.Style;
  end;
end;

function TAttribute.Matches(Source: TAttribute): Boolean;
begin
  Result := (Foreground = Source.Foreground) and (Background = Source.Background) and (Style = Source.Style);
end;

{ TAttributes }

constructor TAttributes.Create;
var
  Attr: TAttributeType;
  Config: TAttributeConfig;
  Attribute: TAttribute;
begin
  inherited Create;
  FList := TAttributeList.Create([doOwnsValues]);
  for Attr := Low(Attr) to High(Attr) do begin
    Config := ATTRIBUTE_CONFIGS[Attr];
    Attribute := TAttribute.Create(Attr, Config.StorageName);
    Attribute.Foreground := Config.Foreground;
    Attribute.Background := Config.Background;
    Attribute.Style := Config.Style;
    FList.Add(Attr, Attribute);
  end;
end;

destructor TAttributes.Destroy;
begin
  FList.Free;
  inherited;
end;

function TAttributes.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TAttributes.GetItems(Attr: TAttributeType): TAttribute;
begin
  if not Assigned(FList) then
    Result := nil
  else
    if not FList.TryGetValue(Attr, Result) then
      Result := nil;
end;

procedure TAttributes.Reset;
var
  Attr: TAttributeType;
  Attribute: TAttribute;
begin
  for Attr := Low(Attr) to High(Attr) do begin
    Attribute := Items[Attr];
    if Assigned(Attribute) then
      Attribute.Reset;
  end;
end;

procedure TAttributes.Assign(Source: TAttributes);
var
  Attr: TAttributeType;
  Attribute: TAttribute;
begin
  for Attr := Low(Attr) to High(Attr) do begin
    Attribute := Items[Attr];
    if Assigned(Attribute) then
      Attribute.Assign(Source[Attr]);
  end;
end;

function TAttributes.ReadConfig(Parent: TJsonObject): TJsonObject;
var
  I: TAttributeType;
  Attr: TAttribute;
  Attribute: TJsonObject;
begin
  if not Parent.Find(INI_ATTRIBUTE, Result) then
    Reset
  else
    for I := Low(I) to High(I) do begin
      Attr := Items[I];
      if Result.Find(Attr.StorageName, Attribute) then
        Attr.ReadConfig(Attribute)
      else
        Attr.Reset;
    end;
end;

function TAttributes.WriteConfig(Parent: TJsonObject; const Name: String): TJsonObject;
var
  Attr: TAttributeType;
begin
  Result := Parent.GetObject(Name, True);
  for Attr := Low(Attr) to High(Attr) do
    Result.Add(Items[Attr].StorageName, Items[Attr].WriteConfig);
end;

procedure TAttributes.Populate(List: TStrings);
var
  Attr: TAttributeType;
  Attribute: TAttribute;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for Attr := Low(Attr) to High(Attr) do begin
      Attribute := Items[Attr];
      List.AddObject(Attribute.StorageName, Attribute);
    end;
  finally
    List.EndUpdate;
  end;
end;

function TAttributes.Matches(Source: TAttributes): Boolean;
var
  Attr: TAttributeType;
begin
  Result := True;
  for Attr := Low(Attr) to High(Attr) do
    if not Items[Attr].Matches(Source[Attr]) then begin
      Result := False;
      Break;
    end;
end;

{ TTerminal }

function TTerminal.ReadConfig(Parent: TJsonObject): TJsonObject;
var
  Temp: String = '';
begin
  Result := Parent.GetObject(INI_TERMINAL);
  FComPort := Result.Read(INI_COM_PORT, DEF_COM_PORT);
  FBaud := Result.Read(INI_BAUD, DEF_BAUD);
  FDataBits := Result.Read(INI_DATA_BIT, DEF_DATA_BIT);
  FStopBits := Result.Read(INI_STOP_BIT, DEF_STOP_BIT);
  Temp := Result.Read(INI_PARITY, GetEnumName(TypeInfo(TTerminal.TParity), Ord(DEF_PARITY)));
  FParity := TTerminal.TParity(GetEnumValue(TypeInfo(TTerminal.TParity), Temp));
  Temp := Result.Read(INI_FLOW_CONTROL, GetEnumName(TypeInfo(TTerminal.TFlowControl), Ord(DEF_FLOW_CONTROL)));
  FFlowControl := TTerminal.TFlowControl(GetEnumValue(TypeInfo(TTerminal.TFlowControl), Temp));
  FCharDelay := Result.Read(INI_CHAR_DELAY, DEF_CHAR_DELAY);
  FLineDelay := Result.Read(INI_LINE_DELAY, DEF_LINE_DELAY);
  FUploadCommand := Result.Read(INI_UPLOAD, DEF_UPLOAD);
  FDownloadCommand := Result.Read(INI_DOWNLOAD, DEF_DOWNLOAD);
end;

function TTerminal.WriteConfig(Parent: TJsonObject; const Name: String): TJsonObject;
begin
  Result := Parent.GetObject(Name);
  Result.Write(INI_COM_PORT, FComPort);
  Result.Write(INI_BAUD, FBaud);
  Result.Write(INI_DATA_BIT, FDataBits);
  Result.Write(INI_STOP_BIT, FStopBits);
  Result.Write(INI_PARITY, GetEnumName(TypeInfo(TTerminal.TParity), Ord(FParity)));
  Result.Write(INI_FLOW_CONTROL, GetEnumName(TypeInfo(TTerminal.TFlowControl), Ord(FFlowControl)));
  Result.Write(INI_CHAR_DELAY, FCharDelay);
  Result.Write(INI_LINE_DELAY, FLineDelay);
  Result.Write(INI_UPLOAD, FUploadCommand);
  Result.Write(INI_DOWNLOAD, FDownloadCommand);
end;

function SortByComNumber(List: TStringList; Index1, Index2: Integer): Integer;
var
  First: Integer = 0;
  Second: Integer = 0;
begin
  First := Integer(List.Objects[Index1]);
  Second := Integer(List.Objects[Index2]);
  Result := First - Second;
end;

class procedure TTerminal.PopulateComPort(List: TStrings);
const
  KEY = 'HARDWARE\DEVICEMAP\SERIALCOMM';
var
  Reg: TRegistry;
  Temp: TStringList;
  I: Integer = 0;
  Name: String = '';
  Port: Integer = 0;
begin
  List.BeginUpdate;
  try
    List.Clear;
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly(KEY) then begin
        Temp := TStringList.Create;
        try
          Reg.GetValueNames(Temp);
          for I := 0 to Temp.Count - 1 do begin
            Name := Reg.ReadString(Temp[I]);
            Temp[I] := Name + ':';
            Name := Name.Substring(3, Name.Length - 3);
            Port := StrToIntDef(Name, 0);
            Temp.Objects[I] := TObject(Port);
          end;
          Temp.CustomSort(SortByComNumber);
          for I := 0 to Temp.Count - 1 do begin
            Name := Temp[I];
            Port := Integer(Temp.Objects[I]);
            List.AddObject(Name, TObject(Port));
          end;
        finally
          Temp.Free;
        end;
      end;
    finally
      Reg.Free;
    end;
  finally
    List.EndUpdate;
  end;
end;

class procedure TTerminal.PopulateBaud(List: TStrings);
type
  TBauds = 0..12;
var
  I: TBauds;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(BAUDS[I].ToString, TObject(BAUDS[I]));
  finally
    List.EndUpdate;
  end;
end;

class procedure TTerminal.PopulateDataBits(List: TStrings);
const
  MASK = '%d Data Bits';
var
  I: TTerminal.TDataBit;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(Format(MASK, [I]), TObject(I));
  finally
    List.EndUpdate;
  end;
end;

class procedure TTerminal.PopulateStopBits(List: TStrings);
const
  MASK = '%d Stop Bits';
var
  I: TTerminal.TStopBit;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(Format(MASK, [I]), TObject(I));
  finally
    List.EndUpdate;
  end;
end;

class procedure TTerminal.PopulateParity(List: TStrings);
var
  I: TTerminal.TParity;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(PARITIES[I], TObject(I));
  finally
    List.EndUpdate;
  end;
end;

class procedure TTerminal.PopulateFlowControl(List: TStrings);
var
  I: TTerminal.TFlowControl;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := Low(I) to High(I) do
      List.AddObject(FLOW_CONTROLS[i], TObject(I));
  finally
    List.EndUpdate;
  end;
end;

class function TTerminal.ComPortToStr(Value: Integer): String;
const
  MASK = 'COM%d:';
begin
  Result := Format(MASK, [Value]);
end;

class function TTerminal.DataBitToStr(Value: TDataBit): String;
const
  MASK = '%d Data Bits';
begin
  Result := Format(MASK, [Value]);
end;

class function TTerminal.StopBitToStr(Value: TStopBit): String;
const
  MASK = '%d Stop Bits';
begin
  Result := Format(MASK, [Value]);
end;

class function TTerminal.ParityToStr(Value: TParity): String;
begin
  Result := PARITIES[Value];
end;

class function TTerminal.FlowControlToStr(Value: TFlowControl): String;
begin
  Result := FLOW_CONTROLS[Value];
end;

{ TCustomConfig }

constructor TCustomConfig.Create;
begin
  inherited Create;
  FConfigFileName := GetEnvironmentVariable('APPDATA');
  FConfigFileName := Format(INI_CONFIG, [FConfigFileName]);
end;

function TCustomConfig.GetDocument: TJsonObject;
begin
  if FileExists(ConfigFileName) then
    try
      Result := GetJSON(ReadStrFromFile(ConfigFileName)) as TJsonObject
    except
      Result := TJsonObject.Create;
    end
  else
    Result := TJsonObject.Create;
end;

procedure TCustomConfig.SaveDocument(Document: TJsonObject);
begin
  if Assigned(Document) then
    WriteStrToFile(ConfigFileName, Document.FormatJSON);
end;

{ TConfig }

constructor TConfig.Create;
const
  DEBUG_BIT         = $01;
  PRE_RELEASE_BIT   = $02;
  PATCHED_BIT       = $04;
  PRIVATE_BUILD_BIT = $08;
  SPECIAL_BUILD_BIT = $20;
var
  VersionInfo: TVersionInfo;

  function SearchValue(Info: TVersionStringFileInfo; const Value: string): string;
  var
    S: TVersionStringTable;
    I: integer = 0;
    J: integer = 0;
  begin
    Result := EmptyStr;
    for I := 0 to Info.Count - 1 do begin
      S := Info.Items[I];
      for J := 0 to S.Count - 1 do
        if S.Keys[J] = Value then begin
          Result := S.Values[J];
          Break;
        end;
    end;
  end;

begin
  inherited Create;
  FParams := TParamType.Create;
  VersionInfo := TVersionInfo.Create;
  try
    VersionInfo.Load(Application.Handle);
    FVersion.Major := VersionInfo.FixedInfo.FileVersion[0];
    FVersion.Minor := VersionInfo.FixedInfo.FileVersion[1];
    FVersion.Release := VersionInfo.FixedInfo.FileVersion[2];
    FVersion.Build := VersionInfo.FixedInfo.FileVersion[3];
    FVersion.IsDebug := VersionInfo.FixedInfo.FileFlags and DEBUG_BIT > 0;
    FVersion.IsPreRelease := VersionInfo.FixedInfo.FileFlags and PRE_RELEASE_BIT > 0;
    FVersion.IsPatched := VersionInfo.FixedInfo.FileFlags and PATCHED_BIT > 0;
    FVersion.IsPrivateBuild := VersionInfo.FixedInfo.FileFlags and PRIVATE_BUILD_BIT > 0;
    FVersion.IsSpecialBuild := VersionInfo.FixedInfo.FileFlags and SPECIAL_BUILD_BIT > 0;
    FVersion.CompanyName := SearchValue(VersionInfo.StringFileInfo, 'CompanyName');
    FVersion.InternalName := SearchValue(VersionInfo.StringFileInfo, 'InternalName');
    FVersion.ProjectName := SearchValue(VersionInfo.StringFileInfo, 'FileVersion');
    FVersion.FileVersion := SearchValue(VersionInfo.StringFileInfo, 'ProductName');
  finally
    VersionInfo.Free;
  end;
  FTerminal := TTerminal.Create;
  FMonitorFolder := True;
  FAttributes := TAttributes.Create;
  FSyntax := Default(TSyntaxes);
end;

destructor TConfig.Destroy;
begin
  FAttributes.Free;
  FParams.Free;
  FTerminal.Free;
  inherited;
end;

function TConfig.GetVersionText: String;
const
  MASK = '%d.%d.%d';
begin
  Result := Format(MASK, [Version.Major, Version.Minor, Version.Release]);
end;

function TConfig.GetPlatform: String;
const
{$if defined(Win32)}
  PLATFORM = 'Win32';
{$elseif defined(Win64)}
  PLATFORM = 'Win64';
{$else}
  PLATFORM = EmptyStr;
{$endif}
begin
  Result := PLATFORM;
end;

function TConfig.GetDebug: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Debug');
begin
  Result := CAPTIONS[Version.IsDebug];
end;

function TConfig.GetPreRelease: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Pre-Release');
begin
  Result := CAPTIONS[Version.IsPreRelease];
end;

function TConfig.GetPatched: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Patched');
begin
  Result := CAPTIONS[Version.IsPatched];
end;

function TConfig.GetPrivateBuild: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Private');
begin
  Result := CAPTIONS[Version.IsPrivateBuild];
end;

function TConfig.GetSpecialBuild: String;
const
  CAPTIONS: array[Boolean] of String = ('', 'Special');
begin
  Result := CAPTIONS[Version.IsSpecialBuild];
end;

function TConfig.GetSyntax(Syntax: TSyntax): String;
begin
  Result := FSyntax[Syntax];
end;

procedure TConfig.SetSyntax(Syntax: TSyntax; const Value: String);
begin
  FSyntax[Syntax] := Value;
end;

procedure TConfig.ReadConfig;
var
  Document: TJsonObject;

  function ReadSettings(Parent: TJsonObject): TJsonObject;
  begin
    Result := Parent.GetObject(INI_SETTING);
    FEditFiles := Result.Read(INI_EDIT, INI_EDIT_DEF);
    FExecuteFiles := Result.Read(INI_EXEC, INI_EXEC_DEF);
    FAssemblyFiles := Result.Read(INI_ASSEMBLE, INI_ASSEMBLE_DEF);
    FHtmlFiles := Result.Read(INI_HTML, INI_HTML_DEF);
    FSearchFiles := Result.Read(INI_SEARCH, INI_SEARCH_DEF);
    FUneditableFiles := Result.Read(INI_UNEDITABLE, INI_UNEDITABLE_DEF);
    FSaveWorkspace := Result.Read(INI_SAVE_WORKSPACE, INI_SAVE_WORKSPACE_DEF);
  end;

  function ReadExclude(Parent: TJsonObject): TJsonObject;
  begin
    Result := Parent.GetObject(INI_EXCLUDE);
    FExcludeFiles := Result.Read(INI_EXCLUDE_FILE, INI_EXCLUDE_FILE_DEF);
    FExcludeFolders := Result.Read(INI_EXCLUDE_FOLDER, INI_EXCLUDE_FOLDER_DEF);
  end;

  function ReadSyntax(Parent: TJsonObject): TJsonObject;
  begin
    Result := Parent.GetObject(INI_SYNTAX);
    AssemblySyntax := Result.Read(INI_ASSEMBLY_SYNTAX, ITEM_ASSEMBLY_SYNTAX_DEF);
    BasicSyntax := Result.Read(INI_BASIC_SYNTAX, ITEM_BASIC_SYNTAX_DEF);
    BatchSyntax := Result.Read(INI_BATCH_SYNTAX, ITEM_BATCH_SYNTAX_DEF);
    HtmlSyntax := Result.Read(INI_HTML_SYNTAX, ITEM_HTML_SYNTAX_DEF);
    ImageSyntax := Result.Read(INI_IMAGE_SYNTAX, ITEM_IMAGE_SYNTAX_DEF);
    IniSyntax := Result.Read(INI_INI_SYNTAX, ITEM_INI_SYNTAX_DEF);
    IntelHexSyntax := Result.Read(INI_INTEL_HEX_SYNTAX, ITEM_INTEL_HEX_SYNTAX_DEF);
    JsonSyntax := Result.Read(INI_JSON_SYNTAX, ITEM_JSON_SYNTAX_DEF);
    MarkdownSyntax := Result.Read(INI_MARKDOWN_SYNTAX, ITEM_MARKDOWN_SYNTAX_DEF);
    PascalSyntax := Result.Read(INI_PASCAL_SYNTAX, ITEM_PASCAL_SYNTAX_DEF);
    PdfSyntax := Result.Read(INI_PDF_SYNTAX, ITEM_PDF_SYNTAX_DEF);
    RtfSyntax := Result.Read(INI_RTF_SYNTAX, ITEM_RTF_SYNTAX_DEF);
    SpinSyntax := Result.Read(INI_SPIN_SYNTAX, ITEM_SPIN_SYNTAX_DEF);
    TextSyntax := Result.Read(INI_TEXT_SYNTAX, ITEM_TEXT_SYNTAX_DEF);
    XmlSyntax := Result.Read(INI_XML_SYNTAX, ITEM_XML_SYNTAX_DEF);
    ZipSyntax := Result.Read(INI_ZIP_SYNTAX, ITEM_ZIP_SYNTAX_DEF);
  end;

  function ReadEditor(Parent: TJsonObject): TJsonObject;
  begin
    Result := Parent.GetObject(INI_EDITOR);
    FFontName := Result.Read(INI_EDITOR_FONT_NAME, INI_EDITOR_FONT_NAME_DEF);
    FFontSize := Result.Read(INI_EDITOR_FONT_SIZE, INI_EDITOR_FONT_SIZE_DEF);
    FRightMargin := Result.Read(INI_EDITOR_RIGHT_MARGIN, INI_EDITOR_RIGHT_MARGIN_DEF);
  end;

  function ReadParam(Parent: TJsonObject): TJsonArray;
  var
    Command: String = '';
    Param: String = '';
    I: TJsonEnum;
    Node: TJsonObject;
  begin
    FParams.Clear;
    Result := Parent.GetArray(INI_PARAMS, False);
    for I in Result do begin
      Node := TJsonObject(I.Value);
      Command := Node.Read(INI_COMMAND, EmptyStr);
      Param := Node.Read(INI_PARAM, EmptyStr);
      AddParam(Command, Param);
    end;
  end;

begin
  Document := GetDocument;
  try
    ReadSettings(Document);
    ReadExclude(Document);
    ReadSyntax(Document);
    ReadEditor(Document);
    ReadParam(Document);
    Terminal.ReadConfig(Document);
    Attributes.ReadConfig(Document);
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TConfig.WriteConfig;
var
  Document: TJsonObject;

  function WriteSettings(Parent: TJsonObject; const Name: String): TJsonObject;
  begin
    Result := Parent.GetObject(Name);
    Result.Write(INI_EDIT, FEditFiles);
    Result.Write(INI_EXEC, FExecuteFiles);
    Result.Write(INI_ASSEMBLE, FAssemblyFiles);
    Result.Write(INI_HTML, FHtmlFiles);
    Result.Write(INI_SEARCH, FSearchFiles);
    Result.Write(INI_UNEDITABLE, FUneditableFiles);
    Result.Write(INI_SAVE_WORKSPACE, FSaveWorkspace);
  end;

  function WriteExclude(Parent: TJsonObject; const Name: String): TJsonObject;
  begin
    Result := Parent.GetObject(Name);
    Result.Write(INI_EXCLUDE_FILE, FExcludeFiles);
    Result.Write(INI_EXCLUDE_FOLDER, FExcludeFolders);
  end;

  function WriteSyntax(Parent: TJsonObject; const Name: String): TJsonObject;
  begin
    Result := Parent.GetObject(Name);
    Result.Write(INI_ASSEMBLY_SYNTAX, AssemblySyntax);
    Result.Write(INI_BASIC_SYNTAX, BasicSyntax);
    Result.Write(INI_BATCH_SYNTAX, BatchSyntax);
    Result.Write(INI_HTML_SYNTAX, HtmlSyntax);
    Result.Write(INI_IMAGE_SYNTAX, ImageSyntax);
    Result.Write(INI_INI_SYNTAX, IniSyntax);
    Result.Write(INI_INTEL_HEX_SYNTAX, IntelHexSyntax);
    Result.Write(INI_JSON_SYNTAX, JsonSyntax);
    Result.Write(INI_MARKDOWN_SYNTAX, MarkdownSyntax);
    Result.Write(INI_PASCAL_SYNTAX, PascalSyntax);
    Result.Write(INI_PDF_SYNTAX, PdfSyntax);
    Result.Write(INI_RTF_SYNTAX, RtfSyntax);
    Result.Write(INI_SPIN_SYNTAX, SpinSyntax);
    Result.Write(INI_TEXT_SYNTAX, TextSyntax);
    Result.Write(INI_XML_SYNTAX, XmlSyntax);
    Result.Write(INI_ZIP_SYNTAX, ZipSyntax);
  end;

  function WriteEditor(Parent: TJsonObject; const Name: String): TJsonObject;
  begin
    Result := Parent.GetObject(Name);
    Result.Write(INI_EDITOR_FONT_NAME, FFontName);
    Result.Write(INI_EDITOR_FONT_SIZE, FFontSize);
    Result.Write(INI_EDITOR_RIGHT_MARGIN, FRightMargin);
  end;

  function WriteParam(Parent: TJsonObject; const Name: String): TJsonArray;
  var
    Command: String = '';
    Param: String = '';
    Node: TJsonObject;
  begin
    Result := Parent.GetArray(Name, True);
    for Command in FParams.Keys do begin
      Param := FParams[Command];
      if not Param.IsEmpty then begin
        Node := TJsonObject.Create;
        Node.Add(INI_COMMAND, Command);
        Node.Add(INI_PARAM, Param);
        Result.Add(Node);
      end;
    end;
  end;

begin
  Document := GetDocument;
  try
    WriteSettings(Document, INI_SETTING);
    WriteExclude(Document, INI_EXCLUDE);
    WriteSyntax(Document, INI_SYNTAX);
    WriteEditor(Document, INI_EDITOR);
    WriteParam(Document, INI_PARAMS);
    Terminal.WriteConfig(Document, INI_TERMINAL);
    Attributes.WriteConfig(Document, INI_ATTRIBUTE);
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TConfig.ReadConfig(Form: TForm);
var
  Document: TJsonObject;
  Node: TJsonObject;
  Temp: String = '';
begin
  Document := GetDocument;
  try
    Node := Document.GetForm(Form);
    Form.Height := Node.Read(INI_HEIGHT, Form.Constraints.MinHeight);
    Form.Left := Node.Read(INI_LEFT, INI_LEFT_DEF);
    Form.Top := Node.Read(INI_TOP, INI_TOP_DEF);
    Form.Width := Node.Read(INI_WIDTH, Form.Constraints.MinWidth);
    Temp := Node.Get(INI_WIN_STATE, INI_WIN_STATE_DEF);
    Form.WindowState := TWindowState(GetEnumValue(TypeInfo(TWindowState), Temp));
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TConfig.WriteConfig(Form: TForm);
var
  Document: TJsonObject;
  Node: TJsonObject;
begin
  Document := GetDocument;
  try
    Node := Document.GetForm(Form);
    Node.Write(INI_HEIGHT, Form.Height);
    Node.Write(INI_LEFT, Form.Left);
    Node.Write(INI_TOP, Form.Top);
    Node.Write(INI_WIDTH, Form.Width);
    Node.Write(INI_WIN_STATE, GetEnumName(TypeInfo(TWindowState), Ord(Form.WindowState)));
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TConfig.ReadConfig(ParentMenu: TMenuItem; EventHandler: TNotifyEvent);
var
  Document: TJsonObject;
  List: TJsonArray;
  I: TJsonEnum;
  Node: TJsonObject;
  ImageIndex: Integer = 0;
  Item: TMenuItem;
begin
  Document := GetDocument;
  try
    List := Document.GetArray(INI_MRU);
    for I in List do begin
      Node := TJsonObject(I.Value);
      ImageIndex := Node.Read('ImageIndex', -1);
      Item := TMenuItem.Create(ParentMenu);
      Item.Caption := Node.Read('Caption', EmptyStr);
      Item.Hint := Format(MRU_MASK, [Item.Caption]);
      Item.Tag := ImageIndex;
      Item.ImageIndex := Item.Tag;
      Item.OnClick := EventHandler;
      ParentMenu.Add(Item);
    end;
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TConfig.WriteConfig(ParentMenu: TMenuItem);
var
  Document: TJsonObject;
  Node: TJsonArray;
  Item: TMenuItem;
  Child: TJsonObject;
begin
  Document := GetDocument;
  try
    Node := Document.GetArray(INI_MRU, True);
    for Item in ParentMenu do begin
      Child := TJsonObject.Create;
      Child.Write('Caption', Item.Caption);
      Child.Write('ImageIndex', Item.ImageIndex);
      Node.Add(Child);
    end;
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TConfig.ReadConfig(Panel: TPanel; PageControl: TPageControl; const FolderName: TFileName); overload;
var
  Document: TJsonObject;
  Project: TJsonObject;
begin
  Document := GetDocument;
  try
    Project := Document.GetProject(FolderName);
    Panel.Width := Project.Read(Panel.Name, Panel.Width);
    PageControl.Height := Project.Read(PageControl.Name, PageControl.Height);
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TConfig.WriteConfig(Panel: TPanel; PageControl: TPageControl; const FolderName: TFileName); overload;
var
  Document: TJsonObject;
  Project: TJsonObject;
begin
  Document := GetDocument;
  try
    Project := Document.GetProject(FolderName);
    Project.Write(Panel.Name, Panel.Width);
    Project.Write(PageControl.Name, PageControl.Height);
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TConfig.AddParam(const Command, Param: String);
begin
  FParams.AddOrSetValue(Command, Param);
end;

procedure TConfig.AddParam(const Assembly: String; Platform: TAssemblerPlatform;
      Parameter: String; UpdateSymbols: Boolean);
const
  MASK = '%s;%s;%s';
begin
  FParams.AddOrSetValue(Assembly, Format(MASK, [GetEnumName(TypeInfo(TAssemblerPlatform),
    Ord(Platform)), BoolToStr(UpdateSymbols, True), Parameter]));
end;

function TConfig.GetParam(const Command: String): String;
begin
  if not FParams.TryGetValue(Command, Result) then
    Result := EmptyStr;
end;

function TConfig.GetPlatform(const FileName: TFileName): TAssemblerPlatform;
var
  Temp: String = '';
begin
  if not FParams.TryGetValue(FileName, Temp) then
    Result := apZ80
  else begin
    Temp := ExtractWord(1, Temp, [DELIMITER]);
    if Temp.IsEmpty then
      Result := apZ80
    else
      Result := TAssemblerPlatform(GetEnumValue(TypeInfo(TAssemblerPlatform), Temp));
  end;
end;

function TConfig.GetParameter(const FileName: TFileName): String;
var
  Temp: String = '';
begin
  if not FParams.TryGetValue(FileName, Temp) then
    Result := EmptyStr
  else
    Result := ExtractWord(3, Temp, [DELIMITER]);
end;

function TConfig.GetUpdateSymbols(const FileName: TFileName): Boolean;
var
  Temp: String = '';
begin
  if not FParams.TryGetValue(FileName, Temp) then
    Result := False
  else begin
    Temp := ExtractWord(2, Temp, [DELIMITER]);
    Result := not Temp.IsEmpty;
    if Result then
      Result := StrToBool(Temp);
  end;
end;

function TConfig.ReadWorkspace(const FolderName: string): TStringList;
var
  Document: TJsonObject;
  Project: TJsonObject;
  Projects: TJsonArray;
  I: Integer;
begin
  Result := TStringList.Create;
  if SaveWorkspace then begin
    Document := GetDocument;
    try
      Project := Document.GetProject(FolderName);
      Projects := Project.GetArray(INI_PROJECTS);
      for I := 0 to Projects.Count - 1 do
        Result.Values[Projects.Strings[I]] := EmptyStr;
      SaveDocument(Document);
    finally
      Document.Free;
    end;
  end;
end;

procedure TConfig.WriteWorkspace(const FolderName: string; List: TStringList);
var
  Document: TJsonObject;
  Project: TJsonObject;
  Projects: TJsonArray;
  I: Integer;
begin
  if SaveWorkspace then begin
    Document := GetDocument;
    try
      Project := Document.GetProject(FolderName);
      Projects := Project.GetArray(INI_PROJECTS, True);
      for I := 0 to List.Count - 1 do
        Projects.Add(List.Names[I]);
      SaveDocument(Document);
    finally
      Document.Free;
    end;
  end;
end;

function TConfig.IsEditableFile(const FileName: TFileName): Boolean;
const
  EDITABLE = [synAssembly, synBasic, synBatch, synHex, synHtml, synIni, synIntelHex,
      synJson, synMarkdown, synPascal, synSpin, synText, synXml];
var
  Syntax: TSyntax;
begin
  Syntax := GetSyntax(FileName);
  Result := Syntax in EDITABLE;
end;

function TConfig.IsExecutableFile(const FileName: TFileName): Boolean;
begin
  Result := MatchesMaskList(FileName, ExecuteFiles);
end;

function TConfig.IsAssemblyFile(const FileName: TFileName): Boolean;
begin
  Result := MatchesMaskList(FileName, AssemblyFiles);
end;

function TConfig.IsHtmlFile(const FileName: TFileName): Boolean;
begin
  Result := MatchesMaskList(FileName, HtmlFiles);
end;

function TConfig.HasStructure(const FileName: TFileName): Boolean;
begin
  Result := MatchesMask(FileName, '*.lst');
end;

function TConfig.IsSearchableFile(const FileName: TFileName): Boolean;
begin
  Result := MatchesMaskList(FileName, SearchFiles);
end;

function TConfig.IsReadonlyFile(const FileName: TFileName): Boolean;
begin
  Result := MatchesMaskList(FileName, UneditableFiles);
end;

function TConfig.IsExcludedFile(const FileName: TFileName): Boolean;
begin
  Result := MatchesMaskList(FileName, ExcludeFiles);
end;

function TConfig.IsExcludedFolder(const FolderName: TFileName): Boolean;
const
  INVALID_FOLDERS: array[0..1] of String = ('.', '..');
begin
  Result := AnsiMatchStr(FolderName, INVALID_FOLDERS);
  if not Result then
    Result := MatchesMaskList(FolderName, ExcludeFolders);
end;

function TConfig.GetSyntax(const FileName: TFileName): TSyntax;
var
  I: TSyntax;
begin
  Result := synHex;
  for I := Low(I) to High(I) do
    if MatchesMaskList(FileName, FSyntax[I]) then begin
      Result := I;
      Break;
    end;
end;

{ TBaseConfig }

constructor TBaseConfig.Create;
begin
  inherited Create;
  FFileName := EmptyStr;
  FHomeFolder := EmptyStr;
  FToolFolderName := EmptyStr;
  FHasTools := False;
  FAssemblerFolderName := EmptyStr;
  FHasAssembler := False;
  FAssemblerFileName := EmptyStr;
end;

procedure TBaseConfig.SetToolFolderName(const Value: TFileName);
begin
  FToolFolderName := ExcludeTrailingPathDelimiter(Value);
  FHasTools := DirectoryExists(FToolFolderName);
  if FHasTools then begin
    FAssemblerFolderName := Format(ASSEMBLER_FOLDER_MASK, [FToolFolderName]);
    FHasAssembler := DirectoryExists(FAssemblerFolderName);
    if FHasAssembler then begin
      FAssemblerFileName := Format(ASSEMBLER_FILE_MASK, [FAssemblerFolderName]);
      FHasAssembler := FileExists(FAssemblerFileName);
    end;
  end;
end;

procedure TBaseConfig.ReadConfig(const FolderName : TFileName; const FileName: TFileName);
begin
  FHomeFolder := ExcludeTrailingPathDelimiter(FolderName);
  FFileName := FileName;
end;

{ TProjectConfig }

procedure TProjectConfig.ReadConfig(const FolderName : TFileName; const FileName: TFileName);
var
  Document: TJsonObject;
  Project: TJsonObject;
begin
  inherited ReadConfig(FolderName, FileName);
  Document := GetDocument;
  try
    Project := Document.GetProject(FileName);
    ToolFolderName := Project.Read(INI_TOOLS, EmptyStr);
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TProjectConfig.WriteConfig;
var
  Document: TJsonObject;
  Project: TJsonObject;
begin
  Document := GetDocument;
  try
    Project := Document.GetProject(FFileName);
    Project.Write(INI_TOOLS, FToolFolderName);
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

{ TFolderConfig }

procedure TFolderConfig.ReadConfig(const FolderName : TFileName; const FileName: TFileName);
const
  MASK = '%s\Tools';
var
  Document: TJsonObject;
  Project: TJsonObject;
begin
  inherited ReadConfig(FolderName, FileName);
  Document := GetDocument;
  try
    Project := Document.GetProject(FolderName);
    ToolFolderName := Project.Read(INI_TOOLS, Format(MASK, [FHomeFolder]));
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

procedure TFolderConfig.WriteConfig;
var
  Document: TJsonObject;
  Project: TJsonObject;
begin
  Document := GetDocument;
  try
    Project := Document.GetProject(FHomeFolder);
    Project.Write(INI_TOOLS, FToolFolderName);
    SaveDocument(Document);
  finally
    Document.Free;
  end;
end;

end.

