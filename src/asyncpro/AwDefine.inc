(***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Async Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1991-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{*                   AWDEFINE.INC 4.07b                  *}
{*********************************************************}
{* Compiler defines affecting all APRO units             *}
{*********************************************************}

{Because AWDEFINE.INC is included in all APRO units, you can specify global
compiler options here. AWDEFINE is included *before* each units' required
compiler options, so options specified here might be overridden by hardcode
options in the source file.}

{$S- No stack overflow checking}
{$RANGECHECKS OFF}
{ $ B- Incomplete boolean evaluation}
{$BOOLEVAL OFF}
{ $ P- No open string parameters}
{$OPENSTRINGS OFF}
{ $ Q- No arithmetic overflow checking}
{$OVERFLOWCHECKS OFF}
{ $ T- No type-checked pointers}
{$TYPEDADDRESS OFF}
{ $ V- No var string checking}
{$VARSTRINGCHECKS OFF}
{ $ X+ Extended syntax}
{$EXTENDEDSYNTAX ON}
{$Z- Enumeration size is 256}

  { $ H+ Long string support}
{$LONGSTRINGS ON}
  { $ J+ Writeable typed constants}
{$WRITEABLECONST ON}

{.$DEFINE TapiDebug}                      { creates detailed TAPI debug log }
{.$DEFINE AdModemDebug}                   { adds debug info to dispatcher log }

{-------- !! DON'T CHANGE DEFINES BELOW THIS LINE !! -----------------}

{
  Defines to identify compilers are structured so the define means that
  compiler and later compilers.  The Delphi6 define means that we're in
  Delphi 6 or Delphi 7. When adding support for new compilers, add similar
  defines as required.
}

{$IFDEF VER80}                {Delphi 1.0}
  {$IFNDEF PRNDRV}
  !! Error: APRO is a 32-bit library
  {$ENDIF}
{$ENDIF}

{$IFDEF VER90}                {Delphi 2.0}
  !! Error: APRO is compatible with Delphi 3 and above
{$ENDIF}

{$IFDEF VER93}                {C++ Builder 1.0}
  !! Error: APRO is compatible with C++ Builder 3 and above
{$ENDIF}


{$IFDEF VER100}                {Delphi 3.0}
  {$DEFINE AproDelphi}
{$ENDIF}

{$IFDEF VER120}                {Delphi 4.0}
  {$DEFINE AproDelphi}
{$ENDIF}

{$IFDEF VER130}
  {$IFNDEF BCB}
   {$DEFINE AproDelphi}        {Delphi 5.0}
  {$ENDIF}
{$ENDIF}

{$IFDEF Ver140}
  {$IFNDEF BCB}
    {$DEFINE AproDelphi}       {Delphi 6.0}
  {$ENDIF}
{$ENDIF}

{$IFNDEF VER80}
 {$IFNDEF VER90}
  {$IFNDEF VER93}
    {$DEFINE Delphi3}         {Delphi 3.0 or higher}
    {$DEFINE VERSION3}        {Delphi 3.0^ or BCB 3.0^}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF Delphi3}
  {$IFNDEF VER100}
    {$IFNDEF Ver110}
      {$DEFINE Delphi4}       {Delphi 4.0 or higher}
      {$DEFINE VERSION4}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF Delphi4}
  {$IFNDEF VER120}
    {$IFNDEF Ver125}
      {$DEFINE Delphi5}       {Delphi 5.0 or higher}
      {$DEFINE VERSION5}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF Delphi5}
  {$IFNDEF VER130}
      {$DEFINE Delphi6}       {Delphi 6.0 or higher}
      {$DEFINE VERSION6}
  {$ENDIF}
{$ENDIF}

{$IFDEF Delphi6}
  {$IFNDEF VER140}
      {$DEFINE Delphi7}       {Delphi 7.0 (VER150) or higher}
      {$DEFINE VERSION7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER110}               {C++ Builder 3.0}
  {$ObjExportAll On}
  {$DEFINE AproBCB}
{$ENDIF}

{$IFDEF VER125}               {C++ Builder 4.0}
  {$ObjExportAll On}
  {$DEFINE AproBCB}
{$ENDIF}

{$IFDEF Ver130}               {C++ Builder 5.0}
  {$IFDEF BCB}
    {$ObjExportAll On}
    {$DEFINE AproBCB}
  {$ENDIF}
{$ENDIF}

{$IFDEF Ver140}               {C++ Builder 6.0}
  {$IFDEF BCB}
    {$ObjExportAll On}
    {$DEFINE AproBCB}
  {$ENDIF}
{$ENDIF}

{ APRO supports D3/BCB3 and above, and is a strictly Win32 library }
{ turn off the following warnings }
{$IFDEF Delphi6}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF Delphi7}
    { $WARN UNSAFE_CODE OFF}
    { $WARN UNSAFE_TYPE OFF}
    { $WARN UNSAFE_CAST OFF}
{$ENDIF}
