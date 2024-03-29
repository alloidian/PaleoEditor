@REM  Copyright (C) 2022-2023 by Steve Garcia. All rights reserved.
@REM
@REM  This file is part of the Paleo Editor project.
@REM
@REM  The Paleo Editor is free software: you can redistribute it and/or modify it under the
@REM  terms of the GNU General Public License as published by the Free Software Foundation,
@REM  either version 3 of the License, or (at your option) any later version.
@REM
@REM  The Paleo Editor project is distributed in the hope that it will be useful, but WITHOUT
@REM  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
@REM  PARTICULAR PURPOSE. See the GNU General Public License for more details.
@REM
@REM  You should have received a copy of the GNU General Public License along with the Paleo
@REM  Editor project. If not, see <https://www.gnu.org/licenses/>. }

@SET VER=0.1.4

@CALL :BUILD 32 x86
@CALL :BUILD 64 x64
@EXIT /B

:BUILD
@C:\lazarus\lazbuild.exe -B --build-mode="Release %1" --no-write-project Paleo.lpi
@..\tool\7zip\7zr a -bd ..\bin\Paleo_%VER%_Win%1.7z ..\bin\%2\PaleoEditor.exe ..\Bin\%2\pdfium.dll
@REM "..\tool\upx\upx.exe" -qk "..\bin\%2\PaleoEditor.exe"
@"C:\Program Files (x86)\Inno Setup 6\iscc.exe" /Q "Paleo%1.iss"
@EXIT /B
