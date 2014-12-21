# -*- coding: utf-8 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright Saab AB, 2014 (http://safir.sourceforge.net)
; Copyright Consoden AB, 2014 (http://www.consoden.se)
;
; Created by: Lars Hagstrom / lars.hagstrom@consoden.se
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file is part of Safir SDK Core.
;
; Safir SDK Core is free software: you can redistribute it and/or modify
; it under the terms of version 3 of the GNU General Public License as
; published by the Free Software Foundation.
;
; Safir SDK Core is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Safir SDK Core installer script
;Written by Lars Hagström

;--------------------------------

;Include Modern UI
!include "MUI2.nsh"

;Include logic operations
!include "LogicLib.nsh"

;Useful file functions
!include "FileFunc.nsh"

;Check windows version header
!include WinVer.nsh

;Set a compressor that gives us very good ratios
SetCompressor /SOLID lzma


!ifndef IPersistFile
!define IPersistFile {0000010b-0000-0000-c000-000000000046}
!endif
!ifndef CLSID_ShellLink
!define CLSID_ShellLink {00021401-0000-0000-C000-000000000046}
!define IID_IShellLinkA {000214EE-0000-0000-C000-000000000046}
!define IID_IShellLinkW {000214F9-0000-0000-C000-000000000046}
!define IShellLinkDataList {45e2b4ae-b1c3-11d0-b92f-00a0c90312e1}
	!ifdef NSIS_UNICODE
	!define IID_IShellLink ${IID_IShellLinkW}
	!else
	!define IID_IShellLink ${IID_IShellLinkA}
	!endif
!endif

Function ShellLinkSetRunAs
System::Store S
pop $9
System::Call "ole32::CoCreateInstance(g'${CLSID_ShellLink}',i0,i1,g'${IID_IShellLink}',*i.r1)i.r0"
${If} $0 = 0
	System::Call "$1->0(g'${IPersistFile}',*i.r2)i.r0" ;QI
	${If} $0 = 0
		System::Call "$2->5(w '$9',i 0)i.r0" ;Load
		${If} $0 = 0
			System::Call "$1->0(g'${IShellLinkDataList}',*i.r3)i.r0" ;QI
			${If} $0 = 0
				System::Call "$3->6(*i.r4)i.r0" ;GetFlags
				${If} $0 = 0
					System::Call "$3->7(i $4|0x2000)i.r0" ;SetFlags ;SLDF_RUNAS_USER
					${If} $0 = 0
						System::Call "$2->6(w '$9',i1)i.r0" ;Save
					${EndIf}
				${EndIf}
				System::Call "$3->2()" ;Release
			${EndIf}
		System::Call "$2->2()" ;Release
		${EndIf}
	${EndIf}
	System::Call "$1->2()" ;Release
${EndIf}
push $0
System::Store L
FunctionEnd

!macro CreateInternetShortcut FILENAME URL ICONFILE ICONINDEX
WriteINIStr "${FILENAME}.url" "InternetShortcut" "URL" "${URL}"
WriteINIStr "${FILENAME}.url" "InternetShortcut" "IconFile" "${ICONFILE}"
WriteINIStr "${FILENAME}.url" "InternetShortcut" "IconIndex" "${ICONINDEX}"
!macroend

;--------------------------------


Function .onInit
    ;Check windows version
    ${IfNot} ${AtLeastWin7}
        MessageBox MB_OK "Windows 7 or above required"
        Quit
    ${EndIf}

    ;Set up command line for parsing
    var /GLOBAL cmdLineParams
    Push $R0
    ${GetParameters} $cmdLineParams

    ; /? param (help)
    ClearErrors
    ${GetOptions} $cmdLineParams '/?' $R0
    IfErrors +3 0
    MessageBox MB_OK "Accepted command line arguments are /nodevelopment, /testsuite and /silent!"
    Abort

    Pop $R0

    ; Initialise options
    Var /GLOBAL option_development
    Var /GLOBAL option_testSuite

    StrCpy $option_development	  1
    StrCpy $option_testSuite	  0

    ; Parse Parameters
    Push $R0
    Call parseParameters
    Pop $R0

FunctionEnd

;--------------------------------
;General

  ;Check architecture and set default installation folder
  !if ${ARCH} == "x86"
    InstallDir "$PROGRAMFILES32\Safir SDK Core"
    !define nameBitwidth "32bit"
  !else if ${ARCH} == "x86-64"
    InstallDir "$PROGRAMFILES64\Safir SDK Core"
    !define nameBitwidth "64bit"
  !else
    !error "ARCH needs to be defined on command line to be either x86 or x86-64"
  !endif

  ;Check studio version
  !ifndef STUDIO
    !error "STUDIO needs to be defined on command line. Expected to be 2010 or 2012 etc"
  !endif

  ;Check for debugonly
  !ifdef DEBUGONLY
    !define debugonlyStr "-DebugOnly"
  !else
    !define debugonlyStr ""
  !endif

  ;Check for VERSION
  !ifdef VERSION
    !define versionStr "-${VERSION}"
  !else
    !define versionStr ""
  !endif

  ;Name and file
  Name "Safir SDK Core"
  OutFile "SafirSDKCore${versionStr}-VS${STUDIO}-${nameBitwidth}${debugonlyStr}.exe"

  !define StartMenuDir "$SMPROGRAMS\Safir SDK Core"

  ;Source directories created by build script
  !define StageDirRuntime "..\..\..\stage\Runtime\Program Files\safir-sdk-core"
  !define StageDirDevelopment "..\..\..\stage\Development\Program Files\safir-sdk-core"
  !define StageDirTest "..\..\..\stage\Test\Program Files\safir-sdk-core"


  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "Software\Safir SDK Core" ""

  ;Request application privileges for Windows Vista
  RequestExecutionLevel admin


;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "..\..\..\COPYING.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"


;--------------------------------
;Installer Sections

Section "Runtime" SecRuntime

  SectionIn RO
  SetOutPath "$INSTDIR"

  File /r "${StageDirRuntime}\*"

  SetShellVarContext all
  SetOutPath "$APPDATA\safir-sdk-core\config"
  File "${StageDirRuntime}\docs\example_configuration\*.ini"

  ;TODO start menu:
  #a readme? That mentions licensing terms?

  # Start Menu
  CreateDirectory "${StartMenuDir}"
  CreateDirectory "${StartMenuDir}\Documentation"

  !insertmacro CreateInternetShortcut "${StartMenuDir}\Documentation\Safir SDK Core Web Site" \
                                      "http://www.safirsdkcore.com/" "" "0"

  CreateShortCut "${StartMenuDir}\Documentation\GPLv3 License.lnk" \
				 "$INSTDIR\docs\LICENSE.txt" "" "" "" SW_SHOWNORMAL "" "Open Source license of Safir SDK Core"

  CreateShortCut "${StartMenuDir}\Sate.lnk" \
				 "$INSTDIR\bin\sate.exe" "" "" "" SW_SHOWNORMAL "" "Safir Application Tester"

  CreateShortCut "${StartMenuDir}\Dobexplorer.lnk" \
				 "$INSTDIR\bin\dobexplorer.exe" "" "" "" SW_SHOWNORMAL "" "Explore the Dob internals"

  CreateShortCut "${StartMenuDir}\Uninstall.lnk" \
				 "$INSTDIR\Uninstall.exe" "" "" "" SW_SHOWNORMAL "" "Uninstall Safir SDK Core"

  ;Add to PATH
  nsExec::ExecToLog '"$INSTDIR\installer_utils\pathed" "/MACHINE" "/APPEND" "$INSTDIR\bin"'
  ;Add assemblies to GAC.
  ;This only happens here if we're not installing the test suite,
  ;otherwise we wait until the assemblies from the test suite
  ;have been installed before we run gactool).
  ${If} $option_testSuite == "0"
    nsExec::ExecToLog '"$INSTDIR\installer_utils\gactool" "--install" "$INSTDIR\dotnet"'
  ${EndIf}

  ;Store installation folder
  WriteRegStr HKCU "Software\Safir SDK Core" "" $INSTDIR

  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "DisplayName" "Safir SDK Core"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "ProductID" "Safir SDK Core"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "Publisher" "Saab AB"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "DisplayVersion" "${VERSION}"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "NoRepair" 1
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "UninstallString" "$\"$INSTDIR\Uninstall.exe$\""
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "QuietUninstallString" "$\"$INSTDIR\Uninstall.exe$\" /S"

  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd


Section "Development" SecDevelopment

  SetOutPath "$INSTDIR"

  File /r "${StageDirDevelopment}\*"


  ;TODO start menu:
  #UG d
  #doxygen d

  #
  #Start menu items
  #

  #Shortcuts to config files - as admin
  CreateDirectory "${StartMenuDir}\Configuration"
  CreateShortCut "${StartMenuDir}\Configuration\Edit typesystem.ini.lnk" \
				 "notepad" "$APPDATA\safir-sdk-core\config\typesystem.ini"
  push "${StartMenuDir}\Configuration\Edit typesystem.ini.lnk"
  call ShellLinkSetRunAs
  pop $0

  CreateShortCut "${StartMenuDir}\Configuration\Edit locations.ini.lnk" \
				 "notepad" "$APPDATA\safir-sdk-core\config\locations.ini"
  push "${StartMenuDir}\Configuration\Edit locations.ini.lnk"
  call ShellLinkSetRunAs
  pop $0

  CreateShortCut "${StartMenuDir}\Configuration\Edit logging.ini.lnk" \
				 "notepad" "$APPDATA\safir-sdk-core\config\logging.ini"
  push "${StartMenuDir}\Configuration\Edit logging.ini.lnk"
  call ShellLinkSetRunAs
  pop $0

  CreateShortCut "${StartMenuDir}\Dobmake.lnk" \
				 "$INSTDIR\bin\dobmake.exe" "" "" "" SW_SHOWNORMAL "" "Build libraries from your dou files."

  CreateShortCut "${StartMenuDir}\Documentation\Doxygen.lnk" \
				 "$INSTDIR\docs\html\index.xhtml" "" "" "" SW_SHOWNORMAL "" "Doxygen API documentation."

  !insertmacro CreateInternetShortcut "${StartMenuDir}\Documentation\User's Guide" \
                                      "http://www.safirsdkcore.com/docs" "" "0"

  ;TODO Test all the startmenu stuff
  ;TODO set nice icons

SectionEnd

Section /o "Test suite" SecTest

  SetOutPath "$INSTDIR"

  File /r "${StageDirTest}\*"

  #Install to assemblies to GAC (see also above)
  nsExec::ExecToLog '"$INSTDIR\installer_utils\gactool" "--install" "$INSTDIR\dotnet"'
SectionEnd

;--------------------------------
Function parseParameters
    ; /nodevelopment
    ${GetOptions} $cmdLineParams '/nodevelopment' $R0
    IfErrors +2 0
    StrCpy $option_development 0

    ; /testsuite
    ${GetOptions} $cmdLineParams '/testsuite' $R0
    IfErrors +2 0
    StrCpy $option_testSuite 1


    ${If} $option_development == "0"
      SectionGetFlags ${SecDevelopment} $0
      IntOp $0 $0 ^ ${SF_SELECTED}
      SectionSetFlags ${SecDevelopment} $0
    ${EndIf}

    ${If} $option_testSuite == "1"
      SectionGetFlags ${SecTest} $0
      IntOp $0 $0 ^ ${SF_SELECTED}
      SectionSetFlags ${SecTest} $0
    ${EndIf}
FunctionEnd
;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecRuntime ${LANG_ENGLISH} "The Runtime parts of Safir SDK Core."
  LangString DESC_SecDevelopment ${LANG_ENGLISH} "The development parts of Safir SDK Core."
  LangString DESC_SecTest ${LANG_ENGLISH} "The Safir SDK Core test suite. You probably don't need this."

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecRuntime} $(DESC_SecRuntime)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDevelopment} $(DESC_SecDevelopment)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecTest} $(DESC_SecTest)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;remove from PATH
  nsExec::ExecToLog '"$INSTDIR\installer_utils\pathed" "/MACHINE" "/REMOVE" "$INSTDIR\bin"'

  ;remove assemblies from GAC
  nsExec::ExecToLog '"$INSTDIR\installer_utils\gactool" "--uninstall" "$INSTDIR\dotnet"'

  ; We blindly remove everything from the installation dir. This might blow up if someone decides to
  ; install to a path with other stuff in it, e.g. C:\ (yes, we'd try to remove everything from c:\...).
  ;Also, we try twice, in case f-ing windows doesn't let us delete the files the first time round...
  ; TODO: better way?
  RMDir /r "$INSTDIR"
  IfErrors 0 +2
  RMDir /r "$INSTDIR"

  SetShellVarContext all
  RMDir /r "$APPDATA\safir-sdk-core"

  #Remove start menu stuff
  RMDir /r "${StartMenuDir}"

  DeleteRegKey HKCU "Software\Safir SDK Core"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core"

SectionEnd
