;Safir SDK Core installer script
;Written by Lars Hagström

;--------------------------------
;Include Modern UI

  !include "MUI2.nsh"

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
    !define debugonly "-DebugOnly"
  !endif

  ;Name and file
  Name "Safir SDK Core"
  OutFile "SafirSDKCore-VS$STUDIO-$nameBitwidth$debugonly.exe"

  ;Source directories created by build script
  !define StageDirRuntime "..\..\..\stage\Runtime\Program Files\safir_sdk_core"
  !define StageDirDevelopment "..\..\..\stage\Development\Program Files\safir_sdk_core"
  !define StageDirTest "..\..\..\stage\Test\Program Files\safir_sdk_core"


  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "Software\Safir SDK Core" ""

  ;Request application privileges for Windows Vista
  RequestExecutionLevel admin

  SetCompressor /SOLID lzma
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

  ;Store installation folder
  WriteRegStr HKCU "Software\Safir SDK Core" "" $INSTDIR

  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd


Section "Development" SecDevelopment

  SetOutPath "$INSTDIR"

  File /r "${StageDirDevelopment}\*"

SectionEnd

Section /o "Test suite" SecTest

  SetOutPath "$INSTDIR"

  File /r "${StageDirTest}\*"

SectionEnd
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

  Delete "$INSTDIR\Uninstall.exe"

  RMDir /r "$INSTDIR"

  DeleteRegKey /ifempty HKCU "Software\Safir SDK Core"

SectionEnd
