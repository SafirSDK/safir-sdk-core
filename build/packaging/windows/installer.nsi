﻿;Safir SDK Core installer script
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

  ;Name and file
  Name "Safir SDK Core"
  OutFile "SafirSDKCore-VS${STUDIO}-${nameBitwidth}${debugonlyStr}.exe"

  ;Source directories created by build script
  !define StageDirRuntime "..\..\..\stage\Runtime\Program Files\safir_sdk_core"
  !define StageDirDevelopment "..\..\..\stage\Development\Program Files\safir_sdk_core"
  !define StageDirTest "..\..\..\stage\Test\Program Files\safir_sdk_core"


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
  SetOutPath "$APPDATA\safir_sdk_core\config"
  File "${StageDirRuntime}\docs\example_configuration\*.ini"

  ;TODO start menu (config links with notepad, dobmake, docs, etc)

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
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "NoRepair" 1
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "UninstallString" "$\"$INSTDIR\Uninstall.exe$\""
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core" \
                   "QuietUninstallString" "$\"$INSTDIR\Uninstall.exe$\" /S"

  ;TODO: show version!
   
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
  RMDir /r "$APPDATA\safir_sdk_core"

  DeleteRegKey HKCU "Software\Safir SDK Core"
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Safir SDK Core"

SectionEnd