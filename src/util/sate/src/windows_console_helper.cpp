/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include "windows_console_helper.h"

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#include <cstdio>
#include <iostream>

namespace
{
    void BindStdHandleToCrt(DWORD which, FILE* stream)
    {
        HANDLE h = GetStdHandle(which);
        if (!h || h == INVALID_HANDLE_VALUE) return;

        int fd = _open_osfhandle(reinterpret_cast<intptr_t>(h), _O_TEXT);
        if (fd != -1) {
            _dup2(fd, _fileno(stream));
            setvbuf(stream, nullptr, _IONBF, 0);
        }
    }
}

WindowsConsoleHelper::WindowsConsoleHelper()
    : attached_(false)
    , consoleOutput_(false)
{
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    HANDLE hErr = GetStdHandle(STD_ERROR_HANDLE);
    const bool haveOut = (hOut != nullptr && hOut != INVALID_HANDLE_VALUE);
    const bool haveErr = (hErr != nullptr && hErr != INVALID_HANDLE_VALUE);

    if (haveOut) {
        BindStdHandleToCrt(STD_OUTPUT_HANDLE, stdout);
    }
    if (haveErr) {
        BindStdHandleToCrt(STD_ERROR_HANDLE, stderr);
    }

    if (haveOut || haveErr) {
        std::ios::sync_with_stdio(true);
        // Only relevant for real console (not pipes/files)
        const DWORD outType = haveOut ? GetFileType(hOut) : FILE_TYPE_UNKNOWN;
        const DWORD errType = haveErr ? GetFileType(hErr) : FILE_TYPE_UNKNOWN;
        if (outType == FILE_TYPE_CHAR || errType == FILE_TYPE_CHAR) {
            SetConsoleOutputCP(CP_UTF8);
            consoleOutput_ = true;
        }
        return;
    }

    // No valid std handles: typical GUI-subsystem launched from cmd.exe
    if (AttachConsole(ATTACH_PARENT_PROCESS)) {
        FILE* f = nullptr;
        freopen_s(&f, "CONOUT$", "w", stdout);
        freopen_s(&f, "CONOUT$", "w", stderr);
        freopen_s(&f, "CONIN$",  "r", stdin);
        std::ios::sync_with_stdio(true);
        SetConsoleOutputCP(CP_UTF8);
        attached_ = true;
        consoleOutput_ = true;
    }
}


WindowsConsoleHelper::~WindowsConsoleHelper()
{
    if (consoleOutput_) {
        fputc('\n', stdout);
        fflush(stdout);
        fflush(stderr);
    }
    if (attached_) {
        // Inject an Enter key into the console input buffer to ensure the shell
        // redraws the prompt immediately (mirrors the user pressing Return).
        HANDLE hStdIn = GetStdHandle(STD_INPUT_HANDLE);
        if (hStdIn != nullptr && hStdIn != INVALID_HANDLE_VALUE) {
            INPUT_RECORD ir[2] = {};
            WORD scan = static_cast<WORD>(MapVirtualKey(VK_RETURN, MAPVK_VK_TO_VSC));

            ir[0].EventType = KEY_EVENT;
            ir[0].Event.KeyEvent.bKeyDown = TRUE;
            ir[0].Event.KeyEvent.wRepeatCount = 1;
            ir[0].Event.KeyEvent.wVirtualKeyCode = VK_RETURN;
            ir[0].Event.KeyEvent.wVirtualScanCode = scan;
            ir[0].Event.KeyEvent.uChar.UnicodeChar = L'\r';
            ir[0].Event.KeyEvent.dwControlKeyState = 0;

            ir[1] = ir[0];
            ir[1].Event.KeyEvent.bKeyDown = FALSE;

            DWORD written = 0;
            WriteConsoleInput(hStdIn, ir, 2, &written);
        }

        FreeConsole();
    }
}
#endif
