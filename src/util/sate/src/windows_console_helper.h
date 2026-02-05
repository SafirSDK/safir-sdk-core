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
#pragma once

#ifdef _WIN32
#include <windows.h>


// RAII guard to perform end-of-process cleanup:
// - Emits a trailing newline when writing to a real console.
// - If we attached to the parent console, injects an Enter key to repaint the prompt and frees the console.
class WindowsConsoleHelper
{
public:
    explicit WindowsConsoleHelper();
    ~WindowsConsoleHelper();

    WindowsConsoleHelper(const WindowsConsoleHelper&) = delete;
    WindowsConsoleHelper& operator=(const WindowsConsoleHelper&) = delete;

private:
    bool attached_;
    bool consoleOutput_;
};
#endif
