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
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#pragma warning (disable: 4251)
#endif

#include <QApplication>
#include <QCoreApplication>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#if defined (SAFIR_LINK_QT_STATICALLY) && defined (_MSC_VER)
#include <QtPlugin>
Q_IMPORT_PLUGIN(QWindowsIntegrationPlugin);
Q_IMPORT_PLUGIN(QSvgIconPlugin);

#  if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
Q_IMPORT_PLUGIN(QModernWindowsStylePlugin);
#  else
Q_IMPORT_PLUGIN(QWindowsVistaStylePlugin);
#  endif
#endif

#ifdef _WIN32
#include <windows.h>
#endif

#include "satemainwindow.h"
#include "scriptcli.h"
#include <QCommandLineParser>

int CommandLineApplication(int argc, char* argv[])
{
#ifdef _WIN32
    // Try to attach to the parent console first (if started from command prompt)
    bool hasParentConsole = AttachConsole(ATTACH_PARENT_PROCESS);
    if (!hasParentConsole)
    {
        // No parent console: create a new one so output is visible when double-clicked.
        AllocConsole();
    }

    // Redirect C/C++ std streams to the console
    FILE* fDummy = nullptr;
    freopen_s(&fDummy, "CONOUT$", "w", stdout);
    freopen_s(&fDummy, "CONOUT$", "w", stderr);
    freopen_s(&fDummy, "CONIN$", "r", stdin);

    // Ensure UTF-8 output
    SetConsoleOutputCP(CP_UTF8);

    // If we attached to parent console, print a newline to move past the prompt
    if (hasParentConsole)
    {
        printf("\n");
    }
#endif

    int returnCode = 0;

    // Use QCoreApplication for console mode
    QCoreApplication app(argc, argv);

    // Parse command line arguments
    QCommandLineParser parser;
    parser.setApplicationDescription("Safir Application Testing Environment (SATE)");
    parser.addHelpOption();
    parser.addOption({ {"s", "script"}, "Run in script mode without GUI", "file" });
    parser.addOption({ {"c", "connection"}, "Set Sate connection name. If no name is provided, defaults to 'SATE_CLI'", "name" });
    parser.addOption({ {"w", "websocket"}, "WebSocket connection string", "url" });

    // Parse arguments (doesn't exit on help like process() does)
    if (!parser.parse(QCoreApplication::arguments()))
    {
        fprintf(stderr, "%s\n", qPrintable(parser.errorText()));
        returnCode = 1;
    }
    else if (parser.isSet("help"))
    {
        // Show help and exit - we handle this manually to ensure proper cleanup
        printf("%s\n", qPrintable(parser.helpText()));
        returnCode = 0;
    }
    else if (parser.isSet("script"))
    {
        QString scriptFile = parser.value("script");
        QString connectionName = "SATE_CLI";
        QString websocketUrl;

        // Check if --connection option is set
        if (parser.isSet("connection"))
        {
            connectionName = parser.value("connection");
        }

        // Check if --websocket option is set
        if (parser.isSet("websocket"))
        {
            websocketUrl = parser.value("websocket");
        }

        // Create and execute script CLI
        ScriptCli scriptCli(&app, scriptFile, connectionName, websocketUrl);
        returnCode = scriptCli.Execute();
    }
    else
    {
        fprintf(stderr, "Error: No action specified. Use --help for usage information.\n");
        returnCode = 1;
    }

#ifdef _WIN32
    printf("\n");
    fflush(stdout);

    // If we attached to parent console, we need to send input to restore prompt
    if (hasParentConsole)
    {
        // Send a newline to the console input buffer to trigger the prompt
        HANDLE hStdIn = GetStdHandle(STD_INPUT_HANDLE);
        if (hStdIn != INVALID_HANDLE_VALUE)
        {
            INPUT_RECORD ir;
            ir.EventType = KEY_EVENT;
            ir.Event.KeyEvent.bKeyDown = TRUE;
            ir.Event.KeyEvent.dwControlKeyState = 0;
            ir.Event.KeyEvent.uChar.UnicodeChar = '\r';
            ir.Event.KeyEvent.wRepeatCount = 1;
            ir.Event.KeyEvent.wVirtualKeyCode = VK_RETURN;
            ir.Event.KeyEvent.wVirtualScanCode = MapVirtualKey(VK_RETURN, MAPVK_VK_TO_VSC);

            DWORD written;
            WriteConsoleInput(hStdIn, &ir, 1, &written);

            ir.Event.KeyEvent.bKeyDown = FALSE;
            WriteConsoleInput(hStdIn, &ir, 1, &written);
        }
    }

    FreeConsole();
#endif

    return returnCode;
}

int GuiApplication(int argc, char* argv[])
{
    // Use QApplication for GUI mode
    QApplication app(argc, argv);
    SateMainWindow w;
    w.show();
    return app.exec();
}

int main(int argc, char *argv[])
{
    //Set a default-english locale so we do not have to worry about decimal separators etc
    QLocale::setDefault(QLocale::c());

    if (argc > 1)
    {
        // Run command line application
        return CommandLineApplication(argc, argv);
    }
    else
    {
        // Run GUI application
        return GuiApplication(argc, argv);
    }
}
