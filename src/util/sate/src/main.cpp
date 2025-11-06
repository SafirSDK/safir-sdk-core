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

#include "satemainwindow.h"
#include "scriptcli.h"
#include <QCommandLineParser>

int main(int argc, char *argv[])
{
    //Set a default-english locale so we do not have to worry about decimal separators etc
    QLocale::setDefault(QLocale::c());

    // Parse command line arguments first to determine mode
    QCommandLineParser parser;
    parser.setApplicationDescription("Safir Application Testing Environment (SATE)");
    parser.addHelpOption();
    parser.addVersionOption();
    parser.addOption({{"s", "script"}, "Run in script mode without GUI", "file"});
    parser.addOption({{"c", "connect"}, "Automatically open DOB connection before script execution. If no name is provided, defaults to 'SATE_CLI'", "name"});
    parser.addOption({{"w", "websocket"}, "WebSocket connection string", "url"});
    
    // We need to parse before creating the application to determine which app type to create
    // Use a temporary parser for early detection
    QStringList args;
    for (int i = 0; i < argc; ++i)
    {
        args.append(QString::fromLocal8Bit(argv[i]));
    }
    
    bool scriptMode = args.contains("--script") || args.contains("-s");

    if (scriptMode)
    {
        // Use QCoreApplication for non-GUI mode
        QCoreApplication app(argc, argv);
        parser.process(app);
        
        QString scriptFile = parser.value("script");
        QString connectionName;
        QString websocketUrl;
        
        // Check if --connect option is set
        if (parser.isSet("connect"))
        {
            connectionName = parser.value("connect");
            // If --connect is provided without a value (empty string), use default
            if (connectionName.isEmpty())
            {
                connectionName = "SATE_CLI";
            }
        }
        
        // Check if --websocket option is set
        if (parser.isSet("websocket"))
        {
            websocketUrl = parser.value("websocket");
        }
        
        // Create and execute script CLI
        ScriptCli scriptCli(&app, scriptFile, connectionName, websocketUrl);
        return scriptCli.Execute();
    }
    else
    {
        // Use QApplication for GUI mode
        QApplication app(argc, argv);
        parser.process(app);
        
        SateMainWindow w;
        w.show();
        return app.exec();
    }
}
