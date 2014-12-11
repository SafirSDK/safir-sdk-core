/******************************************************************************
*
* Copyright Consoden AB, 2014 (http://www.consoden.se)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include "dobmake.h"
#include <iostream>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4251)
#endif

#include <QApplication>
#include <QProcess>
#include <QMessageBox>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

bool CheckPython()
{
    QProcess p;
    QStringList params;

    params << "--version";
    p.start("python", params);
    p.waitForFinished(-1);
    return p.error() == QProcess::UnknownError && p.exitStatus() == QProcess::NormalExit && p.exitCode() == 0;
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    if (!CheckPython())
    {
        QMessageBox::critical(NULL, "Python not found!",
                              "The python executable could not be found.\nMake sure you have python installed and in your PATH.");
        return 1;
    }
    Dobmake w;
    w.show();

    return a.exec();
}
