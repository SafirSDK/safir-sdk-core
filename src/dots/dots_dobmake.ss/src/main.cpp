/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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
#include <QMessageBox>

#ifdef _MSC_VER
#pragma warning(pop)
#endif


int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    if (!Dobmake::CheckPython())
    {
        QMessageBox::critical(NULL, "Python not found!",
                              "The python executable could not be found.\nMake sure you have python installed and in your PATH.");
        return 1;
    }
    if (Dobmake::GetDobmakeBatchScript().isEmpty())
    {
        QMessageBox::critical(NULL, "dobmake-batch not found!",
                              "The dobmake-batch script could not be found.\nMake sure you have it in your PATH.");
        return 1;
    }
    try
    {
        Dobmake w;
        w.show();

        return a.exec();
    }
    catch(const std::exception& e)
    {
        QMessageBox::critical(NULL, "Critical error in dobmake!",
                              QString("Dobmake failed completely!\n") + e.what());
        return 1;
    }
}
