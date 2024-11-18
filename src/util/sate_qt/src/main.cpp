/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
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

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#if defined (SAFIR_LINK_QT_STATICALLY) && defined (_MSC_VER)
#include <QtPlugin>
Q_IMPORT_PLUGIN(QWindowsIntegrationPlugin);

#  if (QT_VERSION >= QT_VERSION_CHECK(6, 7, 0))
Q_IMPORT_PLUGIN(QModernWindowsStylePlugin);
#  else
Q_IMPORT_PLUGIN(QWindowsVistaStylePlugin);
#  endif
#endif

#include <QFile>
#include <QTextStream>

#include "satemainwindow.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    SateMainWindow w;
    w.show();


    return a.exec();
}
