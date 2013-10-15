/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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
#ifndef __DOBEXPLORER_COMMON_HEADER_H__
#define __DOBEXPLORER_COMMON_HEADER_H__

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#endif

//need to do this to allow for both qt4 and qt5
#include <QtGui>
#if QT_VERSION >= 0x050000
#  include <QtWidgets>
#endif

#ifdef _MSC_VER
#pragma warning(pop)

#pragma warning (disable: 4355)
#endif

//Define a dummy class to avoid "moc" warning about this file not containing any QObjects. 
//The cmake setup currently runs moc on all .h files.
class Dummy 
    : public QObject
{
    Q_OBJECT
};

#endif
