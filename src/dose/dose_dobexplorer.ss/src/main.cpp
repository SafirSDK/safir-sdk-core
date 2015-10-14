/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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
#include "common_header.h"
#include "dosemon.h"
#include <Safir/Utilities/CrashReporter.h>

int main(int argc, char *argv[])
{
    Safir::Utilities::CrashReporter::Start();

    //ensure call to Stop at application exit
    boost::shared_ptr<void> guard(static_cast<void*>(0), 
                                  boost::bind(Safir::Utilities::CrashReporter::Stop));

    QApplication app(argc, argv);
    boost::shared_ptr<DoseMon> dialog (new DoseMon());

    dialog->show();

    const auto ret = app.exec();

    dialog.reset();

    return ret;


}
