/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Jörgen Johansson / stjrjo
* Modified by: Amin Allalou 2012
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
#include <iostream>
#include "Olib_unit_access.h"
#include <Safir/Databases/Odbc/ReconnectException.h>
#include <Safir/Databases/Odbc/TimeoutException.h>
 
using namespace std;
int main(int argc,char* argv[])
{
    DbUnitAccess unitAccess;
    std::wstring DatabaseLogin = L"DSN=SafirDb;PWD=sysadm;UID=sysadm;";

    if (argc > 1)
    {
        try
        {
            if (argv[1] == std::string("-connect"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-disconnect"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-connectiontimeout"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.SetConnectionTimeout();
                unitAccess.GetConnectionTimeout();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-allocclosestm"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.AllocStmt();
                unitAccess.CloseStmt();
                unitAccess.Disconnect();

            }
            else if (argv[1] == std::string("-connectionpooling"))
            {
                unitAccess.SetConnectionPooling();
                unitAccess.Connect(DatabaseLogin);
                unitAccess.GetConnectionPooling();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-readalltimeout"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.SetReadAllTimeout();
                unitAccess.GetReadAllTimeout();
                unitAccess.Disconnect();

            }
            else if (argv[1] == std::string("-cleartable"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.ClearTables();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-createunit"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.CreateUnit();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-readunit"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.ReadUnit(0);
                unitAccess.EvaluateOutData();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-updateunit"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.UpdateUnit();
                unitAccess.ReadUnit(0);
                unitAccess.EvaluateOutData();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-insertinto42"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.InsertInto42();
                unitAccess.ReadUnit(42);
                unitAccess.EvaluateOutData();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-deleteunit"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.DeleteUnit(42);
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-binaryrw"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.BinaryTestWrite();
                unitAccess.BinaryTestRead();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-blobrw"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.WriteBlob();
                unitAccess.ReadBlob();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-nclobrw"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.WriteNClobs();
                unitAccess.ReadNClobs();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-lotsofinput"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.LotsOfInput();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-outparam"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.TestOutputParameters();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-inoutparam"))
            {
                unitAccess.Connect(DatabaseLogin);
                unitAccess.TestInputOutputParameters();
                unitAccess.Disconnect();
            }
            else if (argv[1] == std::string("-perftest"))
            { //Check what the purpos of this is
                unitAccess.Connect(DatabaseLogin);
                unitAccess.PerfTest();
                unitAccess.Disconnect();
            }
            else
            {
                std::wcout<<"Unknown input paramter"<<std::endl;
                return 1;
            }

            //unitAccess.CloseStmt();

        }
        catch(const Safir::Databases::Odbc::ReconnectException & ex)
        {
            std::wcout
                    << "OlibTest main "
                    << ex.GetExceptionInfo()
                    << std::endl;
            return 1;
        }
        catch(const Safir::Databases::Odbc::TimeoutException & ex)
        {
            std::wcout
                    << "OlibTest main "
                    << ex.GetExceptionInfo()
                    << std::endl;
            return 1;
        }
        catch(const Safir::Dob::Typesystem::SoftwareViolationException & ex)
        {
            std::wcout << "Programming exception " << ex.GetExceptionInfo().c_str()  << std::endl;
            return 1;
        }
        catch(const Safir::Dob::Typesystem::IllegalValueException &e)
        {
            std::wcout<<"IllegalValueException:" <<e.GetExceptionInfo().c_str() <<std::endl;
            return 1;
        }
        catch(...)
        {
            std::wcout <<"ERROR"<<std::endl;
            return 1;
        }
    }
    else
    {
        std::wcout<<"No input paramter."<<std::endl;
        return 1;
    }

    return 0;
}
