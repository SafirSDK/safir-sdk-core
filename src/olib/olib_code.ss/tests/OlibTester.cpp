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
    DbOlibTest dbOlibTest;
    std::wstring DatabaseLogin = L"DSN=SafirDb;PWD=olibtesteruser;UID=olibtesteruser;";

    if (argc > 1)
    {
        try
        {
            if (argv[1] == std::string("-connect"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-disconnect"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-connectiontimeout"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.SetConnectionTimeout();
                dbOlibTest.GetConnectionTimeout();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-allocclosestm"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.AllocStmt();
                dbOlibTest.CloseStmt();
                dbOlibTest.Disconnect();

            }
            else if (argv[1] == std::string("-connectionpooling"))
            {
                dbOlibTest.SetConnectionPooling();
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.GetConnectionPooling();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-readalltimeout"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.SetReadAllTimeout();
                dbOlibTest.GetReadAllTimeout();
                dbOlibTest.Disconnect();

            }
            else if (argv[1] == std::string("-cleartable"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.ClearTables();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-createdata"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.CreateData();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-readdata"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.ReadData(0);
                dbOlibTest.EvaluateOutData();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-updatedata"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.UpdateData();
                dbOlibTest.ReadData(0);
                dbOlibTest.EvaluateOutData();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-insertinto42"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.InsertInto42();
                dbOlibTest.ReadData(42);
                dbOlibTest.EvaluateOutData();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-deletedata"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.DeleteData(42);
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-binaryrw"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.BinaryTestWrite();
                dbOlibTest.BinaryTestRead();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-blobrw"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.WriteBlob();
                dbOlibTest.ReadBlob();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-nclobrw"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.WriteNClobs();
                dbOlibTest.ReadNClobs();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-lotsofinput"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.LotsOfInput();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-outparam"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.TestOutputParameters();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-inoutparam"))
            {
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.TestInputOutputParameters();
                dbOlibTest.Disconnect();
            }
            else if (argv[1] == std::string("-perftest"))
            { //Check what the purpos of this is
                dbOlibTest.Connect(DatabaseLogin);
                dbOlibTest.PerfTest();
                dbOlibTest.Disconnect();
            }
            else
            {
                std::wcout<<"Unknown input paramter"<<std::endl;
                return 1;
            }



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
