/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
#include "DbOlibTest.h"
#include <Safir/Databases/Odbc/ReconnectException.h>
#include <Safir/Databases/Odbc/TimeoutException.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable:4100)
#endif

#include <boost/program_options.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace 
{
    std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
    {
        std::ostringstream ostr;
        ostr << opt;
        return out << ostr.str().c_str();
    }


    const std::string createConnectionString(const std::string& driver,
                                             const std::string& hostname,
                                             const std::string& database)
    {
        std::string result;
        if (driver == "mimer")
        {
            result += "Driver={";
            const char* const env = getenv("MIMER_ODBC_DRIVER_NAME");
            if (env != NULL)
            {
                result += env;
            }
            else
            {
                result += "MIMER";
            }
            result += "};Protocol=tcp;Node=";
        }
        else if (driver == "mysql")
        {
            result += "DRIVER={";
            const char* const env = getenv("MYSQL_ODBC_DRIVER_NAME");
            if (env != NULL)
            {
                result += env;
            }
            else
            {
                result += "MYSQL";
            }
            result += "};Server=";
        }
        
        result += hostname + ";";
        result += "Database=" + database + ";";
        result += "Uid=olibuser;Pwd=olibuser";
        return result;
    }
}

 
using namespace std;
int main(int argc,char* argv[])
{
    std::string testcase;
    std::string driver;
    std::string hostname;
    std::string database;

    using namespace boost::program_options;
    options_description general("Options");
    general.add_options()
        ("help,h", "show help message")
        ("testcase", value<std::string>(&testcase), "the testcase to run, see source code")
        ("driver", value<std::string>(&driver), "choice of mimer, mysql and postgres")
        ("hostname", value<std::string>(&hostname)->default_value("localhost"), "hostname of the database server")
        ("database", value<std::string>(&database), "Database name");
    variables_map vm;

    try
    {
        store(command_line_parser(argc, argv).
              options(general).run(), vm);
        notify(vm);
    }
    catch (const std::exception& exc)
    {
        std::wcout << "Error parsing command line: " << exc.what() << "\n" << std::endl;
        return 1;
    }

    if (vm.count("help"))
    {
        std::wcout << general << std::endl;
        return 1;
    }

    if (vm.count("testcase") != 1)
    {
        std::wcout << "Need testcase argument" << std::endl;
        return 1;
    }

    if (vm.count("driver") != 1)
    {
        std::wcout << "Need driver argument" << std::endl;
        return 1;        
    }

    if (vm.count("database") != 1)
    {
        std::wcout << "Need database argument" << std::endl;
        return 1;        
    }

    const std::string connectionString = createConnectionString(driver,hostname,database);
    const std::wstring wconnectionString(connectionString.begin(), connectionString.end());
    std::wcout << "Using Connection String '" << connectionString.c_str() << "'" << std::endl;

    const bool curlyBracesNeeded = driver == "mimer" || driver == "postgres";
    
    DbOlibTest dbOlibTest;

    try
    {
        if (testcase == "connect")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.Disconnect();
        }
        else if (testcase == "disconnect")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.Disconnect();
        }
        else if (testcase == "connectiontimeout")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.SetConnectionTimeout();
            dbOlibTest.GetConnectionTimeout();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "allocclosestm")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.AllocStmt();
            dbOlibTest.CloseStmt();
            dbOlibTest.Disconnect();

        }
        else if (testcase == "connectionpooling")
        {
            dbOlibTest.SetConnectionPooling();
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.GetConnectionPooling();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "readalltimeout")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.SetReadAllTimeout();
            dbOlibTest.GetReadAllTimeout();
            dbOlibTest.Disconnect();

        }
        else if (testcase == "cleartable")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.ClearTables();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "createdata")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.CreateData();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "readdata")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.ReadData(0);
            dbOlibTest.EvaluateOutData();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "updatedata")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.UpdateData();
            dbOlibTest.ReadData(0);
            dbOlibTest.EvaluateOutData();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "insertinto42")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.InsertInto42();
            dbOlibTest.ReadData(42);
            dbOlibTest.EvaluateOutData();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "deletedata")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.DeleteData(42);
            dbOlibTest.Disconnect();
        }
        else if (testcase == "binaryrw")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.BinaryTestWrite();
            dbOlibTest.BinaryTestRead();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "blobrw")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.WriteBlob();
            dbOlibTest.ReadBlob();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "nclobrw")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.WriteNClobs();
            dbOlibTest.ReadNClobs();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "lotsofinput")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.LotsOfInput();
            dbOlibTest.Disconnect();
        }
        else if (testcase == "outparam")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.TestOutputParameters(curlyBracesNeeded);
            dbOlibTest.Disconnect();
        }
        else if (testcase == "inoutparam")
        {
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.TestInputOutputParameters(curlyBracesNeeded);
            dbOlibTest.Disconnect();
        }
        else if (testcase == "perftest")
        { //Check what the purpos of this is
            dbOlibTest.Connect(wconnectionString);
            dbOlibTest.PerfTest();
            dbOlibTest.Disconnect();
        }
        else
        {
            std::wcout<<"Unknown testcase paramter"<<std::endl;
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

    return 0;
}
