/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#pragma warning(push)
#pragma warning (disable: 4251)
#pragma warning (disable: 4275)
#pragma warning (disable: 4100)
#endif

#include <boost/program_options.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif


#include "OdbcHelper.h"
#include <Safir/Dob/Typesystem/Utilities.h>
#include <clocale>
#include <iostream>


std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}

class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false)
    {
        using namespace boost::program_options;
        options_description general("General Options");
        general.add_options()
            ("help,h", "show help message")
            ("statement", value<std::string>(&statement), "Statement to execute")
            ("connection-string", value<std::string>(&connectionString), "Connection string to use for db connection");

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
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(general);
            return;
        }

        if (statement.empty() || connectionString.empty())
        {
            std::wcout << "Need a statement and a connection string" << std::endl;
            ShowHelp(general);
            return;
        }

        parseOk = true;
    }

    bool parseOk;
    std::string statement;
    std::string connectionString;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << "Execute a statement and show the number of lines in the result.\n\n"
                   << desc << std::endl;
    }

};

void RunStatement(const ProgramOptions& options)
{
    SQLHENV environment;
    SQLHDBC connection;
    OdbcHelper helper;

    SQLRETURN ret = ::SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &environment);
    if (!SQL_SUCCEEDED(ret))
    {
        helper.ThrowException(SQL_HANDLE_ENV, SQL_NULL_HANDLE);
    }

    ret = ::SQLSetEnvAttr(environment,
                            SQL_ATTR_ODBC_VERSION,
                            reinterpret_cast<SQLPOINTER>(SQL_OV_ODBC3),
                            SQL_IS_UINTEGER);
    if (!SQL_SUCCEEDED(ret))
    {
        helper.ThrowException(SQL_HANDLE_ENV, environment);
    }

    ret = ::SQLAllocHandle(SQL_HANDLE_DBC, environment, &connection);
    if (!SQL_SUCCEEDED(ret))
    {
        helper.ThrowException(SQL_HANDLE_ENV, environment);
    }

    helper.Connect(connection, options.connectionString);

    SQLHSTMT statement;
    helper.AllocStatement(&statement, connection);

    helper.Prepare(statement, options.statement);
    helper.Execute(statement);

    int rows = 0;
    SQLSMALLINT columns = 0;

    ret = ::SQLNumResultCols(statement, &columns);
    if (!SQL_SUCCEEDED(ret))
    {
        helper.ThrowException(SQL_HANDLE_STMT,statement);
    }

    if (columns != 0)
    {
        while(helper.Fetch(statement))
        {
            ++rows;
        }
    }
    std::wcout << "Got " << rows << " rows." << std::endl;
}


int main(int argc, char * argv[])
{
    //Mimer requires locale to be set like this for character conversions
    //to work properly. Hopefully this does not adversely affect other
    //databases.
    std::setlocale(LC_CTYPE, "");

    try
    {
        const ProgramOptions options(argc,argv);

        if (!options.parseOk)
        {
            return 1;
        }
        RunStatement(options);
    }
    catch (const std::exception & exc)
    {
        std::wcout << "Caught exception: " << exc.what() << std::endl;
        return 1;
    }
    return 0;
}
