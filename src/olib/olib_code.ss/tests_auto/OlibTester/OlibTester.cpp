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
    std::wstring DatabaseLogin = L"DSN=SafirDb;PWD=olibtesteruser;UID=olibtesteruser;";

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


    //while (bContinue)
    //{
    //    try
    //    {
    //        std::wcout.flush();
    //        std::wcout <<std::endl;
    //        std::wcout<<"----- Main menu -------"<<std::endl;
    //        std::wcout<<"1. Connect to RDBMS"<<std::endl;
    //        std::wcout<<"2. Disconnect from RDBMS"<<std::endl;
    //        std::wcout<<"3. Read all"<<std::endl;
    //        std::wcout<<"4. Create"<<std::endl;
    //        std::wcout<<"5. Update"<<std::endl;
    //        std::wcout<<"6. Delete"<<std::endl;
    //        std::wcout<<"7. Check timeout values. Integer readall statement and connection attributes"<<std::endl;
    //        std::wcout<<"8. Prepare and allocate statements"<<std::endl;
    //        std::wcout<<"9. Free statements"<<std::endl;
    //        std::wcout<<"10. Set readall statement timeout. Integer statement attribute" << std::endl;
    //        std::wcout<<"11. Set connection timeout. Integer connection attribute" << std::endl;
    //        std::wcout<<"12. Check connection pooling. Integer environment attribute" << std::endl;
    //        std::wcout<<"13. Set connection pooling. Integer environment attribute" << std::endl;
    //        std::wcout<<"14. Test output parameters"<<std::endl;
    //        std::wcout<<"15. Test input output parameters"<<std::endl;
    //        //std::wcout<<"16. Performance test"<<std::endl;
    //        std::wcout<<"17. Use Olib_test"<<std::endl;
    //        std::wcout<<"18. Test binary write"<<std::endl;
    //        std::wcout<<"19. Write NClobs"<<std::endl;
    //        std::wcout<<"20. Read NClobs"<<std::endl;
    //        std::wcout<<"21. Write Blob"<<std::endl;
    //        std::wcout<<"22. Read Blob"<<std::endl;
    //        std::wcout<<"23. Test binary read"<<std::endl;
    //        std::wcout<<"24. Lots of input"<<std::endl;
    //        std::wcout<<"25. Long time query"<<std::endl;
    //        std::wcout<<"26. Insert into 42"<<std::endl;
    //        std::wcout<<"99. Quit"<<std::endl;
    //        std::wcout<<"Choose: ";
    //        std::wcout.flush();
    //        std::getline( std::wcin, aString );
    //        choice = boost::lexical_cast<int>( aString.c_str() );

    //        switch (choice)
    //        {
    //        case 99: 
    //            bContinue = false;
    //            break;
    //        case 1: 
    //            unitAccess.Connect();
    //            break;
    //        case 2: 
    //            unitAccess.Disconnect();
    //            break;
    //        case 3:
    //            unitAccess.ReadAllUnits();
    //            break;
    //        case 4:
    //            unitAccess.CreateUnit();
    //            break;
    //        case 5:
    //            unitAccess.UpdateUnit();
    //            break;
    //        case 6:
    //            unitAccess.DeleteUnit();
    //            break;
    //        case 7:
    //            std::wcout<<"ReadAll timeout is " << unitAccess.GetReadAllTimeout() << " seconds" << std::endl;
    //            std::wcout<<"Connection timeout is " << unitAccess.GetConnectionTimeout() << " seconds" << std::endl;
    //            break;
    //        case 8: 
    //            unitAccess.AllocStmt();
    //            break;
    //        case 9: 
    //            unitAccess.CloseStmt();
    //            break;
    //        case 10: 
    //            unitAccess.SetReadAllTimeout();
    //            break;
    //        case 11: 
    //            unitAccess.SetConnectionTimeout();
    //            break;
    //        case 12: 
    //            unitAccess.GetConnectioning();
    //            break;
    //        case 13: 
    //            unitAccess.SetConnectionPooling();
    //            break;
    //        case 14: 
    //            unitAccess.TestOutputParameters();
    //            break;
    //        case 15: 
    //            unitAccess.TestInputOutputParameters();
    //            break;
    //        case 16: 
    //            unitAccess.PerfTest();
    //            break;
    //        case 17: 
    //            unitAccess.UseOlibTest();
    //            break;
    //        case 18: 
    //            unitAccess.BinaryTestWrite();
    //            break;              
    //        case 19: 
    //            unitAccess.WriteNClobs();
    //            break;              
    //        case 20: 
    //            unitAccess.ReadNClobs();
    //            break;
    //        case 21: 
    //            unitAccess.WriteBlob();
    //            break;              
    //        case 22: 
    //            unitAccess.ReadBlob();
    //            break;
    //        case 23: 
    //            unitAccess.BinaryTestRead();
    //            break;     
    //        case 24: 
    //            unitAccess.LotsOfInput();
    //            break;     
    //        case 25: 
    //            unitAccess.LongTimeQuery();
    //            break;     
    //        case 26: 
    //            unitAccess.InsertInto42();
    //            break;     
    //        }
    //    }
    //    catch(const Safir::Databases::Odbc::ReconnectException & ex)
    //    {
    //        std::wcout 
    //            << "OlibTest main " 
    //            << ex.GetExceptionInfo() 
    //            << std::endl;
    //    }
    //    catch(const Safir::Dob::Typesystem::SoftwareViolationException & ex)
    //    {               
    //        std::wcout << "Programming exception " << ex.GetExceptionInfo().c_str()  << std::endl;
    //    }
    //}
    
    return 0;
}
