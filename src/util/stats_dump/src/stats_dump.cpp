/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
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
#include "../../../dose/dose_dobexplorer.ss/src/ConnectionStatisticsCollector.h"
#include <iostream>
#include <fstream>
#include <Safir/Dob/Internal/Initialize.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/ThisNodeParameters.h>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4100)
  #pragma warning (disable : 4267)
#endif

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

std::wostream& operator<<(std::wostream& out, const std::string& str)
{
    return out << str.c_str();
}

std::pair<int, int> GetReqInQAccumulated(const std::vector<ConnectionStatisticsCollector::ReqQStat>& reqInQStat)
{
    int recv = 0;
    int overflows = 0;
    for (const auto& v : reqInQStat)
    {
        recv += v.noPushedRequests;
        overflows += v.noOverflows;
    }
    return std::make_pair(recv, overflows);
}

std::pair<int, int> GetMsgInQAccumulated(const std::vector<ConnectionStatisticsCollector::MsgQStat>& msgQStat)
{
    int recv = 0;
    int overflows = 0;
    for (const auto& v : msgQStat)
    {
        recv += v.noPushedMsg;
        overflows += v.noOverflows;
    }
    return std::make_pair(recv, overflows);
}

std::string ShortConnectionName(const std::string& connectionName)
{
    return std::string(std::find(std::find(connectionName.begin(), connectionName.end(), ';') + 1, connectionName.end(), ';') + 1, connectionName.end());
}

class CmdLine
{
public:
    CmdLine(int argc, char * argv[])
    {
        try
        {
            boost::program_options::options_description desc("Command line options");
            desc.add_options()
                    ("help,h", "Produce help message")
                    ("out,o", boost::program_options::value<std::string>()->required(), "Output csv file. If it already exists it will be overwritten");

            boost::program_options::variables_map vm;
            boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
            if (vm.count("help"))
            {
                std::cout<<desc<<std::endl;
                exit(0);
            }
            boost::program_options::notify(vm); // must occurre after dealt with help, otherwize required options will cause error

            filePath=vm["out"].as<std::string>();
        }
        catch (const std::exception& e)
        {
            std::cout << e.what() << std::endl;
        }
    }

    std::string filePath;
};


int main(int argc, char * argv[])
{
    CmdLine cmd(argc, argv);

    Safir::Dob::Internal::InitializeDoseInternalFromApp();

    // Read all connection statistics from shared memory
    std::vector<ConnectionStatisticsCollector::Stat> connectionStatistics;
    Safir::Dob::Internal::Connections::Instance().ForEachConnectionPtr([&connectionStatistics](const Safir::Dob::Internal::ConnectionPtr& con)
    {
        if (con->IsLocal())
        {
            connectionStatistics.emplace_back();
            auto& stat = connectionStatistics.back();
            ConnectionStatisticsCollector::GetStatistics(con, stat);
        }
    });

    // Create csv-file
    std::ofstream os(cmd.filePath, std::ios::out | std::ios::trunc);
    if (!os.is_open())
    {
        std::cout << "Failed to open file for output: " << cmd.filePath << std::endl;
        return 1;
    }

    // Write column headers
    os << "Node,Connection,#SendReq,#Overflow,#Timeout,#HandledReq,#Overflow,#SentMsg,#Overflow,#RecvMsg,#Overflow" << std::endl;

    // Write data
    auto node = Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Dob::ThisNodeParameters::Name());
    for (const auto& s : connectionStatistics)
    {
        auto reqInQ = GetReqInQAccumulated(s.reqInQStat);
        auto msgInQ = GetMsgInQAccumulated(s.msgInQStat);
        os  << node << ","
            << ShortConnectionName(s.connectionName) << ","
            << s.reqOutQStat.noPushedRequests << ","
            << s.reqOutQStat.noOverflows << ","
            << s.reqOutQStat.noTimeouts << ","
            << reqInQ.first << ","
            << reqInQ.second << ","
            << s.msgOutQStat.noPushedMsg << ","
            << s.msgOutQStat.noOverflows << ","
            << msgInQ.first << ","
            << msgInQ.second << std::endl;
    }

    os.close();

    std::cout << "Successfully created file: " << cmd.filePath << std::endl;
    return 0;
}
