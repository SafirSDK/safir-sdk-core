/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#include <iostream>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include "TestCaseReader.h"
#include "PartnerState.h"
#include "Sequencer.h"

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4100 4127 4244 4702 4267 4251 4275)
#endif

#include <boost/program_options.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/chrono.hpp>
#include <boost/asio.hpp>
#include <boost/bind.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif


namespace po = boost::program_options;


class StopHandler : public Safir::Dob::StopHandler
{
    virtual void OnStopOrder()
    {
        std::wcout << "Got stop order!" <<std::endl;
        exit(0);
    }
};


class Dispatcher:
    public Safir::Dob::Dispatcher,
    private boost::noncopyable
{
public:
    Dispatcher(Safir::Dob::Connection & connection,
               boost::asio::io_service & ioService)
        : m_connection(connection)
        , m_ioService(ioService)
    {}
private:
    virtual void OnDoDispatch() {m_ioService.post(boost::bind(&Dispatcher::Dispatch,this));}
    void Dispatch(){m_connection.Dispatch();}

    Safir::Dob::Connection & m_connection;
    boost::asio::io_service & m_ioService;
};

int GetRandomContext()
{
    return rand()%2;
}

const std::string GetTestcaseDir()
{
    const char * SAFIR_RUNTIME = getenv("SAFIR_RUNTIME");
    boost::filesystem::path path = SAFIR_RUNTIME;
    path /= "data";
    path /= "text";
    path /= "dose_test";
    path /= "testcases";
    return path.string();
}


struct CommandLineResults
{
    int first;
    int last;
    Languages languages;
    std::string testcaseDirectory;
    bool noTimeout;
    int testcaseNo;
    int context;
    bool multinode;
};

const CommandLineResults & HandleCommandLine(int argc, char* argv[])
{
    try
    {
        static CommandLineResults results;

        po::options_description desc("Allowed options");
        desc.add_options()
            ("help,h", "show help message")
            ("testcase-directory,d",po::value<std::string>(&results.testcaseDirectory),"directory that contains the test cases")
            ("languages,l", po::value<std::vector<std::string> >(&results.languages)->multitoken()->default_value(Languages(3,"cpp"),"cpp cpp cpp"), "choose languages to run, e.g.\n--languages cpp ada java")
            ("first", po::value<int>(&results.first)->default_value(0), "first testcase")
            ("last", po::value<int>(&results.last)->default_value(9999), "last testcase")
            ("no-timeout", "Do not time out and exit if a partner does not respond for a long time")
            ("context", po::value<int>(&results.context)->default_value(0), "default context for partner test connection (-1 for random)")
            ("multinode", "Run in multinode mode");

        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);
        po::notify(vm);

        if (vm.count("help"))
        {
            std::ostringstream ostr;
            ostr << desc;
            std::wcout << ostr.str().c_str() << std::endl;
            exit(0);
        }

        TestCaseReader::Initialize(results.testcaseDirectory);

        if (results.testcaseDirectory.empty())
        {
            results.testcaseDirectory = GetTestcaseDir();
        }

        results.noTimeout = vm.count("no-timeout") != 0;
        results.multinode = vm.count("multinode") != 0;

        if (results.languages.size() != 3)
        {
            std::wcout << "Need 3 languages"<<std::endl;
            exit(0);
        }

        std::wcout << "Testcase range is " << results.first << " to " << results.last << std::endl;
        return results;
    }
    catch (const std::exception & e)
    {
        std::wcout << "Got exception while parsing command line: "<< std::endl
            <<e.what() <<std::endl;
        exit(1);
    }

}


int main(int argc, char* argv[])
{
    //call this once before we start, to work around a problem in Chrono <= 1.55.
    //See https://svn.boost.org/trac/boost/ticket/9720
    boost::chrono::steady_clock::now();

    //random numbers are used in command line handling and inside the sequencer, so seed the generator here.
    srand(static_cast<unsigned int>(time(NULL)));

    const CommandLineResults & commandLine = HandleCommandLine(argc,argv);


    try
    {
        const std::wstring nameCommonPart = L"Sequencer";
        const std::wstring nameInstancePart = L"";

        boost::asio::io_service ioService;
        boost::asio::io_service::work keepRunning(ioService);

        StopHandler stopHandler;

        Safir::Dob::Connection connection;
        Dispatcher dispatcher(connection,ioService);
        std::wcout << nameCommonPart.c_str() <<  nameInstancePart.c_str() << ": Started" <<std::endl;

        connection.Open(nameCommonPart,
                        nameInstancePart,
                        0, // Context
                        &stopHandler,
                        &dispatcher);

        Languages languages = commandLine.languages;

        std::wcout << "Languages: ";
        for (Languages::iterator it = languages.begin();
             it != languages.end(); ++it)
        {
            std::wcout << it->c_str() << " ";
        }

        const int context = commandLine.context == -1 ? GetRandomContext() : commandLine.context;

        std::wcout << "Context: " << context << std::endl;

        Sequencer sequencer(commandLine.first,
                            commandLine.last,
                            languages,
                            commandLine.multinode,
                            commandLine.noTimeout,
                            context,
                            ioService);

        ioService.run();

        connection.Close();
        std::wcout << "End" << std::endl;
    }
    catch(std::exception & e)
    {
        std::wcout << "Caught std::exception! Contents of exception is:" << std::endl
                   << e.what()<<std::endl;
    }
    catch (...)
    {
        std::wcout << "Caught ... exception!" << std::endl;
    }

    return 0;
}
