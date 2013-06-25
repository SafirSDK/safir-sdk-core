/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Utilities/Internal/ConfigReader.h>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4702)
#endif

#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include "swre_report_filter.h"
#include "swre_text_serializer.h"
#include "swre_logger_app.h"

#include <Safir/Dob/NotOpenException.h>
#include <Safir/Time/AceTimeConverter.h>

#include <sstream>
#include <vector>
#include <map>

namespace Safir
{
namespace Swre
{

    //swre_logger uses context 0 to connect to the dob. The strange looking negative number
    //is a way to indicate that this is a connection with special privileges.
    const Safir::Dob::Typesystem::Int32 SWRE_LOGGER_CONTEXT = -1000000;

    using namespace Safir::Dob::Typesystem;

//-----------------------------------------------------------------------------
LoggerApp::LoggerApp()
    : ACE_Event_Handler(ACE_Reactor::instance())
    , m_dispatchEvent(m_Connection)
{
    // Start supervision of dump directory
    reactor()->schedule_timer(this,
                              NULL,
                              Safir::Time::AceTimeConverter::ToAceTime(boost::posix_time::seconds(1)),
                              Safir::Time::AceTimeConverter::ToAceTime(boost::posix_time::minutes(10)));
    
};

//-----------------------------------------------------------------------------
LoggerApp::~LoggerApp()
{
    reactor()->cancel_timer(this);
    m_Connection.Close();
};

//-----------------------------------------------------------------------------
void LoggerApp::OnStopOrder()
{
    m_reportHandler.Stop();
    ACE_Reactor::end_event_loop();
};

//-----------------------------------------------------------------------------
/*void LoggerApp::OnEvent(const osin::EventId& id)
{
   if (id == m_dispatchEvent.IdOf())
   {
      m_Connection.Dispatch();
   }
} */

//-----------------------------------------------------------------------------
int LoggerApp::Run(const std::vector<std::string> & args)
{
    static int inst = 0;

    for (;;)
    {
        try
        {
            m_Connection.Open (L"SwreLogger", boost::lexical_cast<std::wstring>(++inst), SWRE_LOGGER_CONTEXT, this, &m_dispatchEvent);
            break;
        }
        catch (const Safir::Dob::NotOpenException&)
        {
        }
    }

    if (!ParseCommandLine(args) || !m_reportHandler.Start())
    {
        // Something wrong with the command line parameters or startup
        return -1;
    }

    ACE_Reactor::instance()->run_reactor_event_loop();
    return 0;
}



//-----------------------------------------------------------------------------
bool LoggerApp::ParseCommandLine(const std::vector<std::string> & arg)
{
    for (std::vector<std::string>::const_iterator it = arg.begin();
        it != arg.end(); ++it)
    {
        //skip first argument since it is program name
        if (it == arg.begin())
        {
            continue;
        }

        if (it->compare("-?") == 0 ||
            it->compare("-h") == 0 ||
            it->compare("-help") == 0)
        {
            Usage();
            return false;
        }
        else if (it->compare("-text") == 0)
        {
            m_reportHandler.SetOutputFormat(ReportHandler::SimpleText);
        }
        else if (it->compare("-xml") == 0)
        {
            std::cerr << "Error: -xml option not supported (yet)" << std::endl;
            return false;
        }
        else if (it->compare("-stdout") == 0)
        {
            m_reportHandler.SetOutputDestination(ReportHandler::StdOut);
        }
        else if (it->compare("-file") == 0)
        {
            m_reportHandler.SetOutputDestination(ReportHandler::File);

            if (++it == arg.end())
            {
                std::cerr << "Error: option -file, no log file given" << std::endl;
                return false;
            }

            if (!m_reportHandler.SetOutputFile(*it))
            {
                std::cerr << "Error: Invalid file path" << std::endl;
                return false;
            }
        }
        else if (it->compare("-filesize") == 0)
        {
            if (++it == arg.end())
            {
                std::cerr << "Error: option -filesize, no log file size given" << std::endl;
                return false;
            }

            unsigned long fileSize;

            fileSize = boost::lexical_cast<unsigned long>(*it);
            if (errno == ERANGE || fileSize == 0)
            {
                std::cerr << "Error: option -filesize, invalid file size" << std::endl;
                return false;
            }

            m_reportHandler.SetMaxFileSize(fileSize);
        }
        else if (it->compare("-typeseqnbr") == 0)
        {

            TextSerializer::Inst().SetIncludeTypeSequenceNumber(true);
        }
        else if (it->compare("-nofatalerr") == 0)
        {
            ReportFilter::Inst().SetFatalErrorLogging(false);
        }
        else if (it->compare("-noerr") == 0)
        {
            ReportFilter::Inst().SetErrorLogging(false);
        }
        else if (it->compare("-nores") == 0)
        {
            ReportFilter::Inst().SetResourceLogging(false);
        }
        else if (it->compare("-nopi") == 0)
        {
            ReportFilter::Inst().SetProgramInfoLogging(false);
        }
        else if (it->compare("-noprogerr") == 0)
        {
            ReportFilter::Inst().SetProgrammingErrorLogging(false);
        }
        else if (it->compare("-excludeapp") == 0)
        {
            if (++it == arg.end())
            {
                std::cerr << "Error: option -excludeapp, no regular expression given" << std::endl;
                return false;
            }

            if (!ReportFilter::Inst().SetConnectionNameRegEx(Safir::Dob::Typesystem::Utilities::ToWstring(*it), false))
            {
                std::cerr <<
                    "Error: option -excludeapp, illegal regular expression given" << std::endl;
                return false;
            }
        }
        else if (it->compare("-includeapp") == 0)
        {
            if (++it == arg.end())
            {
                std::cerr << "Error: option -includeapp, no regular expression given" << std::endl;
                return false;
            }
            if (!ReportFilter::Inst().SetConnectionNameRegEx(Safir::Dob::Typesystem::Utilities::ToWstring(*it), true))
            {
                std::cerr <<
                    "Error: option -includeapp, illegal regular expression given" << std::endl;
                return false;
            }
        }
        else if (it->compare("-excludenode") == 0)
        {
            if (++it == arg.end())
            {
                std::cerr << "Error: option -excludenode, no regular expression given" << std::endl;
                return false;
            }
            if (!ReportFilter::Inst().SetNodeNameRegEx(Safir::Dob::Typesystem::Utilities::ToWstring(*it), false))
            {
                std::cerr <<
                    "Error: option -excludenode, illegal regular expression given" << std::endl;
                return false;
            }
        }
        else if (it->compare("-includenode") == 0)
        {
            if (++it == arg.end())
            {
                std::cerr << "Error: option -includenode, no regular expression given" << std::endl;
                return false;
            }
            if (!ReportFilter::Inst().SetNodeNameRegEx(Safir::Dob::Typesystem::Utilities::ToWstring(*it), true))
            {
                std::cerr <<
                    "Error: option -includenode, illegal regular expression given" << std::endl;
                return false;
            }
        }
        else
        {
            std::wcerr << "Error: illegal option: " << it->c_str() << std::endl;
            Usage();
            return false;
        }
    }
    return true;
}

//-----------------------------------------------------------------------------
void LoggerApp::Usage()
{
    std::wcerr << "Syntax:  swre_logger\n"
               << "\t[-h | -? | -help]\n"
               << "\t[-text | -xml] (Default: -text)\n"
               << "\t[-stdout | -file <file path>] (Default: -stdout)\n"
               << "\t[-filesize <max file size (KB)>] (Default: 500000 KB)\n"
               << "\t[-typeseqnbr]\n"
               << "\t[-nofatalerr]\n"
               << "\t[-noerr]\n"
               << "\t[-nores]\n"
               << "\t[-nopi]\n"
               << "\t[-noprogerr]\n"
               << "\t[(-excludeapp | -includeapp) <regular expression>]\n"
               << "\t[(-excludenode | -includenode) <regular expression>]\n"

               << "\n"

               << " -text\t\t: Output in simple text format\n"
               << " -xml\t\t: Output in XML format (not supported yet)\n"
               << " -stdout\t: Output to std output\n"
               << " -file\t\t: Output to given file\n"
               << " -filesize\t: Max log filesize in KB\n"
               << " -typeseqnbr\t: Include seq nbr for report type\n"
               << " -nofatalerr\t: Don't log Fatal Error reports\n"
               << " -noerr\t\t: Don't log Error reports\n"
               << " -nores\t\t: Don't log Resource reports\n"
               << " -nopi\t\t: Don't log ProgramInfo reports\n"
               << " -noprogerr\t: Don't log Programming Error reports\n"
               << " -excludeapp\t: Exclude applications (connections) that match reg expression\n"
               << " -includeapp\t: Include only applications that match reg expression\n"
               << " -excludenode\t: Exclude nodes that match reg expression\n"
               << " -includenode\t: Include only nodes that match the reg expression\n\n";
}

namespace 
{
    const boost::filesystem::path GetDumpDirectory()
    {
        Safir::Utilities::Internal::ConfigReader config;
        return boost::filesystem::path(config.Locations().get<std::string>("crash_dump_directory"));
    }
}
int LoggerApp::handle_timeout(const ACE_Time_Value & /*currentTime*/, const void * /*act*/)
{
    try 
    {
        namespace bfs = boost::filesystem;
        
        const size_t MAX_NUM_DUMP_FILES = 1000;
        
        const std::vector<bfs::path> dumpFiles = std::vector<bfs::path>(bfs::directory_iterator(GetDumpDirectory()),
                                                                        bfs::directory_iterator());
        
        if (dumpFiles.size() > MAX_NUM_DUMP_FILES)
        {
            std::multimap<std::time_t, bfs::path> sorted;
            for (std::vector<bfs::path>::const_iterator it = dumpFiles.begin();
                 it != dumpFiles.end(); ++it)
            {
                sorted.insert(std::make_pair(bfs::last_write_time(*it),*it));
            }
            
            const size_t tooMany = dumpFiles.size() - MAX_NUM_DUMP_FILES + 10; //remove a few more...
            std::multimap<std::time_t, bfs::path>::iterator it = sorted.begin();
            for (size_t i = 0; i < tooMany; ++i)
            {
                bfs::remove(it->second);
                ++it;
            }
        }
    }
    catch (const boost::filesystem::filesystem_error& )
    {

    }

    return 0;
}

}
}

