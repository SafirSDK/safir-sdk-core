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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include "swre_report_filter.h"
#include "swre_text_serializer.h"
#include "swre_logger_app.h"

#include <Safir/Dob/NotOpenException.h>

#include <sstream>

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
    : m_reportHandler(m_ioService)
    , m_dispatcher(m_connection, m_ioService)
{

};

//-----------------------------------------------------------------------------
LoggerApp::~LoggerApp()
{
    m_connection.Close();
};

//-----------------------------------------------------------------------------
void LoggerApp::OnStopOrder()
{
    m_ioService.stop();
    m_reportHandler.Stop();
};


//-----------------------------------------------------------------------------
int LoggerApp::Run(const std::vector<std::string> & args)
{
    static int inst = 0;

    for (;;)
    {
        try
        {
            m_connection.Open (L"SwreLogger", 
                               boost::lexical_cast<std::wstring>(++inst), 
                               SWRE_LOGGER_CONTEXT, 
                               this, 
                               &m_dispatcher);
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

    boost::asio::io_service::work keepRunning(m_ioService);
    m_ioService.run();

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
            std::wcerr << "Error: -xml option not supported (yet)" << std::endl;
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
                std::wcerr << "Error: option -file, no log file given" << std::endl;
                return false;
            }

            if (!m_reportHandler.SetOutputFile(*it))
            {
                std::wcerr << "Error: Invalid file path" << std::endl;
                return false;
            }
        }
        else if (it->compare("-filesize") == 0)
        {
            if (++it == arg.end())
            {
                std::wcerr << "Error: option -filesize, no log file size given" << std::endl;
                return false;
            }

            unsigned long fileSize;

            fileSize = boost::lexical_cast<unsigned long>(*it);
            if (errno == ERANGE || fileSize == 0)
            {
                std::wcerr << "Error: option -filesize, invalid file size" << std::endl;
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
                std::wcerr << "Error: option -excludeapp, no regular expression given" << std::endl;
                return false;
            }

            if (!ReportFilter::Inst().SetConnectionNameRegEx(Safir::Dob::Typesystem::Utilities::ToWstring(*it), false))
            {
                std::wcerr <<
                    "Error: option -excludeapp, illegal regular expression given" << std::endl;
                return false;
            }
        }
        else if (it->compare("-includeapp") == 0)
        {
            if (++it == arg.end())
            {
                std::wcerr << "Error: option -includeapp, no regular expression given" << std::endl;
                return false;
            }
            if (!ReportFilter::Inst().SetConnectionNameRegEx(Safir::Dob::Typesystem::Utilities::ToWstring(*it), true))
            {
                std::wcerr <<
                    "Error: option -includeapp, illegal regular expression given" << std::endl;
                return false;
            }
        }
        else if (it->compare("-excludenode") == 0)
        {
            if (++it == arg.end())
            {
                std::wcerr << "Error: option -excludenode, no regular expression given" << std::endl;
                return false;
            }
            if (!ReportFilter::Inst().SetNodeNameRegEx(Safir::Dob::Typesystem::Utilities::ToWstring(*it), false))
            {
                std::wcerr <<
                    "Error: option -excludenode, illegal regular expression given" << std::endl;
                return false;
            }
        }
        else if (it->compare("-includenode") == 0)
        {
            if (++it == arg.end())
            {
                std::wcerr << "Error: option -includenode, no regular expression given" << std::endl;
                return false;
            }
            if (!ReportFilter::Inst().SetNodeNameRegEx(Safir::Dob::Typesystem::Utilities::ToWstring(*it), true))
            {
                std::wcerr <<
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
               << " -includenode\t: Include only nodes that match the reg expression\n" << std::endl;
}
}
}

