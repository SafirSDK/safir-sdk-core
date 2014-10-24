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
#include <iostream>

#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Time/AceTimeConverter.h>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/exception.hpp>

#include "swre_report_handler.h"
#include "swre_report_filter.h"
#include "swre_text_serializer.h"
#include <ace/OS_NS_sys_socket.h>



namespace fs = boost::filesystem;


namespace Safir
{
namespace Swre
{

    //-----------------------------------------------------------------------------
    ReportHandler::ReportHandler():
        ACE_Event_Handler(ACE_Reactor::instance()),
        m_outDest(StdOut),
        m_outFormat(SimpleText),
        m_logFile(),
        m_logFilePath(),
        m_oldLogFilePath(),
        m_maxFileSize(500000000) // bytes
    {
        ACE_OS::socket_init();
        if (0 != m_socket.open(ACE_INET_Addr((unsigned short)31221)))
        {
            std::wcout << "Failed to open socket for listening to panic logs! (udp port 31221)" << std::endl;
        }
        reactor()->register_handler(this, ACE_Event_Handler::READ_MASK);
    }

    //-----------------------------------------------------------------------------
    ReportHandler::~ReportHandler()
    {
        reactor()->cancel_timer(this);
        reactor()->remove_handler(this,ACE_Event_Handler::READ_MASK);
        m_socket.close();
    }

    //-----------------------------------------------------------------------------
    void ReportHandler::SetOutputDestination(OutputDest dest)
    {
        m_outDest = dest;
    }

    //-----------------------------------------------------------------------------
    bool ReportHandler::SetOutputFile(const std::string& logFile)
    {
        try
        {
            m_logFilePath = fs::path(logFile, fs::native);

            std::string oldFileName = "old_";
            oldFileName.append(m_logFilePath.leaf());

            m_oldLogFilePath = m_logFilePath.branch_path() / oldFileName;

        }
        catch (boost::filesystem::filesystem_error& e)
        {
            std::cerr << e.what() << std::endl;
            return false;
        }

        return true;
    }

    //-----------------------------------------------------------------------------
    void ReportHandler::SetMaxFileSize(unsigned long size)
    {
        m_maxFileSize = size * 1000;
    }

    //-----------------------------------------------------------------------------
    void ReportHandler::SetOutputFormat(OutputFormat outputFormat)
    {
        m_outFormat = outputFormat;
    }

    //-----------------------------------------------------------------------------
    bool ReportHandler::Start()
    {
        try
        {
            // Start subscription of sw reports base class which means that we will
            // receive all reports
            m_Connection.Attach();
            m_Connection.SubscribeMessage(Safir::SwReports::Internal::Report::ClassTypeId, Safir::Dob::Typesystem::ChannelId(), this);

            if (m_outDest == File)
            {
                // Create log directory if it doesn't exist
                if (m_logFilePath.has_branch_path() && !fs::exists(m_logFilePath.branch_path()))
                {
                    fs::create_directory(m_logFilePath.branch_path());
                }

                CheckLogFile(); // Will rename log file if it is over max limit

                m_logFile.open(m_logFilePath, std::ios_base::app);

                if (!m_logFile)
                {
                    std::cerr << "Fatal error: Can't open " <<  m_logFilePath.native_file_string() << std::endl;
                    return false;
                }

                // Start supervision of log file size
                reactor()->schedule_timer(this,
                                          NULL,
                                          ACE_Time_Value(60),
                                          ACE_Time_Value(60));
            }
            return true;
        }
        catch (fs::filesystem_error& e)
        {
            std::cerr << "Fatal error: " << e.what() <<  std::endl;
            return false;
        }
    }

    //-----------------------------------------------------------------------------
    void ReportHandler::Stop()
    {
        reactor()->cancel_timer(this);
        reactor()->remove_handler(this,ACE_Event_Handler::READ_MASK);
        m_socket.close();
        m_logFile.close();
    }

    //-----------------------------------------------------------------------------
    void ReportHandler::OnMessage(const Safir::Dob::MessageProxy messageProxy)
    {
        const Safir::Dob::MessagePtr message = messageProxy.GetMessage();

        const Safir::SwReports::Internal::ReportPtr report = boost::dynamic_pointer_cast<Safir::SwReports::Internal::Report>(message);

        if (!ReportFilter::Inst().ReportIsToBeLogged(report))
        {
            return;  // *** RETURN ***
        }

        std::wstring serializedReport;

        switch (m_outFormat)
        {
        case SimpleText:
            {
                serializedReport = TextSerializer::Inst().SerializeReport(report);
            }
            break;

        case Xml:
            {
                // STAWI: Not implemented yet
            }
            break;
        }

        switch (m_outDest)
        {
        case StdOut:
            {
                bool illegalCharsFound = false;
                for (std::wstring::iterator it = serializedReport.begin(); it != serializedReport.end(); ++it)
                {
                    if (*it > 127)
                    {
                        *it = '*';
                        illegalCharsFound = true;
                    }
                }

                if (illegalCharsFound)
                {
                    std::wcout << "WARNING! This report contains illegal characters (ASCII char 127++)." << std::endl << std::flush;
                }
                std::wcout << serializedReport << std::flush;
            }
            break;

        case File:
            {
                m_logFile << serializedReport << std::flush;
            }
            break;
        }
    }

    //-----------------------------------------------------------------------------
    int ReportHandler::handle_input(ACE_HANDLE)
    {
        iovec receivedData = {0,0};
        ACE_INET_Addr addr;
        const ssize_t size = m_socket.recv(&receivedData, addr);
        if (size <= 0)
        {
            return 0;
        }

        //TODO: xml formatting is not implemented!

        std::wstring convertedReport;
        try
        {
            convertedReport = Safir::Dob::Typesystem::Utilities::ToWstring
                (std::string(static_cast<const char*>(receivedData.iov_base),static_cast<const char*>(receivedData.iov_base) + receivedData.iov_len));
        }
        catch (...)
        {
            delete [] static_cast<char*>(receivedData.iov_base);
        }

        switch (m_outDest)
        {
        case StdOut:
            {
                std::wcout << "UDP log from " << addr.get_host_addr() << ":" << std::endl;
                std::wcout << convertedReport << std::flush;
            }
            break;

        case File:
            {
                m_logFile << "UDP log from " << addr.get_host_addr() << ":" << std::endl;
                m_logFile << convertedReport << std::flush;
            }
            break;
        }
        return 0;
    }


    //
    //
    // ********    OVERRIDES       ACE_Event_Handler         ************
    //
    //

    // Function:    handle_timeout
    // Parameters:  const ACE_Time_Value & currentTime
    //              const void * act
    // Returns:     -
    // Comments:    See ACE_Event_Handler
    //
    int ReportHandler::handle_timeout(const ACE_Time_Value & /*currentTime*/, const void * /*act*/)
    {
        if (!CheckLogFile())
        {
            std::cerr << "Opening new " << m_logFilePath.native_file_string() << " ..." << std::endl;

            m_logFile.open(m_logFilePath, std::ios_base::app);

            if (!m_logFile)
            {
                std::cerr << "Fatal error: Can't open " <<  m_logFilePath.native_file_string() << std::endl;
            }
        }
        return 0; //means success
    }




    //-----------------------------------------------------------------------------
    bool ReportHandler::CheckLogFile()
    {
        if (fs::exists(m_logFilePath))
        {
            if (fs::file_size(m_logFilePath) >= m_maxFileSize)
            {
                std::cerr << "Size of " << m_logFilePath.native_file_string() << " >= "
                          << m_maxFileSize << " bytes. Closing the file ..." << std::endl;

                if (m_logFile.is_open())
                {
                    m_logFile.close();
                }

                if (fs::exists(m_oldLogFilePath))
                {
                    std::cerr << "Deleting " << m_oldLogFilePath.native_file_string() << " ..." << std::endl;
                    fs::remove(m_oldLogFilePath);
                }

                std::cerr << "Renaming " << m_logFilePath.native_file_string() << " to "
                          << m_oldLogFilePath.native_file_string() << " ..." << std::endl;

                fs::rename(m_logFilePath, m_oldLogFilePath);

                return false;  // Indicate that log file has been renamed
            }
        }

        return true;
    }

}
}
