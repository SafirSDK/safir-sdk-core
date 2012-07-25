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

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/exception.hpp>
#include <boost/bind.hpp>

#include "swre_report_handler.h"
#include "swre_report_filter.h"
#include "swre_text_serializer.h"

//TODO get rid of these!
#ifdef GetMessage
#undef GetMessage
#endif

namespace fs = boost::filesystem;


namespace Safir
{
namespace Swre
{

    //-----------------------------------------------------------------------------
    ReportHandler::ReportHandler(boost::asio::io_service& ioService):
        m_outDest(StdOut),
        m_outFormat(SimpleText),
        m_logFile(),
        m_logFilePath(),
        m_oldLogFilePath(),
        m_maxFileSize(500000000), // bytes
        m_checkFileSizeTime(boost::posix_time::milliseconds(60000)),
        m_ioService(ioService),
        m_socket(ioService),
        m_udpReceiveBuffer(65000,0)
    {
        try
        {
            const boost::asio::ip::udp::endpoint endpoint(boost::asio::ip::udp::v4(), 31221);
            m_socket.open(endpoint.protocol());
            m_socket.bind(endpoint);
            m_socket.async_receive_from
                (boost::asio::buffer(m_udpReceiveBuffer), m_senderEndpoint,
                 boost::bind(&ReportHandler::HandleUdpData, this,
                             boost::asio::placeholders::error,
                             boost::asio::placeholders::bytes_transferred));
        }
        catch (const boost::exception&)
        {
            std::wcerr << "Failed to open socket for listening to panic logs! (udp port 31221)" << std::endl;
            //TODO: make it retry opening socket
            //TODO: forward received panic logs to normal logging.
        }
    }

    //-----------------------------------------------------------------------------
    ReportHandler::~ReportHandler()
    {

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
            m_logFilePath = fs::path(logFile);

            std::string oldFileName = "old_";
#if defined (BOOST_FILESYSTEM_VERSION) && BOOST_FILESYSTEM_VERSION == 3
            oldFileName.append(m_logFilePath.filename().string());
#else
            oldFileName.append(m_logFilePath.filename()); 
#endif
            m_oldLogFilePath = m_logFilePath.parent_path() / oldFileName;

        }
        catch (boost::filesystem::filesystem_error& e)
        {
            std::wcerr << e.what() << std::endl;
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
            m_connection.Attach();
            m_connection.SubscribeMessage(Safir::SwReports::Internal::Report::ClassTypeId, Safir::Dob::Typesystem::ChannelId(), this);

            if (m_outDest == File)
            {
                // Create log directory if it doesn't exist
                if (m_logFilePath.has_parent_path() && !fs::exists(m_logFilePath.parent_path()))
                {
                    fs::create_directory(m_logFilePath.parent_path());
                }

                CheckLogFile(); // Will rename log file if it is over max limit

                m_logFile.open(m_logFilePath, std::ios_base::app);

                if (!m_logFile)
                {
                    std::wcerr << "Fatal error: Can't open " <<  m_logFilePath.string().c_str() << std::endl;
                    return false;
                }

                // Start supervision of log file size
                m_timer.reset(new boost::asio::deadline_timer(m_ioService,m_checkFileSizeTime));
                m_timer->async_wait(boost::bind(&ReportHandler::HandleTimeout,this,_1));
            }
            return true;
        }
        catch (const fs::filesystem_error& e)
        {
            std::wcerr << "Fatal error: " << e.what() <<  std::endl;
            return false;
        }
    }

    //-----------------------------------------------------------------------------
    void ReportHandler::Stop()
    {
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
    void ReportHandler::HandleUdpData(const boost::system::error_code& error,
                                      const size_t bytes_recvd)
    {
        if (error)
        {
            std::wcout << "Error in receiving UDP data!\n" <<error << std::endl;
        }

        //set up to receive more data
        m_socket.async_receive_from
            (boost::asio::buffer(m_udpReceiveBuffer), m_senderEndpoint,
             boost::bind(&ReportHandler::HandleUdpData, this,
                         boost::asio::placeholders::error,
                         boost::asio::placeholders::bytes_transferred));

        if (bytes_recvd <= 0)
        {
            return;
        }

        //TODO: xml formatting is not implemented!

        std::wstring convertedReport;
        
        convertedReport = Safir::Dob::Typesystem::Utilities::ToWstring
            (std::string(m_udpReceiveBuffer.begin(),m_udpReceiveBuffer.begin() + bytes_recvd));

        switch (m_outDest)
        {
        case StdOut:
            {
                std::wcout << "UDP log from " << m_senderEndpoint.address().to_string().c_str() << ":" <<std::endl;
                std::wcout << convertedReport << std::flush;
            }
            break;

        case File:
            {
                m_logFile << "UDP log from " << m_senderEndpoint.address().to_string().c_str() << ":" << std::endl;
                m_logFile << convertedReport << std::flush;
            }
            break;
        }
    }


    //-----------------------------------------------------------------------------
    void ReportHandler::HandleTimeout(const boost::system::error_code & error)
    {
        if (!error)
        {
            if (!CheckLogFile())
            {
                std::wcerr << "Opening new " << m_logFilePath.string().c_str() << " ..." << std::endl;
                
                m_logFile.open(m_logFilePath, std::ios_base::app);
                
                if (!m_logFile)
                {
                    std::wcerr << "Fatal error: Can't open " <<  m_logFilePath.string().c_str() << std::endl;
                }
            }
        }

        //set timer again
        m_timer.reset(new boost::asio::deadline_timer(m_ioService,m_checkFileSizeTime));
        m_timer->async_wait(boost::bind(&ReportHandler::HandleTimeout,this,_1));
    }




    //-----------------------------------------------------------------------------
    bool ReportHandler::CheckLogFile()
    {
        if (fs::exists(m_logFilePath))
        {
            if (fs::file_size(m_logFilePath) >= m_maxFileSize)
            {
                std::wcerr << "Size of " << m_logFilePath.string().c_str() << " >= "
                          << m_maxFileSize << " bytes. Closing the file ..." << std::endl;

                if (m_logFile.is_open())
                {
                    m_logFile.close();
                }

                if (fs::exists(m_oldLogFilePath))
                {
                    std::wcerr << "Deleting " << m_oldLogFilePath.string().c_str() << " ..." << std::endl;
                    fs::remove(m_oldLogFilePath);
                }

                std::wcerr << "Renaming " << m_logFilePath.string().c_str() << " to "
                          << m_oldLogFilePath.string().c_str() << " ..." << std::endl;

                fs::rename(m_logFilePath, m_oldLogFilePath);

                return false;  // Indicate that log file has been renamed
            }
        }

        return true;
    }

}
}
