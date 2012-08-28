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
#ifndef __SWRE_REPORT_HANDLER_H
#define __SWRE_REPORT_HANDLER_H

#include <Safir/Dob/Consumer.h>

#include <boost/date_time/posix_time/posix_time_duration.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/noncopyable.hpp>
#include <boost/asio.hpp>
#include <boost/scoped_ptr.hpp>

namespace Safir
{
namespace Swre
{

class ReportHandler : 
    public Safir::Dob::MessageSubscriber,
    private boost::noncopyable
{

public:

    enum OutputFormat
    {
        SimpleText,
        Xml
    };

    enum OutputDest
    {
        StdOut,
        File
    };

    explicit ReportHandler(boost::asio::io_service& ioService);
    ~ReportHandler();

    // Method     : Start
    // Parameters : -
    // Returns    : True => Startup ok
    // Comments   : Makes neccessary preparations and starts subscription for software reports.
    bool Start();

    // Method     : Stop
    // Parameters : -
    // Returns    : -
    // Comments   : Makes neccessary cleanup.
    void Stop();

    // Method     : SetOutputDestination
    // Parameters : dest [in] - Output destination
    // Returns    : -
    // Comments   : Determines where the output will go.
    void SetOutputDestination(OutputDest dest);

    // Method     : SetOutputFile
    // Parameters : dir [in] - File path and name
    // Returns    : True => Path is valid
    //              False => Path is not valid
    // Comments   : Determines directory and log file name
    bool SetOutputFile(const std::string& logFile);

    // Method     : SetMaxFileSize
    // Parameters : size [in] - Max file size in KB
    // Returns    : -
    // Comments   : Determines max size for the log file.
    void SetMaxFileSize(unsigned long size);

    // Method     : SetOutputFormat
    // Parameters : format [in] - Output format
    // Returns    : -
    // Comments   : Determines output format.
    void SetOutputFormat(OutputFormat format);

private:

    // Implementation of MessageSubscriber interface
    virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy);

    void HandleTimeout(const boost::system::error_code & error);

    void HandleUdpData(const boost::system::error_code& error,
                       const size_t bytes_recvd);

    bool CheckLogFile();

    OutputDest m_outDest;
    OutputFormat m_outFormat;

    boost::filesystem::wofstream m_logFile;
    boost::filesystem::path      m_logFilePath;
    boost::filesystem::path      m_oldLogFilePath;
    boost::uintmax_t              m_maxFileSize;

    const boost::posix_time::time_duration m_checkFileSizeTime;

    Safir::Dob::SecondaryConnection m_connection;
    boost::asio::io_service& m_ioService;
    boost::asio::ip::udp::socket m_socket;
    std::vector<char> m_udpReceiveBuffer;
    boost::asio::ip::udp::endpoint m_senderEndpoint;
    boost::scoped_ptr<boost::asio::deadline_timer> m_timer;
};
}
}


#endif
