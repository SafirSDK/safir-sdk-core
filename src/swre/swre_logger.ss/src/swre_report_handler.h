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

#include <ace/config.h>
#include <ace/Event_Handler.h>
#include <ace/Reactor.h>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/fstream.hpp>
#include <ace/SOCK_Dgram.h>

#ifdef GetMessage
#undef GetMessage
#endif

namespace Safir
{
namespace Swre
{

class ReportHandler : 
    public Safir::Dob::MessageSubscriber,
    public ACE_Event_Handler
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

    ReportHandler();
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

    // No copying or assignment for this class
    ReportHandler(const ReportHandler& rhs);
    ReportHandler& operator=(const ReportHandler& rhs);

    // Implementation of MessageSubscriber interface
    virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy);

    // Implemntation of ACE_Event_Handler interface
    int handle_timeout(const ACE_Time_Value & /*currentTime*/, const void * /*act*/);

    ACE_HANDLE get_handle() const {return m_socket.get_handle();}
    int handle_input(ACE_HANDLE);

    bool CheckLogFile();

    OutputDest m_outDest;
    OutputFormat m_outFormat;

    boost::filesystem::wofstream m_logFile;
    boost::filesystem::path      m_logFilePath;
    boost::filesystem::path      m_oldLogFilePath;
    boost::uintmax_t              m_maxFileSize;

    Safir::Dob::SecondaryConnection m_Connection;
    ACE_SOCK_Dgram m_socket;

};
}
}


#endif
