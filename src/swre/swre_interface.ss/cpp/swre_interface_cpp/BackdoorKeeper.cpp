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
#include "Safir/Application/BackdoorKeeper.h"
#include "Safir/SwReports/SwReport.h"
#include <boost/regex.hpp>

#include <Safir/Application/BackdoorCommand.h>

#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/ConnectionAspectMisc.h>


const Safir::Dob::Typesystem::TypeId PI_CMD_TYPE_ID = Safir::Application::BackdoorCommand::ClassTypeId;
const Safir::Dob::Typesystem::ChannelId PI_CMD_CHANNEL_ID = Safir::Dob::Typesystem::ChannelId::ChannelId();


namespace Safir
{
namespace Application
{

    const wchar_t* delimiters = L" \t\n\r";
    const wchar_t* pingCmd = L"ping";
    const wchar_t* helpCmd = L"help";

    //-----------------------------------------------------------------------------
    BackdoorKeeper::BackdoorKeeper():
        m_backdoor(NULL),
        m_started(false)
    {

    }

    //-----------------------------------------------------------------------------
    void BackdoorKeeper::Start(Safir::Application::Backdoor & handler)
    {
        if (m_started)
        {
            // Already started, just return
            return; // *** RETURN ***
        }

        // Save pointer to Dose interface and handler
        m_connection.Attach();
        m_backdoor = &handler;

        m_connection.SubscribeMessage(PI_CMD_TYPE_ID, PI_CMD_CHANNEL_ID, this);
        m_started = true;
    }

    //-----------------------------------------------------------------------------
    void BackdoorKeeper::Stop()
    {
        if (!m_started)
        {
            // Never started, just return
            return; // *** RETURN ***
        }

        m_connection.UnsubscribeMessage(PI_CMD_TYPE_ID, PI_CMD_CHANNEL_ID, this);
        m_connection.Detach();
        m_backdoor = NULL;
        m_started = false;
    }

    //-----------------------------------------------------------------------------
    void BackdoorKeeper::OnMessage(const Safir::Dob::MessageProxy messageProxy)
    {
        try
        {
            const boost::wregex::flag_type regExpFlags = boost::regex::perl | boost::regex::icase;

            const Safir::Dob::MessagePtr message = messageProxy.GetMessage();

            const Safir::Application::BackdoorCommandPtr cmd =
                boost::dynamic_pointer_cast<Safir::Application::BackdoorCommand>(message);

            if (cmd == NULL)
            {
                // Unexpected message
                return;   // *** RETURN ***
            }

            if (!cmd->NodeName().IsNull())
            {
                if (!boost::regex_search(Safir::Dob::NodeParameters::Nodes(Safir::Dob::ThisNodeParameters::NodeNumber())->NodeName().GetVal(),
                                         boost::wregex(cmd->NodeName().GetVal(), regExpFlags)))
                {
                    // Node name doesn't match
                    return;  // *** RETURN ***
                }
            }

            Safir::Dob::ConnectionAspectMisc connectionAspectMisc(m_connection);
            if (!cmd->ConnectionName().IsNull())
            {
                if (!boost::regex_search(connectionAspectMisc.GetConnectionName(),
                                         boost::wregex(cmd->ConnectionName().GetVal(), regExpFlags)))
                {
                    // Connection name doesn't match
                    return;  // *** RETURN ***
                }
            }

            if (cmd->Command().IsNull())
            {
                // No command given
                return;  // *** RETURN ***
            }

            // Ok, it seems that this PI-command is for this application

            std::vector<std::wstring> cmdTokens;

            Tokenize(cmd->Command().GetVal(), cmdTokens, delimiters);

            if (!cmdTokens.empty())
            {
                if (cmdTokens[0] == pingCmd)
                {
                    // It's a 'ping' command. Answer to it without bothering
                    // the subclass implementator.

                    Safir::SwReports::SendProgramInfoReport(L"Ping reply");

                    return; // *** RETURN ***
                }
                else if (cmdTokens[0] == helpCmd)
                {
                    // Get help text from subclass implementator.
                    Safir::SwReports::SendProgramInfoReport(m_backdoor->GetHelpText());

                    return; // *** RETURN ***
                }
            }

            // Let the subclass handle the command
            m_backdoor->HandleCommand(cmdTokens);
        }
        catch (boost::bad_expression& /* e*/ )
        {
            // An invalid regular expression was used, skip this command
            return;  // *** RETURN ***
        }


    }

    //-----------------------------------------------------------------------------
    void BackdoorKeeper::Tokenize(const std::wstring&        str,
                                   std::vector<std::wstring>& tokens,
                                   const std::wstring&        delimiters)
    {
        using namespace std;

        tokens.clear();

        // Skip delimiters at beginning.
        wstring::size_type currPos = str.find_first_not_of(delimiters, 0);

        while (currPos != string::npos)
        {
            wstring::size_type lastPos = str.find_first_of(delimiters, currPos);

            wstring::size_type count;

            if (lastPos == wstring::npos)
            {
                count = lastPos;
            }
            else
            {
                count = lastPos - currPos;
            }

            tokens.push_back(str.substr(currPos, count));

            currPos = str.find_first_not_of(delimiters, lastPos);
        }
    }

};
};

