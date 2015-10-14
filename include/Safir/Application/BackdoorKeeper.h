/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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
#ifndef __SWRE_BACKDOOR_KEEPER_H__
#define __SWRE_BACKDOOR_KEEPER_H__

#include <Safir/Application/Internal/SwReportExportDefs.h>
#include <Safir/Dob/ConnectionBase.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Application/Backdoor.h>

namespace Safir
{
namespace Application
{

/**
 * Class that provides subscription and filtering for BackdoorCommands.
 * Use this class to "keep" the Backdoor. After Start is called your
 * Backdoor will be called every time there is a Command aimed for
 * the given connection.
 */
class SWRE_INTERFACE_CPP_API BackdoorKeeper :
        public Safir::Dob::MessageSubscriber
{
public:

    /**
     * Constructor.
     *
     * @param connection [in] The connection on which the keeper will subscribe
     *                        to backdoor messages.
     */
    explicit BackdoorKeeper(const Safir::Dob::ConnectionBase& connection);

    /**
     * Destructor.
     */
    virtual ~BackdoorKeeper(){}

    /**
     * Starts subscription for backdoor commands to be sent to the Backdoor.
     *
     * The connection that was passed in the constructor must be opened before Start is called.
     *
     * If the connection is closed and opened again (maybe in a different context)
     * this method must be called again to establish the subscription.
     *
     * The class supports restarting/pausing by calling stop and then start again.
     *
     * @param backdoor [in] - Class that implements the Backdoor interface.
     * @exception Safir::Dob::NotOpenException 'Start' was called before connect to Dob.
     */
    void Start(Safir::Application::Backdoor& backdoor);

    /**
     * Stops subscription for backdoor commands.
     * The backdoor can be started again by calling start after it has been stopped.
     */
    void Stop();

    /**
     * Check if the backdoor keeper has been started.
     *
     * @return true if the keeper is started.
     */
    bool IsStarted() const {return m_started;}

private:
    //Disable copying and assignment
    BackdoorKeeper(const BackdoorKeeper&);
    BackdoorKeeper& operator=(const BackdoorKeeper&);

    // Implementation of message subscriber interface
    virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy);

    void Tokenize(const std::wstring&        str,
                  std::vector<std::wstring>& tokens,
                  const std::wstring&        delimiters = L" \t\n");

    const Safir::Dob::ConnectionBase&   m_connection;
    Backdoor*                           m_backdoor;

    bool m_started;
};

}
}

#endif
