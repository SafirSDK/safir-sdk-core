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
#ifndef __SWRE_BACKDOOR_KEEPER_H__
#define __SWRE_BACKDOOR_KEEPER_H__

#include <Safir/Application/Internal/SwReportExportDefs.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Application/Backdoor.h>

namespace Safir
{
namespace Application
{
    /**
     * Class that provides subscription and filtering for BackdoorCommands.
     * Use this class to "keep" the Backdoor. Call the Start routine with
     * the backdoor and a dob connection to set up subscriptions and then your
     * Backdoor class will be called every time there is a Command aimed for that
     * connection.
    */
    class SWRE_API BackdoorKeeper :
        public Safir::Dob::MessageSubscriber
    {
    public:
        /**
         * Default constructor.
         */
        BackdoorKeeper();

        /**
         * Destructor.
         */
        virtual ~BackdoorKeeper(){};

        /**
         * Starts subscription for Program Information commands to be sent to the Backdoor.
         * 
         * A backdoor will be established for the "first" connection that is opened in
         * the thread that calls this method. (That is, a secondary connection Attach is used
         * internally)
         *
         * If the main connection is closed and opened again (maybe in a different context),
         * this method must be called again
         *
         * The class supports restarting/pausing by calling stop and then start again.
         *
         * @param backdoor [in] - Class that implements the Backdoor interface.
         * @exception Safir::Dob::NotOpenException 'Start' was called before connect to Dob.
         */
        void Start(Safir::Application::Backdoor & backdoor);

        /**
         * Starts subscription for Program Information commands to be sent to the Backdoor using
         * the given named connection.
         *
         * A backdoor will be established for the named connection that is opened in
         * the thread that calls this method.
         * 
         * If the main connection is closed and opened again (maybe in a different context),
         * this method must be called again
         *
         * The class supports restarting/pausing by calling stop and then start again.
         *
         * @param backdoor [in] - Class that implements the Backdoor interface.
         * @param connectionNameCommonPart [in] - Name that identifies the connection but not any particular
         *                                        instance.
         * @param [in] connectionNameInstancePart Name that identifies a particular connection instance.
         * @exception Safir::Dob::NotOpenException The named connection is not opened. 
         */
        void Start(Safir::Application::Backdoor & backdoor,
                   const std::wstring& connectionNameCommonPart,
                   const std::wstring& connectionNameInstancePart);

        /**
         * Stops subscription for Program Information commands.
         * The backdoor can be started again by calling start after it has been stopped.
         */
        void Stop();

        /**
         * Check if the backdoor keeper has been started.
         *
         * @return true if the keeper is started.
         */
        bool IsStarted() const {return m_started;}
    protected:

        /**
         * Returns the dob connection instance set by the start method.
         * It is protected to allow a class that inherits from this class to
         *  use it for convenience.
         *
         * @return The dob connection set by the start method.
         */
        //Safir::Dob::Connection& GetDobConnection();

    private:

        // Implementation of message subscriber interface
        virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy);

        void Tokenize(const std::wstring&        str,
                      std::vector<std::wstring>& tokens,
                      const std::wstring&        delimiters = L" \t\n");

        Safir::Dob::SecondaryConnection m_connection;
        Backdoor * m_backdoor;

        bool m_started;
    };

}
}

#endif
