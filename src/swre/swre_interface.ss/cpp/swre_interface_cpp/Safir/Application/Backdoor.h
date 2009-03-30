/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#ifndef __SWRE_BACKDOOR_H__
#define __SWRE_BACKDOOR_H__

#include <string>
#include <vector>

namespace Safir
{
namespace Application
{
    /**
     * Interface for handling PI commands.
     * Provides the interface needed for classes that want to handle PI commands.
     * The class must use a BackdoorKeeper class to set up the subscriptions, and
     * pass "itself" to its Start-routine. After this the abstract methods in the class
     * will be called when a PI command is received.
     */
    class Backdoor
    {
    public:
        /**
         * Destructor.
         */
        virtual ~Backdoor() {}

        /**
         * Called when a 'Program Info' command aimed for this handler is received.
         * To be implemented by subclasses.
         *
         * @param cmdTokens [in] - Tokenized command string.
        */
        virtual void HandleCommand(const std::vector<std::wstring>& cmdTokens) = 0;

        /**
         * Called when a 'help' command aimed for this handler is received.
         * To be implemented by subclasses.
         *
         * @return Help text, typically describing supported Program Info commands.
         */
        virtual std::wstring GetHelpText() = 0;
    };

}
}

#endif
