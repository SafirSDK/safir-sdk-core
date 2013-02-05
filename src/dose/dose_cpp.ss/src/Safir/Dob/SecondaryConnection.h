/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#ifndef _SAFIR_DOB_SECONDARY_CONNECTION_H
#define _SAFIR_DOB_SECONDARY_CONNECTION_H

#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionBase.h>
#include <Safir/Dob/Typesystem/Defs.h>

namespace Safir
{
namespace Dob
{
    /**
     * A secondary connection attached to a "real" connection.
     *
     * This class is used to attach yourself to an existing connection in the
     * same thread.
     * All attach calls ensure that you will get a connection that is valid in the current thread, but
     * each SecondaryConnection must still only be used from within one thread.
     */
    class DOSE_CPP_API SecondaryConnection : public ConnectionBase
    {
    public:
        /** Constructor.
         */
        SecondaryConnection();

        /** Destructor.
         */
        virtual ~SecondaryConnection();

        /**
         * Attach to a connection in this thread.
         *
         * This method will attach the SecondaryConnection to the first Connection that was
         * opened in this thread.
         *
         * This method can be used to let part of a program, for example a module or a dll, attach to an
         * existing open connection.
         *
         * @throws Safir::Dob::NotOpenException There is no open Connection in this thread.
         */
        void Attach();

        /**
         * Attach to a named connection in this thread.
         *
         * This method will attach the SecondaryConnection to the named Connection if that
         * Connection was opened in this thread.
         *
         * This method can be used to let part of a program, for example a module or a dll, attach to an
         * existing open connection.
         * The connection name parameters are used to identify the connection to attach to.
         * This connection must already be opened, otherwise an exception will be
         * thrown.
         *
         * @param [in] connectionNameCommonPart Name that identifies the connection but not any particular
         *                                        instance.
         * @param [in] connectionNameInstancePart Name that identifies a particular connection instance.
         *
         * @throws Safir::Dob::NotOpenException If the Connection instance we are trying to
         *                                      attach to is not open.
         */
        void Attach(const std::wstring& connectionNameCommonPart,
                    const std::wstring& connectionNameInstancePart);

        /**
         * Detach a SecondaryConnection.
         *
         * When a connection has been detached it can be attached again.
         */
        void Detach();

        /**
         * Check if a SecondaryConnection is attached to an open connection.
         *
         * @return True if the SecondaryConnection is attached to a Connection and that Connection is open.
         */
        bool IsAttached() const;

    private:

        //implementation of pure virtual
        virtual long GetControllerId() const;

        long m_ctrl;

        //Disable copying and assignment
        SecondaryConnection(const SecondaryConnection& d);
        SecondaryConnection& operator=(const SecondaryConnection& rhs);
    };

}
}

#endif
