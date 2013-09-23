/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#ifndef _SAFIR_DOB_CONNECTION_ASPECT_BASE_H
#define _SAFIR_DOB_CONNECTION_ASPECT_BASE_H

#include <Safir/Dob/ConnectionBase.h>
#include <Safir/Dob/DoseCppExportDefs.h>

namespace Safir
{
namespace Dob
{
    /**
     * Base class for all aspects
     */
    class DOSE_CPP_API ConnectionAspectBase
    {
    protected:

        /** 
         * Constructor.
         *
         * @param [in] connection The connection to operate through.
         */
        ConnectionAspectBase(const ConnectionBase& connection):m_ctrl(connection.GetControllerId()) {}

        /** Destructor.
         */
        virtual ~ConnectionAspectBase() {}

        /** Get the id of the controller. */
        long GetControllerId() const {return m_ctrl;}

    private:
        long m_ctrl;
    };
}
}

#endif
