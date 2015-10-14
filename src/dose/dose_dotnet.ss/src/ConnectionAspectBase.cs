/* ****************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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

using System;

namespace Safir.Dob
{
    /// <summary>
    /// Base class for all aspects
    /// </summary>
    public class ConnectionAspectBase
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="connection">The connection to operate through.</param>
        protected ConnectionAspectBase(ConnectionBase connection) {m_ctrl = connection.ControllerId;}

        /// <summary>
        /// Get the id of the controller.
        /// </summary>
        protected int ControllerId
        {
            get { return m_ctrl; }
        }

        private int m_ctrl;
    }
}
