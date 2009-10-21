// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

package com.saabgroup.safir.dob;
/**
 * Base class for all aspects
 */
public class ConnectionAspectBase{

    /**
     * Constructor
     *
     * @param connection The connection to operate through.
     */
    protected ConnectionAspectBase(ConnectionBase connection) {m_ctrl = connection.getControllerId();}

    /**
     * Get the id of the controller.
     */
    protected int getControllerId() {
        return m_ctrl;
    }

    private int m_ctrl;
}
