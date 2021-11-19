// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2021 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
import java.lang.ref.Cleaner;

/**
 * Holds a Cleaner instance for use by the package.
 */
final class ResourceHelper
{
    /**
     * Register an action for cleaning. See java.lang.ref.Cleaner for more info.
     *
     * @return A cleanable.
     */
    public static Cleaner.Cleanable register(Object obj, Runnable action)
    {
        return m_cleaner.register(obj,action);
    }

    private static final Cleaner m_cleaner = Cleaner.create();

    /** This class is not meant to be instantiated */
    private ResourceHelper() {
    }
}
