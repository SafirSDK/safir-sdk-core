// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
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

public class Test {
    public static void main(String[] args) {
        try {
            com.saabgroup.safir.time.TimeProvider.getUtcTime();
            System.out.println("getUtcTime did not throw!");
            System.exit(1);
        }
        catch (com.saabgroup.safir.dob.typesystem.ConfigurationErrorException e) {
            // expected
        }

        try {
            com.saabgroup.safir.time.TimeProvider.getLocalTimeOffset();
            System.out.println("getLocalTimeOffset did not throw!");
            System.exit(1);
        }
        catch (com.saabgroup.safir.dob.typesystem.ConfigurationErrorException e) {
            // expected
        }

        System.out.println("Success");
        System.exit(0);
    }
}
