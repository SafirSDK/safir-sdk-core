/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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

package com.saabgroup.safir.time;

/**
 * This helper class contains methods to convert time between Date and double format.
 */
public class TimeProvider {
    /**
     * Get current UTC time
     * @return Current time in UTC (Seconds and fraction since jan 1 1970 00:00)
     */
    public static double getUtcTime() {
        // Get current Utc time
        double utcTime = Library.getUtcTime();
        if (utcTime < 0) {
            throw new com.saabgroup.safir.dob.typesystem.ConfigurationErrorException("Configuration error in TimeProvider, please check your logs!");
        }
        return utcTime; 
    }

    /**
     * Get the local time offset.
     *
     * @return The offset in seconds between utc and local time zone.
     */
    public static int getLocalTimeOffset() {
        int offset = Library.getLocalTimeOffset();
        if (offset == -1) {
            throw new com.saabgroup.safir.dob.typesystem.ConfigurationErrorException("Configuration error in TimeProvider, please check your logs!");
        }

        return offset;        
    }

    /**
     * Convert specified UTC time to a Double
     *
     * @param utcTime The UTC time
     * @return The seconds (millisecond precision) since jan 1 1970 00:00
     */
    public static double ToDouble(java.util.Date utcTime) {
        return utcTime.getTime() / 1000.0;
    }

    /**
     * Gets specified UTC time in java.util.Date representation
     *
     * @param utcTime The UTC time
     * @return The UTC time stored in a java.util.Date object
     */
    public static java.util.Date ToDate(double utcTime) {
        return new java.util.Date((long)(utcTime * 1000));
    }
}
