/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Erik Adolfsson / sterad
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
using System.Runtime.InteropServices;

namespace Safir.Time
{
    /// <summary>
    /// This helper class contains methods to convert time between DateTime and double format.
    /// </summary>
    public class TimeProvider
    {

        [DllImport("douf_time_library.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DoufTimeC_GetUtcTime")]
        internal static extern void GetUtcTime(out double utcTime, out byte success);
        [DllImport("douf_time_library.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DoufTimeC_GetLocalTimeOffset")]
        internal static extern void GetLocalTimeOffset(out Int32 offset, out byte success);

        private static System.DateTime _1_JAN_1970 = new DateTime( 1970, 1, 1 );

        /// <summary>
        /// Get current UTC time
        /// </summary>
        /// <returns>Current time in UTC (Seconds and fraction since jan 1 1970 00:00)</returns>
        public static double GetUtcTime()
        {
            // Get current Utc time
            double utcTime;
            byte success;
            GetUtcTime(out utcTime, out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                throw new Safir.Dob.Typesystem.ConfigurationErrorException("Configuration error in TimeProvider, please check your logs!");
            }
            return utcTime; 
        }

        /// <summary>
        /// Convert specified UTC time to a Double
        /// </summary>
        /// <param name='utcTime'>The UTC time</param>
        /// <returns>The seconds and fraction since jan 1 1970 00:00</returns>
        public static double ToDouble( System.DateTime utcTime )
        {
            // Convert time to seconds since 01-Jan-1970
            System.TimeSpan timeDiff = utcTime - _1_JAN_1970;
            long seconds = (long)timeDiff.TotalSeconds;
            double fraction = timeDiff.TotalSeconds - seconds;

            return seconds + fraction;
        }

        /// <summary>
        /// Converts local time to UTC time
        /// </summary>
        /// <param name='localTime'>The local time</param>
        /// <returns>The seconds and fraction since jan 1 1970 00:00</returns>
        public static double ToUtcTime( System.DateTime localTime )
        {
            int offset;
            byte success;
            GetLocalTimeOffset(out offset, out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                throw new Safir.Dob.Typesystem.ConfigurationErrorException("Configuration error in TimeProvider, please check your logs!");
            }

            // Convert time to seconds since 01-Jan-1970
            System.TimeSpan timeDiff = localTime - _1_JAN_1970;
            long seconds = (long)timeDiff.TotalSeconds;
            double fraction = timeDiff.TotalSeconds - seconds;

            return seconds + fraction - offset;
        }

        /// <summary>
        /// Gets specified UTC time in System.DateTime representation
        /// </summary>
        /// <param name='utcTime'>The UTC time</param>
        /// <returns>The UTC time stored in a System.DateTime object</returns>
        public static System.DateTime ToDateTime( double utcTime )
        {
            long seconds   = (long)utcTime;
            double fraction = utcTime - seconds;

            return _1_JAN_1970.AddTicks(
                System.TimeSpan.TicksPerSecond * seconds +
                (long)(System.TimeSpan.TicksPerMillisecond * 1000.0 * fraction + 0.5) ); // Micro resolution
        }

        /// <summary>
        /// Returns the local time
        /// </summary>
        /// <param name='utcTime'>The UTC time stamp</param>
        /// <returns>Local time</returns>
        public static System.DateTime ToLocalTime( double utcTime )
        {
            int offset;
            byte success;
            GetLocalTimeOffset(out offset, out success);
            if (!Safir.Dob.Typesystem.Internal.InternalOperations.BoolOf(success))
            {
                throw new Safir.Dob.Typesystem.ConfigurationErrorException("Configuration error in TimeProvider, please check your logs!");
            }

            // Convert seconds to localtime
            double localTime = utcTime + offset;

            return ToDateTime( localTime );
        }

    }
}
