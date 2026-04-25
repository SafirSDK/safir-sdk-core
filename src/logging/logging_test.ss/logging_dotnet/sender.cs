/* *****************************************************************************
*
* Copyright Saab AB, 2013, 2026 (http://safirsdkcore.com)
*
* Created by: Anders Widén <anders.widen@consoden.se>
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

class Sender
{
    static int Main(String[] args)
    {
        Safir.Logging.SendSystemLog(Safir.Logging.Severity.Emergency, "This is an emergency log. Bryn\u00e4s \u00e4r b\u00e4st!\u2620");
        Safir.Logging.SendSystemLog(Safir.Logging.Severity.Alert, "This is an alert log");
        Safir.Logging.SendSystemLog(Safir.Logging.Severity.Critical, "This is a critical log");
        Safir.Logging.SendSystemLog(Safir.Logging.Severity.Error, "This is an error log");
        Safir.Logging.SendSystemLog(Safir.Logging.Severity.Warning, "This is a warning log");
        Safir.Logging.SendSystemLog(Safir.Logging.Severity.Notice, "This is a notice log");
        Safir.Logging.SendSystemLog(Safir.Logging.Severity.Informational, "This is an informational log");
        Safir.Logging.SendSystemLog(Safir.Logging.Severity.Debug, "This is a debug log");

        // Test convenience methods
        Safir.Logging.SendEmergency("This is another emergency log using convenience method");
        Safir.Logging.SendAlert("This is another alert log using convenience method");
        Safir.Logging.SendCritical("This is another critical log using convenience method");
        Safir.Logging.SendError("This is another error log using convenience method");
        Safir.Logging.SendWarning("This is another warning log using convenience method");
        Safir.Logging.SendNotice("This is another notice log using convenience method");
        Safir.Logging.SendInformational("This is another informational log using convenience method");
        Safir.Logging.SendDebug("This is another debug log using convenience method");

        return 0;
    }
}
