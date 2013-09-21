// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
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

import com.saabgroup.safir.Logging;
import java.util.Locale;

/**
 * Test program that send system logs
 */
public class Sender {
    /**
     * @param args
     */
    public static void main(String[] args) {

        com.saabgroup.safir.Logging.sendSystemLog
            (com.saabgroup.safir.Logging.Severity.EMERGENCY,
             "This is an emergency log. Bryn\u00e4s \u00e4r b\u00e4st!\u2620");

        com.saabgroup.safir.Logging.sendSystemLog
            (com.saabgroup.safir.Logging.Severity.ALERT,
             "This is an alert log");

        com.saabgroup.safir.Logging.sendSystemLog
            (com.saabgroup.safir.Logging.Severity.CRITICAL,
             "This is a critical log");

        com.saabgroup.safir.Logging.sendSystemLog
            (com.saabgroup.safir.Logging.Severity.ERROR,
             "This is an error log");

        com.saabgroup.safir.Logging.sendSystemLog
            (com.saabgroup.safir.Logging.Severity.WARNING,
             "This is a warning log");

        com.saabgroup.safir.Logging.sendSystemLog
            (com.saabgroup.safir.Logging.Severity.NOTICE,
             "This is a notice log");

        com.saabgroup.safir.Logging.sendSystemLog
            (com.saabgroup.safir.Logging.Severity.INFORMATIONAL,
             "This is an informational log");

        com.saabgroup.safir.Logging.sendSystemLog
            (com.saabgroup.safir.Logging.Severity.DEBUG,
             "This is a debug log");

    }
}
