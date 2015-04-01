// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

package com.saabgroup.safir.application;


public class BackdoorKeeper
    implements  com.saabgroup.safir.dob.MessageSubscriber
{
    /**
     * Constructor.
     *
     * @param connection The connection on which the keeper will subscribe
     *                   to backdoor messages.
     */
    public BackdoorKeeper(com.saabgroup.safir.dob.ConnectionBase connection) 
    {
        if (connection == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("You must pass a valid connection");
        }

        m_connection = connection;
    }

    /**
     * Starts subscription for backdoor commands to be sent to the Backdoor.
     *
     * The connection that was passed in the constructor must be opened before Start is called.
     *
     * If the connection is closed and opened again (maybe in a different context)
     * this method must be called again to establish the subscription.
     *
     * The class supports restarting/pausing by calling stop and then start again.
     *
     * @param backdoor [in] - Class that implements the Backdoor interface.
     * @exception Safir::Dob::NotOpenException 'Start' was called before connect to Dob.
     */
    public void start(Backdoor backdoor)
    {
        if (backdoor == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("You must pass a valid backdoor");
        }

        stop();

        m_backdoor = backdoor;

        m_connection.subscribeMessage(com.saabgroup.safir.application.BackdoorCommand.ClassTypeId,
                                      new com.saabgroup.safir.dob.typesystem.ChannelId(),
                                      this);
        m_started = true;
    }

    public void stop()
    {
        if (!m_started)
        {
            // Never started, just return
            return; // *** RETURN ***
        }

        if (!m_connection.isOpen())
        {
            // Connection has been closed.
            return;
        }

        m_connection.unsubscribeMessage(com.saabgroup.safir.application.BackdoorCommand.ClassTypeId,
                                        com.saabgroup.safir.dob.typesystem.ChannelId.ALL_CHANNELS,
                                        this);
        m_backdoor = null;
        m_started = false;
    }

    public boolean isStarted()
    {
        return m_started;
    }

    public void onMessage(com.saabgroup.safir.dob.MessageProxy messageProxy)
    {
        com.saabgroup.safir.application.BackdoorCommand cmd =
            (com.saabgroup.safir.application.BackdoorCommand)messageProxy.getMessage();

        try
        {
            if (!cmd.nodeName().isNull())
            {
                java.util.regex.Pattern pattern = java.util.regex.Pattern.compile
                    (cmd.nodeName().getVal(),
                     java.util.regex.Pattern.CASE_INSENSITIVE);
                java.util.regex.Matcher matcher = pattern.matcher
                    (com.saabgroup.safir.dob.ThisNodeParameters.getName());
                if (!matcher.find())
                {
                    // Node name doesn't match
                    return;  // *** RETURN ***
                }
            }

            if (!cmd.connectionName().isNull())
            {
                java.util.regex.Pattern pattern = java.util.regex.Pattern.compile
                    (cmd.connectionName().getVal(),java.util.regex.Pattern.CASE_INSENSITIVE);
                java.util.regex.Matcher matcher = pattern.matcher
                    (new com.saabgroup.safir.dob.ConnectionAspectMisc(m_connection).getConnectionName());
                if (!matcher.find())
                {
                    // Connection name doesn't match
                    return;  // *** RETURN ***
                }
            }
        }
        catch (java.util.regex.PatternSyntaxException exc)
        {
            // An invalid regular expression was used, skip this command
            return;  // *** RETURN ***
        }


        if (cmd.command().isNull())
        {
            // No command given
            return;  // *** RETURN ***
        }

        String[] cmdTokens = cmd.command().getVal().split("\\s+");

        if (cmdTokens.length > 0)
        {
            if (cmdTokens[0].compareToIgnoreCase("ping") == 0)
            {
                // It's a 'ping' command. Answer it without bothering the subclass
                // implementator.
                com.saabgroup.safir.Logging.sendSystemLog
                    (com.saabgroup.safir.Logging.Severity.DEBUG,
                     "Ping reply from "
                     + new com.saabgroup.safir.dob.ConnectionAspectMisc(m_connection).getConnectionName()
                     + " on node "
                     + com.saabgroup.safir.dob.ThisNodeParameters.getName());

                return; // *** RETURN ***
            }
            else if (cmdTokens[0].compareToIgnoreCase("help") == 0)
            {
                // Get help text from subclass implementator.
                com.saabgroup.safir.Logging.sendSystemLog(com.saabgroup.safir.Logging.Severity.DEBUG,
                                                          m_backdoor.getHelpText());

                return; // *** RETURN ***
            }
        }

        // Let the subclass handle the command
        m_backdoor.handleCommand(cmdTokens);
    }

    private com.saabgroup.safir.dob.ConnectionBase m_connection;
    private Backdoor m_backdoor = null;

    private boolean m_started = false;
}
