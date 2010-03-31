/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagstr�m / stlrha
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
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;


namespace Safir.Application
{
    /// <summary>
    /// <para>Interface for handling PI commands.</para>
    /// <para>Provides the interface needed for classes that want to handle PI commands.
    /// The class must use a BackdoorKeeper class to set up the subscriptions, and
    /// pass "itself" to its Start-routine. After this the methods in the interface
    /// will be called when a PI command is received.</para>
    /// </summary>
    public interface Backdoor
    {
        /// <summary>
        /// Called when a 'Program Info' command aimed for this backdoor is received.
        /// To be implemented by subclasses.
        /// </summary>
        /// <param name="cmdTokens">Tokenized command string</param>
        void HandleCommand(string[] cmdTokens);

        /// <summary>
        /// Called when a 'help' command aimed for this backdoor is received.
        /// To be implemented by subclasses.
        /// </summary>
        string GetHelpText();
    };

    /// <summary>
    /// <para>Class that provides subscription and filtering for BackdoorCommands</para>
    /// <para>Use this class to "keep" the Backdoor. Call the Start routine with
    /// the backdoor and a dob connection to set up subscriptions and then your
    /// Backdoor class will be called every time there is a Command aimed for that
    /// connection.</para>
    /// </summary>
    public class BackdoorKeeper : Safir.Dob.MessageSubscriber
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public BackdoorKeeper() { }

        /// <summary>
        /// Starts subscription for Program Information commands to be sent to the Backdoor.
        /// <para>This method can be called several times, but only the first call does anything.
        /// The class supports restarting/pausing by calling stop and then start again.</para>
        /// </summary>
        /// <param name="backdoor">PI command backdoor</param>
        /// <exception cref=" Safir.Dob.NotOpenException">
        /// Programming Error, 'Start' called before connect to Dose
        /// </exception>
        public void Start(Backdoor backdoor)
        {
            if (m_started)
            {
                //already started
                return;
            }

            if (backdoor == null)
            {
                throw new Dob.Typesystem.SoftwareViolationException("You must pass a valid backdoor");
            }
            m_backdoor = backdoor;

            m_connection.Attach();

            m_connection.SubscribeMessage(m_piCmdTypeId, m_piCmdChannelId, this);
            m_started = true;
        }

        /// <summary>
        /// Stops subscription for Program Information commands.
        /// <para>The backdoor can be started again by calling start after it has been stopped.</para>
        /// </summary>
        public void Stop()
        {
            if (!m_started)
            {
                // Never started, just return
                return; // *** RETURN ***
            }

            if (!m_connection.IsAttached())
            {
                // Connection has been closed.
                return;
            }

            m_connection.UnsubscribeMessage(m_piCmdTypeId, m_piCmdChannelId, this);
            m_connection.Detach();
            m_backdoor = null;
            m_started = false;
        }

        void Safir.Dob.MessageSubscriber.OnMessage(Safir.Dob.MessageProxy messageProxy)
        {
            Safir.Dob.Message message = messageProxy.Message;

            Safir.Application.BackdoorCommand cmd = message as Safir.Application.BackdoorCommand;

            if (cmd == null)
            {
                // Unexpected message
                return;   // *** RETURN ***
            }

            try
            {
                if (!cmd.NodeName.IsNull())
                {
                    if (!Regex.IsMatch(Safir.Dob.NodeParameters.Nodes(Safir.Dob.ThisNodeParameters.NodeNumber).NodeName.Val,
                                       cmd.NodeName.Val,
                                       RegexOptions.IgnoreCase))
                    {
                        // Node name doesn't match
                        return;  // *** RETURN ***
                    }
                }

                Safir.Dob.ConnectionAspectMisc connectionAspectMisc = new Safir.Dob.ConnectionAspectMisc(m_connection);
                if (!cmd.ConnectionName.IsNull())
                {
                    if (!Regex.IsMatch(connectionAspectMisc.GetConnectionName(),
                                       cmd.ConnectionName.Val,
                                       RegexOptions.IgnoreCase))
                    {
                        // Connection name doesn't match
                        return;  // *** RETURN ***
                    }
                }
            }
            catch (ArgumentException)
            {
                // An invalid regular expression was used, skip this command
                return;  // *** RETURN ***
            }


            if (cmd.Command.IsNull())
            {
                // No command given
                return;  // *** RETURN ***
            }

            string[] cmdTokens =
                cmd.Command.Val.Split(new char[] { ' ', '\t', '\n', '\r' }, StringSplitOptions.RemoveEmptyEntries);


            if (cmdTokens.Length > 0)
            {
                if (cmdTokens[0].Equals("ping", StringComparison.OrdinalIgnoreCase))
                {
                    // It's a 'ping' command. Answer to it without bothering
                    // the subclass implementator.

                    Safir.SwReports.SwReport.SendProgramInfoReport("Ping reply");

                    return; // *** RETURN ***
                }
                else if (cmdTokens[0].Equals("help", StringComparison.OrdinalIgnoreCase))
                {
                    // Get help text from subclass implementator.
                    Safir.SwReports.SwReport.SendProgramInfoReport(m_backdoor.GetHelpText());

                    return; // *** RETURN ***
                }
            }

            // Let the subclass handle the command
            m_backdoor.HandleCommand(cmdTokens);
        }

        private readonly Int64 m_piCmdTypeId = Safir.Application.BackdoorCommand.ClassTypeId;
        private readonly Safir.Dob.Typesystem.ChannelId m_piCmdChannelId = new Safir.Dob.Typesystem.ChannelId();
        private Safir.Dob.SecondaryConnection m_connection = new Safir.Dob.SecondaryConnection();
        private Backdoor m_backdoor = null;

        private bool m_started = false;
    }
}
