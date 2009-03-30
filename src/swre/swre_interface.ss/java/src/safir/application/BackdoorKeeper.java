/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

package safir.application;


public class BackdoorKeeper
    implements  safir.dob.MessageSubscriber
{
    
    
    
    public BackdoorKeeper() { }
  
    public void start(Backdoor backdoor)
    {
        if (m_started)
        {
            //already started
            return;
        }

        if (backdoor == null)
        {
            throw new safir.dob.typesystem.SoftwareViolationException("You must pass a valid backdoor");
        }
        m_backdoor = backdoor;

        m_connection.attach();
            
        m_connection.subscribeMessage(m_piCmdObjId, this);
        m_started = true;
    }

    
    
    
    
    public void stop()
    {
        if (!m_started)
        {
            // Never started, just return
            return; // *** RETURN ***
        }

        m_connection.unsubscribeMessage(m_piCmdObjId, this);
        m_connection.detach();
        m_backdoor = null;
        m_started = false;
    }

    public void onMessage(safir.dob.Message message)
    {
        if (!message.isOfType(safir.application.BackdoorCommand.ClassTypeId))
        {
            // Unexpected message
            return;   // *** RETURN ***
        }

        safir.application.BackdoorCommand cmd = new safir.application.BackdoorCommand(message);

        try
        {
            if (!cmd.isNullNodeName())
            {
                java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(cmd.getNodeName(),java.util.regex.Pattern.CASE_INSENSITIVE);
                java.util.regex.Matcher matcher = pattern.matcher(safir.dob.NodeParameters.getNodes(safir.dob.ThisNodeParameters.getNodeNumber()).getNodeName());
                if (!matcher.find())
                {
                    // Node name doesn't match
                    return;  // *** RETURN ***
                }
            }

            if (!cmd.isNullConnectionName())
            {
                java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(cmd.getConnectionName(),java.util.regex.Pattern.CASE_INSENSITIVE);
                java.util.regex.Matcher matcher = pattern.matcher(m_connection.getConnectionName());
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
         

        if (cmd.isNullCommand())
        {
            // No command given
            return;  // *** RETURN ***
        }
        
        String[] cmdTokens = cmd.getCommand().split("\\s+");
        
        if (cmdTokens.length > 0)
        {
            if (cmdTokens[0].compareToIgnoreCase("ping") == 0)
            {
                // It's a 'ping' command. Answer to it without bothering
                // the subclass implementator.

                safir.swreports.SwReport.SendProgramInfoReport("Ping reply");

                return; // *** RETURN ***
            }
            else if (cmdTokens[0].compareToIgnoreCase("help") == 0)
            {
                // Get help text from subclass implementator.
                safir.swreports.SwReport.SendProgramInfoReport(m_backdoor.getHelpText());

                return; // *** RETURN ***
            }
        }
        
        // Let the subclass handle the command
        m_backdoor.handleCommand(cmdTokens);
    }

    private final safir.dob.typesystem.ObjectId m_piCmdObjId = 
        new safir.dob.typesystem.ObjectId(safir.application.BackdoorCommand.ClassTypeId, safir.dob.typesystem.Constants.WHOLE_CLASS);

    private safir.dob.SecondaryConnection m_connection = new safir.dob.SecondaryConnection();
    private Backdoor m_backdoor = null;

    private boolean m_started = false;
}
