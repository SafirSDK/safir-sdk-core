/******************************************************************************
*
* Copyright Saab AB, 2008-2009 (http://www.safirsdk.com)
*
* Created by: Petter Lönnstedt / stpeln
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

namespace VehicleAppCs
{
    /// <summary>
    /// This class sends the message. Make this class a singleton.
    /// The reason is that it is used from several classes in this
    /// application. 
    /// </summary>
    class MessageSender :
        Safir.Dob.MessageSender
    {
        // The one and only instance of this class.
        private static MessageSender m_Instance;

        // This class uses this secondary connection for Dob calls.
        private Safir.Dob.SecondaryConnection m_connection;

        /// <summary>
        /// Constructor.
        /// </summary>
        public MessageSender()
        {
            m_connection = new Safir.Dob.SecondaryConnection();
        }
    
        /// <summary>
        /// Returns the one and only instanse of this class.
        /// </summary>
        /// <returns>The instance.</returns>
        public static MessageSender Instance
        {
            get
            {
                if (m_Instance == null)
                    m_Instance = new MessageSender();
                return m_Instance;
            }
        }

        /// <summary>
        /// Initiates this class. Creates a secondary Dob connection.
        /// </summary>
        public void Init()
        {
            m_connection.Attach();  
        }
        
        //
        // Methods derived from Safir::Dob::ServiceHandler.
        //

        public void OnNotMessageOverflow()
        {
            // Retry to send the message.
            SendMaxNofVehicleMsg();
        }
        
        public void SendMaxNofVehicleMsg()
        {
            // This is just a test to see how you create messages.
            // This is not part of the design pattern.
            Capabilities.Vehicles.VehicleMsg msg =
                new Capabilities.Vehicles.VehicleMsg();
            
            msg.MessageText.Val = "Number of vehicles reached defined limit.";

            try
            {
                m_connection.Send(msg, new Safir.Dob.Typesystem.ChannelId(), this);
            }
            catch (Safir.Dob.OverflowException)
            {
                // Do nothing OnNotMessageOverflow() will be called when 
                // overflow situation solved.
            }
        }
    }
}
