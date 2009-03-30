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

using System;
using System.Windows.Forms;

namespace VehicleMmiCsWinForms
{
    /// <summary>
    /// Implements class that subscribes for VehicleMsg and shows a dialog 
    /// with the text received the message.
    /// </summary>
    public partial class MessageDialog : Form
        //StartRemoveInExercise
        ,
        // Allows this class to subscribe for messages
        Safir.Dob.MessageSubscriber
        //StopRemoveInExercise
    {
        ////////////////////////////////////////
        // Private members and functions

        /// <summary>
        /// Secondary Dob connection
        /// </summary>
        private Safir.Dob.SecondaryConnection m_secDobConnection;

        /// <summary>
        /// Cancel button pressed by operator, close the dialog 
        /// </summary>
        private void buttonClose_Click(object sender, EventArgs e)
        {
            this.Hide();
        }
        ////////////////////////////////////////////////
        // Public members and functions

        /// <summary>
        /// Default constructor, initialize component and attach to the Dob
        /// </summary>
        public MessageDialog()
        {
            InitializeComponent();
            m_secDobConnection = new Safir.Dob.SecondaryConnection();
            m_secDobConnection.Attach();
            //StartRemoveInExercise
            m_secDobConnection.SubscribeMessage(Capabilities.Vehicles.VehicleMsg.ClassTypeId,
                new Safir.Dob.Typesystem.ChannelId(), this);
            //StopRemoveInExercise
        }


        //StartRemoveInExercise
        /// <summary>
        /// Overrides Safir.Dob.MessageSubscriber. Called by the Dob 
        /// when a message is sent that is subscribed for.
        /// </summary>
        public void OnMessage(Safir.Dob.MessageProxy messageProxy)
        {
            Capabilities.Vehicles.VehicleMsg msg = (Capabilities.Vehicles.VehicleMsg)messageProxy.Message;

            if (!msg.MessageText.IsNull())
            {
                labelMessage.Text = msg.MessageText.Val.ToString();
            }
            else
            {
                labelMessage.Text = "No text in received message";
            }
            this.Show();
        }
        //StopRemoveInExercise
    }
}
