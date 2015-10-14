/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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
    /// Implements dialog to use service CalculateSpeedDifference.
    /// The dialog sends a service request and receives the
    /// service response.
    /// </summary>
    public partial class ServiceDialog :
        Form,
        // Allows this class to receive response on sent requests
        Safir.Dob.Requestor
    {
        ////////////////////////////////////////
        // Private members and functions

        /// <summary>
        /// Secondary Dob connection
        /// </summary>
        private Safir.Dob.SecondaryConnection m_secDobConnection;

        /// <summary>
        /// Vehicle data in dialog read from the Dob.
        /// </summary>
        private Capabilities.Vehicles.Vehicle m_vehicle;

        /// <summary>
        /// Cancel button pressed by operator, closes the dialog
        /// </summary>
        private void buttonClose_Click(object sender, EventArgs e)
        {
            this.Hide();
        }

        /// <summary>
        /// Apply button pressed by operator, send service request
        /// </summary>
        private void buttonApply_Click(object sender, EventArgs e)
        {
            //StartRemoveInExercise9
            if (textBoxCurrentSpeed.Text.Length == 0)
            {
                statusStrip.Items["toolStripStatus"].Text = "Current speed must exist.";
                return;
            }

            if (textboxNewSpeed.Text.Length == 0)
            {
                statusStrip.Items["toolStripStatus"].Text = "New speed must be given.";
                return;
            }

            // Create service request
            Capabilities.CalculateSpeedDifference req = new Capabilities.CalculateSpeedDifference();

            // Set service request values
            try
            {
                req.Speed.Val = float.Parse(textboxNewSpeed.Text);
            }
            catch
            {
                statusStrip.Items["toolStripStatus"].Text = "Illegal speed format!";
                return;
            }

            //this will cause the req object to point to m_vehicle, but that is okay since
            //the req object is never modified and is short-lived.
            req.ObjectWithSpeed.Obj = m_vehicle;

            try
            {
                // Send service request
                m_secDobConnection.ServiceRequest(req, new Safir.Dob.Typesystem.HandlerId(), this);
                statusStrip.Items["toolStripStatus"].Text = "OK";
            }
            catch(Safir.Dob.OverflowException)
            {
                statusStrip.Items["toolStripStatus"].Text = "Overflow when sending, please wait!";
            }
            //StopRemoveInExercise9
        }
        ////////////////////////////////////////////////
        // Public members and functions

        /// <summary>
        /// Default constructor, initialize component and attach to the Dob
        /// </summary>
        public ServiceDialog()
        {
            InitializeComponent();
            m_secDobConnection = new Safir.Dob.SecondaryConnection();
            m_secDobConnection.Attach();
        }

        /// <summary>
        /// Opens dialog to calculate speed difference
        /// </summary>
        public bool Open(Safir.Dob.Typesystem.EntityId entityId)
        {
            textBoxIdentification.Text = "";
            textBoxCurrentSpeed.Text = "";
            textboxNewSpeed.Text = "";
            textboxSpeedDiff.Text = "";
            statusStrip.Items["toolStripStatus"].Text = "OK";

            // Get data from Dob and fill dialog
            try
            {
                using (Safir.Dob.EntityProxy entityProxy = m_secDobConnection.Read(entityId))
                {
                    m_vehicle = (Capabilities.Vehicles.Vehicle)entityProxy.Entity;
                }
            }
            catch
            {
                // The specified instance of the entity does not exist
                return false;
            }

            if (!m_vehicle.Identification.IsNull())
            {
                textBoxIdentification.Text = m_vehicle.Identification.Val.ToString();
            }

            if (!m_vehicle.Speed.IsNull())
            {
                textBoxCurrentSpeed.Text = m_vehicle.Speed.Val.ToString();
            }
            else
            {
                statusStrip.Items["toolStripStatus"].Text = "Current speed must exist.";
            }

            this.Show();

            return true;
        }

        /// <summary>
        /// Overrides Safir.Dob.Requestor. Response for sent request
        /// </summary>
       public void OnResponse(Safir.Dob.ResponseProxy responseProxy)
        {
            if (responseProxy.IsSuccess)
            {
                Capabilities.CalculateSpeedDifferenceResponse response = (Capabilities.CalculateSpeedDifferenceResponse)responseProxy.Response;
                textboxSpeedDiff.Text = response.SpeedDifference.Val.ToString();
                statusStrip.Items["toolStripStatus"].Text = "OK";
            }
            else
            {
                statusStrip.Items["toolStripStatus"].Text = "Error in speed service request.";
            }
        }

        /// <summary>
        /// Overrides Safir.Dob.Requestor. Notification that overflow situatuion is solved
        /// </summary>
        public void OnNotRequestOverflow()
        {
            // No automatic resending is made.
            // Inform operator that requests can be made now.
            statusStrip.Items["toolStripStatus"].Text = "Overflow situation solved. Make request again!";
        }
    }
}
