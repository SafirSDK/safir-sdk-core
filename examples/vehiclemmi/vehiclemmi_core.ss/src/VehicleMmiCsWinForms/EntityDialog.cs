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
    /// Implements dialog to present detailed information of vehicles.
    /// The dialog sends create and update requests for vehicles.
    /// </summary> 
    public partial class EntityDialog :
        Form,
        // Allows this class to receive response on sent requests
        Safir.Dob.Requestor
    {
        ///////////////////////////////////
        // Private members and functions    

        /// <summary>
        /// Vehicle data in dialog read from the Dob.
        /// </summary>
        private Capabilities.Vehicles.Vehicle m_vehicle;

        /// <summary>
        /// Secondary Dob connection
        /// </summary>
        private Safir.Dob.SecondaryConnection m_secDobConnection;

        /// <summary>
        /// Indicates if apply or OK button pressed
        /// </summary>
        private bool m_apply = false;

        /// <summary>
        /// Indicates if operator made create or update Vehicle choice
        /// </summary>
        private bool m_create = true;

        /// <summary>
        /// Send a create request for Vehicle       
        /// </summary>
        private void SendCreateRequest()
        {
            Capabilities.Vehicles.Vehicle vehicle = new Capabilities.Vehicles.Vehicle();

            // Identification
            if (textBoxIdentification.Text == "")
            {
                statusStrip.Items["toolStripStatus"].Text = "Identification must be given.";
                return;
            }
            else
            {
                vehicle.Identification.Val = textBoxIdentification.Text;
            }

            // Category 
            int sel = comboBoxCategory.SelectedIndex;
            if (sel == -1)
            {
                vehicle.VehicleCategory.SetNull();
            }
            else
            {
                vehicle.VehicleCategory.Val = (Capabilities.Vehicles.VehicleCategoryCode.Enumeration)sel;
            }

            //StartRemoveInExercise5
            // Speed
            if (textBoxSpeed.Text == "")
            {
                vehicle.Speed.SetNull();
            }
            else
            {
                try
                {
                    vehicle.Speed.Val = float.Parse(textBoxSpeed.Text);
                }
                catch
                {
                    statusStrip.Items["toolStripStatus"].Text = "Illegal speed format!";        
                }
            }
            //StopRemoveInExercise5

            // Position
            Safir.Geodesy.Position pos = new Safir.Geodesy.Position();

            try
            {
                if (textBoxPosLat.Text == "" || textBoxPosLong.Text == "")
                {
                    vehicle.Position.SetNull();
                }
                else
                {
                    pos.Latitude.Val = float.Parse(textBoxPosLat.Text);
                    pos.Longitude.Val = float.Parse(textBoxPosLong.Text);
                    pos.Altitude.Val = Safir.Geodesy.Position.DummyAltitude;

                    vehicle.Position.Obj = pos;
                }
            }
            catch (System.FormatException)
            {
                statusStrip.Items["toolStripStatus"].Text = "Position must be a float";
                return;
            }

            //StartRemoveInExercise5
            try
            {
                m_secDobConnection.CreateRequest(vehicle, 
                    new Safir.Dob.Typesystem.HandlerId(), this);
            }
            catch (Safir.Dob.OverflowException)
            {
                statusStrip.Items["toolStripStatus"].Text = "Overflow when sending, please wait!";
                return;
            }
            //StopRemoveInExercise5
        }


        /// <summary>
        /// Send an update request for Vehicle      
        /// </summary>
        private void SendUpdateRequest()
        {
            Capabilities.Vehicles.Vehicle vehicle = new Capabilities.Vehicles.Vehicle();

            // Get values that can be changed at update
            // Category
            int sel = comboBoxCategory.SelectedIndex;
            if (sel == -1)
            {
                vehicle.VehicleCategory.SetNull();
            }
            else
            {
                vehicle.VehicleCategory.Val = (Capabilities.Vehicles.VehicleCategoryCode.Enumeration)sel;
            }

            //StartRemoveInExercise5
            // Speed
            if (textBoxSpeed.Text == "")
            {
                vehicle.Speed.SetNull();
            }
            else
            {
                try
                {
                    vehicle.Speed.Val = float.Parse(textBoxSpeed.Text);
                }
                catch
                {
                    statusStrip.Items["toolStripStatus"].Text = "Illegal speed format!";
                    return;        
                }
            }
            //StopRemoveInExercise5

            // Position
            Safir.Geodesy.Position pos = new Safir.Geodesy.Position();

            try
            {
                if (textBoxPosLat.Text == "" || textBoxPosLong.Text == "")
                {
                    vehicle.Position.SetNull();
                }
                else
                {
                    pos.Latitude.Val = float.Parse(textBoxPosLat.Text);
                    pos.Longitude.Val = float.Parse(textBoxPosLong.Text);
                    pos.Altitude.Val = Safir.Geodesy.Position.DummyAltitude;

                    vehicle.Position.Obj = pos;
                }
            }
            catch (FormatException)
            {
                statusStrip.Items["toolStripStatus"].Text = "Position must be a float";
                return;
            }

            // Send request
            //StartRemoveInExercise5
            try
            {
                m_secDobConnection.UpdateRequest(vehicle, 
                   new Safir.Dob.Typesystem.InstanceId(textBoxIdentification.Text), this);
            }
            catch (Safir.Dob.OverflowException)
            {
                statusStrip.Items["toolStripStatus"].Text = "Overflow when sending, please wait!";
            }
            //StopRemoveInExercise5
        }

        /// <summary>
        /// Cancel button pressed by operator, closes the dialog 
        /// </summary>
        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.Hide();
        }


        private void buttonOk_Click(object sender, EventArgs e)
        {
            m_apply = false;
            if (m_create)
            {
                SendCreateRequest();
            }
            else
            {
                SendUpdateRequest();
            }
        }

        /// <summary>
        /// Apply button pressed by operator. 
        /// Send a create or update request.
        /// </summary>
        private void buttonApply_Click(object sender, EventArgs e)
        {
            m_apply = true;
            if (m_create)
            {
                SendCreateRequest();
            }
            else
            {
                SendUpdateRequest();
            }
        }


        ///////////////////////////////////
        // Public members and functions    

        /// <summary>
        /// Constructor, initialize dialog and attach to the Dob.
        /// </summary>
        public EntityDialog()
        {
            InitializeComponent();
            m_secDobConnection = new Safir.Dob.SecondaryConnection();
            m_secDobConnection.Attach();

            // Fill combo-box with values for vehicle category
            foreach (int i in Enum.GetValues(typeof(Capabilities.Vehicles.VehicleCategoryCode.Enumeration)))
            {
                comboBoxCategory.Items.Add(((Capabilities.Vehicles.VehicleCategoryCode.Enumeration)(i)).ToString());
            }
        }


        /// <summary>
        /// Open dialog to create a new vehicle
        /// </summary>
        public void CreateVehicle()
        {
            this.Text = "Create Vehicle";
            ClearAllControls();
            textBoxIdentification.Focus();
            m_create = true;
            this.Show();
        }

        /// <summary>
        /// Open dialog to update a vehicle
        /// </summary>
        public bool UpdateVehicle(Safir.Dob.Typesystem.EntityId entityId)
        {
            this.Text = "Update vehicle";
            ClearAllControls();
            textBoxIdentification.ReadOnly = true; // Not allowed to change identification
            comboBoxCategory.Focus();
            m_create = false;

            try
            {
                using (Safir.Dob.EntityProxy entityProxy = m_secDobConnection.Read(entityId))
                {
                    m_vehicle = (Capabilities.Vehicles.Vehicle)entityProxy.Entity;
                }
            }
            catch (Safir.Dob.NotFoundException)
            {
                // The specified instance of the entity does not exist
                return false;
            }

            if (!m_vehicle.Identification.IsNull())
            {
                textBoxIdentification.Text = m_vehicle.Identification.Val.ToString();
            }

            if (!m_vehicle.VehicleCategory.IsNull())
            {
                comboBoxCategory.SelectedItem = m_vehicle.VehicleCategory.Val.ToString();
            }

            if (!m_vehicle.Speed.IsNull())
            {
                textBoxSpeed.Text = m_vehicle.Speed.Val.ToString();
            }
            else
            {
                textBoxSpeed.Text = "";
            }

            if (!m_vehicle.Position.IsNull())
            {
                // Position (latitude)
                if (!m_vehicle.Position.Obj.Latitude.IsNull())
                {
                    textBoxPosLat.Text = m_vehicle.Position.Obj.Latitude.Val.ToString();
                }

                // Position (longitude)
                if (!m_vehicle.Position.Obj.Longitude.IsNull())
                {
                    textBoxPosLong.Text = m_vehicle.Position.Obj.Longitude.Val.ToString();
                }
            }

            this.Show();

            return true;
        }

        /// <summary>
        /// Set all controls valid
        /// </summary>
        public void ClearAllControls()
        {
            textBoxIdentification.ReadOnly = false;
            textBoxIdentification.Text = "";
            textBoxSpeed.Text = "";
            textBoxPosLat.Text = "";
            textBoxPosLong.Text = "";
            comboBoxCategory.SelectedItem = Capabilities.Vehicles.VehicleCategoryCode.Enumeration.Car.ToString();
            statusStrip.Items["toolStripStatus"].Text = "OK";
        }


        /// <summary>
        /// Overrides Safir.Dob.Requestor. Response for sent request.
        /// </summary>
        public void OnResponse(Safir.Dob.ResponseProxy responseProxy)
        {
            if (responseProxy.IsSuccess)
            {
                if (m_apply)
                {
                    statusStrip.Items["toolStripStatus"].Text = "OK";
                }
                else
                {
                    this.Hide();
                }
            }
            //StartRemoveInExercise6
            else 
            {
                Safir.Dob.ErrorResponse errorResponse = (Safir.Dob.ErrorResponse)responseProxy.Response;
                //if (errorResponse.AdditionalInfo.IsNull())
                if(errorResponse.Code.IsNull())
                {
                    // No error code.
                    statusStrip.Items["toolStripStatus"].Text = "Unspecified error";
                }
                else
                {
                    // Error code specified.
                    if (errorResponse.Code.Val == Safir.Dob.ResponseGeneralErrorCodes.SafirReqErr)
                    {
                        statusStrip.Items["toolStripStatus"].Text = "General request error";
                    }
                }
            }
            //StopRemoveInExercise6
        }

        /// <summary>
        /// Overrides Safir.Dob.Requestor. Notification that overflow situatuion is solved
        /// </summary>
        public void OnNotRequestOverflow()
        {
            // No automatic resending is made. 
            statusStrip.Items["toolStripStatus"].Text = "Overflow situation solved. Make request again!";
        }
    }
}
