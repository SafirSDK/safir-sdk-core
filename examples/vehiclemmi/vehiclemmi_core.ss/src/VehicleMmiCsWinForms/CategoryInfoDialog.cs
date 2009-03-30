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
    public partial class CategoryInfoDialog :
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
        /// Send a service request for category       
        /// </summary>
        private void SendServiceRequest()
        {
            Capabilities.Vehicles.SetVehicleCategoryService req = new Capabilities.Vehicles.SetVehicleCategoryService();
            Capabilities.Vehicles.VehicleCategoryInfo vehicleCategoryInfo = new Capabilities.Vehicles.VehicleCategoryInfo();

            req.VehicleCategoryInfo.Obj = vehicleCategoryInfo;

            try
            {
                req.VehicleCategoryInfo.Obj.VehicleCategory.Val = (Capabilities.Vehicles.VehicleCategoryCode.Enumeration)Enum.Parse(typeof(Capabilities.Vehicles.VehicleCategoryCode.Enumeration), textBoxCategoryCode.Text);
            }
            catch
            {
                statusStrip.Items["toolStripStatus"].Text = "Illegal category information format!";
            }

            if (textBoxSpeed.Text == "")
            {
                req.VehicleCategoryInfo.Obj.MaxSpeed.SetNull();
            }
            else
            {
                try
                {
                    req.VehicleCategoryInfo.Obj.MaxSpeed.Val = float.Parse(textBoxSpeed.Text);
                }
                catch
                {
                    statusStrip.Items["toolStripStatus"].Text = "Illegal speed format!";
                }
            }
            
            if (textboxAdditionalRemark.Text == "")
            {
                req.VehicleCategoryInfo.Obj.Remark.SetNull();
            }
            else
            {
                req.VehicleCategoryInfo.Obj.Remark.Val = textboxAdditionalRemark.Text;
            }

            req.VehicleCategoryInfo.Obj.IsDrivingLicenceRequired.Val = checkBoxDriversLicenseReq.Checked;

            // Send the request.
            try
            {
                m_secDobConnection.ServiceRequest(req, new Safir.Dob.Typesystem.HandlerId(), this); 
            }
            catch(Safir.Dob.OverflowException)
            {
                statusStrip.Items["toolStripStatus"].Text = "Overflow when sending, please wait!";
            }
        }

        /// <summary>
        /// Cancel button pressed by operator, closes the dialog 
        /// </summary>
        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.Hide();
        }

        private void buttonApply_Click(object sender, EventArgs e)
        {
            m_apply = false;
            SendServiceRequest();
        }

        ///////////////////////////////////
        // Public members and functions    

        /// <summary>
        /// Constructor, initialize dialog and attach to the Dob.
        /// </summary>
        public CategoryInfoDialog()
        {
            InitializeComponent();
            m_secDobConnection = new Safir.Dob.SecondaryConnection();
            m_secDobConnection.Attach();
        }

        /// <summary>
        /// Opens dialog for category information
        /// </summary>
        public bool Open(Safir.Dob.Typesystem.EntityId entityId)
        {
            ClearAllControls();

            try
            {
                // Get data from Dob and fill dialog
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

            if (!m_vehicle.VehicleCategory.IsNull())
            {
                textBoxCategoryCode.Text = m_vehicle.VehicleCategory.Val.ToString();

                Capabilities.Vehicles.GetVehicleCategoryService req = new Capabilities.Vehicles.GetVehicleCategoryService();
                req.VehicleCategory.Val = m_vehicle.VehicleCategory.Val;
                try
                {
                    // Send service request
                    m_secDobConnection.ServiceRequest(req, new Safir.Dob.Typesystem.HandlerId(), this);
                    statusStrip.Items["toolStripStatus"].Text = "OK";
                }
                catch (Safir.Dob.OverflowException)
                {
                    statusStrip.Items["toolStripStatus"].Text = "Overflow when sending, please wait!";
                }

                this.Show();

                return true;
            }

            return false;
        }

        /// <summary>
        /// Set all controls valid
        /// </summary>
        public void ClearAllControls()
        {
            textBoxCategoryCode.Text = "";
            textBoxSpeed.Text = "";
            textboxAdditionalRemark.Text = "";
            checkBoxDriversLicenseReq.Checked = false;
            statusStrip.Items["toolStripStatus"].Text = "OK";
        }

        /// <summary>
        /// Overrides Safir.Dob.Requestor. Response for sent request
        /// </summary>
        public void OnResponse(Safir.Dob.ResponseProxy responseProxy)
        {
            if (responseProxy.RequestTypeId == Capabilities.Vehicles.GetVehicleCategoryService.ClassTypeId)
            {
                if (responseProxy.IsSuccess)
                {
                    Capabilities.Vehicles.GetVehicleCategoryResponse response = (Capabilities.Vehicles.GetVehicleCategoryResponse)responseProxy.Response;
                    
                    if (!response.VehicleCategoryInfo.Obj.MaxSpeed.IsNull())
                    {
                        textBoxSpeed.Text = response.VehicleCategoryInfo.Obj.MaxSpeed.Val.ToString();
                    }
                    if (!response.VehicleCategoryInfo.Obj.Remark.IsNull())
                    {
                        textboxAdditionalRemark.Text = response.VehicleCategoryInfo.Obj.Remark.Val;
                    }
                    if (!response.VehicleCategoryInfo.Obj.IsDrivingLicenceRequired.IsNull())
                    {
                        checkBoxDriversLicenseReq.Checked = response.VehicleCategoryInfo.Obj.IsDrivingLicenceRequired.Val;
                    }
                }
                else
                {
                    // The chosen category had no information stored persistently
                }
            }
            else if(responseProxy.RequestTypeId == Capabilities.Vehicles.SetVehicleCategoryService.ClassTypeId)
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
                else
                {
                    statusStrip.Items["toolStripStatus"].Text = "Error in category information service request.";
                }
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
