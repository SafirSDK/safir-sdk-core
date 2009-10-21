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
    /// Implements list view frame that contains list view and buttons to create, 
    /// show/update and delete vehicles. It also contain button to calculate speed.
    /// </summary>
    
    public partial class EntityFrame : 
        Form
        // Allows this class to receive response on sent requests
        //StartRemoveInExercise
        ,Safir.Dob.Requestor
        //StopRemoveInExercise
    {
        //////////////////////////////////
        // Private members and functions
        
        /// <summary>
        /// List view that subscribes vehicle entity data
        /// </summary>
        private EntityListHandler m_vehicleEntityListHandler;
        
        /// <summary>
        /// Secondary Dob connection
        /// </summary>
        private Safir.Dob.SecondaryConnection m_secDobConnection;

        /// <summary>
        /// Opens dialog to create vehicle
        /// </summary>
        private void toolStripButtonCreate_Click(object sender, EventArgs e)
        {
            m_vehicleEntityListHandler.OpenVehicleCreateDlg();
        }

        /// <summary>
        /// Opens dialog to update vehicle 
        /// </summary>
        private void toolStripButtonUpdate_Click(object sender, EventArgs e)
        {
            //StartRemoveInExercise
            m_vehicleEntityListHandler.OpenVehicleUpdateDlg();
            //StopRemoveInExercise
        }

        //StartRemoveInExercise
        /// <summary>
        /// Removes selected vehicle
        /// </summary>
        private void toolStripButtonDelete_Click(object sender, EventArgs e)
        {
            Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId();

            if (m_vehicleEntityListHandler.GetSelectedEntityId(out entityId))
            {
                m_secDobConnection.DeleteRequest(entityId, this);
                statusStrip.Items["toolStripStatus"].Text = "OK";
            }
            else
            {
                statusStrip.Items["toolStripStatus"].Text = "No vehicle selected.";
            }
        }

        /// <summary>
        /// Opens dialog to calculate speed
        /// </summary>
        private void toolStripButtonSpeed_Click(object sender, EventArgs e)
        {
            m_vehicleEntityListHandler.OpenCalculateSpeedDiffDlg();
        }

        //StopRemoveInExercise
        /// <summary>
        /// Opens dialog to show category information
        /// </summary>
        private void toolStripButtonCategory_Click(object sender, EventArgs e)
        {
            m_vehicleEntityListHandler.OpenCategoryInfoDlg();
        }

        /// <summary>
        /// Sends a service request to  delete additional information of a 
        /// certain category
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void toolStripButtonDeleteCategoryInfo_Click(object sender, EventArgs e)
        {
            //StartRemoveInExercise
            Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId();

            if (m_vehicleEntityListHandler.GetSelectedEntityId(out entityId))
            {
                Capabilities.Vehicles.Vehicle vehicle = new Capabilities.Vehicles.Vehicle();
                try
                {
                    // Get data from Dob and fill dialog
                    using (Safir.Dob.EntityProxy entityProxy = m_secDobConnection.Read(entityId))
                    {
                         vehicle = (Capabilities.Vehicles.Vehicle)entityProxy.Entity;
                    }
                }
                catch
                {
                    // The specified instance of the entity does not exist
                    statusStrip.Items["toolStripStatus"].Text = "Data was not found in database";
                }

                if (!vehicle.VehicleCategory.IsNull())
                {
                    Capabilities.Vehicles.DeleteVehicleCategoryService req = new Capabilities.Vehicles.DeleteVehicleCategoryService();
                    req.VehicleCategory.Val = vehicle.VehicleCategory.Val;

                    m_secDobConnection.ServiceRequest(req, new Safir.Dob.Typesystem.HandlerId(), this);
                    statusStrip.Items["toolStripStatus"].Text = "OK";
                }
            }
            else
            {
                statusStrip.Items["toolStripStatus"].Text = "No vehicle selected.";
            }
            //StopRemoveInExercise
        }

        /// <summary>
        /// Attach a secondary connection to the Dob connection.
        /// Start subscription of vehicles and show frame.
        /// </summary>       
        public EntityFrame()
        {
            InitializeComponent();
            m_secDobConnection = new Safir.Dob.SecondaryConnection();
            m_secDobConnection.Attach();

            // Create vehicles entity subscriber and list view
            m_vehicleEntityListHandler = new EntityListHandler(listViewVehicles, statusStrip.Items["toolStripStatus"]);
        }

        private void EntityFrame_Load(object sender, EventArgs e)
        {
            // Start subscription to vehicle entities.
            // Prior to this, the application must be ready to
            // receive Dob events (the message pump must be
            // started), which is the reason for why the subscription
            // start is performed here.
            m_vehicleEntityListHandler.StartSubscription();
        }

        //StartRemoveInExercise
        /// <summary>
        /// Overrides Safir.Dob.Requestor. Response for sent request
        /// </summary>
        public void OnResponse(Safir.Dob.ResponseProxy responseProxy)
        {
            if (responseProxy.RequestTypeId == Capabilities.Vehicles.DeleteVehicleCategoryService.ClassTypeId)
            {
                if (responseProxy.IsSuccess)
                {
                    statusStrip.Items["toolStripStatus"].Text = "Category info for the chosen object has succesfully been deleted";
                }
                else
                {
                    statusStrip.Items["toolStripStatus"].Text = "Category info for the chosen object could not be deleted";
                }
            }
            else
            {
                if (responseProxy.IsSuccess)
                {
                    statusStrip.Items["toolStripStatus"].Text = "OK";
                }
                else
                {
                    Safir.Dob.ErrorResponse errorResponse = (Safir.Dob.ErrorResponse)responseProxy.Response;
                    if (errorResponse.AdditionalInfo.IsNull())
                        statusStrip.Items["toolStripStatus"].Text = "Error";
                    else
                        statusStrip.Items["toolStripStatus"].Text = "Error: " + errorResponse.AdditionalInfo.Val;
                }
            }
        }

        /// <summary>
        /// Overrides Safir.Dob.Requestor. Notification that overflow situatuion is solved
        /// </summary>
        public void OnNotRequestOverflow()
        {
            // No automatic resending is made. 
            statusStrip.Items["toolStripStatus"].Text = "Overflow situation solved. Make request again!";
        }
        //StopRemoveInExercise
    }
}
