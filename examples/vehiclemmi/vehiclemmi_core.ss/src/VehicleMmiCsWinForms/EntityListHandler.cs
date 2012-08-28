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
    /// Implements list view to present vehicles.
    /// Subscribes for vehicle data.
    /// </summary>
    public class EntityListHandler : 
        // Allows this class to subscribe for entity changes
        Safir.Dob.EntitySubscriber
    {
        //////////////////////////////////
        // Private members and functions

        /// <summary>
        /// Constants for column index in listview
        /// </summary>
        private const int c_iColumnIdentification = 0;
        private const int c_iColumnCategory = 1;
        private const int c_iColumnSpeed = 2;

  
        /// <summary>
        /// Secondary Dob connection
        /// </summary>
        private Safir.Dob.SecondaryConnection m_secDobConnection;

        /// <summary>
        /// Vehicles list view, that presents selected information of vehicles
        /// </summary>
        private System.Windows.Forms.ListView m_ListView;
        
        /// <summary>
        /// Vehicles dialog, presents detailed information of vehicles.
        /// Used for creation and updates of vehicles.
        /// </summary>
        private EntityDialog m_VehicleDialog;

        /// <summary>
        /// Dialog to calculate bearing and distance for a vehicle
        /// </summary>
        private ServiceDialog m_CalculateSpeedDlg;
            
        /// <summary>
        /// Dialog to present category information 
        /// </summary>
        private CategoryInfoDialog m_CategoryInfoDlg;

        // Need to disable "... is assigned but its value is never used" warning,
        // since the dialog does its work "on its own" once it has been instantiated.
#pragma warning disable 414
        /// <summary>
        /// Dialog to present vehicle message 
        /// </summary>
        private MessageDialog m_MessageDialog;
#pragma warning restore 414

        /// <summary>
        /// Reference to frame status strip to show errors for operator
        /// </summary>
        private System.Windows.Forms.ToolStripItem m_frameStatusStrip;

        //////////////////////////////////
        // Public members and functions

        /// <summary>
        /// Attach to the Dob and map list view items to the type of Dob objects
        /// </summary>
        public EntityListHandler(System.Windows.Forms.ListView listView, System.Windows.Forms.ToolStripItem frameStatusStrip)
        {
            m_secDobConnection = new Safir.Dob.SecondaryConnection();
            m_secDobConnection.Attach();
            m_ListView = listView;
            m_frameStatusStrip = frameStatusStrip;

            // Create dialogs
            m_VehicleDialog = new EntityDialog();
            m_CalculateSpeedDlg = new ServiceDialog();
            m_CategoryInfoDlg = new CategoryInfoDialog();
            m_MessageDialog = new MessageDialog();
        }


        /// <summary>
        /// Set up subscription to vehicles
        /// </summary>
        public void StartSubscription()
        {
            //StartRemoveInExercise3
             m_secDobConnection.SubscribeEntity(Capabilities.Vehicles.Vehicle.ClassTypeId, this);
            //StopRemoveInExercise3
        }


        /// <summary>
        /// Get object id for the object being selected
        /// </summary>
        public bool GetSelectedEntityId(out Safir.Dob.Typesystem.EntityId entityId)
        {
            entityId = new Safir.Dob.Typesystem.EntityId();
            entityId.TypeId = Capabilities.Vehicles.Vehicle.ClassTypeId;

            if (m_ListView.SelectedItems.Count > 0)
            {
                ListViewItem lastSelectedItem = 
                    m_ListView.SelectedItems[m_ListView.SelectedItems.Count - 1];
                entityId.InstanceId = 
                    new Safir.Dob.Typesystem.InstanceId(Convert.ToInt64(lastSelectedItem.Name));
                return true;
            }
            else
            {
                return false;
            }
        }

        public bool EntityIdExist(Safir.Dob.Typesystem.EntityId entityId)
        {
            int index = m_ListView.Items.IndexOfKey(entityId.InstanceId.ToString());
            if (index == -1)
                return false;
            else
                return true;
        }

        /// <summary>
        /// Add a new vehicle to a row in the list view
        /// </summary>
        public void OnNewEntity(Safir.Dob.EntityProxy entityProxy)
        {
            // Add the vehicle to the view
            if (!m_ListView.Items.ContainsKey(entityProxy.InstanceId.ToString()))
            {
                // Create a new row
                ListViewItem item = new ListViewItem();
                item.Name = entityProxy.InstanceId.ToString();
                m_ListView.Items.Add(item);

                // Create all sub-items for the row (one in each column)
                for (int columnCount = 0; columnCount < m_ListView.Columns.Count; ++columnCount)
                {
                    item.SubItems.Add(" ");
                }
            }
            OnUpdatedEntity(entityProxy);
        }


        /// <summary>
        /// Update row in listview with new data
        /// </summary>
        public void OnUpdatedEntity(Safir.Dob.EntityProxy entityProxy)
        {
            Capabilities.Vehicles.Vehicle vehicle = (Capabilities.Vehicles.Vehicle)entityProxy.Entity;
            
            int index = m_ListView.Items.IndexOfKey(entityProxy.EntityId.InstanceId.ToString());
            ListViewItem item = m_ListView.Items[index];

            // Identification
            if (!vehicle.Identification.IsNull())
            {
                item.SubItems[c_iColumnIdentification].Text = vehicle.Identification.Val.ToString();
            }
            else
            {
                item.SubItems[c_iColumnIdentification].Text = "-";
            }

            // Category
            if (!vehicle.VehicleCategory.IsNull())
            {
                item.SubItems[c_iColumnCategory].Text = vehicle.VehicleCategory.Val.ToString();
            }
            else
            {
                item.SubItems[c_iColumnCategory].Text = "-";
            }

            //StartRemoveInExercise4
            // Speed
            if (!vehicle.Speed.IsNull())
            {
                item.SubItems[c_iColumnSpeed].Text = vehicle.Speed.Val.ToString();
            }
            else
            {
                item.SubItems[c_iColumnSpeed].Text = "-";
            }
            //StopRemoveInExercise4

            // Position
            item.SubItems[3].Text = "-";
            item.SubItems[4].Text = "-";
            if (!vehicle.Position.IsNull())
            {
                // Position (latitude)
                if (!vehicle.Position.Obj.Latitude.IsNull())
                {
                    item.SubItems[3].Text = vehicle.Position.Obj.Latitude.Val.ToString();
                }

                // Position (longitude)
                if (!vehicle.Position.Obj.Longitude.IsNull())
                {
                    item.SubItems[4].Text = vehicle.Position.Obj.Longitude.Val.ToString();
                }
            }
        }


        /// <summary>
        /// Remove the object from the list and the object map
        /// </summary>
        public void OnDeletedEntity(Safir.Dob.EntityProxy entityProxy, bool deletedByOwner)
        {
            m_ListView.Items.RemoveByKey(entityProxy.InstanceId.ToString());
        }


        /// <summary>
        /// Opens vehicles dialog
        /// </summary>
        public void OpenVehicleCreateDlg()
        {
            m_VehicleDialog.CreateVehicle();
            m_frameStatusStrip.Text = "OK";
        }


        /// <summary>
        /// Open the last selected items dialog with detailed information
        /// </summary>
        public void OpenVehicleUpdateDlg()
        {
            if (m_ListView.SelectedItems.Count > 0)
            {
                ListViewItem lastSelectedItem =
                    m_ListView.SelectedItems[m_ListView.SelectedItems.Count - 1];
                Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId
                    (Capabilities.Vehicles.Vehicle.ClassTypeId, 
                     new Safir.Dob.Typesystem.InstanceId(Convert.ToInt64(lastSelectedItem.Name)));

                if (!m_VehicleDialog.UpdateVehicle(entityId))
                {
                    m_frameStatusStrip.Text = "Vehicle could not be read from Dob.";
                }
                else
                {
                    m_frameStatusStrip.Text = "OK";
                }
            }
            else
            {
                m_frameStatusStrip.Text = "No vehicle selected.";
            }
        }

        /// <summary>
        /// Open a calculation of speed dialog for the last selected item
        /// </summary>
        public void OpenCalculateSpeedDiffDlg()
        {
            if (m_ListView.SelectedItems.Count > 0)
            {
                ListViewItem lastSelectedItem = m_ListView.SelectedItems[m_ListView.SelectedItems.Count - 1];
                Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId
                    (Capabilities.Vehicles.Vehicle.ClassTypeId,
                      new Safir.Dob.Typesystem.InstanceId(Convert.ToInt64(lastSelectedItem.Name)));

                if (!m_CalculateSpeedDlg.Open(entityId))
                {
                    m_frameStatusStrip.Text = "Vehicle could not be read from Dob";
                }
                else
                {
                    m_frameStatusStrip.Text = "OK";
                }
            }
            else
            {
                m_frameStatusStrip.Text = "No vehicle selected.";
            }
        }
            

        public void OpenCategoryInfoDlg()
        {
            if (m_ListView.SelectedItems.Count > 0)
            {
                ListViewItem lastSelectedItem = m_ListView.SelectedItems[m_ListView.SelectedItems.Count - 1];
                Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId
                    (Capabilities.Vehicles.Vehicle.ClassTypeId,
                      new Safir.Dob.Typesystem.InstanceId(Convert.ToInt64(lastSelectedItem.Name)));

                if (!m_CategoryInfoDlg.Open(entityId))
                {
                    m_frameStatusStrip.Text = "Vehicle could not be read from Dob";
                }
                else
                {
                    m_frameStatusStrip.Text = "OK";
                }
            }
            else
            {
                m_frameStatusStrip.Text = "No vehicle selected.";
            }
        }

    }
}
