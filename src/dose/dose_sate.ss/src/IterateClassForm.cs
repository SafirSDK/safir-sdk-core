/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
* 
* Created by: Joel Ottosson / stjoot
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
using System.Drawing;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows.Forms;

namespace Sate
{

    /// <summary>
    /// Summary description for IterateClassForm.
    /// </summary>
    public class IterateClassForm : System.Windows.Forms.Form
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.Container components = null;
        private System.Windows.Forms.ListBox listBox1;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label resultlabel;
        private System.Windows.Forms.Panel topPanel;
        private System.Windows.Forms.Button okbutton;
        private System.Windows.Forms.Panel bottompanel;
        private Button deleteAllInstancesButton;

        private long typeId;

        public IterateClassForm(long typeId)
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            //
            // TODO: Add any constructor code after InitializeComponent call
            //
            this.typeId=typeId;
            this.listBox1.DoubleClick+=new EventHandler(listBox1_DoubleClick);

            this.Text="Iterate class: "+Safir.Dob.Typesystem.Operations.GetName(typeId);

            UpdateListBox();
        }

        private void UpdateListBox()
        {
            int noFound = 0;
            this.listBox1.Items.Clear();

            foreach (Safir.Dob.EntityProxy entityProxy in MainForm.Instance.Dose.GetEntityEnumerator(typeId, true))
            {
                ++noFound;
                this.listBox1.Items.Add(entityProxy.InstanceId.ToString());
            }

            this.resultlabel.Text = noFound.ToString() + " existing instances";
        }

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        protected override void Dispose( bool disposing )
        {
            if( disposing )
            {
                if(components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose( disposing );
        }

        #region Windows Form Designer generated code
        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(IterateClassForm));
            this.listBox1 = new System.Windows.Forms.ListBox();
            this.label3 = new System.Windows.Forms.Label();
            this.resultlabel = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.topPanel = new System.Windows.Forms.Panel();
            this.deleteAllInstancesButton = new System.Windows.Forms.Button();
            this.bottompanel = new System.Windows.Forms.Panel();
            this.okbutton = new System.Windows.Forms.Button();
            this.topPanel.SuspendLayout();
            this.bottompanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // listBox1
            // 
            this.listBox1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.listBox1.Location = new System.Drawing.Point(2, 66);
            this.listBox1.Name = "listBox1";
            this.listBox1.Size = new System.Drawing.Size(300, 238);
            this.listBox1.TabIndex = 0;
            // 
            // label3
            // 
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, ((System.Drawing.FontStyle)((System.Drawing.FontStyle.Bold | System.Drawing.FontStyle.Underline))), System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(8, 8);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(56, 23);
            this.label3.TabIndex = 3;
            this.label3.Text = "Result:";
            // 
            // resultlabel
            // 
            this.resultlabel.ForeColor = System.Drawing.SystemColors.ActiveCaption;
            this.resultlabel.Location = new System.Drawing.Point(64, 8);
            this.resultlabel.Name = "resultlabel";
            this.resultlabel.Size = new System.Drawing.Size(224, 23);
            this.resultlabel.TabIndex = 4;
            this.resultlabel.Text = "0 existing instances";
            // 
            // label5
            // 
            this.label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label5.Location = new System.Drawing.Point(8, 40);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(272, 23);
            this.label5.TabIndex = 5;
            this.label5.Text = "Existing instances (double click to open)";
            // 
            // topPanel
            // 
            this.topPanel.Controls.Add(this.deleteAllInstancesButton);
            this.topPanel.Controls.Add(this.label3);
            this.topPanel.Controls.Add(this.resultlabel);
            this.topPanel.Controls.Add(this.label5);
            this.topPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.topPanel.Location = new System.Drawing.Point(2, 2);
            this.topPanel.Name = "topPanel";
            this.topPanel.Size = new System.Drawing.Size(300, 64);
            this.topPanel.TabIndex = 6;
            // 
            // deleteAllInstancesButton
            // 
            this.deleteAllInstancesButton.Location = new System.Drawing.Point(177, 8);
            this.deleteAllInstancesButton.Name = "deleteAllInstancesButton";
            this.deleteAllInstancesButton.Size = new System.Drawing.Size(111, 23);
            this.deleteAllInstancesButton.TabIndex = 6;
            this.deleteAllInstancesButton.Text = "Delete all instances";
            this.deleteAllInstancesButton.UseVisualStyleBackColor = true;
            this.deleteAllInstancesButton.Click += new System.EventHandler(this.deleteAllInstancesButton_Click);
            // 
            // bottompanel
            // 
            this.bottompanel.Controls.Add(this.okbutton);
            this.bottompanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.bottompanel.Location = new System.Drawing.Point(2, 316);
            this.bottompanel.Name = "bottompanel";
            this.bottompanel.Padding = new System.Windows.Forms.Padding(5, 5, 10, 5);
            this.bottompanel.Size = new System.Drawing.Size(300, 32);
            this.bottompanel.TabIndex = 7;
            // 
            // okbutton
            // 
            this.okbutton.Dock = System.Windows.Forms.DockStyle.Right;
            this.okbutton.Location = new System.Drawing.Point(215, 5);
            this.okbutton.Name = "okbutton";
            this.okbutton.Size = new System.Drawing.Size(75, 22);
            this.okbutton.TabIndex = 0;
            this.okbutton.Text = "OK";
            this.okbutton.Click += new System.EventHandler(this.okbutton_Click);
            // 
            // IterateClassForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(304, 350);
            this.Controls.Add(this.listBox1);
            this.Controls.Add(this.topPanel);
            this.Controls.Add(this.bottompanel);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "IterateClassForm";
            this.Padding = new System.Windows.Forms.Padding(2);
            this.Text = "Iterate class";
            this.topPanel.ResumeLayout(false);
            this.bottompanel.ResumeLayout(false);
            this.ResumeLayout(false);

        }
        #endregion

        private void okbutton_Click(object sender, System.EventArgs e)
        {
            DialogResult=DialogResult.OK;
        }

        private void listBox1_DoubleClick(object sender, EventArgs e)
        {
            // Instance <string>
            // Instance <number>
            string s = (string)this.listBox1.SelectedItem;
            Safir.Dob.Typesystem.EntityId entityId;

            try
            {
                Int64 instance = Int64.Parse(s);
                entityId = new Safir.Dob.Typesystem.EntityId(typeId, new Safir.Dob.Typesystem.InstanceId(instance));
            }
            catch
            {
                string instanceStr = s;
                entityId = new Safir.Dob.Typesystem.EntityId(typeId, new Safir.Dob.Typesystem.InstanceId(instanceStr));
            }

            try
            {
                using (Safir.Dob.EntityProxy entityProxy = MainForm.Instance.Dose.Read(entityId))
                {
                    EntityInfo entityInfo = new EntityInfo();
                    entityInfo.Obj = entityProxy.Entity;
                    entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);
                    entityInfo.setInstanceId(entityProxy.InstanceId);
                    MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
                }
            }
            catch
            {
                MessageBox.Show(entityId.InstanceId.ToString() + " does no longer exist.");
            }
        }

        private void deleteAllInstancesButton_Click(object sender, EventArgs e)
        {
            bool isHandler = false;

            foreach (Int64 loop_typeId in MainForm.Instance.requestorDecidesTypeIdList)
            {
                if (loop_typeId == typeId)
                    isHandler = true;
            }
            foreach (Int64 loop_typeId in MainForm.Instance.handlerDecidesTypeIdList)
            {
                if (loop_typeId == typeId)
                    isHandler = true;
            }


            List<EntityInfo> entityInfoList = new List<EntityInfo>();

            foreach (Safir.Dob.EntityProxy entityProxy in MainForm.Instance.Dose.GetEntityEnumerator(typeId, true))
            {
                EntityInfo entityInfo = new EntityInfo();
                entityInfo.Obj = entityProxy.Entity;
                entityInfo.setInstanceId(entityProxy.InstanceId);
                entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);
                entityInfoList.Add(entityInfo);
            }

            foreach (EntityInfo entityInfo in entityInfoList)
            {
                if (isHandler)
                {
                    MainForm.Instance.DeleteEntity(entityInfo);
                }
                else
                {
                    MainForm.Instance.DeleteRequest(entityInfo);
                }
            }

            UpdateListBox();
            /*}
            else
            {
                string msg = "You can only do this for classes that you handle!";
                MessageBox.Show(msg, "Handler Not Found", System.Windows.Forms.MessageBoxButtons.OK, MessageBoxIcon.Error);
            } */
        }
    }
}

