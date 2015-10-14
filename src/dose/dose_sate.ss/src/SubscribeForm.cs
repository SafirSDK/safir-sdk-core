/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Sate
{

    public partial class SubscribeForm : Form
    {
        public SubscribeForm(long typeId)
        {
            InitializeComponent();
        }

        public void InitEntitySubForm()
        {
            includeUpdatesCheckBox.Enabled = true;
            includeUpdatesCheckBox.Checked = true;
            includeSubClassesCheckBox.Enabled = true;
            includeSubClassesCheckBox.Checked = true;
            restartSubscriptionCheckBox.Enabled = true;
            restartSubscriptionCheckBox.Checked = true;
            permanentSubcheckBox.Enabled = true;
            permanentSubcheckBox.Checked = false;
            allInstancesCheckBox.Enabled = true;
            allInstancesCheckBox.Checked = true;
        }

        public bool DataUpdSub
        {
            get { return includeUpdatesCheckBox.Checked; }
        }

        public bool SubClassesSub
        {
            get { return includeSubClassesCheckBox.Checked; }
        }

        public bool RestartSub
        {
            get { return restartSubscriptionCheckBox.Checked; }
        }

        public bool AllInstancesSub
        {
            get { return allInstancesCheckBox.Checked; }
        }

        public Safir.Dob.Typesystem.InstanceId Instance
        {
            get
            {
                try
                {
                    return new Safir.Dob.Typesystem.InstanceId(Int64.Parse(instanceTextBox.Text));
                }
                catch
                {
                    string idString;
                    if (instanceTextBox.Text.StartsWith("\"") && (instanceTextBox.Text.EndsWith("\"")) && instanceTextBox.Text.Length > 2)
                    {
                        // remove quotation
                        idString = instanceTextBox.Text.Substring(1, instanceTextBox.Text.Length - 2);
                    }
                    else
                    {
                        idString = instanceTextBox.Text;
                    }

                    if (idString == "")
                    {
                        return null;
                    }
                    return new Safir.Dob.Typesystem.InstanceId(idString);
                }
            }
        }

        public bool PermanentSub
        {
            get { return permanentSubcheckBox.Checked; }
        }

        private void okbutton_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.OK;
        }

        private void cancelbutton_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
        }

        private void allInstancesCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            if (allInstancesCheckBox.Checked)
            {
                instanceTextBox.Text = "";
            }
        }

        private void instanceTextBox_TextChanged(object sender, EventArgs e)
        {
            if (instanceTextBox.Text != "")
            {
                allInstancesCheckBox.Checked = false;
                includeSubClassesCheckBox.Checked = false;
            }
            else
            {
                allInstancesCheckBox.Checked = true;
            }
        }
    }
}
