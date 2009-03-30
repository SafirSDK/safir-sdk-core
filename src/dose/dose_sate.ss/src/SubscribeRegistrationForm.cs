/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
* 
* Created by: Stefan Lindström / stsyli
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
    public partial class SubscribeRegistrationForm : Form
    {
        public SubscribeRegistrationForm(long typeId)
        {
            InitializeComponent();
        }

        public void InitSubForm()
        {
            permanentSubcheckBox.Enabled = true;
            permanentSubcheckBox.Checked = false;
            allHandlersCheckBox1.Enabled = true;
            allHandlersCheckBox1.Checked = true;
            handlerIdTextBox.Enabled = false;
            includeSubClassesCheckBox.Enabled = true;
            includeSubClassesCheckBox.Checked = true;
            restartCheckBox.Enabled = true;
            restartCheckBox.Checked = true;
        }

        public bool PermanentSub
        {
            get { return permanentSubcheckBox.Checked; }
        }

        public bool IncludeSubClassesSub
        {
            get { return includeSubClassesCheckBox.Checked; }
        }

        public bool RestartSub
        {
            get { return restartCheckBox.Checked; }
        }

        private void okbutton_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.OK;
        }

        private void cancelbutton_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
        }

        public string HandlerIdTextBox
        {
            get { return handlerIdTextBox.Text; }
        }

        private void handlerIdTextBox_TextChanged(object sender, EventArgs e)
        {
           handlerIdTextBox.BackColor = ColorMap.ENABLED;
        }

        private void allHandlersCheckBox1_CheckedChanged(object sender, EventArgs e)
        {
            handlerIdTextBox.Enabled = !handlerIdTextBox.Enabled;
        }
    }
}
