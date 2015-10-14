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

    public partial class RegisterForm : Form
    {
        public RegisterForm(long typeId)
        {
            InitializeComponent();
        }

        public void InitEntitySubForm()
        {
            pendingcheckBox.Checked = false;
            pendingcheckBox.Enabled = true;
            injectionCheckBox.Checked = false;
            injectionCheckBox.Enabled = true;
        }

        public void InitServiceSubForm()
        {
            pendingcheckBox.Checked = false;
            pendingcheckBox.Enabled = true;
            injectionCheckBox.Checked = false;
            injectionCheckBox.Enabled = false;
        }

        private void okbutton_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.OK;
        }

        private void cancelbutton_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
        }

        public bool PendingReg
        {
            get { return this.pendingcheckBox.Checked; }
        }

        public bool InjectionReg
        {
            get { return this.injectionCheckBox.Checked; }
        }

        public bool PermanentReg
        {
            get { return permanentregcheckBox.Checked; }
        }

        public string HandlerId
        {
            get { return handlerIdTextBox.Text; }
        }

        public bool RequestorDecides
        {
            get { return requestorDecidesRadioButton.Checked; }
        }

        // handlerid is read from the HandlerId property. I.e. do not set any values in this method.
        private void handlerIdTextBox_TextChanged(object sender, EventArgs e)
        {
            try
            {
                //string s = handlerIdTextBox.Text;
                handlerIdTextBox.BackColor = ColorMap.ENABLED;
            }
            catch
            {
                handlerIdTextBox.BackColor = ColorMap.ERROR;
            }
        }

        /* only injection or pending can be checked in at the same time */
        private void injectionCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            if (InjectionReg)
            {
                pendingcheckBox.Checked = false;
            }
        }

        private void pendingcheckBox_CheckedChanged(object sender, EventArgs e)
        {
            if (PendingReg)
            {
                injectionCheckBox.Checked = false;
            }
        }
    }
}
