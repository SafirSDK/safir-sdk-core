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
using System.Drawing;
using System.Windows.Forms;

namespace Sate
{
    public partial class PanelLabelControl : UserControl
    {
        public delegate void OnCloseEventHandler(object sender, EventArgs e);

        public PanelLabelControl(string labelText)
        {
            InitializeComponent();
            Dock = DockStyle.Top;
            textlabel.Text = labelText;
        }

        public string LabelText
        {
            get { return textlabel.Text; }
            set { textlabel.Text = value; }
        }

        public override Color BackColor
        {
            get { return base.BackColor; }
            set
            {
                base.BackColor = value;
                textlabel.BackColor = value;
                closelabel.BackColor = value;
            }
        }

        public override Color ForeColor
        {
            get { return base.ForeColor; }
            set
            {
                base.ForeColor = value;
                textlabel.ForeColor = value;
                closelabel.ForeColor = value;
            }
        }

        public event OnCloseEventHandler CloseEvent;

        private void closelabel_Click(object sender, EventArgs e)
        {
            if (CloseEvent != null)
                CloseEvent(this, e);
        }
    }
}