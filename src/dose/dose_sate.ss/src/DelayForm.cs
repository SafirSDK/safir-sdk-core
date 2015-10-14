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
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace Sate
{
    /// <summary>
    /// Summary description for DelayForm.
    /// </summary>
    public class DelayForm : System.Windows.Forms.Form
    {
        private int delay = 100;

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox delaytextBox;
        private System.Windows.Forms.Button okbutton;
        private System.Windows.Forms.Button cancelbutton;
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.Container components = null;

        public DelayForm()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

        }

        public int Delay
        {
            get {return delay;}
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
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(DelayForm));
            this.label1 = new System.Windows.Forms.Label();
            this.delaytextBox = new System.Windows.Forms.TextBox();
            this.okbutton = new System.Windows.Forms.Button();
            this.cancelbutton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            //
            // label1
            //
            this.label1.Location = new System.Drawing.Point(8, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(128, 23);
            this.label1.TabIndex = 0;
            this.label1.Text = "Delay (milliseconds): ";
            //
            // delaytextBox
            //
            this.delaytextBox.Location = new System.Drawing.Point(136, 16);
            this.delaytextBox.Name = "delaytextBox";
            this.delaytextBox.TabIndex = 1;
            this.delaytextBox.Text = "100";
            //
            // okbutton
            //
            this.okbutton.Location = new System.Drawing.Point(80, 48);
            this.okbutton.Name = "okbutton";
            this.okbutton.TabIndex = 2;
            this.okbutton.Text = "OK";
            this.okbutton.Click += new System.EventHandler(this.okbutton_Click);
            //
            // cancelbutton
            //
            this.cancelbutton.Location = new System.Drawing.Point(160, 48);
            this.cancelbutton.Name = "cancelbutton";
            this.cancelbutton.TabIndex = 3;
            this.cancelbutton.Text = "Cancel";
            this.cancelbutton.Click += new System.EventHandler(this.cancelbutton_Click);
            //
            // DelayForm
            //
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(240, 78);
            this.Controls.Add(this.cancelbutton);
            this.Controls.Add(this.okbutton);
            this.Controls.Add(this.delaytextBox);
            this.Controls.Add(this.label1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "DelayForm";
            this.Text = "Set delay";
            this.ResumeLayout(false);

        }
        #endregion

        private void okbutton_Click(object sender, System.EventArgs e)
        {
            try
            {
                delay=int.Parse(this.delaytextBox.Text);
                this.DialogResult=DialogResult.OK;
            }
            catch
            {
                MessageBox.Show("Illegal delay value");
            }

        }

        private void cancelbutton_Click(object sender, System.EventArgs e)
        {
            this.DialogResult=DialogResult.Cancel;
        }
    }
}
