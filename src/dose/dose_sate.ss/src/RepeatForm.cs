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
using System.ComponentModel;
using System.Windows.Forms;

namespace Sate
{
    /// <summary>
    ///     Summary description for RepeatForm.
    /// </summary>
    public class RepeatForm : Form
    {
        /// <summary>
        ///     Required designer variable.
        /// </summary>
        private readonly Container components = null;

        private Button cancelbutton;

        private RadioButton infiniteradioButton;

        private RadioButton manualradioButton;
        private Button okbutton;
        private TextBox repstextBox;

        public RepeatForm(int defaultReps)
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            Repetitions = defaultReps;
            if (Repetitions < 0)
            {
                infiniteradioButton.Checked = true;
            }
            else
            {
                repstextBox.Text = Repetitions.ToString();
            }
        }

        public int Repetitions { get; private set; } = 10;

        /// <summary>
        ///     Clean up any resources being used.
        /// </summary>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        ///     Required method for Designer support - do not modify
        ///     the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.manualradioButton = new System.Windows.Forms.RadioButton();
            this.infiniteradioButton = new System.Windows.Forms.RadioButton();
            this.repstextBox = new System.Windows.Forms.TextBox();
            this.cancelbutton = new System.Windows.Forms.Button();
            this.okbutton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            //
            // manualradioButton
            //
            this.manualradioButton.Checked = true;
            this.manualradioButton.Location = new System.Drawing.Point(8, 8);
            this.manualradioButton.Name = "manualradioButton";
            this.manualradioButton.Size = new System.Drawing.Size(136, 24);
            this.manualradioButton.TabIndex = 0;
            this.manualradioButton.TabStop = true;
            this.manualradioButton.Text = "Number of repetitions";
            //
            // infiniteradioButton
            //
            this.infiniteradioButton.Location = new System.Drawing.Point(8, 32);
            this.infiniteradioButton.Name = "infiniteradioButton";
            this.infiniteradioButton.TabIndex = 1;
            this.infiniteradioButton.Text = "Infinite loop";
            //
            // repstextBox
            //
            this.repstextBox.Location = new System.Drawing.Point(136, 8);
            this.repstextBox.Name = "repstextBox";
            this.repstextBox.Size = new System.Drawing.Size(72, 20);
            this.repstextBox.TabIndex = 2;
            this.repstextBox.Text = "10";
            //
            // cancelbutton
            //
            this.cancelbutton.Location = new System.Drawing.Point(136, 72);
            this.cancelbutton.Name = "cancelbutton";
            this.cancelbutton.TabIndex = 3;
            this.cancelbutton.Text = "Cancel";
            this.cancelbutton.Click += new System.EventHandler(this.cancelbutton_Click);
            //
            // okbutton
            //
            this.okbutton.Location = new System.Drawing.Point(56, 72);
            this.okbutton.Name = "okbutton";
            this.okbutton.TabIndex = 4;
            this.okbutton.Text = "OK";
            this.okbutton.Click += new System.EventHandler(this.okbutton_Click);
            //
            // RepeatForm
            //
            this.ClientSize = new System.Drawing.Size(218, 104);
            this.Controls.Add(this.okbutton);
            this.Controls.Add(this.cancelbutton);
            this.Controls.Add(this.repstextBox);
            this.Controls.Add(this.infiniteradioButton);
            this.Controls.Add(this.manualradioButton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Name = "RepeatForm";
            this.Text = "Repeat action sequence";
            this.ResumeLayout(false);
        }

        #endregion

        private void okbutton_Click(object sender, EventArgs e)
        {
            try
            {
                if (manualradioButton.Checked)
                {
                    Repetitions = int.Parse(repstextBox.Text);
                    DialogResult = DialogResult.OK;
                }
                else
                {
                    Repetitions = -1;
                    DialogResult = DialogResult.OK;
                }
            }
            catch
            {
                MessageBox.Show("Illegal value");
            }
        }

        private void cancelbutton_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
        }
    }
}