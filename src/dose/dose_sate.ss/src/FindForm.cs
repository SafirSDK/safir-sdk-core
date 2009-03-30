/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
    /// Summary description for FindForm.
    /// </summary>
    public class FindForm : System.Windows.Forms.Form
    {
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button findbutton;
        private System.Windows.Forms.Button closebutton;
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.Container components = null;
        private System.Windows.Forms.TextBox findtextBox;

        private System.Windows.Forms.RichTextBox richEdit  = null;
        private System.Windows.Forms.CheckBox matchCasecheckBox;
        private int currentIndex = -1;

        public FindForm(System.Windows.Forms.RichTextBox richEdit)
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            //
            // TODO: Add any constructor code after InitializeComponent call
            //
            this.richEdit=richEdit;
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FindForm));
            this.findtextBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.matchCasecheckBox = new System.Windows.Forms.CheckBox();
            this.findbutton = new System.Windows.Forms.Button();
            this.closebutton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            //
            // findtextBox
            //
            this.findtextBox.Location = new System.Drawing.Point(72, 8);
            this.findtextBox.Name = "findtextBox";
            this.findtextBox.Size = new System.Drawing.Size(226, 20);
            this.findtextBox.TabIndex = 0;
            //
            // label1
            //
            this.label1.Location = new System.Drawing.Point(8, 8);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(64, 23);
            this.label1.TabIndex = 1;
            this.label1.Text = "Find what:";
            //
            // matchCasecheckBox
            //
            this.matchCasecheckBox.Location = new System.Drawing.Point(13, 32);
            this.matchCasecheckBox.Name = "matchCasecheckBox";
            this.matchCasecheckBox.Size = new System.Drawing.Size(104, 24);
            this.matchCasecheckBox.TabIndex = 2;
            this.matchCasecheckBox.Text = "Match case";
            //
            // findbutton
            //
            this.findbutton.Location = new System.Drawing.Point(142, 35);
            this.findbutton.Name = "findbutton";
            this.findbutton.Size = new System.Drawing.Size(75, 23);
            this.findbutton.TabIndex = 3;
            this.findbutton.Text = "Find";
            this.findbutton.Click += new System.EventHandler(this.findbutton_Click);
            //
            // closebutton
            //
            this.closebutton.Location = new System.Drawing.Point(223, 35);
            this.closebutton.Name = "closebutton";
            this.closebutton.Size = new System.Drawing.Size(75, 23);
            this.closebutton.TabIndex = 4;
            this.closebutton.Text = "Close";
            this.closebutton.Click += new System.EventHandler(this.closebutton_Click);
            //
            // FindForm
            //
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(305, 70);
            this.Controls.Add(this.closebutton);
            this.Controls.Add(this.findbutton);
            this.Controls.Add(this.matchCasecheckBox);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.findtextBox);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "FindForm";
            this.Text = "Find";
            this.TopMost = true;
            this.ResumeLayout(false);
            this.PerformLayout();

        }
        #endregion

        private void findbutton_Click(object sender, System.EventArgs e)
        {
            FindNext();
        }

        private void FindNext()
        {
            richEdit.HideSelection=false;
            string find=this.findtextBox.Text;
            RichTextBoxFinds finds=RichTextBoxFinds.None;
            if (this.matchCasecheckBox.Checked)
                finds=RichTextBoxFinds.MatchCase;
            currentIndex=this.richEdit.Find(find, currentIndex+1, richEdit.Text.Length, finds);
            if (currentIndex<0)
            {
                MessageBox.Show("The text '"+find+"' was not found!");
                return;
            }
            richEdit.SelectionStart=currentIndex;
        }

        private void closebutton_Click(object sender, EventArgs e)
        {
            this.Hide();
        }
    }
}
