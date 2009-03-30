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

namespace Sate
{
    partial class PanelLabelControl
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.textlabel = new System.Windows.Forms.Label();
            this.closelabel = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // textlabel
            // 
            this.textlabel.AutoSize = true;
            this.textlabel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.textlabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.textlabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.textlabel.Location = new System.Drawing.Point(0, 0);
            this.textlabel.Name = "textlabel";
            this.textlabel.Size = new System.Drawing.Size(28, 13);
            this.textlabel.TabIndex = 0;
            this.textlabel.Text = "Text";
            // 
            // closelabel
            // 
            this.closelabel.AutoSize = true;
            this.closelabel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.closelabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.closelabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.closelabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.closelabel.Location = new System.Drawing.Point(135, 0);
            this.closelabel.Margin = new System.Windows.Forms.Padding(3, 0, 5, 0);
            this.closelabel.Name = "closelabel";
            this.closelabel.Size = new System.Drawing.Size(15, 13);
            this.closelabel.TabIndex = 1;
            this.closelabel.Text = "X";
            this.closelabel.Click += new System.EventHandler(this.closelabel_Click);
            // 
            // PanelLabelControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.Controls.Add(this.closelabel);
            this.Controls.Add(this.textlabel);
            this.Name = "PanelLabelControl";
            this.Size = new System.Drawing.Size(150, 15);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label textlabel;
        private System.Windows.Forms.Label closelabel;

    }
}
