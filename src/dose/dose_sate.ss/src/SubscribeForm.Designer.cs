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
 
    partial class SubscribeForm
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SubscribeForm));
            this.okbutton = new System.Windows.Forms.Button();
            this.cancelbutton = new System.Windows.Forms.Button();
            this.includeUpdatesCheckBox = new System.Windows.Forms.CheckBox();
            this.permanentSubcheckBox = new System.Windows.Forms.CheckBox();
            this.instanceTextBox = new System.Windows.Forms.TextBox();
            this.instanceLabel = new System.Windows.Forms.Label();
            this.allInstancesCheckBox = new System.Windows.Forms.CheckBox();
            this.restartSubscriptionCheckBox = new System.Windows.Forms.CheckBox();
            this.includeSubClassesCheckBox = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // okbutton
            // 
            this.okbutton.Location = new System.Drawing.Point(139, 181);
            this.okbutton.Name = "okbutton";
            this.okbutton.Size = new System.Drawing.Size(75, 23);
            this.okbutton.TabIndex = 5;
            this.okbutton.Text = "OK";
            this.okbutton.UseVisualStyleBackColor = true;
            this.okbutton.Click += new System.EventHandler(this.okbutton_Click);
            // 
            // cancelbutton
            // 
            this.cancelbutton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancelbutton.Location = new System.Drawing.Point(220, 181);
            this.cancelbutton.Name = "cancelbutton";
            this.cancelbutton.Size = new System.Drawing.Size(75, 23);
            this.cancelbutton.TabIndex = 6;
            this.cancelbutton.Text = "Cancel";
            this.cancelbutton.UseVisualStyleBackColor = true;
            this.cancelbutton.Click += new System.EventHandler(this.cancelbutton_Click);
            // 
            // includeUpdatesCheckBox
            // 
            this.includeUpdatesCheckBox.AutoSize = true;
            this.includeUpdatesCheckBox.Location = new System.Drawing.Point(15, 91);
            this.includeUpdatesCheckBox.Name = "includeUpdatesCheckBox";
            this.includeUpdatesCheckBox.Size = new System.Drawing.Size(102, 17);
            this.includeUpdatesCheckBox.TabIndex = 7;
            this.includeUpdatesCheckBox.Text = "Include updates";
            this.includeUpdatesCheckBox.UseVisualStyleBackColor = true;
            // 
            // permanentSubcheckBox
            // 
            this.permanentSubcheckBox.AutoSize = true;
            this.permanentSubcheckBox.ForeColor = System.Drawing.Color.DarkRed;
            this.permanentSubcheckBox.Location = new System.Drawing.Point(15, 149);
            this.permanentSubcheckBox.Name = "permanentSubcheckBox";
            this.permanentSubcheckBox.Size = new System.Drawing.Size(264, 17);
            this.permanentSubcheckBox.TabIndex = 8;
            this.permanentSubcheckBox.Text = "Permanent subscription (auto subscribe at start-up)";
            this.permanentSubcheckBox.UseVisualStyleBackColor = true;
            // 
            // instanceTextBox
            // 
            this.instanceTextBox.Location = new System.Drawing.Point(71, 15);
            this.instanceTextBox.Name = "instanceTextBox";
            this.instanceTextBox.Size = new System.Drawing.Size(208, 20);
            this.instanceTextBox.TabIndex = 9;
            this.instanceTextBox.TextChanged += new System.EventHandler(this.instanceTextBox_TextChanged);
            // 
            // instanceLabel
            // 
            this.instanceLabel.AutoSize = true;
            this.instanceLabel.Location = new System.Drawing.Point(10, 18);
            this.instanceLabel.Name = "instanceLabel";
            this.instanceLabel.Size = new System.Drawing.Size(60, 13);
            this.instanceLabel.TabIndex = 10;
            this.instanceLabel.Text = "InstanceId:";
            // 
            // allInstancesCheckBox
            // 
            this.allInstancesCheckBox.AutoSize = true;
            this.allInstancesCheckBox.Location = new System.Drawing.Point(15, 42);
            this.allInstancesCheckBox.Name = "allInstancesCheckBox";
            this.allInstancesCheckBox.Size = new System.Drawing.Size(85, 17);
            this.allInstancesCheckBox.TabIndex = 11;
            this.allInstancesCheckBox.Text = "All instances";
            this.allInstancesCheckBox.UseVisualStyleBackColor = true;
            this.allInstancesCheckBox.CheckedChanged += new System.EventHandler(this.allInstancesCheckBox_CheckedChanged);
            // 
            // restartSubscriptionCheckBox
            // 
            this.restartSubscriptionCheckBox.AutoSize = true;
            this.restartSubscriptionCheckBox.Location = new System.Drawing.Point(15, 129);
            this.restartSubscriptionCheckBox.Name = "restartSubscriptionCheckBox";
            this.restartSubscriptionCheckBox.Size = new System.Drawing.Size(119, 17);
            this.restartSubscriptionCheckBox.TabIndex = 12;
            this.restartSubscriptionCheckBox.Text = "Restart subscription";
            this.restartSubscriptionCheckBox.UseVisualStyleBackColor = true;
            // 
            // includeSubClassesCheckBox
            // 
            this.includeSubClassesCheckBox.AutoSize = true;
            this.includeSubClassesCheckBox.Location = new System.Drawing.Point(15, 110);
            this.includeSubClassesCheckBox.Name = "includeSubClassesCheckBox";
            this.includeSubClassesCheckBox.Size = new System.Drawing.Size(116, 17);
            this.includeSubClassesCheckBox.TabIndex = 13;
            this.includeSubClassesCheckBox.Text = "Include subclasses";
            this.includeSubClassesCheckBox.UseVisualStyleBackColor = true;
            // 
            // SubscribeForm
            // 
            this.AcceptButton = this.okbutton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.cancelbutton;
            this.ClientSize = new System.Drawing.Size(305, 250);
            this.Controls.Add(this.includeSubClassesCheckBox);
            this.Controls.Add(this.restartSubscriptionCheckBox);
            this.Controls.Add(this.allInstancesCheckBox);
            this.Controls.Add(this.instanceLabel);
            this.Controls.Add(this.instanceTextBox);
            this.Controls.Add(this.permanentSubcheckBox);
            this.Controls.Add(this.includeUpdatesCheckBox);
            this.Controls.Add(this.cancelbutton);
            this.Controls.Add(this.okbutton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "SubscribeForm";
            this.Text = "Subscribe";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button okbutton;
        private System.Windows.Forms.Button cancelbutton;
        private System.Windows.Forms.CheckBox includeUpdatesCheckBox;
        private System.Windows.Forms.CheckBox permanentSubcheckBox;
        private System.Windows.Forms.TextBox instanceTextBox;
        private System.Windows.Forms.Label instanceLabel;
        private System.Windows.Forms.CheckBox allInstancesCheckBox;
        private System.Windows.Forms.CheckBox restartSubscriptionCheckBox;
        private System.Windows.Forms.CheckBox includeSubClassesCheckBox;
    }
}
