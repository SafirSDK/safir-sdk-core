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

namespace Sate
{
    partial class SubscribeRegistrationForm
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
            this.okbutton = new System.Windows.Forms.Button();
            this.cancelbutton = new System.Windows.Forms.Button();
            this.permanentSubcheckBox = new System.Windows.Forms.CheckBox();
            this.handlerIdLabel = new System.Windows.Forms.Label();
            this.handlerIdTextBox = new System.Windows.Forms.TextBox();
            this.includeSubClassesCheckBox = new System.Windows.Forms.CheckBox();
            this.restartCheckBox = new System.Windows.Forms.CheckBox();
            this.allHandlersCheckBox1 = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // okbutton
            // 
            this.okbutton.Location = new System.Drawing.Point(137, 160);
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
            this.cancelbutton.Location = new System.Drawing.Point(218, 160);
            this.cancelbutton.Name = "cancelbutton";
            this.cancelbutton.Size = new System.Drawing.Size(75, 23);
            this.cancelbutton.TabIndex = 6;
            this.cancelbutton.Text = "Cancel";
            this.cancelbutton.UseVisualStyleBackColor = true;
            this.cancelbutton.Click += new System.EventHandler(this.cancelbutton_Click);
            // 
            // permanentSubcheckBox
            // 
            this.permanentSubcheckBox.AutoSize = true;
            this.permanentSubcheckBox.ForeColor = System.Drawing.Color.DarkRed;
            this.permanentSubcheckBox.Location = new System.Drawing.Point(11, 137);
            this.permanentSubcheckBox.Name = "permanentSubcheckBox";
            this.permanentSubcheckBox.Size = new System.Drawing.Size(264, 17);
            this.permanentSubcheckBox.TabIndex = 8;
            this.permanentSubcheckBox.Text = "Permanent subscription (auto subscribe at start-up)";
            this.permanentSubcheckBox.UseVisualStyleBackColor = true;
            // 
            // handlerIdLabel
            // 
            this.handlerIdLabel.AutoSize = true;
            this.handlerIdLabel.Location = new System.Drawing.Point(9, 22);
            this.handlerIdLabel.Name = "handlerIdLabel";
            this.handlerIdLabel.Size = new System.Drawing.Size(53, 13);
            this.handlerIdLabel.TabIndex = 10;
            this.handlerIdLabel.Text = "HandlerId";
            // 
            // handlerIdTextBox
            // 
            this.handlerIdTextBox.Location = new System.Drawing.Point(68, 19);
            this.handlerIdTextBox.Name = "handlerIdTextBox";
            this.handlerIdTextBox.Size = new System.Drawing.Size(211, 20);
            this.handlerIdTextBox.TabIndex = 11;
            this.handlerIdTextBox.Text = "DEFAULT_HANDLER";
            this.handlerIdTextBox.TextChanged += new System.EventHandler(this.handlerIdTextBox_TextChanged);
            // 
            // includeSubClassesCheckBox
            // 
            this.includeSubClassesCheckBox.AutoSize = true;
            this.includeSubClassesCheckBox.Location = new System.Drawing.Point(11, 95);
            this.includeSubClassesCheckBox.Name = "includeSubClassesCheckBox";
            this.includeSubClassesCheckBox.Size = new System.Drawing.Size(116, 17);
            this.includeSubClassesCheckBox.TabIndex = 12;
            this.includeSubClassesCheckBox.Text = "Include subclasses";
            this.includeSubClassesCheckBox.UseVisualStyleBackColor = true;
            // 
            // restartCheckBox
            // 
            this.restartCheckBox.AutoSize = true;
            this.restartCheckBox.Location = new System.Drawing.Point(11, 117);
            this.restartCheckBox.Name = "restartCheckBox";
            this.restartCheckBox.Size = new System.Drawing.Size(119, 17);
            this.restartCheckBox.TabIndex = 13;
            this.restartCheckBox.Text = "Restart subscription";
            this.restartCheckBox.UseVisualStyleBackColor = true;
            // 
            // allHandlersCheckBox1
            // 
            this.allHandlersCheckBox1.AutoSize = true;
            this.allHandlersCheckBox1.Location = new System.Drawing.Point(13, 49);
            this.allHandlersCheckBox1.Name = "allHandlersCheckBox1";
            this.allHandlersCheckBox1.Size = new System.Drawing.Size(80, 17);
            this.allHandlersCheckBox1.TabIndex = 14;
            this.allHandlersCheckBox1.Text = "All handlers";
            this.allHandlersCheckBox1.UseVisualStyleBackColor = true;
            this.allHandlersCheckBox1.CheckedChanged += new System.EventHandler(this.allHandlersCheckBox1_CheckedChanged);
            // 
            // SubscribeRegistrationForm
            // 
            this.AcceptButton = this.okbutton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.cancelbutton;
            this.ClientSize = new System.Drawing.Size(305, 197);
            this.Controls.Add(this.allHandlersCheckBox1);
            this.Controls.Add(this.restartCheckBox);
            this.Controls.Add(this.includeSubClassesCheckBox);
            this.Controls.Add(this.handlerIdTextBox);
            this.Controls.Add(this.handlerIdLabel);
            this.Controls.Add(this.permanentSubcheckBox);
            this.Controls.Add(this.cancelbutton);
            this.Controls.Add(this.okbutton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Name = "SubscribeRegistrationForm";
            this.Text = "Subscribe for registrations";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button okbutton;
        private System.Windows.Forms.Button cancelbutton;
        private System.Windows.Forms.CheckBox permanentSubcheckBox;
        private System.Windows.Forms.Label handlerIdLabel;
        private System.Windows.Forms.TextBox handlerIdTextBox;
        private System.Windows.Forms.CheckBox includeSubClassesCheckBox;
        private System.Windows.Forms.CheckBox restartCheckBox;
        private System.Windows.Forms.CheckBox allHandlersCheckBox1;
    }

}
