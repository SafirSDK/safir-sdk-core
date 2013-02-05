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
    partial class SubscribeMessageForm
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
            this.channelLabel = new System.Windows.Forms.Label();
            this.okbutton = new System.Windows.Forms.Button();
            this.cancelbutton = new System.Windows.Forms.Button();
            this.permanentSubcheckBox = new System.Windows.Forms.CheckBox();
            this.channelTextBox = new System.Windows.Forms.TextBox();
            this.allChannelsCheckBox = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // channelLabel
            // 
            this.channelLabel.AutoSize = true;
            this.channelLabel.Location = new System.Drawing.Point(9, 23);
            this.channelLabel.Name = "channelLabel";
            this.channelLabel.Size = new System.Drawing.Size(55, 13);
            this.channelLabel.TabIndex = 1;
            this.channelLabel.Text = "ChannelId";
            // 
            // okbutton
            // 
            this.okbutton.Location = new System.Drawing.Point(141, 101);
            this.okbutton.Name = "okbutton";
            this.okbutton.Size = new System.Drawing.Size(75, 25);
            this.okbutton.TabIndex = 5;
            this.okbutton.Text = "OK";
            this.okbutton.UseVisualStyleBackColor = true;
            this.okbutton.Click += new System.EventHandler(this.okbutton_Click);
            // 
            // cancelbutton
            // 
            this.cancelbutton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancelbutton.Location = new System.Drawing.Point(222, 101);
            this.cancelbutton.Name = "cancelbutton";
            this.cancelbutton.Size = new System.Drawing.Size(75, 25);
            this.cancelbutton.TabIndex = 6;
            this.cancelbutton.Text = "Cancel";
            this.cancelbutton.UseVisualStyleBackColor = true;
            this.cancelbutton.Click += new System.EventHandler(this.cancelbutton_Click);
            // 
            // permanentSubcheckBox
            // 
            this.permanentSubcheckBox.AutoSize = true;
            this.permanentSubcheckBox.ForeColor = System.Drawing.Color.DarkRed;
            this.permanentSubcheckBox.Location = new System.Drawing.Point(15, 78);
            this.permanentSubcheckBox.Name = "permanentSubcheckBox";
            this.permanentSubcheckBox.Size = new System.Drawing.Size(264, 17);
            this.permanentSubcheckBox.TabIndex = 8;
            this.permanentSubcheckBox.Text = "Permanent subscription (auto subscribe at start-up)";
            this.permanentSubcheckBox.UseVisualStyleBackColor = true;
            // 
            // channelTextBox
            // 
            this.channelTextBox.Location = new System.Drawing.Point(68, 19);
            this.channelTextBox.Name = "channelTextBox";
            this.channelTextBox.Size = new System.Drawing.Size(211, 20);
            this.channelTextBox.TabIndex = 9;
            this.channelTextBox.Text = "DEFAULT_CHANNEL";
            this.channelTextBox.TextChanged += new System.EventHandler(this.channelTextBox_TextChanged);
            // 
            // allChannelsCheckBox
            // 
            this.allChannelsCheckBox.AutoSize = true;
            this.allChannelsCheckBox.Location = new System.Drawing.Point(15, 55);
            this.allChannelsCheckBox.Name = "allChannelsCheckBox";
            this.allChannelsCheckBox.Size = new System.Drawing.Size(83, 17);
            this.allChannelsCheckBox.TabIndex = 10;
            this.allChannelsCheckBox.Text = "All channels";
            this.allChannelsCheckBox.UseVisualStyleBackColor = true;
            this.allChannelsCheckBox.CheckedChanged += new System.EventHandler(this.allChannelsCheckBox_CheckedChanged);
            // 
            // SubscribeMessageForm
            // 
            this.AcceptButton = this.okbutton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.cancelbutton;
            this.ClientSize = new System.Drawing.Size(305, 134);
            this.Controls.Add(this.allChannelsCheckBox);
            this.Controls.Add(this.channelTextBox);
            this.Controls.Add(this.permanentSubcheckBox);
            this.Controls.Add(this.cancelbutton);
            this.Controls.Add(this.okbutton);
            this.Controls.Add(this.channelLabel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Name = "SubscribeMessageForm";
            this.Text = "Subscribe message";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label channelLabel;
            private System.Windows.Forms.Button okbutton;
        private System.Windows.Forms.Button cancelbutton;
            private System.Windows.Forms.CheckBox permanentSubcheckBox;
        private System.Windows.Forms.TextBox channelTextBox;
        private System.Windows.Forms.CheckBox allChannelsCheckBox;
        }
    }
