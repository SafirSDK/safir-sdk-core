/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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

    partial class RegisterForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(RegisterForm));
            this.label1 = new System.Windows.Forms.Label();
            this.pendingcheckBox = new System.Windows.Forms.CheckBox();
            this.permanentregcheckBox = new System.Windows.Forms.CheckBox();
            this.okbutton = new System.Windows.Forms.Button();
            this.cancelbutton = new System.Windows.Forms.Button();
            this.handlerIdTextBox = new System.Windows.Forms.TextBox();
            this.injectionCheckBox = new System.Windows.Forms.CheckBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.requestorDecidesRadioButton = new System.Windows.Forms.RadioButton();
            this.handlerDecidesRadioButton = new System.Windows.Forms.RadioButton();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(16, 18);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(53, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "HandlerId";
            // 
            // pendingcheckBox
            // 
            this.pendingcheckBox.AutoSize = true;
            this.pendingcheckBox.Location = new System.Drawing.Point(15, 122);
            this.pendingcheckBox.Name = "pendingcheckBox";
            this.pendingcheckBox.Size = new System.Drawing.Size(119, 17);
            this.pendingcheckBox.TabIndex = 2;
            this.pendingcheckBox.Text = "Pending registration";
            this.pendingcheckBox.UseVisualStyleBackColor = true;
            this.pendingcheckBox.CheckedChanged += new System.EventHandler(this.pendingcheckBox_CheckedChanged);
            // 
            // permanentregcheckBox
            // 
            this.permanentregcheckBox.AutoSize = true;
            this.permanentregcheckBox.ForeColor = System.Drawing.Color.DarkRed;
            this.permanentregcheckBox.Location = new System.Drawing.Point(15, 161);
            this.permanentregcheckBox.Name = "permanentregcheckBox";
            this.permanentregcheckBox.Size = new System.Drawing.Size(248, 17);
            this.permanentregcheckBox.TabIndex = 3;
            this.permanentregcheckBox.Text = "Permanent registration (auto register at start-up)";
            this.permanentregcheckBox.UseVisualStyleBackColor = true;
            // 
            // okbutton
            // 
            this.okbutton.Location = new System.Drawing.Point(133, 191);
            this.okbutton.Name = "okbutton";
            this.okbutton.Size = new System.Drawing.Size(75, 23);
            this.okbutton.TabIndex = 4;
            this.okbutton.Text = "OK";
            this.okbutton.UseVisualStyleBackColor = true;
            this.okbutton.Click += new System.EventHandler(this.okbutton_Click);
            // 
            // cancelbutton
            // 
            this.cancelbutton.Location = new System.Drawing.Point(214, 191);
            this.cancelbutton.Name = "cancelbutton";
            this.cancelbutton.Size = new System.Drawing.Size(75, 23);
            this.cancelbutton.TabIndex = 5;
            this.cancelbutton.Text = "Cancel";
            this.cancelbutton.UseVisualStyleBackColor = true;
            this.cancelbutton.Click += new System.EventHandler(this.cancelbutton_Click);
            // 
            // handlerIdTextBox
            // 
            this.handlerIdTextBox.Location = new System.Drawing.Point(70, 14);
            this.handlerIdTextBox.Name = "handlerIdTextBox";
            this.handlerIdTextBox.Size = new System.Drawing.Size(219, 20);
            this.handlerIdTextBox.TabIndex = 6;
            this.handlerIdTextBox.Text = "DEFAULT_HANDLER";
            this.handlerIdTextBox.TextChanged += new System.EventHandler(this.handlerIdTextBox_TextChanged);
            // 
            // injectionCheckBox
            // 
            this.injectionCheckBox.AutoSize = true;
            this.injectionCheckBox.Location = new System.Drawing.Point(15, 141);
            this.injectionCheckBox.Name = "injectionCheckBox";
            this.injectionCheckBox.Size = new System.Drawing.Size(104, 17);
            this.injectionCheckBox.TabIndex = 7;
            this.injectionCheckBox.Text = "Injection handler";
            this.injectionCheckBox.UseVisualStyleBackColor = true;
            this.injectionCheckBox.CheckedChanged += new System.EventHandler(this.injectionCheckBox_CheckedChanged);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.handlerDecidesRadioButton);
            this.groupBox1.Controls.Add(this.requestorDecidesRadioButton);
            this.groupBox1.Location = new System.Drawing.Point(9, 48);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(191, 68);
            this.groupBox1.TabIndex = 8;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "InstanceId policy";
            // 
            // requestDecidesRadioButton
            // 
            this.requestorDecidesRadioButton.AutoSize = true;
            this.requestorDecidesRadioButton.Checked = true;
            this.requestorDecidesRadioButton.Location = new System.Drawing.Point(7, 20);
            this.requestorDecidesRadioButton.Name = "requestDecidesRadioButton";
            this.requestorDecidesRadioButton.Size = new System.Drawing.Size(114, 17);
            this.requestorDecidesRadioButton.TabIndex = 0;
            this.requestorDecidesRadioButton.TabStop = true;
            this.requestorDecidesRadioButton.Text = "Requestor decides";
            this.requestorDecidesRadioButton.UseVisualStyleBackColor = true;
            // 
            // handlerDecidesRadioButton
            // 
            this.handlerDecidesRadioButton.AutoSize = true;
            this.handlerDecidesRadioButton.Location = new System.Drawing.Point(7, 40);
            this.handlerDecidesRadioButton.Name = "handlerDecidesRadioButton";
            this.handlerDecidesRadioButton.Size = new System.Drawing.Size(102, 17);
            this.handlerDecidesRadioButton.TabIndex = 1;
            this.handlerDecidesRadioButton.Text = "Handler decides";
            this.handlerDecidesRadioButton.UseVisualStyleBackColor = true;
            // 
            // RegisterForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(315, 229);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.injectionCheckBox);
            this.Controls.Add(this.handlerIdTextBox);
            this.Controls.Add(this.cancelbutton);
            this.Controls.Add(this.okbutton);
            this.Controls.Add(this.permanentregcheckBox);
            this.Controls.Add(this.pendingcheckBox);
            this.Controls.Add(this.label1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "RegisterForm";
            this.Text = "Register";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox pendingcheckBox;
        private System.Windows.Forms.CheckBox permanentregcheckBox;
        private System.Windows.Forms.Button okbutton;
        private System.Windows.Forms.Button cancelbutton;
        private System.Windows.Forms.TextBox handlerIdTextBox;
        private System.Windows.Forms.CheckBox injectionCheckBox;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.RadioButton handlerDecidesRadioButton;
        private System.Windows.Forms.RadioButton requestorDecidesRadioButton;
    }

}
