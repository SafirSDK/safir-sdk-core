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
    partial class TimestampCalculatorForm
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
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.int64RadioButton = new System.Windows.Forms.RadioButton();
            this.second64RadioButton = new System.Windows.Forms.RadioButton();
            this.convertToHumanButton = new System.Windows.Forms.Button();
            this.convertToMsButton = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.numberTextBox = new System.Windows.Forms.TextBox();
            this.humanTextBox = new System.Windows.Forms.TextBox();
            this.closeButton = new System.Windows.Forms.Button();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.groupBox2);
            this.groupBox1.Controls.Add(this.convertToHumanButton);
            this.groupBox1.Controls.Add(this.convertToMsButton);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.numberTextBox);
            this.groupBox1.Controls.Add(this.humanTextBox);
            this.groupBox1.Location = new System.Drawing.Point(7, 9);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(561, 197);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Calculate timestamp";
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.int64RadioButton);
            this.groupBox2.Controls.Add(this.second64RadioButton);
            this.groupBox2.Location = new System.Drawing.Point(28, 130);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(194, 61);
            this.groupBox2.TabIndex = 10;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Conversion mode";
            // 
            // int64RadioButton
            // 
            this.int64RadioButton.AutoSize = true;
            this.int64RadioButton.Checked = true;
            this.int64RadioButton.Location = new System.Drawing.Point(6, 19);
            this.int64RadioButton.Name = "int64RadioButton";
            this.int64RadioButton.Size = new System.Drawing.Size(154, 17);
            this.int64RadioButton.TabIndex = 0;
            this.int64RadioButton.TabStop = true;
            this.int64RadioButton.Text = "Int64 (us since 2008-01-01)";
            this.int64RadioButton.UseVisualStyleBackColor = true;
            // 
            // second64RadioButton
            // 
            this.second64RadioButton.AutoSize = true;
            this.second64RadioButton.Location = new System.Drawing.Point(6, 38);
            this.second64RadioButton.Name = "second64RadioButton";
            this.second64RadioButton.Size = new System.Drawing.Size(173, 17);
            this.second64RadioButton.TabIndex = 1;
            this.second64RadioButton.TabStop = true;
            this.second64RadioButton.Text = "Second64 (s since 1970-01-01)";
            this.second64RadioButton.UseVisualStyleBackColor = true;
            // 
            // convertToHumanButton
            // 
            this.convertToHumanButton.Location = new System.Drawing.Point(432, 96);
            this.convertToHumanButton.Name = "convertToHumanButton";
            this.convertToHumanButton.Size = new System.Drawing.Size(109, 20);
            this.convertToHumanButton.TabIndex = 7;
            this.convertToHumanButton.Text = "Convert to human";
            this.convertToHumanButton.UseVisualStyleBackColor = true;
            this.convertToHumanButton.Click += new System.EventHandler(this.convertToHumanButton_Click);
            // 
            // convertToMsButton
            // 
            this.convertToMsButton.Location = new System.Drawing.Point(432, 51);
            this.convertToMsButton.Name = "convertToMsButton";
            this.convertToMsButton.Size = new System.Drawing.Size(110, 20);
            this.convertToMsButton.TabIndex = 6;
            this.convertToMsButton.Text = "Convert to digits";
            this.convertToMsButton.UseVisualStyleBackColor = true;
            this.convertToMsButton.Click += new System.EventHandler(this.convertToMsButton_Click);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(30, 81);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(95, 13);
            this.label2.TabIndex = 5;
            this.label2.Text = "Enter time in digits:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(30, 33);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(202, 13);
            this.label1.TabIndex = 4;
            this.label1.Text = "Enter time in (YYYY-MM-DD HH:MM:SS):";
            // 
            // numberTextBox
            // 
            this.numberTextBox.Location = new System.Drawing.Point(28, 97);
            this.numberTextBox.Name = "numberTextBox";
            this.numberTextBox.Size = new System.Drawing.Size(398, 20);
            this.numberTextBox.TabIndex = 1;
            // 
            // humanTextBox
            // 
            this.humanTextBox.Location = new System.Drawing.Point(28, 51);
            this.humanTextBox.Name = "humanTextBox";
            this.humanTextBox.Size = new System.Drawing.Size(398, 20);
            this.humanTextBox.TabIndex = 0;
            // 
            // closeButton
            // 
            this.closeButton.Location = new System.Drawing.Point(471, 212);
            this.closeButton.Name = "closeButton";
            this.closeButton.Size = new System.Drawing.Size(87, 25);
            this.closeButton.TabIndex = 2;
            this.closeButton.Text = "Close";
            this.closeButton.UseVisualStyleBackColor = true;
            this.closeButton.Click += new System.EventHandler(this.closeButton_Click);
            // 
            // TimestampCalculatorForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(574, 247);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.closeButton);
            this.Name = "TimestampCalculatorForm";
            this.Text = "TimestampCalculatorForm";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button convertToHumanButton;
        private System.Windows.Forms.Button convertToMsButton;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button closeButton;
        private System.Windows.Forms.TextBox numberTextBox;
        private System.Windows.Forms.TextBox humanTextBox;
        private System.Windows.Forms.RadioButton int64RadioButton;
        private System.Windows.Forms.RadioButton second64RadioButton;
        private System.Windows.Forms.GroupBox groupBox2;
    }
}
