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

    partial class CustomImageForm
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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CustomImageForm));
            this.baseFillpanel = new System.Windows.Forms.Panel();
            this.baseFillRightpanel = new System.Windows.Forms.Panel();
            this.removeimagebutton = new System.Windows.Forms.Button();
            this.addimagebutton = new System.Windows.Forms.Button();
            this.baseFillFillpanel = new System.Windows.Forms.Panel();
            this.imagelistView = new System.Windows.Forms.ListView();
            this.imageList = new System.Windows.Forms.ImageList(this.components);
            this.baseBottompanel = new System.Windows.Forms.Panel();
            this.flowLayoutPanel1 = new System.Windows.Forms.FlowLayoutPanel();
            this.cusomtImgaeForLabel = new System.Windows.Forms.Label();
            this.classlabel = new System.Windows.Forms.Label();
            this.defaultimagecheckBox = new System.Windows.Forms.CheckBox();
            this.applyToChildclassescheckBox = new System.Windows.Forms.CheckBox();
            this.okbutton = new System.Windows.Forms.Button();
            this.cancelbutton = new System.Windows.Forms.Button();
            this.imageOpenFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.customImagesgroupBox = new System.Windows.Forms.GroupBox();
            this.baseFillpanel.SuspendLayout();
            this.baseFillRightpanel.SuspendLayout();
            this.baseFillFillpanel.SuspendLayout();
            this.baseBottompanel.SuspendLayout();
            this.flowLayoutPanel1.SuspendLayout();
            this.customImagesgroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // baseFillpanel
            // 
            this.baseFillpanel.Controls.Add(this.baseFillRightpanel);
            this.baseFillpanel.Controls.Add(this.baseFillFillpanel);
            this.baseFillpanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.baseFillpanel.Location = new System.Drawing.Point(5, 18);
            this.baseFillpanel.Name = "baseFillpanel";
            this.baseFillpanel.Size = new System.Drawing.Size(389, 240);
            this.baseFillpanel.TabIndex = 0;
            // 
            // baseFillRightpanel
            // 
            this.baseFillRightpanel.Controls.Add(this.removeimagebutton);
            this.baseFillRightpanel.Controls.Add(this.addimagebutton);
            this.baseFillRightpanel.Dock = System.Windows.Forms.DockStyle.Right;
            this.baseFillRightpanel.Location = new System.Drawing.Point(292, 0);
            this.baseFillRightpanel.Name = "baseFillRightpanel";
            this.baseFillRightpanel.Size = new System.Drawing.Size(97, 240);
            this.baseFillRightpanel.TabIndex = 2;
            // 
            // removeimagebutton
            // 
            this.removeimagebutton.Location = new System.Drawing.Point(11, 35);
            this.removeimagebutton.Name = "removeimagebutton";
            this.removeimagebutton.Size = new System.Drawing.Size(75, 23);
            this.removeimagebutton.TabIndex = 1;
            this.removeimagebutton.Text = "Remove";
            this.removeimagebutton.UseVisualStyleBackColor = true;
            this.removeimagebutton.Click += new System.EventHandler(this.removeimagebutton_Click);
            // 
            // addimagebutton
            // 
            this.addimagebutton.Location = new System.Drawing.Point(11, 6);
            this.addimagebutton.Name = "addimagebutton";
            this.addimagebutton.Size = new System.Drawing.Size(75, 23);
            this.addimagebutton.TabIndex = 0;
            this.addimagebutton.Text = "Open...";
            this.addimagebutton.UseVisualStyleBackColor = true;
            this.addimagebutton.Click += new System.EventHandler(this.addimagebutton_Click);
            // 
            // baseFillFillpanel
            // 
            this.baseFillFillpanel.Controls.Add(this.imagelistView);
            this.baseFillFillpanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.baseFillFillpanel.Location = new System.Drawing.Point(0, 0);
            this.baseFillFillpanel.Name = "baseFillFillpanel";
            this.baseFillFillpanel.Size = new System.Drawing.Size(389, 240);
            this.baseFillFillpanel.TabIndex = 1;
            // 
            // imagelistView
            // 
            this.imagelistView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.imagelistView.LargeImageList = this.imageList;
            this.imagelistView.Location = new System.Drawing.Point(0, 0);
            this.imagelistView.Margin = new System.Windows.Forms.Padding(50);
            this.imagelistView.Name = "imagelistView";
            this.imagelistView.Size = new System.Drawing.Size(389, 240);
            this.imagelistView.StateImageList = this.imageList;
            this.imagelistView.TabIndex = 0;
            this.imagelistView.UseCompatibleStateImageBehavior = false;
            // 
            // imageList
            // 
            this.imageList.ColorDepth = System.Windows.Forms.ColorDepth.Depth8Bit;
            this.imageList.ImageSize = new System.Drawing.Size(16, 16);
            this.imageList.TransparentColor = System.Drawing.Color.Transparent;
            // 
            // baseBottompanel
            // 
            this.baseBottompanel.Controls.Add(this.flowLayoutPanel1);
            this.baseBottompanel.Controls.Add(this.defaultimagecheckBox);
            this.baseBottompanel.Controls.Add(this.applyToChildclassescheckBox);
            this.baseBottompanel.Controls.Add(this.okbutton);
            this.baseBottompanel.Controls.Add(this.cancelbutton);
            this.baseBottompanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.baseBottompanel.Location = new System.Drawing.Point(10, 273);
            this.baseBottompanel.Name = "baseBottompanel";
            this.baseBottompanel.Size = new System.Drawing.Size(399, 112);
            this.baseBottompanel.TabIndex = 1;
            // 
            // flowLayoutPanel1
            // 
            this.flowLayoutPanel1.Controls.Add(this.cusomtImgaeForLabel);
            this.flowLayoutPanel1.Controls.Add(this.classlabel);
            this.flowLayoutPanel1.Location = new System.Drawing.Point(15, 11);
            this.flowLayoutPanel1.Name = "flowLayoutPanel1";
            this.flowLayoutPanel1.Size = new System.Drawing.Size(379, 25);
            this.flowLayoutPanel1.TabIndex = 6;
            // 
            // cusomtImgaeForLabel
            // 
            this.cusomtImgaeForLabel.AutoSize = true;
            this.cusomtImgaeForLabel.Location = new System.Drawing.Point(3, 0);
            this.cusomtImgaeForLabel.Name = "cusomtImgaeForLabel";
            this.cusomtImgaeForLabel.Size = new System.Drawing.Size(94, 13);
            this.cusomtImgaeForLabel.TabIndex = 2;
            this.cusomtImgaeForLabel.Text = "Custom image for: ";
            // 
            // classlabel
            // 
            this.classlabel.AutoSize = true;
            this.classlabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.classlabel.ForeColor = System.Drawing.SystemColors.ControlText;
            this.classlabel.Location = new System.Drawing.Point(103, 0);
            this.classlabel.Name = "classlabel";
            this.classlabel.Size = new System.Drawing.Size(74, 13);
            this.classlabel.TabIndex = 3;
            this.classlabel.Text = "<Unknown>";
            // 
            // defaultimagecheckBox
            // 
            this.defaultimagecheckBox.AutoSize = true;
            this.defaultimagecheckBox.Location = new System.Drawing.Point(15, 65);
            this.defaultimagecheckBox.Name = "defaultimagecheckBox";
            this.defaultimagecheckBox.Size = new System.Drawing.Size(108, 17);
            this.defaultimagecheckBox.TabIndex = 5;
            this.defaultimagecheckBox.Text = "Set default image";
            this.defaultimagecheckBox.UseVisualStyleBackColor = true;
            // 
            // applyToChildclassescheckBox
            // 
            this.applyToChildclassescheckBox.AutoSize = true;
            this.applyToChildclassescheckBox.Location = new System.Drawing.Point(15, 42);
            this.applyToChildclassescheckBox.Name = "applyToChildclassescheckBox";
            this.applyToChildclassescheckBox.Size = new System.Drawing.Size(127, 17);
            this.applyToChildclassescheckBox.TabIndex = 4;
            this.applyToChildclassescheckBox.Text = "Apply to child classes";
            this.applyToChildclassescheckBox.UseVisualStyleBackColor = true;
            // 
            // okbutton
            // 
            this.okbutton.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.okbutton.Location = new System.Drawing.Point(232, 83);
            this.okbutton.Name = "okbutton";
            this.okbutton.Size = new System.Drawing.Size(75, 23);
            this.okbutton.TabIndex = 1;
            this.okbutton.Text = "OK";
            this.okbutton.UseVisualStyleBackColor = true;
            this.okbutton.Click += new System.EventHandler(this.okbutton_Click);
            // 
            // cancelbutton
            // 
            this.cancelbutton.Anchor = System.Windows.Forms.AnchorStyles.Right;
            this.cancelbutton.Location = new System.Drawing.Point(313, 83);
            this.cancelbutton.Name = "cancelbutton";
            this.cancelbutton.Size = new System.Drawing.Size(75, 23);
            this.cancelbutton.TabIndex = 0;
            this.cancelbutton.Text = "Cancel";
            this.cancelbutton.UseVisualStyleBackColor = true;
            this.cancelbutton.Click += new System.EventHandler(this.cancelbutton_Click);
            // 
            // imageOpenFileDialog
            // 
            this.imageOpenFileDialog.FileName = "openFileDialog1";
            // 
            // customImagesgroupBox
            // 
            this.customImagesgroupBox.Controls.Add(this.baseFillpanel);
            this.customImagesgroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.customImagesgroupBox.Location = new System.Drawing.Point(10, 10);
            this.customImagesgroupBox.Margin = new System.Windows.Forms.Padding(10);
            this.customImagesgroupBox.Name = "customImagesgroupBox";
            this.customImagesgroupBox.Padding = new System.Windows.Forms.Padding(5);
            this.customImagesgroupBox.Size = new System.Drawing.Size(399, 263);
            this.customImagesgroupBox.TabIndex = 2;
            this.customImagesgroupBox.TabStop = false;
            this.customImagesgroupBox.Text = "Custom Images";
            // 
            // CustomImageForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(419, 395);
            this.Controls.Add(this.customImagesgroupBox);
            this.Controls.Add(this.baseBottompanel);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "CustomImageForm";
            this.Padding = new System.Windows.Forms.Padding(10);
            this.Text = "Custom Image";
            this.baseFillpanel.ResumeLayout(false);
            this.baseFillRightpanel.ResumeLayout(false);
            this.baseFillFillpanel.ResumeLayout(false);
            this.baseBottompanel.ResumeLayout(false);
            this.baseBottompanel.PerformLayout();
            this.flowLayoutPanel1.ResumeLayout(false);
            this.flowLayoutPanel1.PerformLayout();
            this.customImagesgroupBox.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel baseFillpanel;
        private System.Windows.Forms.Panel baseBottompanel;
        private System.Windows.Forms.Panel baseFillRightpanel;
        private System.Windows.Forms.Panel baseFillFillpanel;
        private System.Windows.Forms.Button okbutton;
        private System.Windows.Forms.Button cancelbutton;
        private System.Windows.Forms.Button addimagebutton;
        private System.Windows.Forms.ListView imagelistView;
        private System.Windows.Forms.Button removeimagebutton;
        private System.Windows.Forms.CheckBox defaultimagecheckBox;
        private System.Windows.Forms.CheckBox applyToChildclassescheckBox;
        private System.Windows.Forms.Label classlabel;
        private System.Windows.Forms.Label cusomtImgaeForLabel;
        private System.Windows.Forms.OpenFileDialog imageOpenFileDialog;
        private System.Windows.Forms.ImageList imageList;
        private System.Windows.Forms.GroupBox customImagesgroupBox;
        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
    }
}
