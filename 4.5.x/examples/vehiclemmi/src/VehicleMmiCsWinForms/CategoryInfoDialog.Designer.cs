namespace VehicleMmiCsWinForms
{
    partial class CategoryInfoDialog
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
            Hide();
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CategoryInfoDialog));
            this.textboxAdditionalRemark = new System.Windows.Forms.TextBox();
            this.labelCurrentSpeed = new System.Windows.Forms.Label();
            this.labelSpeed = new System.Windows.Forms.Label();
            this.textBoxSpeed = new System.Windows.Forms.TextBox();
            this.labelCategoryCode = new System.Windows.Forms.Label();
            this.labelAdditionalRemark = new System.Windows.Forms.Label();
            this.textBoxCategoryCode = new System.Windows.Forms.TextBox();
            this.checkBoxDriversLicenseReq = new System.Windows.Forms.CheckBox();
            this.buttonClose = new System.Windows.Forms.Button();
            this.buttonApply = new System.Windows.Forms.Button();
            this.statusStrip = new System.Windows.Forms.StatusStrip();
            this.toolStripStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.statusStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // textboxAdditionalRemark
            // 
            this.textboxAdditionalRemark.Location = new System.Drawing.Point(105, 66);
            this.textboxAdditionalRemark.Name = "textboxAdditionalRemark";
            this.textboxAdditionalRemark.Size = new System.Drawing.Size(100, 20);
            this.textboxAdditionalRemark.TabIndex = 5;
            // 
            // labelCurrentSpeed
            // 
            this.labelCurrentSpeed.AutoSize = true;
            this.labelCurrentSpeed.Location = new System.Drawing.Point(9, 45);
            this.labelCurrentSpeed.Name = "labelCurrentSpeed";
            this.labelCurrentSpeed.Size = new System.Drawing.Size(38, 13);
            this.labelCurrentSpeed.TabIndex = 2;
            this.labelCurrentSpeed.Text = "Speed";
            // 
            // labelSpeed
            // 
            this.labelSpeed.AutoSize = true;
            this.labelSpeed.Location = new System.Drawing.Point(9, 45);
            this.labelSpeed.Name = "labelSpeed";
            this.labelSpeed.Size = new System.Drawing.Size(38, 13);
            this.labelSpeed.TabIndex = 25;
            this.labelSpeed.Text = "Speed";
            // 
            // textBoxSpeed
            // 
            this.textBoxSpeed.Location = new System.Drawing.Point(105, 40);
            this.textBoxSpeed.Name = "textBoxSpeed";
            this.textBoxSpeed.Size = new System.Drawing.Size(100, 20);
            this.textBoxSpeed.TabIndex = 3;
            // 
            // labelCategoryCode
            // 
            this.labelCategoryCode.AutoSize = true;
            this.labelCategoryCode.Location = new System.Drawing.Point(9, 19);
            this.labelCategoryCode.Name = "labelCategoryCode";
            this.labelCategoryCode.Size = new System.Drawing.Size(76, 13);
            this.labelCategoryCode.TabIndex = 0;
            this.labelCategoryCode.Text = "Category code";
            // 
            // labelAdditionalRemark
            // 
            this.labelAdditionalRemark.AutoSize = true;
            this.labelAdditionalRemark.Location = new System.Drawing.Point(9, 71);
            this.labelAdditionalRemark.Name = "labelAdditionalRemark";
            this.labelAdditionalRemark.Size = new System.Drawing.Size(88, 13);
            this.labelAdditionalRemark.TabIndex = 4;
            this.labelAdditionalRemark.Text = "Additional remark";
            // 
            // textBoxCategoryCode
            // 
            this.textBoxCategoryCode.Enabled = false;
            this.textBoxCategoryCode.Location = new System.Drawing.Point(105, 14);
            this.textBoxCategoryCode.Name = "textBoxCategoryCode";
            this.textBoxCategoryCode.Size = new System.Drawing.Size(100, 20);
            this.textBoxCategoryCode.TabIndex = 1;
            // 
            // checkBoxDriversLicenseReq
            // 
            this.checkBoxDriversLicenseReq.AutoSize = true;
            this.checkBoxDriversLicenseReq.Location = new System.Drawing.Point(12, 101);
            this.checkBoxDriversLicenseReq.Name = "checkBoxDriversLicenseReq";
            this.checkBoxDriversLicenseReq.Size = new System.Drawing.Size(136, 17);
            this.checkBoxDriversLicenseReq.TabIndex = 6;
            this.checkBoxDriversLicenseReq.Text = "Drivers license required";
            this.checkBoxDriversLicenseReq.UseVisualStyleBackColor = true;
            // 
            // buttonClose
            // 
            this.buttonClose.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.buttonClose.Location = new System.Drawing.Point(93, 144);
            this.buttonClose.Name = "buttonClose";
            this.buttonClose.Size = new System.Drawing.Size(75, 23);
            this.buttonClose.TabIndex = 8;
            this.buttonClose.Text = "Close";
            this.buttonClose.UseVisualStyleBackColor = true;
            this.buttonClose.Click += new System.EventHandler(this.buttonCancel_Click);
            // 
            // buttonApply
            // 
            this.buttonApply.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonApply.Location = new System.Drawing.Point(174, 144);
            this.buttonApply.Name = "buttonApply";
            this.buttonApply.Size = new System.Drawing.Size(75, 23);
            this.buttonApply.TabIndex = 9;
            this.buttonApply.Text = "Apply";
            this.buttonApply.UseVisualStyleBackColor = true;
            this.buttonApply.Click += new System.EventHandler(this.buttonApply_Click);
            // 
            // statusStrip
            // 
            this.statusStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripStatus});
            this.statusStrip.Location = new System.Drawing.Point(0, 170);
            this.statusStrip.Name = "statusStrip";
            this.statusStrip.Size = new System.Drawing.Size(252, 22);
            this.statusStrip.TabIndex = 10;
            this.statusStrip.Text = "OK";
            // 
            // toolStripStatus
            // 
            this.toolStripStatus.Name = "toolStripStatus";
            this.toolStripStatus.Size = new System.Drawing.Size(21, 17);
            this.toolStripStatus.Text = "OK";
            // 
            // CategoryInfoDialog
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.buttonClose;
            this.ClientSize = new System.Drawing.Size(252, 192);
            this.Controls.Add(this.statusStrip);
            this.Controls.Add(this.buttonClose);
            this.Controls.Add(this.buttonApply);
            this.Controls.Add(this.checkBoxDriversLicenseReq);
            this.Controls.Add(this.textboxAdditionalRemark);
            this.Controls.Add(this.labelCurrentSpeed);
            this.Controls.Add(this.labelSpeed);
            this.Controls.Add(this.textBoxSpeed);
            this.Controls.Add(this.labelCategoryCode);
            this.Controls.Add(this.labelAdditionalRemark);
            this.Controls.Add(this.textBoxCategoryCode);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "CategoryInfoDialog";
            this.Text = "Category information";
            this.statusStrip.ResumeLayout(false);
            this.statusStrip.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox textboxAdditionalRemark;
        private System.Windows.Forms.Label labelCurrentSpeed;
        private System.Windows.Forms.Label labelSpeed;
        private System.Windows.Forms.TextBox textBoxSpeed;
        private System.Windows.Forms.Label labelCategoryCode;
        private System.Windows.Forms.Label labelAdditionalRemark;
        private System.Windows.Forms.TextBox textBoxCategoryCode;
        private System.Windows.Forms.CheckBox checkBoxDriversLicenseReq;
        private System.Windows.Forms.StatusStrip statusStrip;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatus;
        private System.Windows.Forms.Button buttonClose;
        private System.Windows.Forms.Button buttonApply;
    }
}