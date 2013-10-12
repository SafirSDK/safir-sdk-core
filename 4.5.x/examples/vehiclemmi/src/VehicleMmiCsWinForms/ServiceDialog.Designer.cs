namespace VehicleMmiCsWinForms
{
    partial class ServiceDialog
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        /// private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            /*if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);*/
            Hide();
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ServiceDialog));
            this.textBoxIdentification = new System.Windows.Forms.TextBox();
            this.labelNewSpeed = new System.Windows.Forms.Label();
            this.labelSpeedDiff = new System.Windows.Forms.Label();
            this.textBoxCurrentSpeed = new System.Windows.Forms.TextBox();
            this.labelSpeed = new System.Windows.Forms.Label();
            this.buttonClose = new System.Windows.Forms.Button();
            this.buttonApply = new System.Windows.Forms.Button();
            this.textboxSpeedDiff = new System.Windows.Forms.TextBox();
            this.textboxNewSpeed = new System.Windows.Forms.TextBox();
            this.labelIdentification = new System.Windows.Forms.Label();
            this.labelCurrentSpeed = new System.Windows.Forms.Label();
            this.statusStrip = new System.Windows.Forms.StatusStrip();
            this.toolStripStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.statusStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // textBoxIdentification
            // 
            this.textBoxIdentification.Enabled = false;
            this.textBoxIdentification.Location = new System.Drawing.Point(108, 9);
            this.textBoxIdentification.Name = "textBoxIdentification";
            this.textBoxIdentification.Size = new System.Drawing.Size(100, 20);
            this.textBoxIdentification.TabIndex = 1;
            // 
            // labelNewSpeed
            // 
            this.labelNewSpeed.AutoSize = true;
            this.labelNewSpeed.Location = new System.Drawing.Point(12, 66);
            this.labelNewSpeed.Name = "labelNewSpeed";
            this.labelNewSpeed.Size = new System.Drawing.Size(63, 13);
            this.labelNewSpeed.TabIndex = 4;
            this.labelNewSpeed.Text = "New Speed";
            // 
            // labelSpeedDiff
            // 
            this.labelSpeedDiff.AutoSize = true;
            this.labelSpeedDiff.Location = new System.Drawing.Point(12, 91);
            this.labelSpeedDiff.Name = "labelSpeedDiff";
            this.labelSpeedDiff.Size = new System.Drawing.Size(90, 13);
            this.labelSpeedDiff.TabIndex = 6;
            this.labelSpeedDiff.Text = "Speed Difference";
            // 
            // textBoxCurrentSpeed
            // 
            this.textBoxCurrentSpeed.Enabled = false;
            this.textBoxCurrentSpeed.Location = new System.Drawing.Point(108, 35);
            this.textBoxCurrentSpeed.Name = "textBoxCurrentSpeed";
            this.textBoxCurrentSpeed.Size = new System.Drawing.Size(100, 20);
            this.textBoxCurrentSpeed.TabIndex = 3;
            // 
            // labelSpeed
            // 
            this.labelSpeed.AutoSize = true;
            this.labelSpeed.Location = new System.Drawing.Point(12, 40);
            this.labelSpeed.Name = "labelSpeed";
            this.labelSpeed.Size = new System.Drawing.Size(75, 13);
            this.labelSpeed.TabIndex = 7;
            this.labelSpeed.Text = "Current Speed";
            // 
            // buttonClose
            // 
            this.buttonClose.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.buttonClose.Location = new System.Drawing.Point(12, 116);
            this.buttonClose.Name = "buttonClose";
            this.buttonClose.Size = new System.Drawing.Size(90, 23);
            this.buttonClose.TabIndex = 8;
            this.buttonClose.Text = "Close";
            this.buttonClose.UseVisualStyleBackColor = true;
            this.buttonClose.Click += new System.EventHandler(this.buttonClose_Click);
            // 
            // buttonApply
            // 
            this.buttonApply.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonApply.Location = new System.Drawing.Point(118, 116);
            this.buttonApply.Name = "buttonApply";
            this.buttonApply.Size = new System.Drawing.Size(90, 23);
            this.buttonApply.TabIndex = 9;
            this.buttonApply.Text = "Apply";
            this.buttonApply.UseVisualStyleBackColor = true;
            this.buttonApply.Click += new System.EventHandler(this.buttonApply_Click);
            // 
            // textboxSpeedDiff
            // 
            this.textboxSpeedDiff.Enabled = false;
            this.textboxSpeedDiff.Location = new System.Drawing.Point(108, 87);
            this.textboxSpeedDiff.Name = "textboxSpeedDiff";
            this.textboxSpeedDiff.Size = new System.Drawing.Size(100, 20);
            this.textboxSpeedDiff.TabIndex = 7;
            // 
            // textboxNewSpeed
            // 
            this.textboxNewSpeed.Location = new System.Drawing.Point(108, 61);
            this.textboxNewSpeed.Name = "textboxNewSpeed";
            this.textboxNewSpeed.Size = new System.Drawing.Size(100, 20);
            this.textboxNewSpeed.TabIndex = 5;
            // 
            // labelIdentification
            // 
            this.labelIdentification.AutoSize = true;
            this.labelIdentification.Location = new System.Drawing.Point(12, 14);
            this.labelIdentification.Name = "labelIdentification";
            this.labelIdentification.Size = new System.Drawing.Size(67, 13);
            this.labelIdentification.TabIndex = 0;
            this.labelIdentification.Text = "Identification";
            // 
            // labelCurrentSpeed
            // 
            this.labelCurrentSpeed.AutoSize = true;
            this.labelCurrentSpeed.Location = new System.Drawing.Point(12, 40);
            this.labelCurrentSpeed.Name = "labelCurrentSpeed";
            this.labelCurrentSpeed.Size = new System.Drawing.Size(75, 13);
            this.labelCurrentSpeed.TabIndex = 2;
            this.labelCurrentSpeed.Text = "Current Speed";
            // 
            // statusStrip
            // 
            this.statusStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripStatus});
            this.statusStrip.Location = new System.Drawing.Point(0, 142);
            this.statusStrip.Name = "statusStrip";
            this.statusStrip.Size = new System.Drawing.Size(220, 22);
            this.statusStrip.TabIndex = 10;
            this.statusStrip.Text = "OK";
            // 
            // toolStripStatus
            // 
            this.toolStripStatus.Name = "toolStripStatus";
            this.toolStripStatus.Size = new System.Drawing.Size(21, 17);
            this.toolStripStatus.Text = "OK";
            // 
            // ServiceDialog
            // 
            this.AcceptButton = this.buttonClose;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.buttonClose;
            this.ClientSize = new System.Drawing.Size(220, 164);
            this.Controls.Add(this.statusStrip);
            this.Controls.Add(this.textboxNewSpeed);
            this.Controls.Add(this.textboxSpeedDiff);
            this.Controls.Add(this.buttonApply);
            this.Controls.Add(this.buttonClose);
            this.Controls.Add(this.labelCurrentSpeed);
            this.Controls.Add(this.labelSpeed);
            this.Controls.Add(this.textBoxCurrentSpeed);
            this.Controls.Add(this.labelSpeedDiff);
            this.Controls.Add(this.labelIdentification);
            this.Controls.Add(this.labelNewSpeed);
            this.Controls.Add(this.textBoxIdentification);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "ServiceDialog";
            this.Text = "Calculate Speed";
            this.statusStrip.ResumeLayout(false);
            this.statusStrip.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox textBoxIdentification;
        private System.Windows.Forms.Label labelNewSpeed;
        private System.Windows.Forms.Label labelSpeedDiff;
        private System.Windows.Forms.TextBox textBoxCurrentSpeed;
        private System.Windows.Forms.Label labelSpeed;
        private System.Windows.Forms.Button buttonClose;
        private System.Windows.Forms.Button buttonApply;
        private System.Windows.Forms.Label labelIdentification;
        private System.Windows.Forms.Label labelCurrentSpeed;
        private System.Windows.Forms.TextBox textboxSpeedDiff;
        private System.Windows.Forms.TextBox textboxNewSpeed;
        private System.Windows.Forms.StatusStrip statusStrip;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatus;
    }
}
