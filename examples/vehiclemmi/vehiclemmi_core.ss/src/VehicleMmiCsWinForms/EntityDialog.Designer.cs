namespace VehicleMmiCsWinForms
{
    partial class EntityDialog
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(EntityDialog));
            this.labelIdentification = new System.Windows.Forms.Label();
            this.textBoxIdentification = new System.Windows.Forms.TextBox();
            this.comboBoxCategory = new System.Windows.Forms.ComboBox();
            //StartRemoveInExercise
            this.labelSpeed = new System.Windows.Forms.Label();
            //StopRemoveInExercise
            this.labelCategory = new System.Windows.Forms.Label();
            //StartRemoveInExercise
            this.textBoxSpeed = new System.Windows.Forms.TextBox();
            //StopRemoveInExercise
            this.buttonApply = new System.Windows.Forms.Button();
            this.buttonCancel = new System.Windows.Forms.Button();
            this.buttonOk = new System.Windows.Forms.Button();
            this.statusStrip = new System.Windows.Forms.StatusStrip();
            this.toolStripStatus = new System.Windows.Forms.ToolStripStatusLabel();
            //StartRemoveInExercise
            this.labelPosLong = new System.Windows.Forms.Label();
            this.labelPosLat = new System.Windows.Forms.Label();
            this.textBoxPosLong = new System.Windows.Forms.TextBox();
            this.textBoxPosLat = new System.Windows.Forms.TextBox();
            //StopRemoveInExercise
            this.statusStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // labelIdentification
            // 
            this.labelIdentification.AutoSize = true;
            this.labelIdentification.Location = new System.Drawing.Point(13, 20);
            this.labelIdentification.Name = "labelIdentification";
            this.labelIdentification.Size = new System.Drawing.Size(67, 13);
            this.labelIdentification.TabIndex = 0;
            this.labelIdentification.Text = "Identification";
            // 
            // textBoxIdentification
            // 
            this.textBoxIdentification.Location = new System.Drawing.Point(88, 13);
            this.textBoxIdentification.Name = "textBoxIdentification";
            this.textBoxIdentification.Size = new System.Drawing.Size(121, 20);
            this.textBoxIdentification.TabIndex = 1;
            // 
            // comboBoxCategory
            // 
            this.comboBoxCategory.FormattingEnabled = true;
            this.comboBoxCategory.Location = new System.Drawing.Point(88, 39);
            this.comboBoxCategory.Name = "comboBoxCategory";
            this.comboBoxCategory.Size = new System.Drawing.Size(121, 21);
            this.comboBoxCategory.TabIndex = 3;
            //StartRemoveInExercise
            // 
            // labelSpeed
            //
            this.labelSpeed.AutoSize = true;
            this.labelSpeed.Location = new System.Drawing.Point(12, 73);
            this.labelSpeed.Name = "labelSpeed";
            this.labelSpeed.Size = new System.Drawing.Size(38, 13);
            this.labelSpeed.TabIndex = 4;
            this.labelSpeed.Text = "Speed";
            //StopRemoveInExercise
            // 
            // labelCategory
            // 
            this.labelCategory.AutoSize = true;
            this.labelCategory.Location = new System.Drawing.Point(12, 46);
            this.labelCategory.Name = "labelCategory";
            this.labelCategory.Size = new System.Drawing.Size(49, 13);
            this.labelCategory.TabIndex = 2;
            this.labelCategory.Text = "Category";
            //StartRemoveInExercise
            // 
            // textBoxSpeed
            //
            this.textBoxSpeed.Location = new System.Drawing.Point(88, 66);
            this.textBoxSpeed.Name = "textBoxSpeed";
            this.textBoxSpeed.Size = new System.Drawing.Size(121, 20);
            this.textBoxSpeed.TabIndex = 5;
            //StopRemoveInExercise
            // 
            // buttonApply
            // 
            this.buttonApply.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonApply.Location = new System.Drawing.Point(195, 178);
            this.buttonApply.Name = "buttonApply";
            this.buttonApply.Size = new System.Drawing.Size(75, 23);
            this.buttonApply.TabIndex = 12;
            this.buttonApply.Text = "Apply";
            this.buttonApply.UseVisualStyleBackColor = true;
            this.buttonApply.Click += new System.EventHandler(this.buttonApply_Click);
            // 
            // buttonCancel
            // 
            this.buttonCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.buttonCancel.Location = new System.Drawing.Point(114, 178);
            this.buttonCancel.Name = "buttonCancel";
            this.buttonCancel.Size = new System.Drawing.Size(75, 23);
            this.buttonCancel.TabIndex = 11;
            this.buttonCancel.Text = "Cancel";
            this.buttonCancel.UseVisualStyleBackColor = true;
            this.buttonCancel.Click += new System.EventHandler(this.buttonCancel_Click);
            // 
            // buttonOk
            // 
            this.buttonOk.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonOk.Location = new System.Drawing.Point(33, 178);
            this.buttonOk.Name = "buttonOk";
            this.buttonOk.Size = new System.Drawing.Size(75, 23);
            this.buttonOk.TabIndex = 10;
            this.buttonOk.Text = "OK";
            this.buttonOk.UseVisualStyleBackColor = true;
            this.buttonOk.Click += new System.EventHandler(this.buttonOk_Click);
            // 
            // statusStrip
            // 
            this.statusStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripStatus});
            this.statusStrip.Location = new System.Drawing.Point(0, 206);
            this.statusStrip.Name = "statusStrip";
            this.statusStrip.Size = new System.Drawing.Size(274, 22);
            this.statusStrip.TabIndex = 13;
            this.statusStrip.Text = "OK";
            // 
            // toolStripStatus
            // 
            this.toolStripStatus.Name = "toolStripStatus";
            this.toolStripStatus.Size = new System.Drawing.Size(21, 17);
            this.toolStripStatus.Text = "OK";
            //StartRemoveInExercise
            // 
            // labelPosLong
            // 
            this.labelPosLong.AutoSize = true;
            this.labelPosLong.Location = new System.Drawing.Point(12, 124);
            this.labelPosLong.Name = "labelPosLong";
            this.labelPosLong.Size = new System.Drawing.Size(73, 13);
            this.labelPosLong.TabIndex = 8;
            this.labelPosLong.Text = "Position (long)";
            // 
            // labelPosLat
            // 
            this.labelPosLat.AutoSize = true;
            this.labelPosLat.Location = new System.Drawing.Point(12, 99);
            this.labelPosLat.Name = "labelPosLat";
            this.labelPosLat.Size = new System.Drawing.Size(64, 13);
            this.labelPosLat.TabIndex = 6;
            this.labelPosLat.Text = "Position (lat)";
            // 
            // textBoxPosLong
            // 
            this.textBoxPosLong.Location = new System.Drawing.Point(89, 118);
            this.textBoxPosLong.Name = "textBoxPosLong";
            this.textBoxPosLong.Size = new System.Drawing.Size(121, 20);
            this.textBoxPosLong.TabIndex = 9;
            // 
            // textBoxPosLat
            // 
            this.textBoxPosLat.Location = new System.Drawing.Point(89, 92);
            this.textBoxPosLat.Name = "textBoxPosLat";
            this.textBoxPosLat.Size = new System.Drawing.Size(121, 20);
            this.textBoxPosLat.TabIndex = 7;
            //StopRemoveInExercise
            // 
            // EntityDialog
            // 
            this.AcceptButton = this.buttonOk;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.buttonCancel;
            this.ClientSize = new System.Drawing.Size(274, 228);
            //StartRemoveInExercise
            this.Controls.Add(this.labelPosLong);
            this.Controls.Add(this.labelPosLat);
            this.Controls.Add(this.textBoxPosLong);
            this.Controls.Add(this.textBoxPosLat);
            //StopRemoveInExercise
            this.Controls.Add(this.statusStrip);
            this.Controls.Add(this.buttonOk);
            this.Controls.Add(this.buttonCancel);
            this.Controls.Add(this.buttonApply);
            //StartRemoveInExercise
            this.Controls.Add(this.textBoxSpeed);
            //StopRemoveInExercise
            this.Controls.Add(this.labelCategory);
            //StartRemoveInExercise
            this.Controls.Add(this.labelSpeed);
            //StopRemoveInExercise
            this.Controls.Add(this.comboBoxCategory);
            this.Controls.Add(this.textBoxIdentification);
            this.Controls.Add(this.labelIdentification);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "EntityDialog";
            this.Text = "Update Vehicle";
            this.statusStrip.ResumeLayout(false);
            this.statusStrip.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label labelIdentification;
        private System.Windows.Forms.TextBox textBoxIdentification;
        private System.Windows.Forms.ComboBox comboBoxCategory;
        //StartRemoveInExercise
        private System.Windows.Forms.Label labelSpeed;
        //StopRemoveInExercise
        private System.Windows.Forms.Label labelCategory;
        //StartRemoveInExercise
        private System.Windows.Forms.TextBox textBoxSpeed;
        //StopRemoveInExercise
        private System.Windows.Forms.Button buttonApply;
        private System.Windows.Forms.Button buttonCancel;
        private System.Windows.Forms.Button buttonOk;
        private System.Windows.Forms.StatusStrip statusStrip;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatus;
        //StartRemoveInExercise
        private System.Windows.Forms.Label labelPosLong;
        private System.Windows.Forms.Label labelPosLat;
        private System.Windows.Forms.TextBox textBoxPosLong;
        private System.Windows.Forms.TextBox textBoxPosLat;
        //StopRemoveInExercise
    }
}
