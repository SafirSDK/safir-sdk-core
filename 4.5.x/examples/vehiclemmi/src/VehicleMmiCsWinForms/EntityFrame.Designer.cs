namespace VehicleMmiCsWinForms
{
    partial class EntityFrame
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(EntityFrame));
            this.listViewVehicles = new System.Windows.Forms.ListView();
            this.columnIdentification = new System.Windows.Forms.ColumnHeader();
            this.columnCategory = new System.Windows.Forms.ColumnHeader();
            this.columnSpeed = new System.Windows.Forms.ColumnHeader();
            this.columnPositionLat = new System.Windows.Forms.ColumnHeader();
            this.columnPositionLong = new System.Windows.Forms.ColumnHeader();
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.toolStripButtonCreate = new System.Windows.Forms.ToolStripButton();
            this.toolStripButtonUpdate = new System.Windows.Forms.ToolStripButton();
            this.toolStripButtonDelete = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.toolStripButtonSpeed = new System.Windows.Forms.ToolStripButton();
            this.toolStripButton1 = new System.Windows.Forms.ToolStripSeparator();
            this.toolStripButtonCategory = new System.Windows.Forms.ToolStripButton();
            this.toolStripButtonDeleteCategoryInfo = new System.Windows.Forms.ToolStripButton();
            this.statusStrip = new System.Windows.Forms.StatusStrip();
            this.toolStripStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.toolStrip1.SuspendLayout();
            this.statusStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // listViewVehicles
            // 
            this.listViewVehicles.AllowColumnReorder = true;
            this.listViewVehicles.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.listViewVehicles.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnIdentification,
            this.columnCategory,
            this.columnSpeed,
            this.columnPositionLat,
            this.columnPositionLong});
            this.listViewVehicles.FullRowSelect = true;
            this.listViewVehicles.Location = new System.Drawing.Point(0, 39);
            this.listViewVehicles.MultiSelect = false;
            this.listViewVehicles.Name = "listViewVehicles";
            this.listViewVehicles.Scrollable = false;
            this.listViewVehicles.Size = new System.Drawing.Size(522, 165);
            this.listViewVehicles.TabIndex = 0;
            this.listViewVehicles.UseCompatibleStateImageBehavior = false;
            this.listViewVehicles.View = System.Windows.Forms.View.Details;
            // 
            // columnIdentification
            // 
            this.columnIdentification.Text = "Identification";
            this.columnIdentification.Width = 110;
            // 
            // columnCategory
            // 
            this.columnCategory.Text = "Category";
            this.columnCategory.Width = 110;
            // 
            // columnSpeed
            // 
            this.columnSpeed.Text = "Speed";
            this.columnSpeed.Width = 90;
            // 
            // columnPositionLat
            // 
            this.columnPositionLat.Text = "Position (lat)";
            this.columnPositionLat.Width = 100;
            // 
            // columnPositionLong
            // 
            this.columnPositionLong.Text = "Position (long)";
            this.columnPositionLong.Width = 100;
            // 
            // toolStrip1
            // 
            this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripButtonCreate,
            this.toolStripButtonUpdate,
            this.toolStripButtonDelete,
            this.toolStripSeparator1,
            this.toolStripButtonSpeed,
            this.toolStripButton1,
            this.toolStripButtonCategory,
            this.toolStripButtonDeleteCategoryInfo});
            this.toolStrip1.Location = new System.Drawing.Point(0, 0);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.Size = new System.Drawing.Size(522, 37);
            this.toolStrip1.TabIndex = 2;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // toolStripButtonCreate
            // 
            this.toolStripButtonCreate.Image = global::VehicleMmiCsWinForms.Properties.Resources.create;
            this.toolStripButtonCreate.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolStripButtonCreate.Name = "toolStripButtonCreate";
            this.toolStripButtonCreate.Size = new System.Drawing.Size(44, 34);
            this.toolStripButtonCreate.Text = "Create";
            this.toolStripButtonCreate.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.toolStripButtonCreate.Click += new System.EventHandler(this.toolStripButtonCreate_Click);
            // 
            // toolStripButtonUpdate
            // 
            this.toolStripButtonUpdate.Image = global::VehicleMmiCsWinForms.Properties.Resources.update;
            this.toolStripButtonUpdate.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolStripButtonUpdate.Name = "toolStripButtonUpdate";
            this.toolStripButtonUpdate.Size = new System.Drawing.Size(46, 34);
            this.toolStripButtonUpdate.Text = "Update";
            this.toolStripButtonUpdate.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.toolStripButtonUpdate.Click += new System.EventHandler(this.toolStripButtonUpdate_Click);
            // 
            // toolStripButtonDelete
            // 
            this.toolStripButtonDelete.Image = global::VehicleMmiCsWinForms.Properties.Resources.delete;
            this.toolStripButtonDelete.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolStripButtonDelete.Name = "toolStripButtonDelete";
            this.toolStripButtonDelete.Size = new System.Drawing.Size(42, 34);
            this.toolStripButtonDelete.Text = "Delete";
            this.toolStripButtonDelete.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.toolStripButtonDelete.ToolTipText = "Delete";
            this.toolStripButtonDelete.Click += new System.EventHandler(this.toolStripButtonDelete_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 37);
            // 
            // toolStripButtonSpeed
            // 
            this.toolStripButtonSpeed.Image = global::VehicleMmiCsWinForms.Properties.Resources.calculator;
            this.toolStripButtonSpeed.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.toolStripButtonSpeed.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolStripButtonSpeed.Name = "toolStripButtonSpeed";
            this.toolStripButtonSpeed.Size = new System.Drawing.Size(88, 34);
            this.toolStripButtonSpeed.Text = "Calculate Speed";
            this.toolStripButtonSpeed.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.toolStripButtonSpeed.ToolTipText = "Calculate Speed";
            this.toolStripButtonSpeed.Click += new System.EventHandler(this.toolStripButtonSpeed_Click);
            // 
            // toolStripButton1
            // 
            this.toolStripButton1.Name = "toolStripButton1";
            this.toolStripButton1.Size = new System.Drawing.Size(6, 37);
            // 
            // toolStripButtonCategory
            // 
            this.toolStripButtonCategory.Image = global::VehicleMmiCsWinForms.Properties.Resources.properties;
            this.toolStripButtonCategory.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.toolStripButtonCategory.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolStripButtonCategory.Name = "toolStripButtonCategory";
            this.toolStripButtonCategory.Size = new System.Drawing.Size(115, 34);
            this.toolStripButtonCategory.Text = "Category Information";
            this.toolStripButtonCategory.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.toolStripButtonCategory.Click += new System.EventHandler(this.toolStripButtonCategory_Click);
            // 
            // toolStripButtonDeleteCategoryInfo
            // 
            this.toolStripButtonDeleteCategoryInfo.Image = ((System.Drawing.Image)(resources.GetObject("toolStripButtonDeleteCategoryInfo.Image")));
            this.toolStripButtonDeleteCategoryInfo.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolStripButtonDeleteCategoryInfo.Name = "toolStripButtonDeleteCategoryInfo";
            this.toolStripButtonDeleteCategoryInfo.Size = new System.Drawing.Size(113, 34);
            this.toolStripButtonDeleteCategoryInfo.Text = "Delete Category Info";
            this.toolStripButtonDeleteCategoryInfo.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.toolStripButtonDeleteCategoryInfo.Click += new System.EventHandler(this.toolStripButtonDeleteCategoryInfo_Click);
            // 
            // statusStrip
            // 
            this.statusStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripStatus});
            this.statusStrip.Location = new System.Drawing.Point(0, 207);
            this.statusStrip.Name = "statusStrip";
            this.statusStrip.Size = new System.Drawing.Size(522, 22);
            this.statusStrip.TabIndex = 3;
            this.statusStrip.Text = "statusStrip";
            // 
            // toolStripStatus
            // 
            this.toolStripStatus.Name = "toolStripStatus";
            this.toolStripStatus.Size = new System.Drawing.Size(21, 17);
            this.toolStripStatus.Text = "OK";
            // 
            // EntityFrame
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(522, 229);
            this.Controls.Add(this.listViewVehicles);
            this.Controls.Add(this.statusStrip);
            this.Controls.Add(this.toolStrip1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "EntityFrame";
            this.Text = "Vehicles";
            this.Load += new System.EventHandler(this.EntityFrame_Load);
            this.toolStrip1.ResumeLayout(false);
            this.toolStrip1.PerformLayout();
            this.statusStrip.ResumeLayout(false);
            this.statusStrip.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ListView listViewVehicles;
        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.ToolStripButton toolStripButtonCreate;
        private System.Windows.Forms.ToolStripButton toolStripButtonUpdate;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripButton toolStripButtonSpeed;
        private System.Windows.Forms.ToolStripButton toolStripButtonDelete;
        private System.Windows.Forms.ColumnHeader columnSpeed;
        private System.Windows.Forms.ColumnHeader columnCategory;
        private System.Windows.Forms.ColumnHeader columnIdentification;
        private System.Windows.Forms.StatusStrip statusStrip;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatus;
        private System.Windows.Forms.ColumnHeader columnPositionLat;
        private System.Windows.Forms.ColumnHeader columnPositionLong;
        private System.Windows.Forms.ToolStripSeparator toolStripButton1;
        private System.Windows.Forms.ToolStripButton toolStripButtonCategory;
        private System.Windows.Forms.ToolStripButton toolStripButtonDeleteCategoryInfo;
    }
}

