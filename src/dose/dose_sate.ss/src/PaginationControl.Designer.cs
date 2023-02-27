namespace Sate
{
    partial class PaginationControl
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.label1 = new System.Windows.Forms.Label();
            this.currentPage = new System.Windows.Forms.NumericUpDown();
            this.totalNumberOfPagesLabel = new System.Windows.Forms.Label();
            this.divider = new System.Windows.Forms.Label();
            this.itemsPerPage = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.currentPage)).BeginInit();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 14);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(70, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Page number";
            // 
            // currentPage
            // 
            this.currentPage.Location = new System.Drawing.Point(80, 11);
            this.currentPage.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.currentPage.Name = "currentPage";
            this.currentPage.Size = new System.Drawing.Size(65, 20);
            this.currentPage.TabIndex = 5;
            this.currentPage.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.currentPage.ValueChanged += new System.EventHandler(this.currentPage_ValueChanged);
            // 
            // totalNumberOfPagesLabel
            // 
            this.totalNumberOfPagesLabel.AutoSize = true;
            this.totalNumberOfPagesLabel.Location = new System.Drawing.Point(150, 14);
            this.totalNumberOfPagesLabel.Name = "totalNumberOfPagesLabel";
            this.totalNumberOfPagesLabel.Size = new System.Drawing.Size(35, 13);
            this.totalNumberOfPagesLabel.TabIndex = 6;
            this.totalNumberOfPagesLabel.Text = "of 100";
            // 
            // divider
            // 
            this.divider.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.divider.Location = new System.Drawing.Point(-3, 0);
            this.divider.Name = "divider";
            this.divider.Size = new System.Drawing.Size(600, 2);
            this.divider.TabIndex = 7;
            // 
            // itemsPerPage
            // 
            this.itemsPerPage.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.itemsPerPage.FormattingEnabled = true;
            this.itemsPerPage.Location = new System.Drawing.Point(440, 11);
            this.itemsPerPage.Name = "itemsPerPage";
            this.itemsPerPage.Size = new System.Drawing.Size(70, 21);
            this.itemsPerPage.TabIndex = 8;
            this.itemsPerPage.SelectedIndexChanged += new System.EventHandler(this.itemsPerPage_SelectedIndexChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(360, 14);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(77, 13);
            this.label2.TabIndex = 9;
            this.label2.Text = "Items per page";
            // 
            // PaginationControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.label2);
            this.Controls.Add(this.itemsPerPage);
            this.Controls.Add(this.divider);
            this.Controls.Add(this.totalNumberOfPagesLabel);
            this.Controls.Add(this.currentPage);
            this.Controls.Add(this.label1);
            this.Name = "PaginationControl";
            this.Size = new System.Drawing.Size(600, 38);
            ((System.ComponentModel.ISupportInitialize)(this.currentPage)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.NumericUpDown currentPage;
        private System.Windows.Forms.Label totalNumberOfPagesLabel;
        private System.Windows.Forms.Label divider;
        private System.Windows.Forms.ComboBox itemsPerPage;
        private System.Windows.Forms.Label label2;
    }
}
