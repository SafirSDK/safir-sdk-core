namespace Sate
{
    partial class FilterControl
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
            this.filterText = new System.Windows.Forms.TextBox();
            this.clearButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // filterText
            // 
            this.filterText.Dock = System.Windows.Forms.DockStyle.Fill;
            this.filterText.Location = new System.Drawing.Point(5, 0);
            this.filterText.Name = "filterText";
            this.filterText.Size = new System.Drawing.Size(345, 20);
            this.filterText.TabIndex = 0;
            // 
            // clearButton
            // 
            this.clearButton.Dock = System.Windows.Forms.DockStyle.Right;
            this.clearButton.ForeColor = System.Drawing.SystemColors.WindowText;
            this.clearButton.Location = new System.Drawing.Point(305, 0);
            this.clearButton.Name = "clearButton";
            this.clearButton.Size = new System.Drawing.Size(45, 21);
            this.clearButton.TabIndex = 1;
            this.clearButton.Text = "Clear";
            this.clearButton.UseVisualStyleBackColor = true;
            this.clearButton.Click += new System.EventHandler(this.clearButton_Click);
            // 
            // FilterControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.clearButton);
            this.Controls.Add(this.filterText);
            this.Margin = new System.Windows.Forms.Padding(3, 3, 3, 15);
            this.Name = "FilterControl";
            this.Padding = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.Size = new System.Drawing.Size(355, 21);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox filterText;
        private System.Windows.Forms.Button clearButton;
    }
}
