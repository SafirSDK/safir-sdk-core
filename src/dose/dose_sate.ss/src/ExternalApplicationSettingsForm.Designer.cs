namespace Sate
{
    partial class ExternalApplicationSettingsForm
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
            this.okButton = new System.Windows.Forms.Button();
            this.cancelButton = new System.Windows.Forms.Button();
            this.externalApplicationsGrid = new System.Windows.Forms.DataGridView();
            this.description = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.command = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.arguments = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
            ((System.ComponentModel.ISupportInitialize)(this.externalApplicationsGrid)).BeginInit();
            this.tableLayoutPanel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // okButton
            // 
            this.okButton.Location = new System.Drawing.Point(341, 453);
            this.okButton.Name = "okButton";
            this.okButton.Size = new System.Drawing.Size(75, 23);
            this.okButton.TabIndex = 1;
            this.okButton.Text = "OK";
            this.okButton.UseVisualStyleBackColor = true;
            this.okButton.Click += new System.EventHandler(this.okButton_Click);
            // 
            // cancelButton
            // 
            this.cancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cancelButton.Location = new System.Drawing.Point(422, 453);
            this.cancelButton.Name = "cancelButton";
            this.cancelButton.Size = new System.Drawing.Size(75, 23);
            this.cancelButton.TabIndex = 2;
            this.cancelButton.Text = "Cancel";
            this.cancelButton.UseVisualStyleBackColor = true;
            // 
            // externalApplicationsGrid
            // 
            this.externalApplicationsGrid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
            this.externalApplicationsGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.externalApplicationsGrid.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.description,
            this.command,
            this.arguments});
            this.tableLayoutPanel1.SetColumnSpan(this.externalApplicationsGrid, 3);
            this.externalApplicationsGrid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.externalApplicationsGrid.Location = new System.Drawing.Point(3, 3);
            this.externalApplicationsGrid.MultiSelect = false;
            this.externalApplicationsGrid.Name = "externalApplicationsGrid";
            this.externalApplicationsGrid.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect;
            this.externalApplicationsGrid.Size = new System.Drawing.Size(494, 444);
            this.externalApplicationsGrid.TabIndex = 5;
            // 
            // description
            // 
            this.description.FillWeight = 30F;
            this.description.HeaderText = "Description";
            this.description.Name = "description";
            this.description.ToolTipText = "This text will appear in the submenu, with ... appended to it.";
            // 
            // command
            // 
            this.command.FillWeight = 40F;
            this.command.HeaderText = "Command";
            this.command.Name = "command";
            this.command.ToolTipText = "The executable to run. Can be a full path to an executable, or just the name of t" +
    "he executable if it is in your PATH.";
            // 
            // arguments
            // 
            this.arguments.FillWeight = 30F;
            this.arguments.HeaderText = "Arguments";
            this.arguments.Name = "arguments";
            this.arguments.ToolTipText = "Arguments to pass to the executable. Special symbols will be replaced with info f" +
    "rom the selected type/instance: %t = TypeId, %n = TypeName, %d = dou-file path, " +
    "%i = InstanceId.";
            // 
            // tableLayoutPanel1
            // 
            this.tableLayoutPanel1.ColumnCount = 3;
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.tableLayoutPanel1.Controls.Add(this.okButton, 1, 1);
            this.tableLayoutPanel1.Controls.Add(this.externalApplicationsGrid, 0, 0);
            this.tableLayoutPanel1.Controls.Add(this.cancelButton, 2, 1);
            this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.tableLayoutPanel1.Name = "tableLayoutPanel1";
            this.tableLayoutPanel1.RowCount = 2;
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel1.Size = new System.Drawing.Size(500, 479);
            this.tableLayoutPanel1.TabIndex = 8;
            // 
            // ExternalApplicationSettingsForm
            // 
            this.AcceptButton = this.okButton;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.CancelButton = this.cancelButton;
            this.ClientSize = new System.Drawing.Size(500, 479);
            this.Controls.Add(this.tableLayoutPanel1);
            this.Name = "ExternalApplicationSettingsForm";
            this.Text = "External Applications";
            ((System.ComponentModel.ISupportInitialize)(this.externalApplicationsGrid)).EndInit();
            this.tableLayoutPanel1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.Button okButton;
        private System.Windows.Forms.Button cancelButton;
        private System.Windows.Forms.DataGridView externalApplicationsGrid;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
        private System.Windows.Forms.DataGridViewTextBoxColumn description;
        private System.Windows.Forms.DataGridViewTextBoxColumn command;
        private System.Windows.Forms.DataGridViewTextBoxColumn arguments;
    }
}