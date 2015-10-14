/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
    partial class ScenarioControl
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
            this.saveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.panel3 = new System.Windows.Forms.Panel();
            this.reclistBox = new System.Windows.Forms.ListBox();
            this.reclistcontextMenu = new System.Windows.Forms.ContextMenu();
            this.deletemenuItem = new System.Windows.Forms.MenuItem();
            this.delaymenuItem = new System.Windows.Forms.MenuItem();
            this.repeatmenuItem = new System.Windows.Forms.MenuItem();
            this.incrInstmenuItem = new System.Windows.Forms.MenuItem();
            this.recbutton = new System.Windows.Forms.Button();
            this.panel1 = new System.Windows.Forms.Panel();
            this.buttonpanel = new System.Windows.Forms.Panel();
            this.statuslabel = new System.Windows.Forms.Label();
            this.playbutton = new System.Windows.Forms.Button();
            this.stopbutton = new System.Windows.Forms.Button();
            this.delaypanel = new System.Windows.Forms.Panel();
            this.label3 = new System.Windows.Forms.Label();
            this.delaylinkLabel = new System.Windows.Forms.LinkLabel();
            this.panel3.SuspendLayout();
            this.panel1.SuspendLayout();
            this.buttonpanel.SuspendLayout();
            this.delaypanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(8, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(208, 23);
            this.label1.TabIndex = 0;
            this.label1.Text = "Default delay between recorded actions";
            // 
            // saveFileDialog
            // 
            this.saveFileDialog.DefaultExt = "scn";
            // 
            // openFileDialog
            // 
            this.openFileDialog.DefaultExt = "scn";
            // 
            // panel3
            // 
            this.panel3.Controls.Add(this.reclistBox);
            this.panel3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel3.Location = new System.Drawing.Point(0, 0);
            this.panel3.Name = "panel3";
            this.panel3.Padding = new System.Windows.Forms.Padding(10, 0, 10, 0);
            this.panel3.Size = new System.Drawing.Size(385, 382);
            this.panel3.TabIndex = 12;
            // 
            // reclistBox
            // 
            this.reclistBox.ContextMenu = this.reclistcontextMenu;
            this.reclistBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.reclistBox.Location = new System.Drawing.Point(10, 0);
            this.reclistBox.Name = "reclistBox";
            this.reclistBox.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
            this.reclistBox.Size = new System.Drawing.Size(365, 381);
            this.reclistBox.TabIndex = 6;
            // 
            // reclistcontextMenu
            // 
            this.reclistcontextMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.deletemenuItem,
            this.delaymenuItem,
            this.repeatmenuItem,
            this.incrInstmenuItem});
            // 
            // deletemenuItem
            // 
            this.deletemenuItem.Index = 0;
            this.deletemenuItem.Text = "Delete action";
            this.deletemenuItem.Click += new System.EventHandler(this.deletemenuItem_Click);
            // 
            // delaymenuItem
            // 
            this.delaymenuItem.Index = 1;
            this.delaymenuItem.Text = "Delay...";
            this.delaymenuItem.Click += new System.EventHandler(this.delaymenuItem_Click);
            // 
            // repeatmenuItem
            // 
            this.repeatmenuItem.Index = 2;
            this.repeatmenuItem.Text = "Repeat sequence";
            this.repeatmenuItem.Click += new System.EventHandler(this.repeatmenuItem_Click);
            // 
            // incrInstmenuItem
            // 
            this.incrInstmenuItem.Index = 3;
            this.incrInstmenuItem.Text = "InstanceNumber++";
            this.incrInstmenuItem.Click += new System.EventHandler(this.incrInstmenuItem_Click);
            // 
            // recbutton
            // 
            this.recbutton.Location = new System.Drawing.Point(72, 8);
            this.recbutton.Name = "recbutton";
            this.recbutton.Size = new System.Drawing.Size(64, 23);
            this.recbutton.TabIndex = 2;
            this.recbutton.Text = "Record";
            this.recbutton.Click += new System.EventHandler(this.recbutton_Click);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.panel3);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel1.Location = new System.Drawing.Point(0, 48);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(385, 382);
            this.panel1.TabIndex = 19;
            // 
            // buttonpanel
            // 
            this.buttonpanel.Controls.Add(this.statuslabel);
            this.buttonpanel.Controls.Add(this.playbutton);
            this.buttonpanel.Controls.Add(this.recbutton);
            this.buttonpanel.Controls.Add(this.stopbutton);
            this.buttonpanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.buttonpanel.Location = new System.Drawing.Point(0, 430);
            this.buttonpanel.Name = "buttonpanel";
            this.buttonpanel.Size = new System.Drawing.Size(385, 40);
            this.buttonpanel.TabIndex = 18;
            // 
            // statuslabel
            // 
            this.statuslabel.AutoSize = true;
            this.statuslabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.statuslabel.ForeColor = System.Drawing.SystemColors.ActiveCaption;
            this.statuslabel.Location = new System.Drawing.Point(215, 8);
            this.statuslabel.Name = "statuslabel";
            this.statuslabel.Size = new System.Drawing.Size(77, 20);
            this.statuslabel.TabIndex = 4;
            this.statuslabel.Text = "Stopped";
            // 
            // playbutton
            // 
            this.playbutton.Enabled = false;
            this.playbutton.Location = new System.Drawing.Point(8, 8);
            this.playbutton.Name = "playbutton";
            this.playbutton.Size = new System.Drawing.Size(64, 23);
            this.playbutton.TabIndex = 1;
            this.playbutton.Text = "Play";
            this.playbutton.Click += new System.EventHandler(this.playbutton_Click);
            // 
            // stopbutton
            // 
            this.stopbutton.Location = new System.Drawing.Point(136, 8);
            this.stopbutton.Name = "stopbutton";
            this.stopbutton.Size = new System.Drawing.Size(64, 23);
            this.stopbutton.TabIndex = 3;
            this.stopbutton.Text = "Stop";
            this.stopbutton.Click += new System.EventHandler(this.stopbutton_Click);
            // 
            // delaypanel
            // 
            this.delaypanel.Controls.Add(this.label3);
            this.delaypanel.Controls.Add(this.delaylinkLabel);
            this.delaypanel.Controls.Add(this.label1);
            this.delaypanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.delaypanel.Location = new System.Drawing.Point(0, 0);
            this.delaypanel.Name = "delaypanel";
            this.delaypanel.Size = new System.Drawing.Size(385, 48);
            this.delaypanel.TabIndex = 17;
            // 
            // label3
            // 
            this.label3.Location = new System.Drawing.Point(264, 16);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(100, 23);
            this.label3.TabIndex = 8;
            this.label3.Text = "milliseconds.";
            // 
            // delaylinkLabel
            // 
            this.delaylinkLabel.Location = new System.Drawing.Point(216, 16);
            this.delaylinkLabel.Name = "delaylinkLabel";
            this.delaylinkLabel.Size = new System.Drawing.Size(48, 23);
            this.delaylinkLabel.TabIndex = 7;
            this.delaylinkLabel.TabStop = true;
            this.delaylinkLabel.Text = "100";
            this.delaylinkLabel.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.delaylinkLabel_LinkClicked);
            // 
            // ScenarioControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.buttonpanel);
            this.Controls.Add(this.delaypanel);
            this.Name = "ScenarioControl";
            this.Size = new System.Drawing.Size(385, 470);
            this.panel3.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.buttonpanel.ResumeLayout(false);
            this.buttonpanel.PerformLayout();
            this.delaypanel.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.SaveFileDialog saveFileDialog;
        private System.Windows.Forms.OpenFileDialog openFileDialog;
        private System.Windows.Forms.Panel panel3;
        private System.Windows.Forms.ListBox reclistBox;
        private System.Windows.Forms.ContextMenu reclistcontextMenu;
        private System.Windows.Forms.MenuItem deletemenuItem;
        private System.Windows.Forms.MenuItem delaymenuItem;
        private System.Windows.Forms.MenuItem repeatmenuItem;
        private System.Windows.Forms.MenuItem incrInstmenuItem;
        private System.Windows.Forms.Button recbutton;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Panel buttonpanel;
        private System.Windows.Forms.Button playbutton;
        private System.Windows.Forms.Button stopbutton;
        private System.Windows.Forms.Panel delaypanel;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.LinkLabel delaylinkLabel;
        private System.Windows.Forms.Label statuslabel;
    }
}
