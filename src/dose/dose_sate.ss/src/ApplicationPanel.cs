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

using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Sate
{
    /// <summary>
    ///     Summary description for ApplicationPanel.
    /// </summary>
    public class ApplicationPanel : Panel
    {
        private static ApplicationPanel instance;

        /// <summary>
        ///     Required designer variable.
        /// </summary>
        private readonly Container components = null;

        private ColumnHeader appColHeader;
        private ColumnHeader columnHeader1;

        private ListView listView;

        private ApplicationPanel()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            //TitleBar
            var titleLabel = new Label();
            titleLabel.Text = "Applications";
            titleLabel.BackColor = SystemColors.ActiveCaption;
            titleLabel.ForeColor = SystemColors.ActiveCaptionText;
            titleLabel.Height = 15;
            titleLabel.Dock = DockStyle.Top;
            Controls.Add(titleLabel);
        }

        public static ApplicationPanel Instance
        {
            get
            {
                if (instance == null)
                    instance = new ApplicationPanel();
                return instance;
            }
        }

        /// <summary>
        ///     Clean up any resources being used.
        /// </summary>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        ///     Required method for Designer support - do not modify
        ///     the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.listView = new System.Windows.Forms.ListView();
            this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
            this.appColHeader = new System.Windows.Forms.ColumnHeader();
            this.SuspendLayout();
            //
            // listView
            //
            this.listView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[]
            {
                this.columnHeader1,
                this.appColHeader
            });
            this.listView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.listView.GridLines = true;
            this.listView.Location = new System.Drawing.Point(0, 0);
            this.listView.Name = "listView";
            this.listView.Size = new System.Drawing.Size(248, 266);
            this.listView.TabIndex = 0;
            this.listView.View = System.Windows.Forms.View.Details;
            //
            // columnHeader1
            //
            this.columnHeader1.Text = "";
            this.columnHeader1.Width = 10;
            //
            // appColHeader
            //
            this.appColHeader.Text = "Unique application name";
            this.appColHeader.Width = 209;
            //
            // ApplicationPanel
            //
            this.ClientSize = new System.Drawing.Size(248, 266);
            this.Controls.Add(this.listView);
            this.Name = "ApplicationPanel";
            this.Text = "Applications";
            this.ResumeLayout(false);
        }

        #endregion
    }
}