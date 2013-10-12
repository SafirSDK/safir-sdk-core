/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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

using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace Sate
{
    /// <summary>
    /// Summary description for SubscriptionResponses.
    /// </summary>
    public class InboxPanel : System.Windows.Forms.Panel
    {
        private System.Windows.Forms.ListView listView;
        private System.Windows.Forms.ColumnHeader subColumnHeader;
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.ColumnHeader typeColumnHeader;
        private System.Windows.Forms.ColumnHeader iconColumnHeader;
        private System.Windows.Forms.ImageList smallImageList;

        private static InboxPanel instance = null;

        private InboxPanel()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            this.SuspendLayout();

            //TitleBar
            PanelLabelControl titleLabel = new PanelLabelControl("Inbox");
            titleLabel.CloseEvent += new PanelLabelControl.OnCloseEventHandler(titleLabel_CloseEvent);
            this.Controls.Add(titleLabel);

            listView.ContextMenu = new ContextMenu(new MenuItem[] { new MenuItem("Clear", new EventHandler(OnClearInbox_click)),
                                                                    new MenuItem("Serialize to xml", new EventHandler(OnToXml_click))});

            this.listView.DoubleClick+=new EventHandler(listView_DoubleClick);

            this.listView.Scrollable=true;

            this.ResumeLayout(false);
        }

        void OnClearInbox_click(object sender, EventArgs e)
        {
            listView.Items.Clear();
        }

        void OnToXml_click(object sender, EventArgs e)
        {
            try
            {
                ObjectInfo objInfo = (ObjectInfo)listView.SelectedItems[0].Tag;
                MainForm.Instance.AddTabPage(new XmlTabPage(objInfo));
            }
            catch { }
        }

        void titleLabel_CloseEvent(object sender, EventArgs e)
        {
            MainForm.Instance.ShowHideInbox(false);
        }

        public static InboxPanel Instance
        {
            get
            {
                if (instance==null)
                    instance=new InboxPanel();
                return instance;
            }
        }

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        protected override void Dispose( bool disposing )
        {
            if( disposing )
            {
                if(components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose( disposing );
        }

        #region Windows Form Designer generated code
        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(InboxPanel));
            this.listView = new System.Windows.Forms.ListView();
            this.iconColumnHeader = new System.Windows.Forms.ColumnHeader();
            this.subColumnHeader = new System.Windows.Forms.ColumnHeader();
            this.typeColumnHeader = new System.Windows.Forms.ColumnHeader();
            this.smallImageList = new System.Windows.Forms.ImageList(this.components);
            this.SuspendLayout();
            //
            // listView
            //
            this.listView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.iconColumnHeader,
            this.subColumnHeader,
            this.typeColumnHeader});
            this.listView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.listView.FullRowSelect = true;
            this.listView.GridLines = true;
            this.listView.Location = new System.Drawing.Point(0, 0);
            this.listView.Name = "listView";
            this.listView.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.listView.Size = new System.Drawing.Size(10, 10);
            this.listView.SmallImageList = this.smallImageList;
            this.listView.TabIndex = 0;
            this.listView.UseCompatibleStateImageBehavior = false;
            this.listView.View = System.Windows.Forms.View.Details;
            //
            // iconColumnHeader
            //
            this.iconColumnHeader.Text = "";
            this.iconColumnHeader.Width = 22;
            //
            // subColumnHeader
            //
            this.subColumnHeader.Text = "Description";
            this.subColumnHeader.Width = 195;
            //
            // typeColumnHeader
            //
            this.typeColumnHeader.Text = "Data type";
            this.typeColumnHeader.Width = 156;
            //
            // smallImageList
            //
            this.smallImageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("smallImageList.ImageStream")));
            this.smallImageList.TransparentColor = System.Drawing.Color.Transparent;
            this.smallImageList.Images.SetKeyName(0, "");
            this.smallImageList.Images.SetKeyName(1, "");
            //
            // InboxPanel
            //
            this.AutoScroll = true;
            this.AutoSize = true;
            this.Controls.Add(this.listView);
            this.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.Size = new System.Drawing.Size(10, 10);
            this.Text = "Inbox";
            this.ResumeLayout(false);

        }
        #endregion

        public void AddResponse(ObjectInfo objInfo, string description)
        {
            string type;

            type = Safir.Dob.Typesystem.Operations.GetName(objInfo.Obj.GetTypeId());
         
            ListViewItem item = new ListViewItem(new string[] { "", description, type }, 0);

            item.Tag = objInfo;
         
            if (listView.Items.Count >= Settings.Sate.InboxQueueuLength)
                this.listView.Items.RemoveAt(0);
            
            this.listView.Items.Add(item);
            this.Invalidate();
        }

        /* used for response that don't have an object to display */
        public void AddNonDisplayableResponse(Int64 typeId, string description)
        {
            string type;

            type = Safir.Dob.Typesystem.Operations.GetName(typeId);

            ListViewItem item = new ListViewItem(new string[] { "", description, type }, 0);

            item.Tag = null;

            if (listView.Items.Count >= Settings.Sate.InboxQueueuLength)
                this.listView.Items.RemoveAt(0);

            this.listView.Items.Add(item);
            this.Invalidate();
        }

        private void listView_DoubleClick(object sender, EventArgs e)
        {
            this.listView.SelectedItems[0].ImageIndex=1;
            
            if (listView.SelectedItems[0].Tag is MessageInfo)
            {
                MessageInfo msgInfo = (MessageInfo)listView.SelectedItems[0].Tag;
                MainForm.Instance.AddTabPage(new ObjectEditTabPage(msgInfo));
            }
            else if (listView.SelectedItems[0].Tag is ServiceHandlerInfo)
            {
                ServiceHandlerInfo srvInfo = (ServiceHandlerInfo)listView.SelectedItems[0].Tag;
                MainForm.Instance.AddTabPage(new ObjectEditTabPage(srvInfo));
            }
            else if (listView.SelectedItems[0].Tag is ResponseInfo)
            {
                ResponseInfo replyInfo = (ResponseInfo)listView.SelectedItems[0].Tag;
                MainForm.Instance.AddTabPage(new ObjectEditTabPage(replyInfo));
            }
            else if (listView.SelectedItems[0].Tag is EntityInfo)
            {
                EntityInfo entityInfo = (EntityInfo)listView.SelectedItems[0].Tag;
                MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
            }
            else if (listView.SelectedItems[0].Tag is Safir.Dob.Typesystem.Object)
            {
                ObjectInfo objInfo = (ObjectInfo)listView.SelectedItems[0].Tag;
                MainForm.Instance.AddTabPage(new ObjectEditTabPage(objInfo));
            }
#if STSYLI
            else if (listView.SelectedItems[0].Tag is Safir.Dob.Typesystem.ObjectId)
            {
                try
                {
                    MainForm.Instance.Dose.Read((Safir.Dob.Typesystem.ObjectId)listView.SelectedItems[0].Tag);
                }
                catch { }
            }
#endif
            // also invalidate children
            Invalidate(true);
        }
    }

}
