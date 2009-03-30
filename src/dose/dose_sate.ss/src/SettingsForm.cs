/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
    /// Summary description for SettingsForm.
    /// </summary>
    public class SettingsForm : System.Windows.Forms.Form
    {
        private class MyListViewItem : ListViewItem
        {
            public Int64 typeId;
            public Safir.Dob.Typesystem.HandlerId handlerId;
            public Safir.Dob.Typesystem.EntityId entityId;
            public Safir.Dob.Typesystem.ChannelId channelId;
            public bool pending, upd, injection, requestorDecides;
            public bool includeSubClasses, restartSubscription;

            //For register
            public MyListViewItem(Int64 typeId, Safir.Dob.Typesystem.HandlerId handlerId, bool pending, bool injection, bool requestorDecides)
            {
                this.Text = Safir.Dob.Typesystem.Operations.GetName(typeId);
                this.typeId = typeId;
                this.handlerId = handlerId;
                this.pending = pending;
                this.injection = injection;
                this.requestorDecides = requestorDecides;

                this.SubItems.Add(handlerId.ToString());
                if (pending)
                    this.SubItems.Add("yes");
                else
                    this.SubItems.Add("no");
                if (injection)
                    this.SubItems.Add("yes" );
                else
                    this.SubItems.Add("no");
                if (requestorDecides)
                    this.SubItems.Add("Requestor");
                else
                    this.SubItems.Add("Handler");
            }

            //For subscribe
            public MyListViewItem(Int64 typeId, Safir.Dob.Typesystem.EntityId entityId, Safir.Dob.Typesystem.ChannelId channelId, bool upd, bool restart)
            {
                this.Text = Safir.Dob.Typesystem.Operations.GetName(typeId);

                this.typeId = typeId;
                this.entityId = entityId;
                this.channelId = channelId;
                this.upd = upd;
                this.restartSubscription = restart;
                
                string infoStr;
                if (channelId == null)
                {
                    if (entityId == null)
                    {
                        infoStr = "";
                    }
                    else
                    {
                        infoStr = entityId.InstanceId.ToString();
                    }
                }
                else
                    infoStr = channelId.ToString();

                this.SubItems.Add(infoStr);

                string subscrStr = "";
                bool slash = false;
                if (upd)
                {
                    if (slash) subscrStr += " / ";
                    subscrStr += "update";
                    slash = true;
                }
                if (restartSubscription)
                {
                    if (slash) subscrStr += " / ";
                    subscrStr += "restart";
                    slash = true;
                }
                this.SubItems.Add(subscrStr);
            }

            //For subscribe registrations
            public MyListViewItem(Int64 typeId, Safir.Dob.Typesystem.HandlerId handlerId, bool includeSubClasses, bool restartSubscription)
            {
                this.Text = Safir.Dob.Typesystem.Operations.GetName(typeId);

                this.typeId = typeId;
                this.handlerId = handlerId;
                this.includeSubClasses = includeSubClasses;
                this.restartSubscription = restartSubscription;
                
                this.SubItems.Add(handlerId.ToString());
                
                string subscrStr = "";
                bool slash = false;
                if (includeSubClasses)
                {
                    if (slash) subscrStr += " / ";
                    subscrStr += "subclasses";
                    slash = true;
                }
                if (restartSubscription)
                {
                    if (slash) subscrStr += " / ";
                    subscrStr += "restart";
                    slash = true;
                }
                this.SubItems.Add(subscrStr);
            }

        }

        private System.Windows.Forms.TabControl tabControl;
        private System.Windows.Forms.TabPage generalTabPage;
        private System.Windows.Forms.TabPage handlersTabPage;
        private System.Windows.Forms.TabPage subscriptionTabPage;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox inboxQueueLengthtextBox;
        private System.Windows.Forms.CheckBox autoConnectCheckBox;
        private System.Windows.Forms.Panel bottompanel;
        private System.Windows.Forms.Button okbutton;
        private System.Windows.Forms.Button cancelbutton;
        private System.Windows.Forms.Button applybutton;

        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox connectionNameTextBox;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.RadioButton inheritanceradioButton;
        private System.Windows.Forms.RadioButton namespacesradioButton;
        private GroupBox groupBox2;
        private CheckBox deleteReqcheckBox;
        private CheckBox updateReqcheckBox;
        private CheckBox createReqcheckBox;
        private Button openReplybutton;
        private GroupBox groupBox3;
        private CheckBox noResponseCheckBox;
        private CheckBox noDispatchCheckBox;
        private ListView sublistView;
        private ColumnHeader classcolumnHeader;
        private ColumnHeader channelIdColumnHeader;
        private ColumnHeader subtypecolumnHeader;
        private ListView reglistView;
        private ColumnHeader columnHeader1;
        private ColumnHeader columnHeader2;
        private ColumnHeader columnHeader3;
        private ColumnHeader columnHeader4;
        private TabPage tabPage1;
        private ListView subRegListView;
        private ColumnHeader columnHeader5;
        private ColumnHeader columnHeader6;
        private Label label3;
        private ColumnHeader columnHeader7;
        private ColumnHeader columnHeader8;
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.Container components = null;

        public SettingsForm()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            //
            // TODO: Add any constructor code after InitializeComponent call
            //

            sublistView.ContextMenu = new ContextMenu(new MenuItem[] { new MenuItem("Remove", new EventHandler(OnRemoveSub_click)) });
            subRegListView.ContextMenu = new ContextMenu(new MenuItem[] { new MenuItem("Remove", new EventHandler(OnRemoveSubReg_click)) });
            reglistView.ContextMenu = new ContextMenu(new MenuItem[] { new MenuItem("Remove", new EventHandler(OnRemoveReg_click)) });

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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SettingsForm));
            this.tabControl = new System.Windows.Forms.TabControl();
            this.generalTabPage = new System.Windows.Forms.TabPage();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.autoConnectCheckBox = new System.Windows.Forms.CheckBox();
            this.label8 = new System.Windows.Forms.Label();
            this.connectionNameTextBox = new System.Windows.Forms.TextBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.noDispatchCheckBox = new System.Windows.Forms.CheckBox();
            this.noResponseCheckBox = new System.Windows.Forms.CheckBox();
            this.openReplybutton = new System.Windows.Forms.Button();
            this.deleteReqcheckBox = new System.Windows.Forms.CheckBox();
            this.updateReqcheckBox = new System.Windows.Forms.CheckBox();
            this.createReqcheckBox = new System.Windows.Forms.CheckBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.namespacesradioButton = new System.Windows.Forms.RadioButton();
            this.inheritanceradioButton = new System.Windows.Forms.RadioButton();
            this.inboxQueueLengthtextBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.handlersTabPage = new System.Windows.Forms.TabPage();
            this.reglistView = new System.Windows.Forms.ListView();
            this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader3 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader4 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader8 = new System.Windows.Forms.ColumnHeader();
            this.label2 = new System.Windows.Forms.Label();
            this.subscriptionTabPage = new System.Windows.Forms.TabPage();
            this.sublistView = new System.Windows.Forms.ListView();
            this.classcolumnHeader = new System.Windows.Forms.ColumnHeader();
            this.channelIdColumnHeader = new System.Windows.Forms.ColumnHeader();
            this.subtypecolumnHeader = new System.Windows.Forms.ColumnHeader();
            this.label7 = new System.Windows.Forms.Label();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.subRegListView = new System.Windows.Forms.ListView();
            this.columnHeader5 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader6 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader7 = new System.Windows.Forms.ColumnHeader();
            this.label3 = new System.Windows.Forms.Label();
            this.bottompanel = new System.Windows.Forms.Panel();
            this.applybutton = new System.Windows.Forms.Button();
            this.cancelbutton = new System.Windows.Forms.Button();
            this.okbutton = new System.Windows.Forms.Button();
            this.tabControl.SuspendLayout();
            this.generalTabPage.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.handlersTabPage.SuspendLayout();
            this.subscriptionTabPage.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.bottompanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl
            // 
            this.tabControl.Controls.Add(this.generalTabPage);
            this.tabControl.Controls.Add(this.handlersTabPage);
            this.tabControl.Controls.Add(this.subscriptionTabPage);
            this.tabControl.Controls.Add(this.tabPage1);
            this.tabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl.Location = new System.Drawing.Point(0, 0);
            this.tabControl.Margin = new System.Windows.Forms.Padding(5);
            this.tabControl.Name = "tabControl";
            this.tabControl.SelectedIndex = 0;
            this.tabControl.Size = new System.Drawing.Size(504, 482);
            this.tabControl.TabIndex = 0;
            // 
            // generalTabPage
            // 
            this.generalTabPage.Controls.Add(this.groupBox3);
            this.generalTabPage.Controls.Add(this.groupBox2);
            this.generalTabPage.Controls.Add(this.groupBox1);
            this.generalTabPage.Controls.Add(this.inboxQueueLengthtextBox);
            this.generalTabPage.Controls.Add(this.label1);
            this.generalTabPage.Location = new System.Drawing.Point(4, 22);
            this.generalTabPage.Name = "generalTabPage";
            this.generalTabPage.Size = new System.Drawing.Size(496, 456);
            this.generalTabPage.TabIndex = 0;
            this.generalTabPage.Text = "General";
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.autoConnectCheckBox);
            this.groupBox3.Controls.Add(this.label8);
            this.groupBox3.Controls.Add(this.connectionNameTextBox);
            this.groupBox3.Location = new System.Drawing.Point(11, 12);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(208, 100);
            this.groupBox3.TabIndex = 7;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Connection";
            // 
            // autoConnectCheckBox
            // 
            this.autoConnectCheckBox.Location = new System.Drawing.Point(6, 56);
            this.autoConnectCheckBox.Name = "autoConnectCheckBox";
            this.autoConnectCheckBox.Size = new System.Drawing.Size(200, 24);
            this.autoConnectCheckBox.TabIndex = 2;
            this.autoConnectCheckBox.Text = "Auto connect to DOSE at start-up";
            // 
            // label8
            // 
            this.label8.Location = new System.Drawing.Point(6, 21);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(100, 23);
            this.label8.TabIndex = 3;
            this.label8.Text = "Connection name";
            // 
            // connectionNameTextBox
            // 
            this.connectionNameTextBox.Location = new System.Drawing.Point(115, 19);
            this.connectionNameTextBox.MaxLength = 8;
            this.connectionNameTextBox.Name = "connectionNameTextBox";
            this.connectionNameTextBox.Size = new System.Drawing.Size(72, 20);
            this.connectionNameTextBox.TabIndex = 4;
            this.connectionNameTextBox.Text = "SATE";
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.noDispatchCheckBox);
            this.groupBox2.Controls.Add(this.noResponseCheckBox);
            this.groupBox2.Controls.Add(this.openReplybutton);
            this.groupBox2.Controls.Add(this.deleteReqcheckBox);
            this.groupBox2.Controls.Add(this.updateReqcheckBox);
            this.groupBox2.Controls.Add(this.createReqcheckBox);
            this.groupBox2.Location = new System.Drawing.Point(11, 203);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(291, 152);
            this.groupBox2.TabIndex = 6;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Auto Reply";
            // 
            // noDispatchCheckBox
            // 
            this.noDispatchCheckBox.AutoSize = true;
            this.noDispatchCheckBox.Location = new System.Drawing.Point(152, 19);
            this.noDispatchCheckBox.Name = "noDispatchCheckBox";
            this.noDispatchCheckBox.Size = new System.Drawing.Size(94, 17);
            this.noDispatchCheckBox.TabIndex = 10;
            this.noDispatchCheckBox.Text = "Don\'t dispatch";
            this.noDispatchCheckBox.UseVisualStyleBackColor = true;
            this.noDispatchCheckBox.CheckedChanged += new System.EventHandler(this.noDispatchCheckBox_CheckedChanged);
            // 
            // noResponseCheckBox
            // 
            this.noResponseCheckBox.AutoSize = true;
            this.noResponseCheckBox.Location = new System.Drawing.Point(6, 19);
            this.noResponseCheckBox.Name = "noResponseCheckBox";
            this.noResponseCheckBox.Size = new System.Drawing.Size(123, 17);
            this.noResponseCheckBox.TabIndex = 10;
            this.noResponseCheckBox.Text = "Don\'t send response";
            this.noResponseCheckBox.UseVisualStyleBackColor = true;
            this.noResponseCheckBox.CheckedChanged += new System.EventHandler(this.noResponseCheckBox_CheckedChanged);
            // 
            // openReplybutton
            // 
            this.openReplybutton.Location = new System.Drawing.Point(6, 123);
            this.openReplybutton.Name = "openReplybutton";
            this.openReplybutton.Size = new System.Drawing.Size(134, 23);
            this.openReplybutton.TabIndex = 3;
            this.openReplybutton.Text = "Open default response...";
            this.openReplybutton.UseVisualStyleBackColor = true;
            this.openReplybutton.Click += new System.EventHandler(this.openReplybutton_Click);
            // 
            // deleteReqcheckBox
            // 
            this.deleteReqcheckBox.AutoSize = true;
            this.deleteReqcheckBox.Checked = true;
            this.deleteReqcheckBox.CheckState = System.Windows.Forms.CheckState.Checked;
            this.deleteReqcheckBox.Location = new System.Drawing.Point(6, 95);
            this.deleteReqcheckBox.Name = "deleteReqcheckBox";
            this.deleteReqcheckBox.Size = new System.Drawing.Size(171, 17);
            this.deleteReqcheckBox.TabIndex = 2;
            this.deleteReqcheckBox.Text = "Delete Entity on delete request";
            this.deleteReqcheckBox.UseVisualStyleBackColor = true;
            this.deleteReqcheckBox.CheckedChanged += new System.EventHandler(this.deleteReqcheckBox_CheckedChanged);
            // 
            // updateReqcheckBox
            // 
            this.updateReqcheckBox.AutoSize = true;
            this.updateReqcheckBox.Checked = true;
            this.updateReqcheckBox.CheckState = System.Windows.Forms.CheckState.Checked;
            this.updateReqcheckBox.Location = new System.Drawing.Point(6, 72);
            this.updateReqcheckBox.Name = "updateReqcheckBox";
            this.updateReqcheckBox.Size = new System.Drawing.Size(179, 17);
            this.updateReqcheckBox.TabIndex = 1;
            this.updateReqcheckBox.Text = "Update Entity on update request";
            this.updateReqcheckBox.UseVisualStyleBackColor = true;
            this.updateReqcheckBox.CheckedChanged += new System.EventHandler(this.updateReqcheckBox_CheckedChanged);
            // 
            // createReqcheckBox
            // 
            this.createReqcheckBox.AutoSize = true;
            this.createReqcheckBox.Checked = true;
            this.createReqcheckBox.CheckState = System.Windows.Forms.CheckState.Checked;
            this.createReqcheckBox.Location = new System.Drawing.Point(6, 49);
            this.createReqcheckBox.Name = "createReqcheckBox";
            this.createReqcheckBox.Size = new System.Drawing.Size(172, 17);
            this.createReqcheckBox.TabIndex = 0;
            this.createReqcheckBox.Text = "Create Entity on create request";
            this.createReqcheckBox.UseVisualStyleBackColor = true;
            this.createReqcheckBox.CheckedChanged += new System.EventHandler(this.createReqcheckBox_CheckedChanged);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.namespacesradioButton);
            this.groupBox1.Controls.Add(this.inheritanceradioButton);
            this.groupBox1.Location = new System.Drawing.Point(11, 121);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(206, 72);
            this.groupBox1.TabIndex = 5;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Default Explorer View";
            // 
            // namespacesradioButton
            // 
            this.namespacesradioButton.Location = new System.Drawing.Point(8, 40);
            this.namespacesradioButton.Name = "namespacesradioButton";
            this.namespacesradioButton.Size = new System.Drawing.Size(104, 24);
            this.namespacesradioButton.TabIndex = 1;
            this.namespacesradioButton.Text = "Namespaces";
            // 
            // inheritanceradioButton
            // 
            this.inheritanceradioButton.Location = new System.Drawing.Point(8, 16);
            this.inheritanceradioButton.Name = "inheritanceradioButton";
            this.inheritanceradioButton.Size = new System.Drawing.Size(104, 24);
            this.inheritanceradioButton.TabIndex = 0;
            this.inheritanceradioButton.Text = "Inheritance";
            // 
            // inboxQueueLengthtextBox
            // 
            this.inboxQueueLengthtextBox.Location = new System.Drawing.Point(114, 370);
            this.inboxQueueLengthtextBox.Name = "inboxQueueLengthtextBox";
            this.inboxQueueLengthtextBox.Size = new System.Drawing.Size(72, 20);
            this.inboxQueueLengthtextBox.TabIndex = 1;
            this.inboxQueueLengthtextBox.Text = "10";
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(8, 370);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(100, 23);
            this.label1.TabIndex = 0;
            this.label1.Text = "Inbox queue length";
            // 
            // handlersTabPage
            // 
            this.handlersTabPage.Controls.Add(this.reglistView);
            this.handlersTabPage.Controls.Add(this.label2);
            this.handlersTabPage.Location = new System.Drawing.Point(4, 22);
            this.handlersTabPage.Margin = new System.Windows.Forms.Padding(5);
            this.handlersTabPage.Name = "handlersTabPage";
            this.handlersTabPage.Size = new System.Drawing.Size(496, 456);
            this.handlersTabPage.TabIndex = 1;
            this.handlersTabPage.Text = "Handlers";
            // 
            // reglistView
            // 
            this.reglistView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader1,
            this.columnHeader2,
            this.columnHeader3,
            this.columnHeader4,
            this.columnHeader8});
            this.reglistView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.reglistView.FullRowSelect = true;
            this.reglistView.Location = new System.Drawing.Point(0, 32);
            this.reglistView.Name = "reglistView";
            this.reglistView.Size = new System.Drawing.Size(496, 424);
            this.reglistView.TabIndex = 1;
            this.reglistView.UseCompatibleStateImageBehavior = false;
            this.reglistView.View = System.Windows.Forms.View.Details;
            // 
            // columnHeader1
            // 
            this.columnHeader1.Text = "Class";
            this.columnHeader1.Width = 162;
            // 
            // columnHeader2
            // 
            this.columnHeader2.Text = "HandlerId";
            this.columnHeader2.Width = 121;
            // 
            // columnHeader3
            // 
            this.columnHeader3.Text = "Pending";
            this.columnHeader3.Width = 54;
            // 
            // columnHeader4
            // 
            this.columnHeader4.Text = "Injection";
            this.columnHeader4.Width = 55;
            // 
            // columnHeader8
            // 
            this.columnHeader8.Text = "InstanceId policy";
            this.columnHeader8.Width = 99;
            // 
            // label2
            // 
            this.label2.Dock = System.Windows.Forms.DockStyle.Top;
            this.label2.Location = new System.Drawing.Point(0, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(496, 32);
            this.label2.TabIndex = 0;
            this.label2.Text = "Listed typeIds and services are handled by SATE when connection to DOSE is perfor" +
                "med.";
            // 
            // subscriptionTabPage
            // 
            this.subscriptionTabPage.Controls.Add(this.sublistView);
            this.subscriptionTabPage.Controls.Add(this.label7);
            this.subscriptionTabPage.Location = new System.Drawing.Point(4, 22);
            this.subscriptionTabPage.Name = "subscriptionTabPage";
            this.subscriptionTabPage.Padding = new System.Windows.Forms.Padding(5);
            this.subscriptionTabPage.Size = new System.Drawing.Size(496, 456);
            this.subscriptionTabPage.TabIndex = 2;
            this.subscriptionTabPage.Text = "Subscriptions";
            // 
            // sublistView
            // 
            this.sublistView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.classcolumnHeader,
            this.channelIdColumnHeader,
            this.subtypecolumnHeader});
            this.sublistView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.sublistView.FullRowSelect = true;
            this.sublistView.Location = new System.Drawing.Point(5, 37);
            this.sublistView.Name = "sublistView";
            this.sublistView.Size = new System.Drawing.Size(486, 414);
            this.sublistView.TabIndex = 16;
            this.sublistView.UseCompatibleStateImageBehavior = false;
            this.sublistView.View = System.Windows.Forms.View.Details;
            // 
            // classcolumnHeader
            // 
            this.classcolumnHeader.Text = "Class";
            this.classcolumnHeader.Width = 200;
            // 
            // channelIdColumnHeader
            // 
            this.channelIdColumnHeader.Text = "ChannelId/Instance";
            this.channelIdColumnHeader.Width = 120;
            // 
            // subtypecolumnHeader
            // 
            this.subtypecolumnHeader.Text = "Options";
            this.subtypecolumnHeader.Width = 120;
            // 
            // label7
            // 
            this.label7.Dock = System.Windows.Forms.DockStyle.Top;
            this.label7.Location = new System.Drawing.Point(5, 5);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(486, 32);
            this.label7.TabIndex = 8;
            this.label7.Text = "The objects will be subscribed for by SATE when connecting to DOSE.";
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.subRegListView);
            this.tabPage1.Controls.Add(this.label3);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(496, 456);
            this.tabPage1.TabIndex = 3;
            this.tabPage1.Text = "Subscriptions of registrations";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // subRegListView
            // 
            this.subRegListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader5,
            this.columnHeader6,
            this.columnHeader7});
            this.subRegListView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.subRegListView.FullRowSelect = true;
            this.subRegListView.Location = new System.Drawing.Point(3, 35);
            this.subRegListView.Name = "subRegListView";
            this.subRegListView.Size = new System.Drawing.Size(490, 418);
            this.subRegListView.TabIndex = 17;
            this.subRegListView.UseCompatibleStateImageBehavior = false;
            this.subRegListView.View = System.Windows.Forms.View.Details;
            // 
            // columnHeader5
            // 
            this.columnHeader5.Text = "Class";
            this.columnHeader5.Width = 200;
            // 
            // columnHeader6
            // 
            this.columnHeader6.Text = "HandlerId";
            this.columnHeader6.Width = 140;
            // 
            // columnHeader7
            // 
            this.columnHeader7.Text = "Options";
            this.columnHeader7.Width = 95;
            // 
            // label3
            // 
            this.label3.Dock = System.Windows.Forms.DockStyle.Top;
            this.label3.Location = new System.Drawing.Point(3, 3);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(490, 32);
            this.label3.TabIndex = 9;
            this.label3.Text = "The objects will be subscribed for by SATE when connecting to DOSE.";
            // 
            // bottompanel
            // 
            this.bottompanel.Controls.Add(this.applybutton);
            this.bottompanel.Controls.Add(this.cancelbutton);
            this.bottompanel.Controls.Add(this.okbutton);
            this.bottompanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.bottompanel.Location = new System.Drawing.Point(0, 482);
            this.bottompanel.Name = "bottompanel";
            this.bottompanel.Size = new System.Drawing.Size(504, 48);
            this.bottompanel.TabIndex = 1;
            // 
            // applybutton
            // 
            this.applybutton.Location = new System.Drawing.Point(416, 16);
            this.applybutton.Name = "applybutton";
            this.applybutton.Size = new System.Drawing.Size(75, 23);
            this.applybutton.TabIndex = 2;
            this.applybutton.Text = "Apply";
            this.applybutton.Click += new System.EventHandler(this.applybutton_Click);
            // 
            // cancelbutton
            // 
            this.cancelbutton.Location = new System.Drawing.Point(336, 16);
            this.cancelbutton.Name = "cancelbutton";
            this.cancelbutton.Size = new System.Drawing.Size(75, 23);
            this.cancelbutton.TabIndex = 1;
            this.cancelbutton.Text = "Cancel";
            this.cancelbutton.Click += new System.EventHandler(this.cancelbutton_Click);
            // 
            // okbutton
            // 
            this.okbutton.Location = new System.Drawing.Point(256, 16);
            this.okbutton.Name = "okbutton";
            this.okbutton.Size = new System.Drawing.Size(75, 23);
            this.okbutton.TabIndex = 0;
            this.okbutton.Text = "OK";
            this.okbutton.Click += new System.EventHandler(this.okbutton_Click);
            // 
            // SettingsForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(504, 530);
            this.Controls.Add(this.tabControl);
            this.Controls.Add(this.bottompanel);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "SettingsForm";
            this.Text = "Settings";
            this.tabControl.ResumeLayout(false);
            this.generalTabPage.ResumeLayout(false);
            this.generalTabPage.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.handlersTabPage.ResumeLayout(false);
            this.subscriptionTabPage.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.bottompanel.ResumeLayout(false);
            this.ResumeLayout(false);

        }
        #endregion

        private void cancelbutton_Click(object sender, System.EventArgs e)
        {
            this.DialogResult=DialogResult.Cancel;
        }

        private void okbutton_Click(object sender, System.EventArgs e)
        {
            if (SaveSettings())
                this.DialogResult=DialogResult.OK;
        }

        public new void ShowDialog()
        {
            //---------------------------
            //Connection
            //---------------------------
            this.connectionNameTextBox.Text=Settings.Sate.ConnectionName;
            this.autoConnectCheckBox.Checked=Settings.Sate.ConnectAtStartUp;

            //---------------------------
            //Inbox
            //---------------------------
            this.inboxQueueLengthtextBox.Text=Settings.Sate.InboxQueueuLength.ToString();

            //---------------------------
            //Explorer view
            //---------------------------

            if (Settings.Sate.DefaultExplorerView)
            {
                this.inheritanceradioButton.Checked=true;
                this.namespacesradioButton.Checked=false;
            }
            else
            {
                this.inheritanceradioButton.Checked=false;
                this.namespacesradioButton.Checked=true;
            }

            //---------------------------
            //Reply
            //---------------------------
            noDispatchCheckBox.Checked = Settings.Sate.NoDispatch;
            noResponseCheckBox.Checked = Settings.Sate.NoResponse;
            this.createReqcheckBox.Checked = Settings.Sate.AutoCreate;
            this.updateReqcheckBox.Checked = Settings.Sate.AutoUpdate;
            this.deleteReqcheckBox.Checked = Settings.Sate.AutoDelete;

            //---------------------------
            //Auto Reg/Sub
            //---------------------------
            LoadObjectIds();
            base.ShowDialog();
        }


        private void applybutton_Click(object sender, System.EventArgs e)
        {
            SaveSettings();
        }

        private bool SaveSettings()
        {
            Cursor=Cursors.WaitCursor;

            //Connection name
            Settings.Sate.ConnectionName=this.connectionNameTextBox.Text;

            //Auto connect
            Settings.Sate.ConnectAtStartUp=this.autoConnectCheckBox.Checked;

            //DefaultExplorerView
            Settings.Sate.DefaultExplorerView=inheritanceradioButton.Checked;

            //InboxQueueuLength
            try
            {
                Settings.Sate.InboxQueueuLength=int.Parse(this.inboxQueueLengthtextBox.Text);
            }
            catch
            {
                MessageBox.Show("Inbox queue length is invalid!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                Cursor=Cursors.Default;
                return false;
            }

            //Save everything
            Settings.Save();

            Cursor=Cursors.Default;
            return true;
        }

#if REMOVED_CODE
        private void DeleteFromListBox(ListView lw)
        {
            foreach (int i in lw.SelectedIndices)
            {
                lw.Items.RemoveAt(i);
            }
        }
#endif
        private void LoadObjectIds()
        {
            foreach (RegInfo ri in Settings.Sate.Register)
            {
                try
                {
                    MyListViewItem ml = new MyListViewItem(ri.typeId, ri.handlerIdSer.HandlerId(), ri.pending, ri.injection);
                    reglistView.Items.Add(ml);
                }
                catch (Safir.Dob.Typesystem.IllegalValueException)
                {
                }
            }
            foreach (SubInfo si in Settings.Sate.Subscribe)
            {
                try
                {
                    MyListViewItem ml = new MyListViewItem(si.typeId, si.entityIdSer != null ? si.entityIdSer.EntityId() : null, si.channelIdSer.ChannelId(), si.upd, si.restartSubscription);
                    sublistView.Items.Add(ml);
                }
                catch (Safir.Dob.Typesystem.IllegalValueException)
                {
                }
            }
            foreach (SubRegInfo sri in Settings.Sate.SubscribeReg)
            {
                try
                {
                    MyListViewItem ml = new MyListViewItem(sri.typeId, sri.handlerIdSer.HandlerId(), sri.includeSubClasses, sri.restartSubscription, false);
                    subRegListView.Items.Add(ml);
                }
                catch (Safir.Dob.Typesystem.IllegalValueException)
                {
                }
            }
        }

        private void createReqcheckBox_CheckedChanged(object sender, EventArgs e)
        {
            Settings.Sate.AutoCreate = this.createReqcheckBox.Checked;
            Settings.Save();
        }

        private void updateReqcheckBox_CheckedChanged(object sender, EventArgs e)
        {
            Settings.Sate.AutoUpdate = this.updateReqcheckBox.Checked;
            Settings.Save();
        }

        private void deleteReqcheckBox_CheckedChanged(object sender, EventArgs e)
        {
            Settings.Sate.AutoDelete = this.deleteReqcheckBox.Checked;
            Settings.Save();
        }

        private void openReplybutton_Click(object sender, EventArgs e)
        {
            ObjectInfo objInfo = new ObjectInfo();
            objInfo.Obj = Settings.Sate.AutoResponse;
           MainForm.Instance.AddTabPage(new ObjectEditTabPage(objInfo));
        }

        private void noResponseCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            Settings.Sate.NoResponse = noResponseCheckBox.Checked;
            Settings.Save();
        }

        private void noDispatchCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            Settings.Sate.NoDispatch = noDispatchCheckBox.Checked;
            Settings.Save();

            if (!Settings.Sate.NoDispatch)
            {
                MainForm.Instance.KickDispatch();
            }
        }

        void OnRemoveSub_click(object sender, EventArgs e)
        {
            foreach (ListViewItem li in sublistView.SelectedItems)
            {
                this.sublistView.Items.Remove(li);
            }

            SubInfo[] tmp = new SubInfo[sublistView.Items.Count];
            int i = 0;
            foreach (MyListViewItem l in sublistView.Items)
            {
                tmp[i++] = new SubInfo(l.typeId, new EntityIdSerializeable(l.entityId), new ChannelIdSerializable(l.channelId), l.upd, l.includeSubClasses, l.restartSubscription);
            }
            Settings.Sate.Subscribe = tmp;
            Settings.Save();
            }

        void OnRemoveSubReg_click(object sender, EventArgs e)
        {
            foreach (ListViewItem li in subRegListView.SelectedItems)
            {
                this.subRegListView.Items.Remove(li);
            }

            SubRegInfo[] tmp = new SubRegInfo[subRegListView.Items.Count];
            int i = 0;
            foreach (MyListViewItem l in subRegListView.Items)
            {
                tmp[i++] = new SubRegInfo(l.typeId, new HandlerIdSerializeable(l.handlerId), l.includeSubClasses, l.restartSubscription);
            }
            Settings.Sate.SubscribeReg = tmp;
            Settings.Save();
        }


        void OnRemoveReg_click(object sender, EventArgs e)
        {
            foreach (ListViewItem li in reglistView.SelectedItems)
            {
                this.reglistView.Items.Remove(li);
            }

            RegInfo[] tmp = new RegInfo[reglistView.Items.Count];
            int i = 0;
            foreach (MyListViewItem l in reglistView.Items)
            {
                tmp[i++] = new RegInfo(l.typeId, new HandlerIdSerializeable(l.handlerId), l.pending, l.injection, l.requestorDecides);
            }
            Settings.Sate.Register = tmp;
            Settings.Save();
        }
    }
}
