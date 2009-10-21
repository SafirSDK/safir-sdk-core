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
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

namespace Sate
{
    delegate void CallDispatchDelegate();

    /// <summary>
    /// This is the main window in SATE.
    /// </summary>
    public class MainForm : System.Windows.Forms.Form,
                            Safir.Dob.Dispatcher,
                            Safir.Dob.StopHandler,
                            Safir.Dob.MessageSender,
                            Safir.Dob.MessageSubscriber,
                            Safir.Dob.Requestor,
                            Safir.Dob.ServiceHandler,
                            Safir.Dob.ServiceHandlerPending,
                            Safir.Dob.RegistrationSubscriber,
                            Safir.Dob.EntitySubscriber,
                            Safir.Dob.EntityHandler,
                            Safir.Dob.EntityHandlerInjection,
                            Safir.Dob.EntityHandlerPending
    {
        private delegate void OnConnectedDelegate();
        private event OnConnectedDelegate ConnectedEvent;

        //Must prevent the callback delegate from garbage collection. Thats why its declared here
        private CallDispatchDelegate callDispatch;

        private System.Diagnostics.Process doseMainProcess;

        //dynamically loaded assembly
        private System.Reflection.Assembly dotsGenerated;

        //DOB
        private Safir.Dob.Connection dose = new Safir.Dob.Connection();

        private AboutForm aboutForm = new AboutForm();
        private System.Windows.Forms.StatusBar statusBar;
        private System.Windows.Forms.Panel leftPanel;
        private System.Windows.Forms.Splitter leftSplitter;
        private System.Windows.Forms.Panel bottomPanel;
        private System.Windows.Forms.Splitter bottomSplitter;
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.ImageList imageList;
        private bool isConnected = false;
        private Panel fillPanel;
        private TabControl tabControl;
        private MenuStrip menuStrip1;
        private ToolStripMenuItem connectionToolStripMenuItem;
        private ToolStripMenuItem connectToolStripMenuItem;
        private ToolStripMenuItem disconnectToolStripMenuItem;
        private ToolStripMenuItem viewToolStripMenuItem;
        private ToolStripMenuItem classExplorerToolStripMenuItem;
        private ToolStripMenuItem inboxToolStripMenuItem;
        private ToolStripMenuItem outputToolStripMenuItem;
        private ToolStripMenuItem toolsToolStripMenuItem;
        private ToolStripMenuItem settingsToolStripMenuItem;
        private ToolStripMenuItem scenarioPlayrecordToolStripMenuItem;
        private ToolStripMenuItem helpToolStripMenuItem;
        private ToolStripMenuItem aboutSATEToolStripMenuItem;
        private Panel bottomFillPanel;
        private Splitter bottomRightSplitter;
        private Panel bottomRightPanel;
        private PanelLabelControl fillPanelLabel = new PanelLabelControl("");
        private Panel fillfillpanel;
        private ToolStripSeparator toolStripMenuItem1;
        private ToolStripMenuItem openToolStripMenuItem;
        private ToolStripMenuItem saveToolStripMenuItem;
        private ToolStripMenuItem helpToolStripMenuItem1;
        private ToolStripSeparator toolStripMenuItem2;
        private ToolStripMenuItem findToolStripMenuItem;
        private ToolStripMenuItem opendoufileToolStripMenuItem;
        private ToolStripMenuItem openscenarioToolStripMenuItem;
        private ToolStripMenuItem openserializedObjectToolStripMenuItem;
        private ToolStripSeparator toolStripMenuItem3;
        private ToolStripMenuItem connectWithContextToolStripMenuItem;

        private static MainForm instance;
        private ToolStripSeparator toolStripMenuItem4;
        private ToolStripMenuItem exitToolStripMenuItem;
        private ToolStripMenuItem rescentFilesToolStripMenuItem;
        private ToolStripMenuItem calculatorsToolStripMenuItem;
        private ToolStripMenuItem typeIdToolStripMenuItem;
        private ToolStripMenuItem timestampToolStripMenuItem;

        private Int32 connectContext = 0;
        private ToolStripSeparator toolStripSeparator1;
        private ToolStripMenuItem runGarbageCollectorToolStripMenuItem;

        public List<Int64> handlerDecidesTypeIdList = new List<long>();
        public List<Int64> requestorDecidesTypeIdList = new List<long>();


        private MainForm()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            fillPanelLabel.BackColor = System.Drawing.SystemColors.Control;
            fillPanelLabel.ForeColor = Color.Black;
            fillPanelLabel.CloseEvent += new PanelLabelControl.OnCloseEventHandler(fillPanelLabel_CloseEvent);
            fillPanelLabel.ContextMenu = new ContextMenu(new MenuItem[] { new MenuItem("Close all tabs", new EventHandler(OnCloseAllTabs)) });
            this.fillfillpanel.Controls.Add(fillPanelLabel);
            this.fillPanel.Visible = false;

            ConnectedEvent+=new OnConnectedDelegate(OnConnected);

            callDispatch = new CallDispatchDelegate(dose.Dispatch);


            AllowDrop = true;
            DragEnter += new DragEventHandler(OnDragEnter);
            DragDrop += new DragEventHandler(OnDragDrop);


            //-------------------------------------------------------------
            //First of all read the parameters saved in sate_settings.xml
            //-------------------------------------------------------------
            Settings.Load();

            //Create Auto Reply
            if (Settings.Sate.XmlReplyObject != null) //try to load saved response
            {
                Settings.Sate.AutoResponse = new Safir.Dob.Response();
                bool loadedOk=true;
                try
                {
                    Settings.Sate.AutoResponse = (Safir.Dob.Response)Safir.Dob.Typesystem.Serialization.ToObject(Settings.Sate.XmlReplyObject);
                }
                catch { loadedOk = false; }

                if (!loadedOk)
                {
                    Settings.Sate.AutoResponse = new Safir.Dob.SuccessResponse();
                    Settings.Sate.XmlReplyObject = null;
                    Settings.Save();
                }
            }
            else //no stored response, use a simple success-reply
            {
                Settings.Sate.AutoResponse = new Safir.Dob.SuccessResponse();
            }
            //------------ End Read Parameters -------------


            leftPanel.Dock=DockStyle.Left;

            ExplorerPanel.Instance.Dock=DockStyle.Fill;
            leftPanel.Controls.Add(ExplorerPanel.Instance);

            InboxPanel.Instance.Dock = DockStyle.Fill;

            this.bottomFillPanel.Controls.Add(InboxPanel.Instance);

            OutputPanel.Instance.Dock=DockStyle.Fill;
            this.bottomRightPanel.Controls.Add(OutputPanel.Instance);

        }

        //Event handler for close all tabs context menu
        void OnCloseAllTabs(object sender, EventArgs e)
        {
            tabControl.TabPages.Clear();
            fillPanel.Visible = false;
        }

        //Event handler for drag and drop of files on SATE.
        void OnDragDrop(object sender, DragEventArgs e)
        {
            string[] files = (string[])e.Data.GetData(DataFormats.FileDrop);
            foreach (string f in files)
            {
                string str=f.ToLower();
                if (str.EndsWith(".dou"))
                {
                    OpenDouFile(str);
                }
                else if (str.EndsWith(".dos"))
                {
                    ScenarioTabPage.Instance.Player.LoadScenario(str);
                    AddTabPage(ScenarioTabPage.Instance);
                }
                else if (str.EndsWith(".xml"))
                {
                    OpenSerializedObject(str);
                }
            }
        }
        //Needed to get drag and drop working
        void OnDragEnter(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                e.Effect = DragDropEffects.All;
            }
        }

        //Event handler for closing a tab
        // Selects the tab to the right of the closing tab (if possible)
        void fillPanelLabel_CloseEvent(object sender, EventArgs e)
        {
            // save value
            int selectedIndex = tabControl.SelectedIndex;
            if (selectedIndex >= 0)
            {
                tabControl.TabPages.RemoveAt(selectedIndex);
                if ((tabControl.TabCount > 1) && (selectedIndex != 0))
                {

                    if (selectedIndex < tabControl.TabCount)
                    {
                        tabControl.SelectTab(selectedIndex);
                    }
                    else
                    {
                        tabControl.SelectTab(tabControl.TabCount - 1);
                    }
                }

            }
            if (tabControl.TabPages.Count == 0)
            {
                this.fillPanel.Visible = false;
            }
        }

        //Get the only instance of MainForm (singleton)
        public static MainForm Instance
        {
            get
            {
                if (instance==null)
                    instance=new MainForm();
                return instance;
            }
        }

        //Get the connection to the DOB
        public Safir.Dob.Connection Dose
        {
            get
            {
                return dose;
            }
        }

        //Get the dots_generated assembly
        public System.Reflection.Assembly DotsGenerated
        {
            get
            {
                return dotsGenerated;
            }
        }

        //Shows a new tab
        public void AddTabPage(TabPage tp)
        {
            this.fillPanel.Visible = true;
#if STSYLI
            if (!(tp is ObjectEditTabPage && UpdateLiveData(((ObjectEditTabPage)tp).ObjEditCtrl.GetObject())))
#else
                  //if (tp is ObjectEditTabPage)
#endif
            {
                try
                {
                    tabControl.TabPages.Add(tp);
                    tabControl.SelectedIndex = tabControl.TabPages.Count - 1;
                }
                catch (System.ComponentModel.Win32Exception e)
                {
                    string msg = "Unable to display more windows in SATE! Try closing other tabs first! - Exception: " + e.Message;
                    OutputPanel.Instance.LogEvent(msg, true);
                    tp.Dispose();
                    throw;
                }
            }

        }

        public void KickDispatch()
        {
            System.GC.Collect();
            // Invoke will force a thread switch!
            this.Invoke(callDispatch);
        }

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        protected override void Dispose( bool disposing )
        {
            if( disposing )
            {
                if (components != null)
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
            this.imageList = new System.Windows.Forms.ImageList(this.components);
            this.statusBar = new System.Windows.Forms.StatusBar();
            this.bottomSplitter = new System.Windows.Forms.Splitter();
            this.bottomPanel = new System.Windows.Forms.Panel();
            this.bottomFillPanel = new System.Windows.Forms.Panel();
            this.bottomRightSplitter = new System.Windows.Forms.Splitter();
            this.bottomRightPanel = new System.Windows.Forms.Panel();
            this.leftSplitter = new System.Windows.Forms.Splitter();
            this.leftPanel = new System.Windows.Forms.Panel();
            this.fillPanel = new System.Windows.Forms.Panel();
            this.fillfillpanel = new System.Windows.Forms.Panel();
            this.tabControl = new System.Windows.Forms.TabControl();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.connectionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.connectToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.connectWithContextToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.disconnectToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripSeparator();
            this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.opendoufileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openscenarioToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openserializedObjectToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.saveToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.findToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.rescentFilesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem4 = new System.Windows.Forms.ToolStripSeparator();
            this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.viewToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.classExplorerToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.inboxToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.outputToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.calculatorsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.typeIdToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.timestampToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.scenarioPlayrecordToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.runGarbageCollectorToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem3 = new System.Windows.Forms.ToolStripSeparator();
            this.settingsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.helpToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.helpToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem2 = new System.Windows.Forms.ToolStripSeparator();
            this.aboutSATEToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.bottomPanel.SuspendLayout();
            this.fillPanel.SuspendLayout();
            this.fillfillpanel.SuspendLayout();
            this.menuStrip1.SuspendLayout();
            this.SuspendLayout();
            //
            // imageList
            //
            this.imageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList.ImageStream")));
            this.imageList.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList.Images.SetKeyName(0, "");
            this.imageList.Images.SetKeyName(1, "");
            this.imageList.Images.SetKeyName(2, "");
            this.imageList.Images.SetKeyName(3, "");
            this.imageList.Images.SetKeyName(4, "");
            this.imageList.Images.SetKeyName(5, "");
            this.imageList.Images.SetKeyName(6, "");
            //
            // statusBar
            //
            this.statusBar.Location = new System.Drawing.Point(0, 744);
            this.statusBar.Name = "statusBar";
            this.statusBar.Size = new System.Drawing.Size(1042, 22);
            this.statusBar.TabIndex = 3;
            this.statusBar.Text = "Not connected!";
            //
            // bottomSplitter
            //
            this.bottomSplitter.BackColor = System.Drawing.SystemColors.Control;
            this.bottomSplitter.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.bottomSplitter.Location = new System.Drawing.Point(188, 621);
            this.bottomSplitter.MinSize = 50;
            this.bottomSplitter.Name = "bottomSplitter";
            this.bottomSplitter.Size = new System.Drawing.Size(854, 3);
            this.bottomSplitter.TabIndex = 3;
            this.bottomSplitter.TabStop = false;
            //
            // bottomPanel
            //
            this.bottomPanel.BackColor = System.Drawing.SystemColors.Control;
            this.bottomPanel.Controls.Add(this.bottomFillPanel);
            this.bottomPanel.Controls.Add(this.bottomRightSplitter);
            this.bottomPanel.Controls.Add(this.bottomRightPanel);
            this.bottomPanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.bottomPanel.Location = new System.Drawing.Point(188, 624);
            this.bottomPanel.Name = "bottomPanel";
            this.bottomPanel.Size = new System.Drawing.Size(854, 120);
            this.bottomPanel.TabIndex = 2;
            //
            // bottomFillPanel
            //
            this.bottomFillPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.bottomFillPanel.Location = new System.Drawing.Point(0, 0);
            this.bottomFillPanel.Name = "bottomFillPanel";
            this.bottomFillPanel.Size = new System.Drawing.Size(458, 120);
            this.bottomFillPanel.TabIndex = 2;
            //
            // bottomRightSplitter
            //
            this.bottomRightSplitter.Dock = System.Windows.Forms.DockStyle.Right;
            this.bottomRightSplitter.Location = new System.Drawing.Point(458, 0);
            this.bottomRightSplitter.Name = "bottomRightSplitter";
            this.bottomRightSplitter.Size = new System.Drawing.Size(3, 120);
            this.bottomRightSplitter.TabIndex = 1;
            this.bottomRightSplitter.TabStop = false;
            //
            // bottomRightPanel
            //
            this.bottomRightPanel.Dock = System.Windows.Forms.DockStyle.Right;
            this.bottomRightPanel.Location = new System.Drawing.Point(461, 0);
            this.bottomRightPanel.Name = "bottomRightPanel";
            this.bottomRightPanel.Size = new System.Drawing.Size(393, 120);
            this.bottomRightPanel.TabIndex = 0;
            //
            // leftSplitter
            //
            this.leftSplitter.BackColor = System.Drawing.SystemColors.Control;
            this.leftSplitter.Location = new System.Drawing.Point(185, 24);
            this.leftSplitter.MinSize = 185;
            this.leftSplitter.Name = "leftSplitter";
            this.leftSplitter.Size = new System.Drawing.Size(3, 720);
            this.leftSplitter.TabIndex = 1;
            this.leftSplitter.TabStop = false;
            //
            // leftPanel
            //
            this.leftPanel.BackColor = System.Drawing.SystemColors.Control;
            this.leftPanel.Dock = System.Windows.Forms.DockStyle.Left;
            this.leftPanel.Location = new System.Drawing.Point(0, 24);
            this.leftPanel.Name = "leftPanel";
            this.leftPanel.Size = new System.Drawing.Size(185, 720);
            this.leftPanel.TabIndex = 0;
            //
            // fillPanel
            //
            this.fillPanel.Controls.Add(this.fillfillpanel);
            this.fillPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.fillPanel.Location = new System.Drawing.Point(188, 24);
            this.fillPanel.Name = "fillPanel";
            this.fillPanel.Size = new System.Drawing.Size(854, 597);
            this.fillPanel.TabIndex = 4;
            //
            // fillfillpanel
            //
            this.fillfillpanel.Controls.Add(this.tabControl);
            this.fillfillpanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.fillfillpanel.Location = new System.Drawing.Point(0, 0);
            this.fillfillpanel.Name = "fillfillpanel";
            this.fillfillpanel.Size = new System.Drawing.Size(854, 597);
            this.fillfillpanel.TabIndex = 1;
            //
            // tabControl
            //
            this.tabControl.Appearance = System.Windows.Forms.TabAppearance.FlatButtons;
            this.tabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.tabControl.ImageList = this.imageList;
            this.tabControl.Location = new System.Drawing.Point(0, 0);
            this.tabControl.Name = "tabControl";
            this.tabControl.SelectedIndex = 0;
            this.tabControl.Size = new System.Drawing.Size(854, 597);
            this.tabControl.TabIndex = 0;
            this.tabControl.SelectedIndexChanged += new System.EventHandler(this.tabControl_selectedIndexChanged);
            //
            // menuStrip1
            //
            this.menuStrip1.BackColor = System.Drawing.SystemColors.Menu;
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.connectionToolStripMenuItem,
            this.viewToolStripMenuItem,
            this.toolsToolStripMenuItem,
            this.helpToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(1042, 24);
            this.menuStrip1.TabIndex = 5;
            this.menuStrip1.Text = "menuStrip1";
            //
            // connectionToolStripMenuItem
            //
            this.connectionToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.connectToolStripMenuItem,
            this.connectWithContextToolStripMenuItem,
            this.disconnectToolStripMenuItem,
            this.toolStripMenuItem1,
            this.openToolStripMenuItem,
            this.saveToolStripMenuItem,
            this.findToolStripMenuItem,
            this.rescentFilesToolStripMenuItem,
            this.toolStripMenuItem4,
            this.exitToolStripMenuItem});
            this.connectionToolStripMenuItem.Name = "connectionToolStripMenuItem";
            this.connectionToolStripMenuItem.Size = new System.Drawing.Size(73, 20);
            this.connectionToolStripMenuItem.Text = "Connection";
            //
            // connectToolStripMenuItem
            //
            this.connectToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("connectToolStripMenuItem.Image")));
            this.connectToolStripMenuItem.Name = "connectToolStripMenuItem";
            this.connectToolStripMenuItem.Size = new System.Drawing.Size(200, 22);
            this.connectToolStripMenuItem.Text = "Connect";
            this.connectToolStripMenuItem.Click += new System.EventHandler(this.connectmenuItem_Click);
            //
            // connectWithContextToolStripMenuItem
            //
            this.connectWithContextToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("connectWithContextToolStripMenuItem.Image")));
            this.connectWithContextToolStripMenuItem.Name = "connectWithContextToolStripMenuItem";
            this.connectWithContextToolStripMenuItem.Size = new System.Drawing.Size(200, 22);
            this.connectWithContextToolStripMenuItem.Text = "Connect with context...";
            this.connectWithContextToolStripMenuItem.Click += new System.EventHandler(this.connectWithContextToolStripMenuItem_Click);
            //
            // disconnectToolStripMenuItem
            //
            this.disconnectToolStripMenuItem.Enabled = false;
            this.disconnectToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("disconnectToolStripMenuItem.Image")));
            this.disconnectToolStripMenuItem.Name = "disconnectToolStripMenuItem";
            this.disconnectToolStripMenuItem.Size = new System.Drawing.Size(200, 22);
            this.disconnectToolStripMenuItem.Text = "Disconnect";
            this.disconnectToolStripMenuItem.Click += new System.EventHandler(this.disconnectmenuItem_Click);
            //
            // toolStripMenuItem1
            //
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(197, 6);
            //
            // openToolStripMenuItem
            //
            this.openToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.opendoufileToolStripMenuItem,
            this.openscenarioToolStripMenuItem,
            this.openserializedObjectToolStripMenuItem});
            this.openToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("openToolStripMenuItem.Image")));
            this.openToolStripMenuItem.Name = "openToolStripMenuItem";
            this.openToolStripMenuItem.Size = new System.Drawing.Size(200, 22);
            this.openToolStripMenuItem.Text = "Open";
            //
            // opendoufileToolStripMenuItem
            //
            this.opendoufileToolStripMenuItem.Name = "opendoufileToolStripMenuItem";
            this.opendoufileToolStripMenuItem.Size = new System.Drawing.Size(175, 22);
            this.opendoufileToolStripMenuItem.Text = "Dou file...";
            this.opendoufileToolStripMenuItem.Click += new System.EventHandler(this.opendoufileToolStripMenuItem_Click);
            //
            // openscenarioToolStripMenuItem
            //
            this.openscenarioToolStripMenuItem.Name = "openscenarioToolStripMenuItem";
            this.openscenarioToolStripMenuItem.Size = new System.Drawing.Size(175, 22);
            this.openscenarioToolStripMenuItem.Text = "Scenario file...";
            this.openscenarioToolStripMenuItem.Click += new System.EventHandler(this.openscenarioToolStripMenuItem_Click);
            //
            // openserializedObjectToolStripMenuItem
            //
            this.openserializedObjectToolStripMenuItem.Name = "openserializedObjectToolStripMenuItem";
            this.openserializedObjectToolStripMenuItem.Size = new System.Drawing.Size(175, 22);
            this.openserializedObjectToolStripMenuItem.Text = "Serialized object...";
            this.openserializedObjectToolStripMenuItem.Click += new System.EventHandler(this.openserializedObjectToolStripMenuItem_Click);
            //
            // saveToolStripMenuItem
            //
            this.saveToolStripMenuItem.Enabled = false;
            this.saveToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("saveToolStripMenuItem.Image")));
            this.saveToolStripMenuItem.Name = "saveToolStripMenuItem";
            this.saveToolStripMenuItem.Size = new System.Drawing.Size(200, 22);
            this.saveToolStripMenuItem.Text = "Save...";
            this.saveToolStripMenuItem.Click += new System.EventHandler(this.saveToolStripMenuItem_Click);
            //
            // findToolStripMenuItem
            //
            this.findToolStripMenuItem.Enabled = false;
            this.findToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("findToolStripMenuItem.Image")));
            this.findToolStripMenuItem.Name = "findToolStripMenuItem";
            this.findToolStripMenuItem.Size = new System.Drawing.Size(200, 22);
            this.findToolStripMenuItem.Text = "Find...";
            this.findToolStripMenuItem.Click += new System.EventHandler(this.findToolStripMenuItem_Click);
            //
            // rescentFilesToolStripMenuItem
            //
            this.rescentFilesToolStripMenuItem.Name = "rescentFilesToolStripMenuItem";
            this.rescentFilesToolStripMenuItem.Size = new System.Drawing.Size(200, 22);
            this.rescentFilesToolStripMenuItem.Text = "Recent files";
            //
            // toolStripMenuItem4
            //
            this.toolStripMenuItem4.Name = "toolStripMenuItem4";
            this.toolStripMenuItem4.Size = new System.Drawing.Size(197, 6);
            //
            // exitToolStripMenuItem
            //
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(200, 22);
            this.exitToolStripMenuItem.Text = "Exit";
            this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
            //
            // viewToolStripMenuItem
            //
            this.viewToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.classExplorerToolStripMenuItem,
            this.inboxToolStripMenuItem,
            this.outputToolStripMenuItem});
            this.viewToolStripMenuItem.Name = "viewToolStripMenuItem";
            this.viewToolStripMenuItem.Size = new System.Drawing.Size(41, 20);
            this.viewToolStripMenuItem.Text = "View";
            //
            // classExplorerToolStripMenuItem
            //
            this.classExplorerToolStripMenuItem.Checked = true;
            this.classExplorerToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.classExplorerToolStripMenuItem.Name = "classExplorerToolStripMenuItem";
            this.classExplorerToolStripMenuItem.Size = new System.Drawing.Size(153, 22);
            this.classExplorerToolStripMenuItem.Text = "Class Explorer";
            this.classExplorerToolStripMenuItem.Click += new System.EventHandler(this.classExplorermenuItem_Click);
            //
            // inboxToolStripMenuItem
            //
            this.inboxToolStripMenuItem.Checked = true;
            this.inboxToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.inboxToolStripMenuItem.Name = "inboxToolStripMenuItem";
            this.inboxToolStripMenuItem.Size = new System.Drawing.Size(153, 22);
            this.inboxToolStripMenuItem.Text = "Inbox";
            this.inboxToolStripMenuItem.Click += new System.EventHandler(this.subrespmenuItem_Click);
            //
            // outputToolStripMenuItem
            //
            this.outputToolStripMenuItem.Checked = true;
            this.outputToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.outputToolStripMenuItem.Name = "outputToolStripMenuItem";
            this.outputToolStripMenuItem.Size = new System.Drawing.Size(153, 22);
            this.outputToolStripMenuItem.Text = "Output";
            this.outputToolStripMenuItem.Click += new System.EventHandler(this.eventlogmenuItem_Click);
            //
            // toolsToolStripMenuItem
            //
            this.toolsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.calculatorsToolStripMenuItem,
            this.scenarioPlayrecordToolStripMenuItem,
            this.toolStripSeparator1,
            this.runGarbageCollectorToolStripMenuItem,
            this.toolStripMenuItem3,
            this.settingsToolStripMenuItem});
            this.toolsToolStripMenuItem.Name = "toolsToolStripMenuItem";
            this.toolsToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
            this.toolsToolStripMenuItem.Text = "Tools";
            //
            // calculatorsToolStripMenuItem
            //
            this.calculatorsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.typeIdToolStripMenuItem,
            this.timestampToolStripMenuItem});
            this.calculatorsToolStripMenuItem.Image = global::Sate.Resources.TYPEIDCALC;
            this.calculatorsToolStripMenuItem.Name = "calculatorsToolStripMenuItem";
            this.calculatorsToolStripMenuItem.Size = new System.Drawing.Size(196, 22);
            this.calculatorsToolStripMenuItem.Text = "Calculators";
            //
            // typeIdToolStripMenuItem
            //
            this.typeIdToolStripMenuItem.Name = "typeIdToolStripMenuItem";
            this.typeIdToolStripMenuItem.Size = new System.Drawing.Size(136, 22);
            this.typeIdToolStripMenuItem.Text = "TypeId";
            this.typeIdToolStripMenuItem.Click += new System.EventHandler(this.typeIdToolStripMenuItem_Click);
            //
            // timestampToolStripMenuItem
            //
            this.timestampToolStripMenuItem.Name = "timestampToolStripMenuItem";
            this.timestampToolStripMenuItem.Size = new System.Drawing.Size(136, 22);
            this.timestampToolStripMenuItem.Text = "Timestamp";
            this.timestampToolStripMenuItem.Click += new System.EventHandler(this.timestampToolStripMenuItem_Click);
            //
            // scenarioPlayrecordToolStripMenuItem
            //
            this.scenarioPlayrecordToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("scenarioPlayrecordToolStripMenuItem.Image")));
            this.scenarioPlayrecordToolStripMenuItem.Name = "scenarioPlayrecordToolStripMenuItem";
            this.scenarioPlayrecordToolStripMenuItem.Size = new System.Drawing.Size(196, 22);
            this.scenarioPlayrecordToolStripMenuItem.Text = "Scenario play/record...";
            this.scenarioPlayrecordToolStripMenuItem.Click += new System.EventHandler(this.scenariomenuItem_Click);
            //
            // toolStripSeparator1
            //
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(193, 6);
            //
            // runGarbageCollectorToolStripMenuItem
            //
            this.runGarbageCollectorToolStripMenuItem.Name = "runGarbageCollectorToolStripMenuItem";
            this.runGarbageCollectorToolStripMenuItem.Size = new System.Drawing.Size(196, 22);
            this.runGarbageCollectorToolStripMenuItem.Text = "Run Garbage Collector";
            this.runGarbageCollectorToolStripMenuItem.Click += new System.EventHandler(this.runGarbageCollectorToolStripMenuItem_Click);
            //
            // toolStripMenuItem3
            //
            this.toolStripMenuItem3.Name = "toolStripMenuItem3";
            this.toolStripMenuItem3.Size = new System.Drawing.Size(193, 6);
            //
            // settingsToolStripMenuItem
            //
            this.settingsToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("settingsToolStripMenuItem.Image")));
            this.settingsToolStripMenuItem.Name = "settingsToolStripMenuItem";
            this.settingsToolStripMenuItem.Size = new System.Drawing.Size(196, 22);
            this.settingsToolStripMenuItem.Text = "Settings...";
            this.settingsToolStripMenuItem.Click += new System.EventHandler(this.settingsmenuItem_Click);
            //
            // helpToolStripMenuItem
            //
            this.helpToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.helpToolStripMenuItem1,
            this.toolStripMenuItem2,
            this.aboutSATEToolStripMenuItem});
            this.helpToolStripMenuItem.Name = "helpToolStripMenuItem";
            this.helpToolStripMenuItem.Size = new System.Drawing.Size(40, 20);
            this.helpToolStripMenuItem.Text = "Help";
            //
            // helpToolStripMenuItem1
            //
            this.helpToolStripMenuItem1.Image = ((System.Drawing.Image)(resources.GetObject("helpToolStripMenuItem1.Image")));
            this.helpToolStripMenuItem1.Name = "helpToolStripMenuItem1";
            this.helpToolStripMenuItem1.Size = new System.Drawing.Size(154, 22);
            this.helpToolStripMenuItem1.Text = "Help...";
            //
            // toolStripMenuItem2
            //
            this.toolStripMenuItem2.Name = "toolStripMenuItem2";
            this.toolStripMenuItem2.Size = new System.Drawing.Size(151, 6);
            //
            // aboutSATEToolStripMenuItem
            //
            this.aboutSATEToolStripMenuItem.Image = global::Sate.Resources.sate_logo;
            this.aboutSATEToolStripMenuItem.Name = "aboutSATEToolStripMenuItem";
            this.aboutSATEToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.aboutSATEToolStripMenuItem.Text = "About SATE...";
            this.aboutSATEToolStripMenuItem.Click += new System.EventHandler(this.aboutmenuItem_Click);
            //
            // MainForm
            //
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.BackColor = System.Drawing.SystemColors.ControlDark;
            this.ClientSize = new System.Drawing.Size(1042, 766);
            this.Controls.Add(this.fillPanel);
            this.Controls.Add(this.bottomSplitter);
            this.Controls.Add(this.bottomPanel);
            this.Controls.Add(this.leftSplitter);
            this.Controls.Add(this.leftPanel);
            this.Controls.Add(this.statusBar);
            this.Controls.Add(this.menuStrip1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "MainForm";
            this.Text = "SATE";
            this.Load += new System.EventHandler(this.MainForm_Load);
            this.bottomPanel.ResumeLayout(false);
            this.fillPanel.ResumeLayout(false);
            this.fillfillpanel.ResumeLayout(false);
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }
        #endregion

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.Run(MainForm.Instance);
        }

        //This is executed when the MainForm is first loaded
        private void MainForm_Load(object sender, System.EventArgs e)
        {
            OutputPanel.Instance.LogEvent("- SATE started.", true);

            //------------------------------------------------------
            // Load dots_generated assembly
            //------------------------------------------------------
            OutputPanel.Instance.LogEvent("- Loading class definitions...",  false);
            try
            {
                dotsGenerated=System.Reflection.Assembly.GetAssembly(typeof(Safir.Dob.Entity));
            }
            catch
            {
                OutputPanel.Instance.LogEvent("Failed!", true);
                string msg="Falied to load DOB-classes, missing assembly!\nResolve the problem and restart SATE!";
                MessageBox.Show(msg, "Classes Not Found", System.Windows.Forms.MessageBoxButtons.OK, MessageBoxIcon.Error);
                throw;
            }
            OutputPanel.Instance.LogEvent("Success!", true);

            //---------------------------------------------
            //Fill class explorer with existing data types
            //---------------------------------------------

            ExplorerPanel.Instance.LoadClassHierarchy();

            //------------------------------------------
            //Connect to DOSE
            //------------------------------------------
            if (Settings.Sate.ConnectAtStartUp)
            {
                Connect(true,0);
            }

            this.AutoOrganize(); //organizes the windows (panels) in the main form
        }

        //------------------------------------------
        // Connect to DOSE
        //------------------------------------------
        private void ConnectToDose() //A new thread is calling the dose.Open
        {
            int instance = 0;
            string cn=Settings.Sate.ConnectionName;

            bool connected = false;

            while (!connected)
            {
                try
                {
                    dose.Open(cn,
                              instance.ToString(),
                              connectContext,
                              this,  // ConnectionOwner
                              this); // Dispatcher

                    connected = true;
                }
                catch (Safir.Dob.NotOpenException)
                {
                    // Instance already used, try another!
                    instance++;
                }
            }

            dose.Close();
            ConnectedEvent();
        }

        private delegate void HandleConnectionDoneDelegate();
        private void OnConnected() //Event signaling that connection can be done
        {
            Invoke(new HandleConnectionDoneDelegate(HandleConnectionDone)); //change thread back to the main thread.
        }

        //Called when connection to dose is established
        private void HandleConnectionDone()
        {
            int instance = 0;

            bool connected = false;

            while (!connected)
            {
                try
                {
                    //connect to dob
                    dose.Open(Settings.Sate.ConnectionName,
                              instance.ToString(),
                              connectContext,
                              this,  // ConnectionOwner
                              this); // Dispatcher
                    connected = true;
                }
                catch (Safir.Dob.NotOpenException)
                {
                    // Instance already used, try another!
                    instance++;
                }
            }

            isConnected = true;


            //there seems to be a bug in a mono version that causes the DoseMainMonitor to fail
            bool useMonitor = true;
            try
            {
                System.Reflection.Assembly ass = System.Reflection.Assembly.Load("Mono.Posix");
                Console.WriteLine("Runtime Version of Mono.Posix.dll assembly = '{0}'", ass.ImageRuntimeVersion);
                if (ass.ImageRuntimeVersion == "v2.0.50727")
                {
                    Console.WriteLine("Not using the DoseMainMonitor");
                    useMonitor = false;
                }
            }
            catch (System.Exception)
            {

            }

            if (useMonitor)
            {
                //monitor dose_main
                System.Diagnostics.Process[] doseProcess=System.Diagnostics.Process.GetProcessesByName("dose_main");
                if (doseProcess.Length==0)
                {
                    doseProcess=System.Diagnostics.Process.GetProcessesByName("dose_maind");
                }
                if (doseProcess.Length>0)
                {
                    doseMainProcess=doseProcess[0];
                    doseMainProcess.Exited += new EventHandler(DoseMainMonitor);
                }
            }

            this.connectToolStripMenuItem.Enabled = false;
            this.connectWithContextToolStripMenuItem.Enabled = false;
            this.disconnectToolStripMenuItem.Enabled=true;

            // get connection name
            Safir.Dob.ConnectionAspectMisc doseAspectMisc = new Safir.Dob.ConnectionAspectMisc(dose);
            this.statusBar.Text = "Connected as " + doseAspectMisc.GetConnectionName();

            OutputPanel.Instance.LogEvent("success!", true);

            //--------------------------------------------------------------------
            //If connection is ok, set up permanent ownerships stored in settings
            //--------------------------------------------------------------------
            OutputPanel.Instance.LogEvent("- Register entities and services", true);
            foreach (RegInfo id in Settings.Sate.Register)
            {
                if (Safir.Dob.Typesystem.Operations.IsOfType(id.typeId, Safir.Dob.Entity.ClassTypeId))
                {
                    ExplorerPanel.Instance.RegisterEntity(id);
                }
                else
                {
                    ExplorerPanel.Instance.RegisterService(id);
                }
            }

            //----------------------------------------------------
            //...and then set up permanent subscriptions
            //----------------------------------------------------
            OutputPanel.Instance.LogEvent("- Setting up subscriptions", true);
            foreach (SubInfo id in Settings.Sate.Subscribe)
            {
                if (Safir.Dob.Typesystem.Operations.IsOfType(id.typeId, Safir.Dob.Entity.ClassTypeId))
                {
                    ExplorerPanel.Instance.SubscribeEntity(id);
                }
                else if (Safir.Dob.Typesystem.Operations.IsOfType(id.typeId, Safir.Dob.Message.ClassTypeId))
                {
                    ExplorerPanel.Instance.SubscribeMessage(id);
                }
            }

            //----------------------------------------------------
            //...and then set up permanent subscriptions of registrations
            //----------------------------------------------------
            OutputPanel.Instance.LogEvent("- Setting up subscriptions of registrations", true);
            foreach (SubRegInfo id in Settings.Sate.SubscribeReg)
            {

                ExplorerPanel.Instance.SubscribeRegistration(id);
            }

        }

        //-------------------------------------------------
        //Organizes the windows (panels)
        //-------------------------------------------------
        private void AutoOrganize()
        {
            //-----------------------------------------
            // Left Panel
            //-----------------------------------------
            if (this.classExplorerToolStripMenuItem.Checked)
            {
                this.leftPanel.Visible = true;
                this.leftSplitter.Visible = true;
            }
            else
            {
                this.leftPanel.Visible = false;
                this.leftSplitter.Visible = false;
            }

            //-----------------------------------------
            // Bottom Panel
            //-----------------------------------------
            if (this.inboxToolStripMenuItem.Checked || this.outputToolStripMenuItem.Checked)
            {
                this.bottomPanel.Visible = true;
                this.bottomSplitter.Visible = true;
            }
            else
            {
                this.bottomPanel.Visible = false;
                this.bottomSplitter.Visible = false;
            }

            // Inbox
            if (inboxToolStripMenuItem.Checked)
            {
                this.bottomFillPanel.Visible = true;
                this.bottomRightPanel.Dock = DockStyle.Right;
            }
            else
            {
                this.bottomFillPanel.Visible = false;
                if (outputToolStripMenuItem.Checked)
                    this.bottomRightPanel.Dock = DockStyle.Fill;
            }
            // Output
            if (outputToolStripMenuItem.Checked)
            {
                if (!InboxPanel.Instance.Visible)
                    bottomRightPanel.Dock = DockStyle.Fill;
                else
                    bottomRightPanel.Dock = DockStyle.Right;
                bottomRightPanel.Visible = true;
            }
            else
            {
                bottomRightPanel.Visible = false;
            }

            Invalidate();
        }

        //Event handler for menu: View -> Class Explorer
        private void classExplorermenuItem_Click(object sender, System.EventArgs e)
        {
            ShowHideExplorer(!classExplorerToolStripMenuItem.Checked);
        }

        public void ShowHideExplorer(bool show)
        {
            this.classExplorerToolStripMenuItem.Checked = show;
            AutoOrganize();
        }

        //Event handler for menu: View -> Output
        private void eventlogmenuItem_Click(object sender, System.EventArgs e)
        {
            ShowHideOutput(!outputToolStripMenuItem.Checked);
        }

        public void ShowHideOutput(bool show)
        {
            this.outputToolStripMenuItem.Checked = show;
            AutoOrganize();
        }

        //Event handler for menu: View -> Inbox
        private void subrespmenuItem_Click(object sender, System.EventArgs e)
        {
            ShowHideInbox(!inboxToolStripMenuItem.Checked);
        }

        public void ShowHideInbox(bool show)
        {
            this.inboxToolStripMenuItem.Checked = show;
            AutoOrganize();
        }

        //-----------------------------------------------------
        // Implemented Consumers and interfaces
        //-----------------------------------------------------

        #region Dispatcher Members

        public void OnDoDispatch()
        {
            try
            {
                if (!Settings.Sate.NoDispatch && !this.IsDisposed)
                {
                    // Invoke will force a thread switch!
                    this.Invoke(callDispatch);
                }
            }
            catch (System.ObjectDisposedException e)
            {
                Console.WriteLine("Caught exception in Dispatch: " + e);
            }
        }

        #endregion

        #region ConnectionOwner Members

        public void OnStopOrder()
        {
            Environment.Exit(0);
            OutputPanel.Instance.LogEvent("- Received stop order, disconnecting...", true);
            Disconnect();
        }

        #endregion

        #region EntitySubscriber Members

        #endregion

        #region EntityOwner Members

#if STSYLI
        public void OnRegistrationStatus(Safir.Dob.Typesystem.ObjectId objectId,
                                         Safir.Dob.RegistrationStatus.Enumeration registrationStatus)
        {
            string text = Safir.Dob.Typesystem.Operations.GetName(objectId.TypeId);
            long inst = objectId.Instance;
            if (inst == -1)
            {
                text = "- Registration of class '" + text + "' is ";
            }
            else
            {
                text = "- Registration of  instance '" + text + " : " + inst + "' is ";
            }

            switch (registrationStatus)
            {
                case Safir.Dob.RegistrationStatus.Enumeration.Completed:
                {
                    text += "Completed";
                    ExplorerPanel.Instance.SetOwned(objectId);
                }
                break;

                case Safir.Dob.RegistrationStatus.Enumeration.Pending:
                {
                    text += "Pending";
                    ExplorerPanel.Instance.SetPending(objectId);
                }
                break;

                case Safir.Dob.RegistrationStatus.Enumeration.Revoked:
                {
                    text += "Revoked";
                    ExplorerPanel.Instance.SetUnregistered(objectId);
                }
                break;
            }

            OutputPanel.Instance.LogEvent(text, true);
        }

        public void OnRegisterAnyInstanceStatus(Safir.Dob.Typesystem.ObjectId objectId,
                                                bool registrationCompleted)
        {
            //Register any instance is not supported by SATE in the current version
            /*
            string name = Safir.Dob.Typesystem.Operations.GetName(objectId.TypeId);
            long inst = objectId.Instance;
            if (registrationCompleted)
            {
                OutputPanel.Instance.LogEvent
                    ("- Registration (any instance) of class '" + name + " : " + inst + "' is completed", true);
            }
            else
            {
                OutputPanel.Instance.LogEvent
                    ("- Registration (any instance) of class '" + name + "' failed", true);
            }
             */
        }

        public void OnPersistentData(Safir.Dob.Entity entity)
        {
            string name = Safir.Dob.Typesystem.Operations.GetName(entity.GetTypeId());
            long inst = entity.InstanceNumber;

            if (Settings.Sate.AutoCreatePersistent)
            {
                OutputPanel.Instance.LogEvent("- Persistent object accepted, '" + name + " : " + inst + "'", true);
                SetEntity(entity);
            }
            else
            {
                OutputPanel.Instance.LogEvent("- Persistent object refused, '" + name + " : " + inst + "'", true);
            }
        }
#endif
        #region EntityRequestBase Members

        public void OnCreateRequest(Safir.Dob.EntityRequestProxy entityRequestProxy, Safir.Dob.ResponseSender responseSender)
        {
            string name=Safir.Dob.Typesystem.Operations.GetName(entityRequestProxy.TypeId);
            bool handlerDecides = false;
            foreach (Int64 typeId in handlerDecidesTypeIdList)
            {
                if (entityRequestProxy.TypeId == typeId)
                {
                    handlerDecides = true;
                    break;
                }
            }

            Safir.Dob.Typesystem.EntityId entityId;
            Safir.Dob.Typesystem.InstanceId instanceId;

            if (handlerDecides)
            {
                entityId = new Safir.Dob.Typesystem.EntityId(entityRequestProxy.TypeId, Safir.Dob.Typesystem.InstanceId.GenerateRandom());
                instanceId = entityId.InstanceId;
            }
            else
            {
                entityId = entityRequestProxy.EntityId;
                instanceId = entityRequestProxy.InstanceId;
            }

            OutputPanel.Instance.LogEvent("- Received create request on instance '"+ name +" : "+ instanceId.ToString() +"'", true);
            Safir.Dob.Entity entity = entityRequestProxy.Request;

            EntityInfo entityInfo = new EntityInfo();
            entityInfo.Obj = entityRequestProxy.Request;
            entityInfo.setInstanceId(instanceId);
            entityInfo.setHandlerId(entityRequestProxy.ReceivingHandlerId);
            entityInfo.Blobsize = Safir.Dob.Typesystem.BlobOperations.GetSize(entityRequestProxy.Blob);

            InboxPanel.Instance.AddResponse(entityInfo, "Create request");
            if (Settings.Sate.AutoCreate)
            {
                dose.SetAll(entity, instanceId, entityInfo.getHandlerId());
            }

            if (!Settings.Sate.NoResponse)
            {
                if (handlerDecides)
                {
                    OutputPanel.Instance.LogEvent("- Response sent", true);
                    responseSender.Send(Safir.Dob.EntityIdResponse.CreateResponse(entityId));
                }
                else
                {
                    OutputPanel.Instance.LogEvent("- Response sent", true);
                    responseSender.Send(Settings.Sate.AutoResponse);
                }
            }
            else
            {
                OutputPanel.Instance.LogEvent("- ResponseSender discarded", true);
                responseSender.Discard();
            }
        }

        public void OnDeleteRequest(Safir.Dob.EntityRequestProxy entityRequestProxy, Safir.Dob.ResponseSender responseSender)
        {
            string name = Safir.Dob.Typesystem.Operations.GetName(entityRequestProxy.TypeId);
            Safir.Dob.Typesystem.EntityId entityId = entityRequestProxy.EntityId;
            Safir.Dob.Typesystem.InstanceId instanceId = entityRequestProxy.InstanceId;

            OutputPanel.Instance.LogEvent("- Received delete request on instance '" + name + " : " + instanceId + "'", true);

            EntityInfo entityInfo = new EntityInfo();
            entityInfo.setInstanceId(entityRequestProxy.InstanceId);
            entityInfo.setHandlerId(entityRequestProxy.ReceivingHandlerId);

            InboxPanel.Instance.AddNonDisplayableResponse(entityId.TypeId, "Delete request");
            if (Settings.Sate.AutoUpdate)
            {
                dose.Delete(entityId, entityInfo.getHandlerId());
            }

            if (!Settings.Sate.NoResponse)
            {
                OutputPanel.Instance.LogEvent("- Response sent", true);
                responseSender.Send(Settings.Sate.AutoResponse);
            }
            else
            {
                OutputPanel.Instance.LogEvent("- ResponseSender discarded", true);
                responseSender.Discard();
            }
        }

        public void OnUpdateRequest(Safir.Dob.EntityRequestProxy entityRequestProxy, Safir.Dob.ResponseSender responseSender)
        {
            string name = Safir.Dob.Typesystem.Operations.GetName(entityRequestProxy.TypeId);
            Safir.Dob.Typesystem.InstanceId instanceId = entityRequestProxy.InstanceId;

            OutputPanel.Instance.LogEvent("- Received update request on instance '" + name + " : " + instanceId + "'", true);
            Safir.Dob.Entity entity = entityRequestProxy.Request;

            EntityInfo entityInfo = new EntityInfo();
            entityInfo.Obj = entityRequestProxy.Request;
            entityInfo.setInstanceId(entityRequestProxy.InstanceId);
            entityInfo.setHandlerId(entityRequestProxy.ReceivingHandlerId);
            entityInfo.Blobsize = Safir.Dob.Typesystem.BlobOperations.GetSize(entityRequestProxy.Blob);
            InboxPanel.Instance.AddResponse(entityInfo, "Update request");

            if (Settings.Sate.AutoUpdate)
            {
                dose.SetChanges(entity, instanceId, entityInfo.getHandlerId());
            }

            if (!Settings.Sate.NoResponse)
            {
                OutputPanel.Instance.LogEvent("- Response sent", true);
                responseSender.Send(Settings.Sate.AutoResponse);
            }
            else
            {
                OutputPanel.Instance.LogEvent("- ResponseSender discarded", true);
                responseSender.Discard();
            }
        }

        #endregion

        #endregion

        #region MessageSender Members

        public void OnNotMessageOverflow()
        {
            OutputPanel.Instance.LogEvent("- Notification: Not overflow in message queue", true);
        }

        #endregion

        #region RegistrationSubscriber Members


        public void OnUnregistered(System.Int64 typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            string name=Safir.Dob.Typesystem.Operations.GetName(typeId);
            OutputPanel.Instance.LogEvent("- Class '" + name + ": " + handlerId + "' is unregistered", true);
            ExplorerPanel.Instance.SetUnregistered(typeId);
        }

        public void OnRegistered(System.Int64 typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            string name=Safir.Dob.Typesystem.Operations.GetName(typeId);
            OutputPanel.Instance.LogEvent("- Class '" + name + ": " + handlerId + "' is registered", true);
            ExplorerPanel.Instance.SetRegistered(typeId);
        }

        #endregion

        #region ServiceProvider Members

        public void OnServiceRequest(Safir.Dob.ServiceRequestProxy serviceRequestProxy, Safir.Dob.ResponseSender rs)
        {
            string name = Safir.Dob.Typesystem.Operations.GetName(serviceRequestProxy.TypeId);
            OutputPanel.Instance.LogEvent("- Received service request '" + name + "' for handler " +  serviceRequestProxy.ReceivingHandlerId, true);

            ServiceHandlerInfo serviceHandlerInfo = new ServiceHandlerInfo();
            serviceHandlerInfo.setHandlerId(serviceRequestProxy.ReceivingHandlerId);
            serviceHandlerInfo.Obj = serviceRequestProxy.Request;
            serviceHandlerInfo.Blobsize = Safir.Dob.Typesystem.BlobOperations.GetSize(serviceRequestProxy.Blob);

            UpdateLiveData(serviceHandlerInfo);

            InboxPanel.Instance.AddResponse(serviceHandlerInfo, "Service request for handler " +  serviceRequestProxy.ReceivingHandlerId);

            if (!Settings.Sate.NoResponse)
            {
                OutputPanel.Instance.LogEvent("- Response sent", true);
                rs.Send(Settings.Sate.AutoResponse);
            }
            else
            {
                OutputPanel.Instance.LogEvent("- ResponseSender discarded", true);
                rs.Discard();
            }
        }


        #endregion

        #region RevokedRegistrationBase Members

        public void OnRevokedRegistration(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            RegInfo regInfo = new RegInfo();
            regInfo.typeId = typeId;
            regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
            ExplorerPanel.Instance.Unregister(regInfo);

            // remove typeids
            requestorDecidesTypeIdList.Remove(typeId);
            handlerDecidesTypeIdList.Remove(typeId);
        }

        #endregion


        #region Requestor Members

        public void OnNotRequestOverflow()
        {
            OutputPanel.Instance.LogEvent("- Notification: Not overflow in request queue", true);
        }

        public void OnResponse(Safir.Dob.ResponseProxy responseProxy)
        {
            Safir.Dob.Response response = responseProxy.Response;
            Safir.Dob.Typesystem.HandlerId handlerId = responseProxy.RequestHandlerId;
            int requestId = responseProxy.RequestId;

            ResponseInfo responseInfo = new ResponseInfo();
            responseInfo.Response = responseProxy.Response;
            responseInfo.Obj = responseProxy.Response;

            if (response != null)
            {
                string name=Safir.Dob.Typesystem.Operations.GetName(response.GetTypeId());
                OutputPanel.Instance.LogEvent("- Received response '"+name+" : "+ handlerId + "' on requestId "+requestId, true);
            }
            else
            {
                OutputPanel.Instance.LogEvent("- Received response 'null' on handlerId " + handlerId, true);
            }

            InboxPanel.Instance.AddResponse(responseInfo, "Reply on request "+requestId);
            UpdateLiveData(responseInfo);

        }

        #endregion

        #region MessageSubscriber Members

        public void OnMessage(Safir.Dob.MessageProxy messageProxy)
        {

            string name=Safir.Dob.Typesystem.Operations.GetName(messageProxy.TypeId);
            OutputPanel.Instance.LogEvent("- Received message '"+name+"' on channel " + messageProxy.ChannelId.ToString(), true);

            MessageInfo msgInfo = new MessageInfo();
            msgInfo.setChannelId(messageProxy.ChannelId);
            msgInfo.Obj = messageProxy.Message;

            //int blobsize = Safir.Dob.Typesystem.BlobOperations.GetSize(messageProxy.Blob);

            InboxPanel.Instance.AddResponse(msgInfo, "Message received on channel " + messageProxy.ChannelId.ToString());

            UpdateLiveData(msgInfo);
        }

        #endregion

        //Event handler for menu: Tools -> Settings
        private void settingsmenuItem_Click(object sender, System.EventArgs e)
        {
            SettingsForm sf=new SettingsForm();
            sf.ShowDialog();
        }

        //Event handler for menu: Help -> About SATE...
        private void aboutmenuItem_Click(object sender, System.EventArgs e)
        {
            aboutForm.Location=new Point(this.Width/2, this.Height/2);
            aboutForm.ShowDialog();
        }

#if REMOVED_CODE
        //Event handler for menu: Connection -> Open
        private void loadmenuItem_Click(object sender, System.EventArgs e)
        {
            OpenFileDialog of=new OpenFileDialog();
            of.AddExtension=true;
            of.Filter = "XML files (*.xml)|*.xml|All files (*.*)|*.*" ;

            if (of.ShowDialog()==DialogResult.OK)
            {
                System.IO.TextReader reader = new System.IO.StreamReader(of.FileName);
                string xmlObj=reader.ReadToEnd();
                reader.Close();

                try
                {
                    Safir.Dob.Typesystem.Object obj = Safir.Dob.Typesystem.Serialization.ToObject(xmlObj);
                    AddTabPage(new ObjectEditTabPage(obj));
                }
                catch (Safir.Dob.Typesystem.IllegalValueException)
                {
                    MessageBox.Show("Failed to deserialize the object. Object is supposed to be in XML-format.",
                        "Deserialization Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        }
#endif

#if REMOVED_CODE
        //Event handler for menu: Tools -> TypeIdCalculator
        private void typeIdCalcmenuItem_Click(object sender, System.EventArgs e)
        {
            new TypeIdCalculatorForm().ShowDialog();
        }
#endif
        //Event handler for menu: Connection -> Connect
        private void connectmenuItem_Click(object sender, System.EventArgs e)
        {
            if (!isConnected)
            {
                ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.Connect, (ScenarioInfo) null);
                Connect(true,0);
            }
        }

        //Event handler for menu: Connection -> Disconnect
        private void disconnectmenuItem_Click(object sender, System.EventArgs e)
        {
            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.Disconnect, (ScenarioInfo)null);
            Disconnect();
        }


        public bool IsConnected
        {
            get {return isConnected;}
        }

        //Event handler called if dose_main terminates in any way
        private void DoseMainMonitor(object sender, EventArgs args)
        {
            System.Windows.Forms.Application.Exit();

            // TODO : For now Sate is exiting without displaying any message boxes. If this is the desired
            //        behaviour the code below can be removed.
            //
            //OutputPanel.Instance.LogEvent("- Process Dose_Main.exe has exited. SATE must be restarted.", true);
            //this.statusBar.Text="Disconnected! - dose_main.exe has been killed, restart SATE.";
            //MessageBox.Show("SATE has detected that "+info+" has exited! You must restart SATE.", "Dose_Main.exe killed", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            //isConnected=false;
            //this.connectToolStripMenuItem.Enabled=false;
            //this.disconnectToolStripMenuItem.Enabled=false;
        }

        //Event handler for menu: Tools -> Scenario
        private void scenariomenuItem_Click(object sender, System.EventArgs e)
        {
            int ix=tabControl.TabPages.IndexOf(ScenarioTabPage.Instance);
            if (ix < 0)
            {
                AddTabPage(ScenarioTabPage.Instance);
            }
            else
            {
                tabControl.SelectedIndex = ix;
            }
            // enable save menu option
            UpdateMainMenu();
        }

        public bool CheckConnection()
        {
            if (!isConnected)
            {
                MessageBox.Show("You are not connected to the DOB!", "Not Connected", MessageBoxButtons.OK, MessageBoxIcon.Information);
                return false;
            }
            return true;
        }

        //-----------------------------------------------------------------------
        // DOSE operations
        //-----------------------------------------------------------------------
        public void Connect(bool startThread, Int32 context)
        {
            //Start thread that hangs on connect
            this.statusBar.Text="Trying to connect...";
            OutputPanel.Instance.LogEvent("- Connecting to DOSE using context " + context + "...", false);
            connectContext = context;
            if (startThread)
            {
                System.Threading.Thread connThread=new System.Threading.Thread(new System.Threading.ThreadStart(this.ConnectToDose));
                connThread.IsBackground=true;
                connThread.Start();
            }
            else
            {
                ConnectToDose();
            }
        }

        public void Disconnect()
        {
            this.Cursor=Cursors.WaitCursor;

            if (isConnected)
            {
                ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.Disconnect, (ScenarioInfo)null);
                dose.Close();
            }
            isConnected=false;

            ExplorerPanel.Instance.LoadClassHierarchy(); //reset explorer

            this.disconnectToolStripMenuItem.Enabled=false;
            this.connectToolStripMenuItem.Enabled=true;
            this.connectWithContextToolStripMenuItem.Enabled = true;
            OutputPanel.Instance.LogEvent("- Disconnected from DOSE.", true);
            this.statusBar.Text="Disconnected!";
            this.Cursor=Cursors.Default;
        }
#if STSYLI
        public void RegisterEntity(Safir.Dob.Typesystem.ObjectId id, bool pending)
        {
            ExplorerPanel.Instance.RegisterEntity(id, pending);
        }

        public void RegisterService(Safir.Dob.Typesystem.ObjectId id, bool pending)
        {
            ExplorerPanel.Instance.RegisterService(id, pending);
        }

        public void Unregister(Safir.Dob.Typesystem.ObjectId id)
        {
            ExplorerPanel.Instance.Unregister(id);
        }

        public void SubscribeRegistration(Safir.Dob.Typesystem.ObjectId id)
        {
            ExplorerPanel.Instance.SubscribeRegistration(id);
        }

        public void SubscribeEntity(Safir.Dob.Typesystem.ObjectId id, bool changeInfo, bool upd)
        {
            ExplorerPanel.Instance.SubscribeEntity(id, changeInfo, upd);
        }
#endif
        /*
        public void SubscribeMessage(Int64 typeId, Safir.Dob.Typesystem.ChannelId channelId)
        {
            SubInfo subInfo = new SubInfo();
            subInfo.typeId = typeId;
            subInfo.channelId = channelId;
            ExplorerPanel.Instance.SubscribeMessage(subInfo);
        //    ExplorerPanel.Instance.SubscribeMessage(typeId, channelId);
        }
        */
#if STSYLI
        public void Unsubscribe(Safir.Dob.Typesystem.ObjectId id)
        {
            ExplorerPanel.Instance.Unsubscribe(id);
        }
#endif

        public void SetChangesEntity(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.SetChangesEntity, entityInfo);

            try
            {
                dose.SetChanges((Safir.Dob.Entity)entityInfo.Obj, entityInfo.getInstanceId(), entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity set changes, '" + entityInfo.Obj.ToString() + "'", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception e)
            {
                OutputPanel.Instance.LogEvent("- Entity set changes failed, error message: " + e.Message, true);
            }
        }

        public void SetAllEntity(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.SetAllEntity, entityInfo);

            try
            {
                dose.SetAll((Safir.Dob.Entity)entityInfo.Obj, entityInfo.getInstanceId(), entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity set all, '" + entityInfo.Obj.ToString() + "'", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception e)
            {
                OutputPanel.Instance.LogEvent("- Entity set all failed, error message: " + e.Message, true);
            }
        }


        public void DeleteEntity(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.DeleteEntity, entityInfo);

            try
            {
                Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId(entityInfo.Obj.GetTypeId(), entityInfo.getInstanceId());
                dose.Delete(entityId, entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity deleted, '" + entityInfo.Obj.ToString() + "'", true);
            }
            catch (Safir.Dob.AccessDeniedException e)
            {
                OutputPanel.Instance.LogEvent("- Entity delete failed, error message: " + e.Message, true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        public void InjectionSetChanges(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.InjectionSetChanges, entityInfo);

            try
            {
                Safir.Dob.ConnectionAspectInjector injector = new Safir.Dob.ConnectionAspectInjector(dose);
                injector.InjectChanges((Safir.Dob.Entity)entityInfo.Obj, entityInfo.getInstanceId(), entityInfo.Timestamp, entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity injection set changes, '" + entityInfo.Obj.ToString() + "'", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception e)
            {
                OutputPanel.Instance.LogEvent("- Entity injection set changes failed, error message: " + e.Message, true);
            }
        }


        public void InjectionInitialSet(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.InjectionInitialSet, entityInfo);

            try
            {
                Safir.Dob.ConnectionAspectInjector injector = new Safir.Dob.ConnectionAspectInjector(dose);
                injector.InitialSet((Safir.Dob.Entity)entityInfo.Obj, entityInfo.getInstanceId(), entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity injection initial set, '" + entityInfo.Obj.ToString() + "'", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception e)
            {
                OutputPanel.Instance.LogEvent("- Entity injection initial set, error message: " + e.Message, true);
            }
        }


        public void InjectionDelete(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.InjectionDelete, entityInfo);

            try
            {
                Safir.Dob.ConnectionAspectInjector injector = new Safir.Dob.ConnectionAspectInjector(dose);
                Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId(entityInfo.Obj.GetTypeId(), entityInfo.getInstanceId());
                injector.InjectDelete(entityId, entityInfo.Timestamp, entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity injection delete, '" + entityInfo.Obj.ToString() + "'", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception e)
            {
                OutputPanel.Instance.LogEvent("- Entity injection delete, error message: " + e.Message, true);
            }
        }


        public void CreateRequest(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.CreateRequest, entityInfo);

            try
            {
                if (entityInfo.RequestorDecides)
                {
                    int reqId = dose.CreateRequest((Safir.Dob.Entity)entityInfo.Obj, entityInfo.getInstanceId(), entityInfo.getHandlerId(), this);
                    OutputPanel.Instance.LogEvent("- Create request (requestor decides) sent for entity '" + entityInfo.Obj.ToString() + "', requestId " + reqId, true);
                }
                else
                {
                    int reqId = dose.CreateRequest((Safir.Dob.Entity)entityInfo.Obj, entityInfo.getHandlerId(), this);
                    OutputPanel.Instance.LogEvent("- Create request (handler decides) sent for entity '" + entityInfo.Obj.ToString() + "', requestId " + reqId, true);
                }
            }
            catch (Safir.Dob.OverflowException)
            {
                OutputPanel.Instance.LogEvent("- Overflow when sending create request '" + entityInfo.Obj.ToString() + "'", true);
            }
            catch (Safir.Dob.NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        public void UpdateRequest(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.UpdateRequest, entityInfo);

            try
            {
                int reqId = dose.UpdateRequest((Safir.Dob.Entity)entityInfo.Obj, entityInfo.getInstanceId(), this);
                OutputPanel.Instance.LogEvent("- Update request sent for entity '" + entityInfo.Obj.ToString() + "', requestId " + reqId, true);
            }
            catch (Safir.Dob.OverflowException)
            {
                OutputPanel.Instance.LogEvent("- Overflow when sending update request '" + entityInfo.Obj.ToString() + "'", true);
            }
            catch (Safir.Dob.NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        public void DeleteRequest(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.DeleteRequest, entityInfo);

            try
            {
                Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId(entityInfo.Obj.GetTypeId(), entityInfo.getInstanceId());
                int reqId = dose.DeleteRequest(entityId, this);
                OutputPanel.Instance.LogEvent("- Delete request sent for entity '" + entityInfo.Obj.ToString() + "', requestId " + reqId, true);
            }
            catch (Safir.Dob.OverflowException)
            {
                OutputPanel.Instance.LogEvent("- Overflow when sending delete request '" + entityInfo.Obj.ToString() + "'", true);
            }
            catch (Safir.Dob.NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }

        }

       // public void DeleteRequest(Safir.Dob.Typesystem.EntityId entityId)
        public void DeleteRequest(EntityIdInfo entityIdInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.DeleteRequest, entityIdInfo);

            try
            {
                int reqId = dose.DeleteRequest(entityIdInfo.entityIdSer.EntityId(), this);
                OutputPanel.Instance.LogEvent("- Delete request sent for entityId '" + entityIdInfo.entityIdSer.EntityId().ToString() + "', requestId " + reqId, true);
            }
            catch (Safir.Dob.OverflowException)
            {
                OutputPanel.Instance.LogEvent("- Overflow when sending delete request '" + entityIdInfo.entityIdSer.EntityId().ToString() + "'", true);
            }
            catch (Safir.Dob.NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }

        }

        public void ServiceRequest(ServiceHandlerInfo srvInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.ServiceRequest, srvInfo);

            try
            {
                dose.ServiceRequest((Safir.Dob.Service)srvInfo.Obj, srvInfo.getHandlerId(), this);
                OutputPanel.Instance.LogEvent("- Service request sent, '" + Safir.Dob.Typesystem.Operations.GetName(((Safir.Dob.Service)srvInfo.Obj).GetTypeId()) + "' to handler: " + srvInfo.getHandlerId().ToString(), true);
            }
            catch (Safir.Dob.OverflowException)
            {
                OutputPanel.Instance.LogEvent("- Overflow when sending service request '" + Safir.Dob.Typesystem.Operations.GetName(((Safir.Dob.Service)srvInfo.Obj).GetTypeId()) + "' to handler: " + srvInfo.getHandlerId().ToString(), true);
            }
            catch (Safir.Dob.NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        public void SendMessage(MessageInfo msgInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.SendMessage, msgInfo);

            try
            {
                dose.Send((Safir.Dob.Message)msgInfo.Obj, msgInfo.getChannelId(), this);
                OutputPanel.Instance.LogEvent("- Message sent, '" + Safir.Dob.Typesystem.Operations.GetName(((Safir.Dob.Message)msgInfo.Obj).GetTypeId()) + "' on channel " + msgInfo.getChannelId().ToString(), true);
            }
            catch (Safir.Dob.OverflowException)
            {
                OutputPanel.Instance.LogEvent("- Overflow when sending message '" + Safir.Dob.Typesystem.Operations.GetName(((Safir.Dob.Message)msgInfo.Obj).GetTypeId()) + "'", true);
            }
            catch (Safir.Dob.NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (Safir.Dob.Typesystem.SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        //Event handler. Called when a the selected tab-window is changed
        private void tabControl_selectedIndexChanged(object sender, EventArgs e)
        {
            UpdateMainMenu();
        }

        private void UpdateMainMenu()
        {
            if (tabControl.SelectedTab is XmlTabPage)
            {
                this.saveToolStripMenuItem.Enabled = true;
                this.findToolStripMenuItem.Enabled = true;
            }
            else if (tabControl.SelectedTab is ObjectEditTabPage)
            {
                this.saveToolStripMenuItem.Enabled = true;
                this.findToolStripMenuItem.Enabled = false;
            }
            else if (tabControl.SelectedTab is ScenarioTabPage)
            {
                this.saveToolStripMenuItem.Enabled = true;
                this.findToolStripMenuItem.Enabled = false;
            }
        }

        private void findToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (tabControl.SelectedTab is XmlTabPage)
            {
                XmlTabPage p=(XmlTabPage)tabControl.SelectedTab;
                new FindForm(p.RichEdit).Show();
            }
            else
            {
                UpdateMainMenu();
            }
        }


        private void opendoufileToolStripMenuItem_Click(object sender, EventArgs e)
        {
            OpenFileDialog of = new OpenFileDialog();
            of.AddExtension = true;
            of.Filter = "DOU files (*.dou)|*.dou|All files (*.*)|*.*";
            of.InitialDirectory = Environment.GetEnvironmentVariable(@"SAFIR_RUNTIME") +
                                    @"\data\text\dots\classes\";

            if (of.ShowDialog() == DialogResult.OK)
            {
                OpenDouFile(of.FileName);
            }
        }

        private void OpenDouFile(string fileName)
        {
            using (System.IO.TextReader reader = new System.IO.StreamReader(fileName))
            {
                string content = reader.ReadToEnd();
                reader.Close();
                AddTabPage(new XmlTabPage(content, fileName));
            }
        }

        private void openscenarioToolStripMenuItem_Click(object sender, EventArgs e)
        {
            OpenFileDialog of = new OpenFileDialog();
            of.AddExtension = true;
            of.Filter = "Scenario files (*.dos)|*.dos|All files (*.*)|*.*";
            if (of.ShowDialog() == DialogResult.OK)
            {
                ScenarioTabPage.Instance.Player.LoadScenario(of.FileName);
                AddTabPage(ScenarioTabPage.Instance);
            }
        }

        private void openserializedObjectToolStripMenuItem_Click(object sender, EventArgs e)
        {
            OpenFileDialog of = new OpenFileDialog();
            of.AddExtension = true;
            of.Filter = "XML files (*.xml)|*.xml|All files (*.*)|*.*";

            if (of.ShowDialog() == DialogResult.OK)
            {
                OpenSerializedObject(of.FileName);
            }
        }

        private void OpenSerializedObject(string fileName)
        {
            using (System.IO.TextReader reader = new System.IO.StreamReader(fileName))
            {
                string xml = reader.ReadToEnd();
                reader.Close();

                //First try to open as serialized object
                try
                {

                    Safir.Dob.Typesystem.Object o = Safir.Dob.Typesystem.Serialization.ToObject(xml);
                    if (o is Safir.Dob.Service)
                    {
                        ServiceHandlerInfo srvInfo = new ServiceHandlerInfo();
                        srvInfo.Obj = o;
                        AddTabPage(new ObjectEditTabPage(srvInfo));
                    }
                    else if (o is Safir.Dob.Entity)
                    {
                        EntityInfo entityInfo = new EntityInfo();
                        entityInfo.Obj = o;
                        AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                    else if (o is Safir.Dob.Message)
                    {
                        MessageInfo msgInfo = new MessageInfo();
                        msgInfo.Obj = o;
                        AddTabPage(new ObjectEditTabPage(msgInfo));
                    }
                    else if (o is Safir.Dob.Response)
                    {
                        ResponseInfo responseInfo = new ResponseInfo();
                        responseInfo.Obj = o;
                        AddTabPage(new ObjectEditTabPage(responseInfo));
                    }
                    else
                    {
                        ObjectInfo objInfo = new ObjectInfo();
                        objInfo.Obj = o;
                        AddTabPage(new ObjectEditTabPage(objInfo));
                    }

                    return;
                }
                catch
                {
                }

                //if not serialized object, open as plain xml
                try
                {
                    AddTabPage(new XmlTabPage(xml, fileName));
                    return;
                }
                catch
                {
                }

                MessageBox.Show("Failed to open file '"+fileName+"'.",
                        "Invalid xml", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (tabControl.SelectedTab is XmlTabPage)
            {
                XmlTabPage p = (XmlTabPage)tabControl.SelectedTab;
                SaveFileDialog sf=new SaveFileDialog();
                sf.AddExtension=false;
                sf.Filter = "XML files (*.xml;*.dou)|*.xml;*.dou|All files (*.*)|*.*" ;
                sf.FileName=p.FileName;
                if (sf.ShowDialog() == DialogResult.OK)
                {
                    p.Save(sf.FileName);
                }
            }
            else if (tabControl.SelectedTab is ObjectEditTabPage)
            {
                ObjectEditTabPage p = (ObjectEditTabPage)tabControl.SelectedTab;

                Safir.Dob.Typesystem.Object obj = p.ObjEditCtrl.GetObject();
                if (obj == null)
                    return;

                string name = Safir.Dob.Typesystem.Operations.GetName(obj.GetTypeId());
                name = name.Substring(name.LastIndexOf('.') + 1);
                //name += "_" + obj.InstanceNumber.ToString() + ".xml";
                name += ".xml";

                SaveFileDialog sf = new SaveFileDialog();
                sf.AddExtension = true;
                sf.Filter = "XML files (*.xml)|*.xml|All files (*.*)|*.*";
                sf.FileName = name;
                if (sf.ShowDialog() == DialogResult.OK)
                {
                    string xml = Safir.Dob.Typesystem.Serialization.ToXml(obj);
                    using (System.IO.TextWriter writer = new System.IO.StreamWriter(sf.FileName))
                    {
                        writer.Write(xml);
                        writer.Flush();
                        writer.Close();
                    }
                }
            }
            else if (tabControl.SelectedTab is ScenarioTabPage)
            {
                SaveFileDialog sf = new SaveFileDialog();
                sf.AddExtension = true;
                sf.Filter = "Scenario files (*.dos)|*.dos|All files (*.*)|*.*";
                if (sf.ShowDialog() == DialogResult.OK)
                {
                    ScenarioTabPage.Instance.Player.SaveScenario(sf.FileName);
                }
            }
        }


        private void connectWithContextToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ConnectWithContextForm form = new ConnectWithContextForm();
            form.ShowDialog();
        }


        private bool UpdateLiveData(ObjectInfo objInfo)
        {
            if (objInfo.Obj == null)
            {
                return false;
            }

            foreach (TabPage tp in tabControl.TabPages)
            {
                try
                {
                    if (tp is ObjectEditTabPage)
                    {
                        ObjectEditTabPage otp = (ObjectEditTabPage)tp;
                        if (otp.ObjEditCtrl.LiveData && otp.ObjEditCtrl.GetObject().GetTypeId() == objInfo.Obj.GetTypeId())
                        {
                            if ((objInfo is EntityInfo) && (otp.ObjEditCtrl.GetObjectInfo() is EntityInfo))
                            {
                                EntityInfo paramEntityInfo = (EntityInfo)objInfo;
                                EntityInfo otpEntityInfo = (EntityInfo)otp.ObjEditCtrl.GetObjectInfo();
                                // only update if instanceid is the same
                                if (paramEntityInfo.InstanceIdSer.InstanceId().Equals(otpEntityInfo.InstanceIdSer.InstanceId()))
                                {
                                    otp.ObjEditCtrl.UpdateData(objInfo);
                                    return true;
                                }
                            }
                        }
                    }
                }
                catch{}
            }
            return false;
        }

#if REMOVED_CODE
        private bool UpdateLiveData(ObjectInfo objectInfo, bool deleted)
        {
            foreach (TabPage tp in tabControl.TabPages)
            {
                try
                {
                    if (tp is ObjectEditTabPage)
                    {
                        ObjectEditTabPage otp = (ObjectEditTabPage)tp;
                        if (otp.ObjEditCtrl.LiveData && otp.ObjEditCtrl.GetObject().GetTypeId() == objectInfo.Obj.GetTypeId()) //&& otp.ObjEditCtrl.ObjId.Instance == oid.Instance)
                        {
                            otp.ObjEditCtrl.DeletedData(deleted);
                            return true;
                        }
                    }
                }
                catch { }
            }
            return false;
        }
#endif
        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.Application.Exit();
        }


        #region EntitySubscriber Members

        public void OnDeletedEntity(Safir.Dob.EntityProxy entityProxy, bool deletedByOwner)
        {
            EntityInfo entityInfo = new EntityInfo();
            entityInfo.setInstanceId(entityProxy.InstanceId);
            entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);

            OutputPanel.Instance.LogEvent("- Received deleted entity: '" + entityProxy.EntityId.ToString()
               + "' for handler '" + entityProxy.OwnerWithStringRepresentation.ToString() + "'", true);
            ExplorerPanel.Instance.DeleteObject(entityProxy.EntityId);
            //InboxPanel.Instance.AddResponse(entityInfo, "Deleted entity");
            InboxPanel.Instance.AddNonDisplayableResponse(entityProxy.EntityId.TypeId, "Deleted entity");

            UpdateLiveData(entityInfo);
        }

        public void OnNewEntity(Safir.Dob.EntityProxy entityProxy)
        {
            EntityInfo entityInfo = new EntityInfo();
            entityInfo.setInstanceId(entityProxy.InstanceId);
            entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);
            entityInfo.Obj = entityProxy.Entity;

            entityInfo.Blobsize = Safir.Dob.Typesystem.BlobOperations.GetSize(entityProxy.Blob);

            OutputPanel.Instance.LogEvent("- Received new entity: '" + entityProxy.EntityId.ToString()
                + "' for handler '" + entityProxy.OwnerWithStringRepresentation.ToString() + "'"
                + " owner '" + entityProxy.OwnerConnectionInfo.ConnectionName.Val + "'", true);
            ExplorerPanel.Instance.AddObject(entityProxy.EntityId);
            InboxPanel.Instance.AddResponse(entityInfo, "New entity");

            UpdateLiveData(entityInfo);
        }

        public void OnUpdatedEntity(Safir.Dob.EntityProxy entityProxy)
        {
            EntityInfo entityInfo = new EntityInfo();
            entityInfo.setInstanceId(entityProxy.InstanceId);
            entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);
            entityInfo.Obj = entityProxy.Entity;

            entityInfo.Blobsize = Safir.Dob.Typesystem.BlobOperations.GetSize(entityProxy.Blob);

            OutputPanel.Instance.LogEvent("- Received updated entity: '" + entityProxy.EntityId.ToString()
               + "' for handler '" + entityProxy.OwnerWithStringRepresentation.ToString() + "'"
               + " owner '" + entityProxy.OwnerConnectionInfo.ConnectionName.Val + "'", true);
            InboxPanel.Instance.AddResponse(entityInfo, "Updated entity");

            UpdateLiveData(entityInfo);
        }

        #endregion

        #region CompletedRegistrationBase Members

        public void OnCompletedRegistration(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            string name = Safir.Dob.Typesystem.Operations.GetName(typeId);
            ExplorerPanel.Instance.SetRegistered(typeId);
            OutputPanel.Instance.LogEvent("- Registration completed for typeId: " + name + " with handlerid '" + handlerId.ToString() + "'", true);
        }

        #endregion

        #region EntityInjectionBase Members

        public void OnInitialInjectionsDone(long typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            string name = Safir.Dob.Typesystem.Operations.GetName(typeId);
            OutputPanel.Instance.LogEvent("- Initial injections done for typeId: " + name + " with handlerid '" + handlerId.ToString() + "'", true);
        }

        public void OnInjectedDeletedEntity(Safir.Dob.InjectedEntityProxy injectedEntityProxy)
        {
            OutputPanel.Instance.LogEvent("- Injected deleted entity. Entity id: '" +  injectedEntityProxy.EntityId.ToString() + "'", true);
        }

        public void OnInjectedNewEntity(Safir.Dob.InjectedEntityProxy injectedEntityProxy)
        {
            OutputPanel.Instance.LogEvent("- Injected new entity. Entity id: '" + injectedEntityProxy.EntityId.ToString() + "'", true);
        }

        public void OnInjectedUpdatedEntity(Safir.Dob.InjectedEntityProxy injectedEntityProxy)
        {
            OutputPanel.Instance.LogEvent("- Injected updated entity. Entity id: '" + injectedEntityProxy.EntityId.ToString() + "'", true);
        }

        #endregion

        private void timestampToolStripMenuItem_Click(object sender, EventArgs e)
        {
            new TimestampCalculatorForm().ShowDialog();
        }

        private void typeIdToolStripMenuItem_Click(object sender, EventArgs e)
        {
            new TypeIdCalculatorForm().ShowDialog();
        }

        private void runGarbageCollectorToolStripMenuItem_Click(object sender, EventArgs e)
        {
            System.GC.Collect();
        }
    }
}
