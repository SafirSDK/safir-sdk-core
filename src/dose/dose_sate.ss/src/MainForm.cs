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

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Threading;
using System.Windows.Forms;
using Safir.Dob;
using Safir.Dob.Typesystem;
using Safir.Dob.Typesystem.Internal;
using Exception = System.Exception;
using Message = Safir.Dob.Message;
using OverflowException = Safir.Dob.OverflowException;

namespace Sate
{
    internal delegate void CallDispatchDelegate();

    /// <summary>
    ///     This is the main window in SATE.
    /// </summary>
    public class MainForm : Form,
        Dispatcher,
        StopHandler,
        MessageSender,
        MessageSubscriber,
        Requestor,
        ServiceHandler,
        ServiceHandlerPending,
        RegistrationSubscriber,
        EntitySubscriber,
        EntityHandler,
        EntityHandlerInjection,
        EntityHandlerPending
    {
        private static MainForm instance;

        //DOB

        private readonly AboutForm aboutForm = new AboutForm();

        //Must prevent the callback delegate from garbage collection. Thats why its declared here
        private readonly CallDispatchDelegate callDispatch;
        private readonly PanelLabelControl fillPanelLabel = new PanelLabelControl("");
        private ToolStripMenuItem aboutSATEToolStripMenuItem;
        private Panel bottomFillPanel;
        private Panel bottomPanel;
        private Panel bottomRightPanel;
        private Splitter bottomRightSplitter;
        private Splitter bottomSplitter;
        private ToolStripMenuItem calculatorsToolStripMenuItem;
        private ToolStripMenuItem classExplorerToolStripMenuItem;
        private IContainer components;

        private int connectContext;
        private ToolStripMenuItem connectionToolStripMenuItem;
        private ToolStripMenuItem connectToolStripMenuItem;
        private ToolStripMenuItem connectWithContextToolStripMenuItem;
        private ToolStripMenuItem disconnectToolStripMenuItem;
        private ToolStripMenuItem exitToolStripMenuItem;
        private Panel fillfillpanel;
        private Panel fillPanel;
        private ToolStripMenuItem findToolStripMenuItem;

        public List<long> handlerDecidesTypeIdList = new List<long>();
        private ToolStripMenuItem helpToolStripMenuItem;
        private ToolStripMenuItem helpToolStripMenuItem1;
        private ImageList imageList;
        private ToolStripMenuItem inboxToolStripMenuItem;
        private Panel leftPanel;
        private Splitter leftSplitter;
        private MenuStrip menuStrip1;
        private ToolStripMenuItem opendoufileToolStripMenuItem;
        private ToolStripMenuItem openscenarioToolStripMenuItem;
        private ToolStripMenuItem openserializedObjectToolStripMenuItem;
        private ToolStripMenuItem openToolStripMenuItem;
        private ToolStripMenuItem outputToolStripMenuItem;
        public List<long> requestorDecidesTypeIdList = new List<long>();
        private ToolStripMenuItem rescentFilesToolStripMenuItem;
        private ToolStripMenuItem runGarbageCollectorToolStripMenuItem;
        private ToolStripMenuItem saveToolStripMenuItem;
        private ToolStripMenuItem scenarioPlayrecordToolStripMenuItem;
        private ToolStripMenuItem settingsToolStripMenuItem;
        private StatusBar statusBar;
        private StatusBarPanel connectionStatusPanel;
        private StatusBarPanel safirInstancePanel;
        private TabControl tabControl;
        private ToolStripMenuItem timestampToolStripMenuItem;
        private ToolStripMenuItem toolsToolStripMenuItem;
        private ToolStripSeparator toolStripMenuItem1;
        private ToolStripSeparator toolStripMenuItem2;
        private ToolStripSeparator toolStripMenuItem3;
        private ToolStripSeparator toolStripMenuItem4;
        private ToolStripSeparator toolStripSeparator1;
        private ToolStripMenuItem typeIdToolStripMenuItem;
        private ToolStripMenuItem viewToolStripMenuItem;
        private ToolStripMenuItem externalApplicationsToolStripMenuItem;
        private readonly Connection _dose = new Connection();

        // Handle dispatch in the case the tempo is to high to keep up with and at the same time maintain a responsive GUI.
        private System.Windows.Forms.Timer dispatchTimer = new System.Windows.Forms.Timer();
        private ulong dispatchCounter = 0;

        private MainForm()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            dispatchTimer.Interval = 150;
            dispatchTimer.Tick += OnDispatchTimer;
            dispatchTimer.Stop();

            fillPanelLabel.BackColor = SystemColors.Control;
            fillPanelLabel.ForeColor = Color.Black;
            fillPanelLabel.CloseEvent += fillPanelLabel_CloseEvent;
            fillPanelLabel.ContextMenu = new ContextMenu(new[] {new MenuItem("Close all tabs", OnCloseAllTabs)});
            fillfillpanel.Controls.Add(fillPanelLabel);
            fillPanel.Visible = false;

            ConnectedEvent += OnConnected;

            callDispatch = Dose.Dispatch;


            AllowDrop = true;
            DragEnter += OnDragEnter;
            DragDrop += OnDragDrop;


            //-------------------------------------------------------------
            //First of all read the parameters saved in settings files
            //-------------------------------------------------------------
            Settings.Load();
            ExternalApplicationSettings.Load();

            //Create Auto Reply
            if (Settings.Sate.XmlReplyObject != null) //try to load saved response
            {
                Settings.Sate.AutoResponse = new Response();
                var loadedOk = true;
                try
                {
                    Settings.Sate.AutoResponse = (Response) Serialization.ToObject(Settings.Sate.XmlReplyObject);
                }
                catch
                {
                    loadedOk = false;
                }

                if (!loadedOk)
                {
                    Settings.Sate.AutoResponse = new SuccessResponse();
                    Settings.Sate.XmlReplyObject = null;
                    Settings.Save();
                }
            }
            else //no stored response, use a simple success-reply
            {
                Settings.Sate.AutoResponse = new SuccessResponse();
            }
            //------------ End Read Parameters -------------


            leftPanel.Dock = DockStyle.Left;

            ExplorerPanel.Instance.Dock = DockStyle.Fill;
            leftPanel.Controls.Add(ExplorerPanel.Instance);

            InboxPanel.Instance.Dock = DockStyle.Fill;

            bottomFillPanel.Controls.Add(InboxPanel.Instance);

            OutputPanel.Instance.Dock = DockStyle.Fill;
            bottomRightPanel.Controls.Add(OutputPanel.Instance);
        }

        //Get the only instance of MainForm (singleton)
        public static MainForm Instance
        {
            get
            {
                if (instance == null)
                    instance = new MainForm();
                return instance;
            }
        }

        //Get the connection to the DOB
        public Connection Dose
        {
            get { return _dose; }
        }


        public bool IsConnected { get; private set; }

        //-----------------------------------------------------
        // Implemented Consumers and interfaces
        //-----------------------------------------------------

        #region Dispatcher Members

        public void OnDoDispatch()
        {
            dispatchCounter++;
            try
            {
                // if the dispatch timer is already running, wait for it to elapse and do nothing
                if (!Settings.Sate.NoDispatch && !IsDisposed && !dispatchTimer.Enabled)
                {
                    Invoke((MethodInvoker) delegate {dispatchTimer.Start();});
                }
            }
            catch (ObjectDisposedException e)
            {
                Console.WriteLine("Caught exception in Dispatch: " + e);
            }
        }

        public void OnDispatchTimer(object myObject, EventArgs myEventArgs)
        {
            try
            {
                var prevCount = dispatchCounter;
                if (!Settings.Sate.NoDispatch && !IsDisposed)
                {
                    Dose.Dispatch();
                }

                dispatchTimer.Stop();
                if (prevCount < dispatchCounter)
                {
                    // If we get here, dispatchThread has made a callback while the timer was already running. In that case we restart timer.
                    // If dispatchThread already started the timer again after the Stop above, then this call to Start has no effect.
                    dispatchTimer.Start();
                }
            }
            catch (LowMemoryException)
            {
                MessageBox.Show("Operation failed due to low shared memory", "Low Memory",
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error);
            }
        }

        #endregion

        #region MessageSender Members

        public void OnNotMessageOverflow()
        {
            OutputPanel.Instance.LogEvent("- Notification: Not overflow in message queue", true);
        }

        #endregion

        #region MessageSubscriber Members

        public void OnMessage(MessageProxy messageProxy)
        {
            var name = Operations.GetName(messageProxy.TypeId);
            OutputPanel.Instance.LogEvent("- Received message '" + name + "' on channel " + messageProxy.ChannelId, true);

            var msgInfo = new MessageInfo();
            msgInfo.setChannelId(messageProxy.ChannelId);
            msgInfo.Obj = messageProxy.Message;

            //int blobsize = Safir.Dob.Typesystem.BlobOperations.GetSize(messageProxy.Blob);

            InboxPanel.Instance.AddResponse(msgInfo, "Message received on channel " + messageProxy.ChannelId);

            UpdateLiveData(msgInfo);
        }

        #endregion

        #region ServiceProvider Members

        public void OnServiceRequest(ServiceRequestProxy serviceRequestProxy, ResponseSender rs)
        {
            var name = Operations.GetName(serviceRequestProxy.TypeId);
            OutputPanel.Instance.LogEvent(
                "- Received service request '" + name + "' for handler " + serviceRequestProxy.ReceivingHandlerId, true);

            var serviceHandlerInfo = new ServiceHandlerInfo();
            serviceHandlerInfo.setHandlerId(serviceRequestProxy.ReceivingHandlerId);
            serviceHandlerInfo.Obj = serviceRequestProxy.Request;
            serviceHandlerInfo.Blobsize = BlobOperations.GetSize(serviceRequestProxy.Blob);

            UpdateLiveData(serviceHandlerInfo);

            InboxPanel.Instance.AddResponse(serviceHandlerInfo,
                "Service request for handler " + serviceRequestProxy.ReceivingHandlerId);

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

        public void OnRevokedRegistration(long typeId, HandlerId handlerId)
        {
            var regInfo = new RegInfo();
            regInfo.typeId = typeId;
            regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
            ExplorerPanel.Instance.RevokedRegistration(regInfo);

            // remove typeids
            requestorDecidesTypeIdList.Remove(typeId);
            handlerDecidesTypeIdList.Remove(typeId);
        }

        #endregion

        #region CompletedRegistrationBase Members

        public void OnCompletedRegistration(long typeId, HandlerId handlerId)
        {
            var name = Operations.GetName(typeId);
            ExplorerPanel.Instance.SetRegistered(typeId);
            OutputPanel.Instance.LogEvent(
                "- Registration completed for typeId: " + name + " with handlerid '" + handlerId + "'", true);
        }

        #endregion

        #region ConnectionOwner Members

        public void OnStopOrder()
        {
            dispatchTimer.Stop();
            OutputPanel.Instance.LogEvent("- Received stop order, disconnecting...", true);
            System.Windows.Forms.Application.Exit();
        }

        #endregion

        private event OnConnectedDelegate ConnectedEvent;

        protected override void OnFormClosing(FormClosingEventArgs e)
        {
            dispatchTimer.Stop();
            base.OnFormClosing(e);
            if (IsConnected)
            {
                Disconnect();
            }
            else if (IsMono()) 
            {
                // Mono and not connected, then we have a running background thread that is trying to connect.
                // It seems that Mono is not able to kill the background thread as it is hanging stuck in unmanaged code.
                // This will at least kill the application and all of its threads.
                System.Diagnostics.Process.GetCurrentProcess().Kill();
            }
        }

        //Event handler for close all tabs context menu
        private void OnCloseAllTabs(object sender, EventArgs e)
        {
            foreach (TabPage tp in tabControl.TabPages)
            {
                tp.Dispose();
            }

            tabControl.TabPages.Clear();
            fillPanel.Visible = false;
        }

        //Event handler for drag and drop of files on SATE.
        private void OnDragDrop(object sender, DragEventArgs e)
        {
            var files = (string[]) e.Data.GetData(DataFormats.FileDrop);
            foreach (var f in files)
            {
                var str = f.ToLower();
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
        private void OnDragEnter(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                e.Effect = DragDropEffects.All;
            }
        }

        //Event handler for closing a tab
        // Selects the tab to the right of the closing tab (if possible)
        private void fillPanelLabel_CloseEvent(object sender, EventArgs e)
        {
            // save value
            var selectedIndex = tabControl.SelectedIndex;
            if (selectedIndex >= 0)
            {
                if (IsMono())
                {
                    menuStrip1.Focus();
                    //Mono bug workaround. Mono crashes if a control on the tabPage has focus when the tab is deleted.
                }

                var tp = tabControl.TabPages[selectedIndex];

                tabControl.TabPages.RemoveAt(selectedIndex);
                tp.Dispose();

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
                fillPanel.Visible = false;
            }
        }

        //Shows a new tab
        public void AddTabPage(TabPage tp)
        {
            fillPanel.Visible = true;

            try
            {
                tabControl.TabPages.Add(tp);
                tabControl.SelectedIndex = tabControl.TabPages.Count - 1;
            }
            catch (Win32Exception e)
            {
                var msg = "Unable to display more windows in SATE! Try closing other tabs first! - Exception: " +
                          e.Message;
                OutputPanel.Instance.LogEvent(msg, true);
                tp.Dispose();
                MessageBox.Show(
                    "A new page can not be created until you close one first!\nSATE has run out of resources.");
            }
        }

        public void KickDispatch()
        {
            GC.Collect();
            // Invoke will force a thread switch!
            Invoke(callDispatch);
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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
            this.imageList = new System.Windows.Forms.ImageList(this.components);
            this.statusBar = new System.Windows.Forms.StatusBar();
            this.connectionStatusPanel = new StatusBarPanel();
            this.safirInstancePanel = new StatusBarPanel();
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
            this.externalApplicationsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
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
            this.connectionStatusPanel.AutoSize = StatusBarPanelAutoSize.Spring;
            this.connectionStatusPanel.Text = "Not connected!";
            this.connectionStatusPanel.BorderStyle = StatusBarPanelBorderStyle.None;
            this.connectionStatusPanel.Alignment = HorizontalAlignment.Left;
            this.safirInstancePanel.BorderStyle = StatusBarPanelBorderStyle.None;
            this.safirInstancePanel.Alignment = HorizontalAlignment.Right;
            this.safirInstancePanel.Width = 150;
            this.safirInstancePanel.Text = "Safir instance: " + Safir.Dob.ConnectionAspectMisc.GetSafirInstance().ToString();
            this.statusBar.Location = new System.Drawing.Point(0, 744);
            this.statusBar.Name = "statusBar";
            this.statusBar.Size = new System.Drawing.Size(1160, 22);
            this.statusBar.TabIndex = 3;
            this.statusBar.ShowPanels = true;
            this.statusBar.Panels.AddRange(new [] { this.connectionStatusPanel, this.safirInstancePanel });
            // 
            // bottomSplitter
            // 
            this.bottomSplitter.BackColor = System.Drawing.SystemColors.Control;
            this.bottomSplitter.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.bottomSplitter.Location = new System.Drawing.Point(249, 621);
            this.bottomSplitter.MinSize = 50;
            this.bottomSplitter.Name = "bottomSplitter";
            this.bottomSplitter.Size = new System.Drawing.Size(911, 3);
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
            this.bottomPanel.Location = new System.Drawing.Point(249, 624);
            this.bottomPanel.Name = "bottomPanel";
            this.bottomPanel.Size = new System.Drawing.Size(911, 120);
            this.bottomPanel.TabIndex = 2;
            // 
            // bottomFillPanel
            // 
            this.bottomFillPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.bottomFillPanel.Location = new System.Drawing.Point(0, 0);
            this.bottomFillPanel.Name = "bottomFillPanel";
            this.bottomFillPanel.Size = new System.Drawing.Size(515, 120);
            this.bottomFillPanel.TabIndex = 2;
            // 
            // bottomRightSplitter
            // 
            this.bottomRightSplitter.Dock = System.Windows.Forms.DockStyle.Right;
            this.bottomRightSplitter.Location = new System.Drawing.Point(515, 0);
            this.bottomRightSplitter.Name = "bottomRightSplitter";
            this.bottomRightSplitter.Size = new System.Drawing.Size(3, 120);
            this.bottomRightSplitter.TabIndex = 1;
            this.bottomRightSplitter.TabStop = false;
            // 
            // bottomRightPanel
            // 
            this.bottomRightPanel.Dock = System.Windows.Forms.DockStyle.Right;
            this.bottomRightPanel.Location = new System.Drawing.Point(518, 0);
            this.bottomRightPanel.Name = "bottomRightPanel";
            this.bottomRightPanel.Size = new System.Drawing.Size(393, 120);
            this.bottomRightPanel.TabIndex = 0;
            // 
            // leftSplitter
            // 
            this.leftSplitter.BackColor = System.Drawing.SystemColors.Control;
            this.leftSplitter.Location = new System.Drawing.Point(246, 24);
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
            this.leftPanel.Size = new System.Drawing.Size(246, 720);
            this.leftPanel.TabIndex = 0;
            // 
            // fillPanel
            // 
            this.fillPanel.Controls.Add(this.fillfillpanel);
            this.fillPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.fillPanel.Location = new System.Drawing.Point(249, 24);
            this.fillPanel.Name = "fillPanel";
            this.fillPanel.Size = new System.Drawing.Size(911, 597);
            this.fillPanel.TabIndex = 4;
            // 
            // fillfillpanel
            // 
            this.fillfillpanel.Controls.Add(this.tabControl);
            this.fillfillpanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.fillfillpanel.Location = new System.Drawing.Point(0, 0);
            this.fillfillpanel.Name = "fillfillpanel";
            this.fillfillpanel.Size = new System.Drawing.Size(911, 597);
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
            this.tabControl.Size = new System.Drawing.Size(911, 597);
            this.tabControl.TabIndex = 0;
            this.tabControl.SelectedIndexChanged += new System.EventHandler(this.tabControl_selectedIndexChanged);
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.connectionToolStripMenuItem,
            this.viewToolStripMenuItem,
            this.toolsToolStripMenuItem,
            this.helpToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(1160, 24);
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
            this.connectionToolStripMenuItem.Size = new System.Drawing.Size(81, 20);
            this.connectionToolStripMenuItem.Text = "Connection";
            // 
            // connectToolStripMenuItem
            // 
            this.connectToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("connectToolStripMenuItem.Image")));
            this.connectToolStripMenuItem.Name = "connectToolStripMenuItem";
            this.connectToolStripMenuItem.Size = new System.Drawing.Size(197, 22);
            this.connectToolStripMenuItem.Text = "Connect";
            this.connectToolStripMenuItem.Click += new System.EventHandler(this.connectmenuItem_Click);
            // 
            // connectWithContextToolStripMenuItem
            // 
            this.connectWithContextToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("connectWithContextToolStripMenuItem.Image")));
            this.connectWithContextToolStripMenuItem.Name = "connectWithContextToolStripMenuItem";
            this.connectWithContextToolStripMenuItem.Size = new System.Drawing.Size(197, 22);
            this.connectWithContextToolStripMenuItem.Text = "Connect with context...";
            this.connectWithContextToolStripMenuItem.Click += new System.EventHandler(this.connectWithContextToolStripMenuItem_Click);
            // 
            // disconnectToolStripMenuItem
            // 
            this.disconnectToolStripMenuItem.Enabled = false;
            this.disconnectToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("disconnectToolStripMenuItem.Image")));
            this.disconnectToolStripMenuItem.Name = "disconnectToolStripMenuItem";
            this.disconnectToolStripMenuItem.Size = new System.Drawing.Size(197, 22);
            this.disconnectToolStripMenuItem.Text = "Disconnect";
            this.disconnectToolStripMenuItem.Click += new System.EventHandler(this.disconnectmenuItem_Click);
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(194, 6);
            // 
            // openToolStripMenuItem
            // 
            this.openToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.opendoufileToolStripMenuItem,
            this.openscenarioToolStripMenuItem,
            this.openserializedObjectToolStripMenuItem});
            this.openToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("openToolStripMenuItem.Image")));
            this.openToolStripMenuItem.Name = "openToolStripMenuItem";
            this.openToolStripMenuItem.Size = new System.Drawing.Size(197, 22);
            this.openToolStripMenuItem.Text = "Open";
            // 
            // opendoufileToolStripMenuItem
            // 
            this.opendoufileToolStripMenuItem.Name = "opendoufileToolStripMenuItem";
            this.opendoufileToolStripMenuItem.Size = new System.Drawing.Size(168, 22);
            this.opendoufileToolStripMenuItem.Text = "Dou file...";
            this.opendoufileToolStripMenuItem.Click += new System.EventHandler(this.opendoufileToolStripMenuItem_Click);
            // 
            // openscenarioToolStripMenuItem
            // 
            this.openscenarioToolStripMenuItem.Name = "openscenarioToolStripMenuItem";
            this.openscenarioToolStripMenuItem.Size = new System.Drawing.Size(168, 22);
            this.openscenarioToolStripMenuItem.Text = "Scenario file...";
            this.openscenarioToolStripMenuItem.Click += new System.EventHandler(this.openscenarioToolStripMenuItem_Click);
            // 
            // openserializedObjectToolStripMenuItem
            // 
            this.openserializedObjectToolStripMenuItem.Name = "openserializedObjectToolStripMenuItem";
            this.openserializedObjectToolStripMenuItem.Size = new System.Drawing.Size(168, 22);
            this.openserializedObjectToolStripMenuItem.Text = "Serialized object...";
            this.openserializedObjectToolStripMenuItem.Click += new System.EventHandler(this.openserializedObjectToolStripMenuItem_Click);
            // 
            // saveToolStripMenuItem
            // 
            this.saveToolStripMenuItem.Enabled = false;
            this.saveToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("saveToolStripMenuItem.Image")));
            this.saveToolStripMenuItem.Name = "saveToolStripMenuItem";
            this.saveToolStripMenuItem.Size = new System.Drawing.Size(197, 22);
            this.saveToolStripMenuItem.Text = "Save...";
            this.saveToolStripMenuItem.Click += new System.EventHandler(this.saveToolStripMenuItem_Click);
            // 
            // findToolStripMenuItem
            // 
            this.findToolStripMenuItem.Enabled = false;
            this.findToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("findToolStripMenuItem.Image")));
            this.findToolStripMenuItem.Name = "findToolStripMenuItem";
            this.findToolStripMenuItem.Size = new System.Drawing.Size(197, 22);
            this.findToolStripMenuItem.Text = "Find...";
            this.findToolStripMenuItem.Click += new System.EventHandler(this.findToolStripMenuItem_Click);
            // 
            // rescentFilesToolStripMenuItem
            // 
            this.rescentFilesToolStripMenuItem.Name = "rescentFilesToolStripMenuItem";
            this.rescentFilesToolStripMenuItem.Size = new System.Drawing.Size(197, 22);
            this.rescentFilesToolStripMenuItem.Text = "Recent files";
            // 
            // toolStripMenuItem4
            // 
            this.toolStripMenuItem4.Name = "toolStripMenuItem4";
            this.toolStripMenuItem4.Size = new System.Drawing.Size(194, 6);
            // 
            // exitToolStripMenuItem
            // 
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(197, 22);
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
            this.viewToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
            this.viewToolStripMenuItem.Text = "View";
            // 
            // classExplorerToolStripMenuItem
            // 
            this.classExplorerToolStripMenuItem.Checked = true;
            this.classExplorerToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.classExplorerToolStripMenuItem.Name = "classExplorerToolStripMenuItem";
            this.classExplorerToolStripMenuItem.Size = new System.Drawing.Size(147, 22);
            this.classExplorerToolStripMenuItem.Text = "Class Explorer";
            this.classExplorerToolStripMenuItem.Click += new System.EventHandler(this.classExplorermenuItem_Click);
            // 
            // inboxToolStripMenuItem
            // 
            this.inboxToolStripMenuItem.Checked = true;
            this.inboxToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.inboxToolStripMenuItem.Name = "inboxToolStripMenuItem";
            this.inboxToolStripMenuItem.Size = new System.Drawing.Size(147, 22);
            this.inboxToolStripMenuItem.Text = "Inbox";
            this.inboxToolStripMenuItem.Click += new System.EventHandler(this.subrespmenuItem_Click);
            // 
            // outputToolStripMenuItem
            // 
            this.outputToolStripMenuItem.Checked = true;
            this.outputToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.outputToolStripMenuItem.Name = "outputToolStripMenuItem";
            this.outputToolStripMenuItem.Size = new System.Drawing.Size(147, 22);
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
            this.externalApplicationsToolStripMenuItem,
            this.settingsToolStripMenuItem});
            this.toolsToolStripMenuItem.Name = "toolsToolStripMenuItem";
            this.toolsToolStripMenuItem.Size = new System.Drawing.Size(46, 20);
            this.toolsToolStripMenuItem.Text = "Tools";
            // 
            // calculatorsToolStripMenuItem
            // 
            this.calculatorsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.typeIdToolStripMenuItem,
            this.timestampToolStripMenuItem});
            // this.calculatorsToolStripMenuItem.Image = global::Sate.Resources.TYPEIDCALC;
            this.calculatorsToolStripMenuItem.Name = "calculatorsToolStripMenuItem";
            this.calculatorsToolStripMenuItem.Size = new System.Drawing.Size(194, 22);
            this.calculatorsToolStripMenuItem.Text = "Calculators";
            // 
            // typeIdToolStripMenuItem
            // 
            this.typeIdToolStripMenuItem.Name = "typeIdToolStripMenuItem";
            this.typeIdToolStripMenuItem.Size = new System.Drawing.Size(133, 22);
            this.typeIdToolStripMenuItem.Text = "TypeId";
            this.typeIdToolStripMenuItem.Click += new System.EventHandler(this.typeIdToolStripMenuItem_Click);
            // 
            // timestampToolStripMenuItem
            // 
            this.timestampToolStripMenuItem.Name = "timestampToolStripMenuItem";
            this.timestampToolStripMenuItem.Size = new System.Drawing.Size(133, 22);
            this.timestampToolStripMenuItem.Text = "Timestamp";
            this.timestampToolStripMenuItem.Click += new System.EventHandler(this.timestampToolStripMenuItem_Click);
            // 
            // scenarioPlayrecordToolStripMenuItem
            // 
            this.scenarioPlayrecordToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("scenarioPlayrecordToolStripMenuItem.Image")));
            this.scenarioPlayrecordToolStripMenuItem.Name = "scenarioPlayrecordToolStripMenuItem";
            this.scenarioPlayrecordToolStripMenuItem.Size = new System.Drawing.Size(194, 22);
            this.scenarioPlayrecordToolStripMenuItem.Text = "Scenario play/record...";
            this.scenarioPlayrecordToolStripMenuItem.Click += new System.EventHandler(this.scenariomenuItem_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(191, 6);
            // 
            // runGarbageCollectorToolStripMenuItem
            // 
            this.runGarbageCollectorToolStripMenuItem.Name = "runGarbageCollectorToolStripMenuItem";
            this.runGarbageCollectorToolStripMenuItem.Size = new System.Drawing.Size(194, 22);
            this.runGarbageCollectorToolStripMenuItem.Text = "Run Garbage Collector";
            this.runGarbageCollectorToolStripMenuItem.Click += new System.EventHandler(this.runGarbageCollectorToolStripMenuItem_Click);
            // 
            // toolStripMenuItem3
            // 
            this.toolStripMenuItem3.Name = "toolStripMenuItem3";
            this.toolStripMenuItem3.Size = new System.Drawing.Size(191, 6);
            // 
            // externalApplicationsToolStripMenuItem
            // 
            this.externalApplicationsToolStripMenuItem.Name = "externalApplicationsToolStripMenuItem";
            this.externalApplicationsToolStripMenuItem.Size = new System.Drawing.Size(194, 22);
            this.externalApplicationsToolStripMenuItem.Text = "External Applications...";
            this.externalApplicationsToolStripMenuItem.Click += new System.EventHandler(this.externalApplicationsToolStripMenuItem_Click);
            // 
            // settingsToolStripMenuItem
            // 
            this.settingsToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("settingsToolStripMenuItem.Image")));
            this.settingsToolStripMenuItem.Name = "settingsToolStripMenuItem";
            this.settingsToolStripMenuItem.Size = new System.Drawing.Size(194, 22);
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
            this.helpToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
            this.helpToolStripMenuItem.Text = "Help";
            // 
            // helpToolStripMenuItem1
            // 
            this.helpToolStripMenuItem1.Image = ((System.Drawing.Image)(resources.GetObject("helpToolStripMenuItem1.Image")));
            this.helpToolStripMenuItem1.Name = "helpToolStripMenuItem1";
            this.helpToolStripMenuItem1.Size = new System.Drawing.Size(144, 22);
            this.helpToolStripMenuItem1.Text = "Help...";
            // 
            // toolStripMenuItem2
            // 
            this.toolStripMenuItem2.Name = "toolStripMenuItem2";
            this.toolStripMenuItem2.Size = new System.Drawing.Size(141, 6);
            // 
            // aboutSATEToolStripMenuItem
            // 
            // this.aboutSATEToolStripMenuItem.Image = global::Sate.Resources.sate_logo;
            this.aboutSATEToolStripMenuItem.Name = "aboutSATEToolStripMenuItem";
            this.aboutSATEToolStripMenuItem.Size = new System.Drawing.Size(144, 22);
            this.aboutSATEToolStripMenuItem.Text = "About SATE...";
            this.aboutSATEToolStripMenuItem.Click += new System.EventHandler(this.aboutmenuItem_Click);
            // 
            // MainForm
            // 
            this.BackColor = System.Drawing.SystemColors.ControlDark;
            this.ClientSize = new System.Drawing.Size(1160, 766);
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

        public static bool IsMono()
        {
            return Type.GetType("Mono.Runtime") != null;
        }

        /// <summary>
        ///     The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main()
        {
            System.Windows.Forms.Application.ThreadException += new ThreadExceptionEventHandler(Application_ThreadException);
            System.Windows.Forms.Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException);
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(CurrentDomain_UnhandledException);
            System.Windows.Forms.Application.Run(Instance);
        }

        static void Application_ThreadException(object sender, ThreadExceptionEventArgs e)
        {
            MessageBox.Show(e.Exception.Message, "Unhandled Thread Exception");
        }

        static void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            MessageBox.Show((e.ExceptionObject as Exception).Message, "Unhandled UI Exception");
        }

        //This is executed when the MainForm is first loaded
        private void MainForm_Load(object sender, EventArgs e)
        {
            OutputPanel.Instance.LogEvent("- SATE started.", true);

            //---------------------------------------------
            //Fill class explorer with existing data types
            //---------------------------------------------

            ExplorerPanel.Instance.LoadClassHierarchy();

            //------------------------------------------
            //Connect to DOSE
            //------------------------------------------
            if (Settings.Sate.ConnectAtStartUp)
            {
                Connect(true, 0);
            }

            AutoOrganize(); //organizes the windows (panels) in the main form
        }

        //------------------------------------------
        // Connect to DOSE
        //------------------------------------------
        private void ConnectToDose() //A new thread is calling the dose.Open
        {
            var instance = 0;
            var cn = Settings.Sate.ConnectionName;

            var connected = false;

            while (!connected)
            {
                try
                {
                    Dose.Open(cn,
                        instance.ToString(),
                        connectContext,
                        this, // ConnectionOwner
                        this); // Dispatcher

                    connected = true;
                }
                catch (NotOpenException)
                {
                    // Instance already used, try another!
                    instance++;
                }
                catch (LowMemoryException)
                {
                    MessageBox.Show("Cannot connect to the Dob due to low shared memory", "Low Memory", MessageBoxButtons.OK,
                                    MessageBoxIcon.Error);
                    System.Windows.Forms.Application.Exit();
                }
            }

            Dose.Close();
            ConnectedEvent();
        }

        private void OnConnected() //Event signaling that connection can be done
        {
            Invoke(new HandleConnectionDoneDelegate(HandleConnectionDone)); //change thread back to the main thread.
        }

        //Called when connection to dose is established
        private void HandleConnectionDone()
        {
            var instance = 0;

            var connected = false;

            while (!connected)
            {
                try
                {
                    //connect to dob
                    Dose.Open(Settings.Sate.ConnectionName,
                        instance.ToString(),
                        connectContext,
                        this, // ConnectionOwner
                        this); // Dispatcher
                    connected = true;
                }
                catch (NotOpenException)
                {
                    // Instance already used, try another!
                    instance++;
                }
                catch (LowMemoryException)
                {
                    MessageBox.Show("Cannot connect to the Dob due to low shared memory", "Low Memory", MessageBoxButtons.OK,
                                    MessageBoxIcon.Error);
                    System.Windows.Forms.Application.Exit();
                }
            }

            IsConnected = true;

            connectToolStripMenuItem.Enabled = false;
            connectWithContextToolStripMenuItem.Enabled = false;
            disconnectToolStripMenuItem.Enabled = true;

            // get connection name
            var doseAspectMisc = new ConnectionAspectMisc(Dose);
            connectionStatusPanel.Text = "Connected as " + doseAspectMisc.GetConnectionName();

            OutputPanel.Instance.LogEvent("success!", true);

            //--------------------------------------------------------------------
            //If connection is ok, set up permanent ownerships stored in settings
            //--------------------------------------------------------------------
            OutputPanel.Instance.LogEvent("- Register entities and services", true);
            foreach (var id in Settings.Sate.Register)
            {
                if (Operations.IsOfType(id.typeId, Entity.ClassTypeId))
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
            foreach (var id in Settings.Sate.Subscribe)
            {
                if (Operations.IsOfType(id.typeId, Entity.ClassTypeId))
                {
                    ExplorerPanel.Instance.SubscribeEntity(id);
                }
                else if (Operations.IsOfType(id.typeId, Message.ClassTypeId))
                {
                    ExplorerPanel.Instance.SubscribeMessage(id);
                }
            }

            //----------------------------------------------------
            //...and then set up permanent subscriptions of registrations
            //----------------------------------------------------
            OutputPanel.Instance.LogEvent("- Setting up subscriptions of registrations", true);
            foreach (var id in Settings.Sate.SubscribeReg)
            {
                ExplorerPanel.Instance.SubscribeRegistration(id);
            }

            // dispatchTimer.Start(); // we do an automatic dispatch every 2 second
        }

        //-------------------------------------------------
        //Organizes the windows (panels)
        //-------------------------------------------------
        
        private void AutoOrganize()
        {
            //-----------------------------------------
            // Left Panel
            //-----------------------------------------
            if (classExplorerToolStripMenuItem.Checked)
            {
                leftPanel.Visible = true;
                leftSplitter.Visible = true;
            }
            else
            {
                leftPanel.Visible = false;
                leftSplitter.Visible = false;
            }

            //-----------------------------------------
            // Bottom Panel
            //-----------------------------------------
            if (inboxToolStripMenuItem.Checked || outputToolStripMenuItem.Checked)
            {
                bottomPanel.Visible = true;
                bottomSplitter.Visible = true;
            }
            else
            {
                bottomPanel.Visible = false;
                bottomSplitter.Visible = false;
            }

            // Inbox
            if (inboxToolStripMenuItem.Checked)
            {
                bottomFillPanel.Visible = true;
                bottomRightPanel.Dock = DockStyle.Right;
            }
            else
            {
                bottomFillPanel.Visible = false;
                if (outputToolStripMenuItem.Checked)
                    bottomRightPanel.Dock = DockStyle.Fill;
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
        private void classExplorermenuItem_Click(object sender, EventArgs e)
        {
            ShowHideExplorer(!classExplorerToolStripMenuItem.Checked);
        }

        public void ShowHideExplorer(bool show)
        {
            classExplorerToolStripMenuItem.Checked = show;
            AutoOrganize();
        }

        //Event handler for menu: View -> Output
        private void eventlogmenuItem_Click(object sender, EventArgs e)
        {
            ShowHideOutput(!outputToolStripMenuItem.Checked);
        }

        public void ShowHideOutput(bool show)
        {
            outputToolStripMenuItem.Checked = show;
            AutoOrganize();
        }

        //Event handler for menu: View -> Inbox
        private void subrespmenuItem_Click(object sender, EventArgs e)
        {
            ShowHideInbox(!inboxToolStripMenuItem.Checked);
        }

        public void ShowHideInbox(bool show)
        {
            inboxToolStripMenuItem.Checked = show;
            AutoOrganize();
        }

        //Event handler for menu: Tools -> Settings
        private void settingsmenuItem_Click(object sender, EventArgs e)
        {
            var sf = new SettingsForm();
            sf.ShowDialog();
        }


        private void externalApplicationsToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var form = new ExternalApplicationSettingsForm();
            form.ShowDialog();
            if (form.DialogResult == DialogResult.OK)
            {
                ExplorerPanel.Instance.UpdateExternalProgramsSubmenu();
            }
        }

        //Event handler for menu: Help -> About SATE...
        private void aboutmenuItem_Click(object sender, EventArgs e)
        {
            aboutForm.Location = new Point(Width/2, Height/2);
            aboutForm.ShowDialog();
        }

        //Event handler for menu: Connection -> Connect
        private void connectmenuItem_Click(object sender, EventArgs e)
        {
            if (!IsConnected)
            {
                ScenarioTabPage.Instance.Player.Record(DobAction.Connect, (ScenarioInfo) null);
                Connect(true, 0);
            }
        }

        //Event handler for menu: Connection -> Disconnect
        private void disconnectmenuItem_Click(object sender, EventArgs e)
        {
            ScenarioTabPage.Instance.Player.Record(DobAction.Disconnect, (ScenarioInfo) null);
            Disconnect();
        }

        //Event handler for menu: Tools -> Scenario
        private void scenariomenuItem_Click(object sender, EventArgs e)
        {
            var ix = tabControl.TabPages.IndexOf(ScenarioTabPage.Instance);
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
            if (!IsConnected)
            {
                MessageBox.Show("You are not connected to the DOB!", "Not Connected", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
                return false;
            }
            return true;
        }

        //-----------------------------------------------------------------------
        // DOSE operations
        //-----------------------------------------------------------------------
        public void Connect(bool startThread, int context)
        {
            //Start thread that hangs on connect
            connectionStatusPanel.Text = "Trying to connect...";
            OutputPanel.Instance.LogEvent("- Connecting to DOSE using context " + context + "...", false);
            connectContext = context;
            if (startThread)
            {
                var connThread = new Thread(ConnectToDose);
                connThread.IsBackground = true;
                connThread.Start();
            }
            else
            {
                ConnectToDose();
            }
        }

        public void Disconnect()
        {
            Cursor = Cursors.WaitCursor;

            if (IsConnected)
            {
                ScenarioTabPage.Instance.Player.Record(DobAction.Disconnect, (ScenarioInfo) null);
                Dose.Close();
            }
            IsConnected = false;

            ExplorerPanel.Instance.LoadClassHierarchy(); //reset explorer

            disconnectToolStripMenuItem.Enabled = false;
            connectToolStripMenuItem.Enabled = true;
            connectWithContextToolStripMenuItem.Enabled = true;
            OutputPanel.Instance.LogEvent("- Disconnected from DOSE.", true);
            connectionStatusPanel.Text = "Disconnected!";
            Cursor = Cursors.Default;
        }


        public void SetChangesEntity(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.SetChangesEntity, entityInfo);

            try
            {
                Dose.SetChanges((Entity) entityInfo.Obj, entityInfo.GetInstanceId(), entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity set changes, '" + entityInfo.Obj + "'", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
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

            ScenarioTabPage.Instance.Player.Record(DobAction.SetAllEntity, entityInfo);

            try
            {
                Dose.SetAll((Entity) entityInfo.Obj, entityInfo.GetInstanceId(), entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity set all, '" + entityInfo.Obj + "'", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
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

            ScenarioTabPage.Instance.Player.Record(DobAction.DeleteEntity, entityInfo);

            try
            {
                var entityId = new EntityId(entityInfo.Obj.GetTypeId(), entityInfo.GetInstanceId());
                Dose.Delete(entityId, entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity deleted, '" + entityInfo.Obj + "'", true);
            }
            catch (AccessDeniedException e)
            {
                OutputPanel.Instance.LogEvent("- Entity delete failed, error message: " + e.Message, true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        public void InjectionSetChanges(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.InjectionSetChanges, entityInfo);

            try
            {
                var injector = new ConnectionAspectInjector(Dose);
                injector.InjectChanges((Entity) entityInfo.Obj, entityInfo.GetInstanceId(), entityInfo.Timestamp,
                    entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity injection set changes, '" + entityInfo.Obj + "'", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
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

            ScenarioTabPage.Instance.Player.Record(DobAction.InjectionInitialSet, entityInfo);

            try
            {
                var injector = new ConnectionAspectInjector(Dose);
                injector.InitialSet((Entity) entityInfo.Obj, entityInfo.GetInstanceId(), entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity injection initial set, '" + entityInfo.Obj + "'", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
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

            ScenarioTabPage.Instance.Player.Record(DobAction.InjectionDelete, entityInfo);

            try
            {
                var injector = new ConnectionAspectInjector(Dose);
                var entityId = new EntityId(entityInfo.Obj.GetTypeId(), entityInfo.GetInstanceId());
                injector.InjectDelete(entityId, entityInfo.Timestamp, entityInfo.getHandlerId());
                OutputPanel.Instance.LogEvent("- Entity injection delete, '" + entityInfo.Obj + "'", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
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

            ScenarioTabPage.Instance.Player.Record(DobAction.CreateRequest, entityInfo);

            try
            {
                if (entityInfo.RequestorDecides)
                {
                    var reqId = Dose.CreateRequest((Entity) entityInfo.Obj, entityInfo.GetInstanceId(),
                        entityInfo.getHandlerId(), this);
                    OutputPanel.Instance.LogEvent(
                        "- Create request (requestor decides) sent for entity '" + entityInfo.Obj + "', requestId " +
                        reqId, true);
                }
                else
                {
                    var reqId = Dose.CreateRequest((Entity) entityInfo.Obj, entityInfo.getHandlerId(), this);
                    OutputPanel.Instance.LogEvent(
                        "- Create request (handler decides) sent for entity '" + entityInfo.Obj + "', requestId " +
                        reqId, true);
                }
            }
            catch (OverflowException)
            {
                OutputPanel.Instance.LogEvent("- Overflow when sending create request '" + entityInfo.Obj + "'", true);
            }
            catch (NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        public void UpdateRequest(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.UpdateRequest, entityInfo);

            try
            {
                var reqId = Dose.UpdateRequest((Entity) entityInfo.Obj, entityInfo.GetInstanceId(), this);
                OutputPanel.Instance.LogEvent(
                    "- Update request sent for entity '" + entityInfo.Obj + "', requestId " + reqId, true);
            }
            catch (OverflowException)
            {
                OutputPanel.Instance.LogEvent("- Overflow when sending update request '" + entityInfo.Obj + "'", true);
            }
            catch (NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        public void DeleteRequest(EntityInfo entityInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.DeleteRequest, entityInfo);

            try
            {
                var entityId = new EntityId(entityInfo.Obj.GetTypeId(), entityInfo.GetInstanceId());
                var reqId = Dose.DeleteRequest(entityId, this);
                OutputPanel.Instance.LogEvent(
                    "- Delete request sent for entity '" + entityInfo.Obj + "', requestId " + reqId, true);
            }
            catch (OverflowException)
            {
                OutputPanel.Instance.LogEvent("- Overflow when sending delete request '" + entityInfo.Obj + "'", true);
            }
            catch (NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        // public void DeleteRequest(Safir.Dob.Typesystem.EntityId entityId)
        public void DeleteRequest(EntityIdInfo entityIdInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.DeleteRequest, entityIdInfo);

            try
            {
                var reqId = Dose.DeleteRequest(entityIdInfo.entityIdSer.EntityId(), this);
                OutputPanel.Instance.LogEvent(
                    "- Delete request sent for entityId '" + entityIdInfo.entityIdSer.EntityId() + "', requestId " +
                    reqId, true);
            }
            catch (OverflowException)
            {
                OutputPanel.Instance.LogEvent(
                    "- Overflow when sending delete request '" + entityIdInfo.entityIdSer.EntityId() + "'", true);
            }
            catch (NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        public void ServiceRequest(ServiceHandlerInfo srvInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.ServiceRequest, srvInfo);

            try
            {
                Dose.ServiceRequest((Service) srvInfo.Obj, srvInfo.getHandlerId(), this);
                OutputPanel.Instance.LogEvent(
                    "- Service request sent, '" + Operations.GetName(((Service) srvInfo.Obj).GetTypeId()) +
                    "' to handler: " + srvInfo.getHandlerId(), true);
            }
            catch (OverflowException)
            {
                OutputPanel.Instance.LogEvent(
                    "- Overflow when sending service request '" +
                    Operations.GetName(((Service) srvInfo.Obj).GetTypeId()) + "' to handler: " + srvInfo.getHandlerId(),
                    true);
            }
            catch (NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        public void SendMessage(MessageInfo msgInfo)
        {
            if (!CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.SendMessage, msgInfo);

            try
            {
                Dose.Send((Message) msgInfo.Obj, msgInfo.getChannelId(), this);
                OutputPanel.Instance.LogEvent(
                    "- Message sent, '" + Operations.GetName(((Message) msgInfo.Obj).GetTypeId()) + "' on channel " +
                    msgInfo.getChannelId(), true);
            }
            catch (OverflowException)
            {
                OutputPanel.Instance.LogEvent(
                    "- Overflow when sending message '" + Operations.GetName(((Message) msgInfo.Obj).GetTypeId()) + "'",
                    true);
            }
            catch (NotOpenException)
            {
                OutputPanel.Instance.LogEvent("- SATE is not connected to the DOB.", true);
            }
            catch (SoftwareViolationException e)
            {
                MessageBox.Show(e.Message, "SoftwareViolationException", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
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
                saveToolStripMenuItem.Enabled = true;
                findToolStripMenuItem.Enabled = true;
            }
            else if (tabControl.SelectedTab is ObjectEditTabPage)
            {
                saveToolStripMenuItem.Enabled = true;
                findToolStripMenuItem.Enabled = false;
            }
            else if (tabControl.SelectedTab is ScenarioTabPage)
            {
                saveToolStripMenuItem.Enabled = true;
                findToolStripMenuItem.Enabled = false;
            }
        }

        private void findToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (tabControl.SelectedTab is XmlTabPage)
            {
                var p = (XmlTabPage) tabControl.SelectedTab;
                new FindForm(p.RichEdit).Show();
            }
            else
            {
                UpdateMainMenu();
            }
        }


        private void opendoufileToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var of = new OpenFileDialog
            {
                AddExtension = true,
                Filter = "DOU files (*.dou)|*.dou|All files (*.*)|*.*",
                InitialDirectory = Path.GetDirectoryName(Path.GetDirectoryName
                    (InternalOperations.GetDouFilePath(Entity.ClassTypeId)))
            };

            if (of.ShowDialog() == DialogResult.OK)
            {
                OpenDouFile(of.FileName);
            }
        }

        private void OpenDouFile(string fileName)
        {
            using (TextReader reader = new StreamReader(fileName))
            {
                var content = reader.ReadToEnd();
                reader.Close();
                AddTabPage(new XmlTabPage(content, fileName));
            }
        }

        private void openscenarioToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var of = new OpenFileDialog
            {
                AddExtension = true,
                Filter = "Scenario files (*.dos)|*.dos|All files (*.*)|*.*"
            };
            if (of.ShowDialog() == DialogResult.OK)
            {
                ScenarioTabPage.Instance.Player.LoadScenario(of.FileName);
                AddTabPage(ScenarioTabPage.Instance);
            }
        }

        private void openserializedObjectToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var of = new OpenFileDialog
            {
                AddExtension = true,
                Filter = "XML files (*.xml)|*.xml|JSON files (*.json)|*.json|All files (*.*)|*.*"
            };

            if (of.ShowDialog() == DialogResult.OK)
            {
                OpenSerializedObject(of.FileName);
            }
        }

        private void OpenSerializedObject(string fileName)
        {
            bool isXml = true;
            using (TextReader reader = new StreamReader(fileName))
            {
                var content = reader.ReadToEnd().Trim();
                reader.Close();

                isXml = Path.GetExtension(fileName).ToLower() == ".xml" || content.StartsWith(@"<?xml version");

                //First try to open as serialized object
                try
                {
                    var o = isXml ? Serialization.ToObject(content) : Serialization.ToObjectFromJson(content);
                    if (o is Service)
                    {
                        var srvInfo = new ServiceHandlerInfo();
                        srvInfo.Obj = o;
                        AddTabPage(new ObjectEditTabPage(srvInfo));
                    }
                    else if (o is Entity)
                    {
                        var entityInfo = new EntityInfo();
                        entityInfo.Obj = o;
                        AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                    else if (o is Message)
                    {
                        var msgInfo = new MessageInfo();
                        msgInfo.Obj = o;
                        AddTabPage(new ObjectEditTabPage(msgInfo));
                    }
                    else if (o is Response)
                    {
                        var responseInfo = new ResponseInfo();
                        responseInfo.Obj = o;
                        AddTabPage(new ObjectEditTabPage(responseInfo));
                    }
                    else
                    {
                        var objInfo = new ObjectInfo();
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
                    if (isXml)
                    {
                        AddTabPage(new XmlTabPage(content, fileName));
                    }
                    else
                    {
                        AddTabPage(new JsonTabPage(content, fileName));
                    }
                    
                    return;
                }
                catch
                {
                }

                MessageBox.Show("Failed to open file '" + fileName + "'.",
                    "Invalid xml or json", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (tabControl.SelectedTab is XmlTabPage)
            {
                var p = (XmlTabPage) tabControl.SelectedTab;
                var sf = new SaveFileDialog();
                sf.AddExtension = false;
                sf.Filter = "XML files (*.xml;*.dou)|*.xml;*.dou|All files (*.*)|*.*";
                sf.FileName = p.FileName;
                if (sf.ShowDialog() == DialogResult.OK)
                {
                    p.Save(sf.FileName);
                }
            }
            else if (tabControl.SelectedTab is ObjectEditTabPage)
            {
                var p = (ObjectEditTabPage) tabControl.SelectedTab;

                var obj = p.ObjEditCtrl.GetObject();
                if (obj == null)
                    return;

                var name = Operations.GetName(obj.GetTypeId());
                name = name.Substring(name.LastIndexOf('.') + 1);
                //name += "_" + obj.InstanceNumber.ToString() + ".xml";
                name += ".xml";

                var sf = new SaveFileDialog();
                sf.AddExtension = true;
                sf.Filter = "XML files (*.xml)|*.xml|All files (*.*)|*.*";
                sf.FileName = name;
                if (sf.ShowDialog() == DialogResult.OK)
                {
                    var xml = Serialization.ToXml(obj);
                    using (TextWriter writer = new StreamWriter(sf.FileName))
                    {
                        writer.Write(xml);
                        writer.Flush();
                        writer.Close();
                    }
                }
            }
            else if (tabControl.SelectedTab is ScenarioTabPage)
            {
                var sf = new SaveFileDialog();
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
            var form = new ConnectWithContextForm();
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
                        var otp = (ObjectEditTabPage) tp;
                        if (otp.ObjEditCtrl.LiveData &&
                            otp.ObjEditCtrl.GetObject().GetTypeId() == objInfo.Obj.GetTypeId())
                        {
                            if (objInfo is EntityInfo && otp.ObjEditCtrl.GetObjectInfo() is EntityInfo)
                            {
                                var paramEntityInfo = (EntityInfo) objInfo;
                                var otpEntityInfo = (EntityInfo) otp.ObjEditCtrl.GetObjectInfo();
                                // only update if instanceid is the same
                                if (
                                    paramEntityInfo.InstanceIdSer.InstanceId()
                                        .Equals(otpEntityInfo.InstanceIdSer.InstanceId()))
                                {
                                    otp.ObjEditCtrl.UpdateData(objInfo);
                                    return true;
                                }
                            }
                        }
                    }
                }
                catch
                {
                }
            }
            return false;
        }


        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.Application.Exit();
        }

        private void timestampToolStripMenuItem_Click(object sender, EventArgs e)
        {
            new TimestampCalculatorForm().ShowDialog();
        }

        private void typeIdToolStripMenuItem_Click(object sender, EventArgs e)
        {
            new TypeIdCalculatorForm().Show();
        }

        private void runGarbageCollectorToolStripMenuItem_Click(object sender, EventArgs e)
        {
            GC.Collect();
        }

        public Type GetType(long typeId)
        {
            return ObjectFactory.Instance.CreateObject(typeId).GetType();
        }

        private delegate void OnConnectedDelegate();

        private delegate void HandleConnectionDoneDelegate();

        #region EntitySubscriber Members

        #endregion

        #region EntityOwner Members

        #region EntityRequestBase Members

        public void OnCreateRequest(EntityRequestProxy entityRequestProxy, ResponseSender responseSender)
        {
            var name = Operations.GetName(entityRequestProxy.TypeId);
            var handlerDecides = false;
            foreach (var typeId in handlerDecidesTypeIdList)
            {
                if (entityRequestProxy.TypeId == typeId)
                {
                    handlerDecides = true;
                    break;
                }
            }

            EntityId entityId;
            InstanceId instanceId;

            if (handlerDecides)
            {
                entityId = new EntityId(entityRequestProxy.TypeId, InstanceId.GenerateRandom());
                instanceId = entityId.InstanceId;
            }
            else
            {
                entityId = entityRequestProxy.EntityId;
                instanceId = entityRequestProxy.InstanceId;
            }

            OutputPanel.Instance.LogEvent("- Received create request on instance '" + name + " : " + instanceId + "'",
                true);
            var entity = entityRequestProxy.Request;

            var entityInfo = new EntityInfo();
            entityInfo.Obj = entityRequestProxy.Request;
            entityInfo.SetInstanceId(instanceId);
            entityInfo.setHandlerId(entityRequestProxy.ReceivingHandlerId);
            entityInfo.Blobsize = BlobOperations.GetSize(entityRequestProxy.Blob);

            InboxPanel.Instance.AddResponse(entityInfo, "Create request");
            if (Settings.Sate.AutoCreate)
            {
                Dose.SetAll(entity, instanceId, entityInfo.getHandlerId());
            }

            if (!Settings.Sate.NoResponse)
            {
                if (handlerDecides)
                {
                    OutputPanel.Instance.LogEvent("- Response sent", true);
                    responseSender.Send(EntityIdResponse.CreateResponse(entityId));
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

        public void OnDeleteRequest(EntityRequestProxy entityRequestProxy, ResponseSender responseSender)
        {
            var name = Operations.GetName(entityRequestProxy.TypeId);
            var entityId = entityRequestProxy.EntityId;
            var instanceId = entityRequestProxy.InstanceId;

            OutputPanel.Instance.LogEvent("- Received delete request on instance '" + name + " : " + instanceId + "'",
                true);

            var entityInfo = new EntityInfo();
            entityInfo.SetInstanceId(entityRequestProxy.InstanceId);
            entityInfo.setHandlerId(entityRequestProxy.ReceivingHandlerId);

            InboxPanel.Instance.AddNonDisplayableResponse(entityId.TypeId, "Delete request");
            if (Settings.Sate.AutoUpdate)
            {
                Dose.Delete(entityId, entityInfo.getHandlerId());
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

        public void OnUpdateRequest(EntityRequestProxy entityRequestProxy, ResponseSender responseSender)
        {
            var name = Operations.GetName(entityRequestProxy.TypeId);
            var instanceId = entityRequestProxy.InstanceId;

            OutputPanel.Instance.LogEvent("- Received update request on instance '" + name + " : " + instanceId + "'",
                true);
            var entity = entityRequestProxy.Request;

            var entityInfo = new EntityInfo();
            entityInfo.Obj = entityRequestProxy.Request;
            entityInfo.SetInstanceId(entityRequestProxy.InstanceId);
            entityInfo.setHandlerId(entityRequestProxy.ReceivingHandlerId);
            entityInfo.Blobsize = BlobOperations.GetSize(entityRequestProxy.Blob);
            InboxPanel.Instance.AddResponse(entityInfo, "Update request");

            if (Settings.Sate.AutoUpdate)
            {
                Dose.SetChanges(entity, instanceId, entityInfo.getHandlerId());
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

        #region RegistrationSubscriber Members

        public void OnUnregistered(long typeId, HandlerId handlerId)
        {
            var name = Operations.GetName(typeId);
            OutputPanel.Instance.LogEvent("- Class '" + name + ": " + handlerId + "' is unregistered", true);
            ExplorerPanel.Instance.SetUnregistered(typeId);
        }

        public void OnRegistered(long typeId, HandlerId handlerId)
        {
            var name = Operations.GetName(typeId);
            OutputPanel.Instance.LogEvent("- Class '" + name + ": " + handlerId + "' is registered", true);
            ExplorerPanel.Instance.SetRegistered(typeId);
        }

        #endregion

        #region Requestor Members

        public void OnNotRequestOverflow()
        {
            OutputPanel.Instance.LogEvent("- Notification: Not overflow in request queue", true);
        }

        public void OnResponse(ResponseProxy responseProxy)
        {
            var response = responseProxy.Response;
            var handlerId = responseProxy.RequestHandlerId;
            var requestId = responseProxy.RequestId;

            var responseInfo = new ResponseInfo();
            responseInfo.Response = responseProxy.Response;
            responseInfo.Obj = responseProxy.Response;

            if (response != null)
            {
                var name = Operations.GetName(response.GetTypeId());
                OutputPanel.Instance.LogEvent(
                    "- Received response '" + name + " : " + handlerId + "' on requestId " + requestId, true);
            }
            else
            {
                OutputPanel.Instance.LogEvent("- Received response 'null' on handlerId " + handlerId, true);
            }

            InboxPanel.Instance.AddResponse(responseInfo, "Reply on request " + requestId);
            UpdateLiveData(responseInfo);
        }

        #endregion

        #region EntitySubscriber Members

        public void OnDeletedEntity(EntityProxy entityProxy, bool deletedByOwner)
        {
            var entityInfo = new EntityInfo();
            entityInfo.SetInstanceId(entityProxy.InstanceId);
            entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);

            OutputPanel.Instance.LogEvent("- Received deleted entity: '" + entityProxy.EntityId
                                          + "' for handler '" + entityProxy.OwnerWithStringRepresentation +
                                          "' deletedByOwner=" + deletedByOwner, true);
            ExplorerPanel.Instance.DeleteObject(entityProxy.EntityId);
            //InboxPanel.Instance.AddResponse(entityInfo, "Deleted entity");
            InboxPanel.Instance.AddNonDisplayableResponse(entityProxy.EntityId.TypeId, "Deleted entity");

            UpdateLiveData(entityInfo);
        }

        public void OnNewEntity(EntityProxy entityProxy)
        {
            var entityInfo = new EntityInfo();
            entityInfo.SetInstanceId(entityProxy.InstanceId);
            entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);
            entityInfo.Obj = entityProxy.Entity;

            entityInfo.Blobsize = BlobOperations.GetSize(entityProxy.Blob);

            OutputPanel.Instance.LogEvent("- Received new entity: '" + entityProxy.EntityId
                                          + "' for handler '" + entityProxy.OwnerWithStringRepresentation + "'"
                                          + " owner '" + entityProxy.OwnerConnectionInfo.ConnectionName.Val + "'", true);
            ExplorerPanel.Instance.AddObject(entityProxy.EntityId);
            InboxPanel.Instance.AddResponse(entityInfo, "New entity");

            UpdateLiveData(entityInfo);
        }

        public void OnUpdatedEntity(EntityProxy entityProxy)
        {
            var entityInfo = new EntityInfo();
            entityInfo.SetInstanceId(entityProxy.InstanceId);
            entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);
            entityInfo.Obj = entityProxy.Entity;

            entityInfo.Blobsize = BlobOperations.GetSize(entityProxy.Blob);

            OutputPanel.Instance.LogEvent("- Received updated entity: '" + entityProxy.EntityId
                                          + "' for handler '" + entityProxy.OwnerWithStringRepresentation + "'"
                                          + " owner '" + entityProxy.OwnerConnectionInfo.ConnectionName.Val + "'", true);
            InboxPanel.Instance.AddResponse(entityInfo, "Updated entity");

            UpdateLiveData(entityInfo);
        }

        #endregion

        #region EntityInjectionBase Members

        public void OnInitialInjectionsDone(long typeId, HandlerId handlerId)
        {
            var name = Operations.GetName(typeId);
            OutputPanel.Instance.LogEvent(
                "- Initial injections done for typeId: " + name + " with handlerid '" + handlerId + "'", true);
        }

        public void OnInjectedDeletedEntity(InjectedEntityProxy injectedEntityProxy)
        {
            OutputPanel.Instance.LogEvent(
                "- Injected deleted entity. Entity id: '" + injectedEntityProxy.EntityId + "'", true);
        }

        public void OnInjectedNewEntity(InjectedEntityProxy injectedEntityProxy)
        {
            OutputPanel.Instance.LogEvent("- Injected new entity. Entity id: '" + injectedEntityProxy.EntityId + "'",
                true);
        }

        public void OnInjectedUpdatedEntity(InjectedEntityProxy injectedEntityProxy)
        {
            OutputPanel.Instance.LogEvent(
                "- Injected updated entity. Entity id: '" + injectedEntityProxy.EntityId + "'", true);
        }

        #endregion

    }
}
