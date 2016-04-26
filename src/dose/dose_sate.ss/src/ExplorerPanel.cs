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
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Safir.Dob;
using Safir.Dob.Typesystem;
using Safir.Dob.Typesystem.Internal;
using Exception = System.Exception;
using Message = Safir.Dob.Message;
using Object = Safir.Dob.Typesystem.Object;

namespace Sate
{
    /// <summary>
    ///     Summary description for ClassViewPanel.
    /// </summary>
    public class ExplorerPanel : Panel
    {
        private static TreeViewImageHandler imageHandler;

        private static ExplorerPanel instance;
        private readonly MenuItem changeImageMenuItem;

        // context menus
        private readonly ContextMenu contextMenu;
        private readonly MenuItem deleteRequestMenuItem;
        private readonly MenuItem dobUnitInfoMenuItem;
        private readonly MenuItem iterateClassMenuItem;
        private readonly MenuItem openMenuItem;
        private readonly MenuItem registerMenuItem;
        private readonly MenuItem registerOptionsMenuItem;
        private readonly MenuItem separator1MenuItem;
        private readonly MenuItem separator2MenuItem;
        private readonly MenuItem separator3MenuItem;
        private readonly MenuItem subscribeMenuItem;
        private readonly MenuItem subscribeOptionsMenuItem;
        private readonly MenuItem subscribeRegistrationMenuItem;
        private readonly MenuItem subscribeRegistrationOptionsMenuItem;
        private readonly TabControl tabControl = new TabControl();
        private readonly TabPage tabInheritance = new TabPage("Inheritance");
        private readonly TabPage tabNamespace = new TabPage("Namespace");
        private readonly TreeView treeViewClassHierarchy;
        private readonly TreeView treeViewNsHierarchy;
        private readonly MenuItem unregisterMenuItem;
        private readonly MenuItem unsubscribeMenuItem;
        private readonly MenuItem unsubscribeRegistrationMenuItem;
        private readonly MenuItem viewDouFileMenuItem;
        private Hashtable clTypeIdHt;
        private IContainer components;
        private ImageList defaultImagesList;
        private Hashtable dobTypeHt;
        private Panel fillpanel;
        private ImageList imageList;
        private Hashtable nsHt;
        private Hashtable nsTypeIdHt;
        private Panel toppanel;


        private ExplorerPanel()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            SuspendLayout();

            //Load images for the treeview
            imageHandler = new TreeViewImageHandler(imageList, defaultImagesList);
            imageHandler.RefreshImageCollection();

            //context menu setup
            openMenuItem = new MenuItem("Open");
            registerMenuItem = new MenuItem("Register handler");
            registerOptionsMenuItem = new MenuItem("Register handler with options...");
            unregisterMenuItem = new MenuItem("Unregister handler");
            separator1MenuItem = new MenuItem("-");
            subscribeMenuItem = new MenuItem("Subscribe");
            subscribeOptionsMenuItem = new MenuItem("Subscribe with options...");
            unsubscribeMenuItem = new MenuItem("Unsubscribe");
            subscribeRegistrationMenuItem = new MenuItem("Subscribe for registrations");
            subscribeRegistrationOptionsMenuItem = new MenuItem("Subscribe for registrations with options...");
            unsubscribeRegistrationMenuItem = new MenuItem("Unsubscribe registrations");
            separator2MenuItem = new MenuItem("-");
            deleteRequestMenuItem = new MenuItem("Send delete request");
            separator3MenuItem = new MenuItem("-");
            iterateClassMenuItem = new MenuItem("Iterate class...");
            changeImageMenuItem = new MenuItem("Change image...");
            dobUnitInfoMenuItem = new MenuItem("View details...");
            viewDouFileMenuItem = new MenuItem("View dou-file...");

            openMenuItem.Click += openMenuItem_Click;

            registerMenuItem.Click += registerMenuItem_Click;
            registerOptionsMenuItem.Click += registerOptionsMenuItem_Click;
            unregisterMenuItem.Click += unregisterMenuItem_Click;

            subscribeMenuItem.Click += subscribeMenuItem_Click;
            subscribeOptionsMenuItem.Click += subscribeOptionsMenuItem_Click;
            unsubscribeMenuItem.Click += unsubscribeMenuItem_Click;

            subscribeRegistrationMenuItem.Click += subscribeRegistrationMenuItem_Click;
            subscribeRegistrationOptionsMenuItem.Click += subscribeRegistrationOptionsMenuItem_Click;
            unsubscribeRegistrationMenuItem.Click += unsubscribeRegistrationMenuItem_Click;

            deleteRequestMenuItem.Click += deleteRequestMenuItem_Click;

            iterateClassMenuItem.Click += iterateClassMenuItem_Click;

            changeImageMenuItem.Click += changeImageMenuItem_Click;
            dobUnitInfoMenuItem.Click += dobUnitInfoMenuItem_Click;
            viewDouFileMenuItem.Click += viewDouFileMenuItem_Click;

            contextMenu = new ContextMenu(new[]
            {
                openMenuItem,
                registerMenuItem,
                registerOptionsMenuItem,
                unregisterMenuItem,
                separator1MenuItem,
                subscribeMenuItem,
                subscribeOptionsMenuItem,
                unsubscribeMenuItem,
                subscribeRegistrationMenuItem,
                subscribeRegistrationOptionsMenuItem,
                unsubscribeRegistrationMenuItem,
                separator2MenuItem,
                deleteRequestMenuItem,
                separator3MenuItem,
                iterateClassMenuItem,
                dobUnitInfoMenuItem,
                viewDouFileMenuItem,
                changeImageMenuItem
            });

            contextMenu.Popup += contextMenu_Popup;

            //treeViewClassHierarchy
            treeViewClassHierarchy = new TreeView();
            treeViewClassHierarchy.ContextMenu = contextMenu;
            treeViewClassHierarchy.Location = new Point(0, 40);
            treeViewClassHierarchy.Dock = DockStyle.Fill;
            treeViewClassHierarchy.ImageList = imageList;

            treeViewClassHierarchy.DoubleClick += treeViewClassHierarchy_DoubleClick;
            treeViewClassHierarchy.MouseDown += treeViewClassHierarchy_MouseDown;
            treeViewClassHierarchy.ItemDrag += treeViewClassHierarchy_ItemDrag;
            treeViewClassHierarchy.TreeViewNodeSorter = new TreeSorter();

            //treeViewNsHierarchy
            treeViewNsHierarchy = new TreeView();
            treeViewNsHierarchy.ContextMenu = contextMenu;
            treeViewNsHierarchy.Location = new Point(0, 40);
            treeViewNsHierarchy.Dock = DockStyle.Fill;
            treeViewNsHierarchy.ImageList = imageList;
            treeViewNsHierarchy.DoubleClick += treeViewNsHierarchy_DoubleClick;
            treeViewNsHierarchy.MouseDown += treeViewNsHierarchy_MouseDown;
            treeViewNsHierarchy.ItemDrag += treeViewNsHierarchy_ItemDrag;
            treeViewNsHierarchy.TreeViewNodeSorter = new TreeSorter();

            //Tabs
            tabControl.Dock = DockStyle.Fill;
            tabInheritance.Controls.Add(treeViewClassHierarchy);
            tabNamespace.Controls.Add(treeViewNsHierarchy);
            fillpanel.Controls.Add(tabControl);
            tabControl.TabPages.AddRange(new[] {tabInheritance, tabNamespace});
            if (Settings.Sate.DefaultExplorerView)
                tabControl.SelectedIndex = 0;
            else
                tabControl.SelectedIndex = 1;
            tabControl.SelectedIndex = 0;

            //TitleBar
            var titleLabel = new PanelLabelControl("Explorer");
            titleLabel.CloseEvent += titleLabel_CloseEvent;
            titleLabel.Height = 15;
            titleLabel.Dock = DockStyle.Top;
            toppanel.Controls.Add(titleLabel);

            ResumeLayout(false);
        }

        public static ExplorerPanel Instance
        {
            get
            {
                if (instance == null)
                    instance = new ExplorerPanel();
                return instance;
            }
        }


        private void dobUnitInfoMenuItem_Click(object sender, EventArgs e)
        {
            var node = (DobUnit) GetSelectedNode();
            var detailsForm = new DobUnitDetailsForm(node.TypeId); //, node.InstanceId);
            detailsForm.ShowDialog();
        }

        private void changeImageMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();

            string mappingName;

            if (node is DobUnit)
            {
                var du = node as DobUnit;
                var tid = du.TypeId;
                mappingName = tid.ToString();
            }
            else if (node is NamespaceNode)
            {
                var nn = node as NamespaceNode;
                mappingName = nn.FullName;
            }
            else
            {
                return;
            }

            var customImageForm = new CustomImageForm(imageHandler, mappingName);
            customImageForm.ShowDialog();
            if (customImageForm.IsDirty)
            {
                UpdateAllImageIndices();
            }
        }

        private void UpdateAllImageIndices()
        {
            imageHandler.RefreshImageCollection();

            //always go through every node is not the most efficient way but it's a simple
            //way to get correct image indices.
            foreach (DictionaryEntry de in clTypeIdHt)
            {
                var tmp = de.Value as DobUnit;
                tmp.ImageIndex = imageHandler.GetImageIndex(tmp.TypeId, tmp.ImageType);
                tmp.SelectedImageIndex = tmp.ImageIndex;
            }

            foreach (DictionaryEntry de in nsTypeIdHt)
            {
                var tmp = de.Value as DobUnit;
                tmp.ImageIndex = imageHandler.GetImageIndex(tmp.TypeId, tmp.ImageType);
                tmp.SelectedImageIndex = tmp.ImageIndex;
            }
        }

        private void treeViewNsHierarchy_ItemDrag(object sender, ItemDragEventArgs e)
        {
            var n = (DobUnit) e.Item;
            DoDragDrop(n.TypeId, DragDropEffects.Move);
        }

        private void treeViewClassHierarchy_ItemDrag(object sender, ItemDragEventArgs e)
        {
            var n = (DobUnit) e.Item;
            DoDragDrop(n.TypeId, DragDropEffects.Move);
        }

        private void titleLabel_CloseEvent(object sender, EventArgs e)
        {
            MainForm.Instance.ShowHideExplorer(false);
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
            var resources = new System.ComponentModel.ComponentResourceManager(typeof(ExplorerPanel));
            this.imageList = new System.Windows.Forms.ImageList(this.components);
            this.toppanel = new System.Windows.Forms.Panel();
            this.fillpanel = new System.Windows.Forms.Panel();
            this.defaultImagesList = new System.Windows.Forms.ImageList(this.components);
            this.SuspendLayout();
            //
            // imageList
            //
            this.imageList.ColorDepth = System.Windows.Forms.ColorDepth.Depth8Bit;
            this.imageList.ImageSize = new System.Drawing.Size(16, 16);
            this.imageList.TransparentColor = System.Drawing.Color.Transparent;
            //
            // toppanel
            //
            this.toppanel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.toppanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.toppanel.Location = new System.Drawing.Point(0, 0);
            this.toppanel.Name = "toppanel";
            this.toppanel.Size = new System.Drawing.Size(224, 15);
            this.toppanel.TabIndex = 0;
            //
            // fillpanel
            //
            this.fillpanel.BackColor = System.Drawing.SystemColors.Control;
            this.fillpanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.fillpanel.ForeColor = System.Drawing.SystemColors.Control;
            this.fillpanel.Location = new System.Drawing.Point(0, 0);
            this.fillpanel.Name = "fillpanel";
            this.fillpanel.Padding = new System.Windows.Forms.Padding(0, 20, 0, 0);
            this.fillpanel.Size = new System.Drawing.Size(224, 504);
            this.fillpanel.TabIndex = 0;
            //
            // defaultImagesList
            //
            this.defaultImagesList.ImageStream =
                ((System.Windows.Forms.ImageListStreamer) (resources.GetObject("defaultImagesList.ImageStream")));
            this.defaultImagesList.TransparentColor = System.Drawing.Color.Transparent;
            this.defaultImagesList.Images.SetKeyName(0, "package.bmp");
            this.defaultImagesList.Images.SetKeyName(1, "entity_default.bmp");
            this.defaultImagesList.Images.SetKeyName(2, "service_default.bmp");
            this.defaultImagesList.Images.SetKeyName(3, "object_default.bmp");
            this.defaultImagesList.Images.SetKeyName(4, "message_default.bmp");
            this.defaultImagesList.Images.SetKeyName(5, "response.bmp");
            this.defaultImagesList.Images.SetKeyName(6, "item.bmp");
            this.defaultImagesList.Images.SetKeyName(7, "struct.bmp");
            this.defaultImagesList.Images.SetKeyName(8, "parameters.bmp");
            this.defaultImagesList.Images.SetKeyName(9, "class_default.bmp");
            //
            // ExplorerPanel
            //
            this.Controls.Add(this.toppanel);
            this.Controls.Add(this.fillpanel);
            this.Size = new System.Drawing.Size(224, 504);
            this.Text = "Class Explorer";
            this.ResumeLayout(false);
        }

        #endregion

        public void LoadClassHierarchy()
        {
            treeViewClassHierarchy.BeginUpdate();
            treeViewNsHierarchy.BeginUpdate();

            treeViewClassHierarchy.Nodes.Clear();
            treeViewNsHierarchy.Nodes.Clear();

            var objType = typeof(Object);
            var objNode = new ClassNode(objType, Object.ClassTypeId, true);
            objNode.Expand();
            dobTypeHt = new Hashtable();
            dobTypeHt[objType] = objNode;
            clTypeIdHt = new Hashtable();
            nsTypeIdHt = new Hashtable();
            nsHt = new Hashtable();
            clTypeIdHt[GetTypeId(objType)] = objNode;

            InsertClassInNsHierarchy(objType, Object.ClassTypeId);
            foreach (var tid in Operations.GetAllTypeIds())
            {
                if (!Operations.IsClass(tid))
                {
                    continue;
                }
                var type = MainForm.Instance.GetType(tid);
                if (type.IsSubclassOf(objType))
                {
                    ClassNode node = null;
                    while (type != objType && dobTypeHt[type] == null)
                    {
                        var typeId = GetTypeId(type);
                        if (node == null)
                        {
                            node = new ClassNode(type, typeId, true);
                        }
                        else
                        {
                            var child = node;
                            node = new ClassNode(type, typeId, true);
                            node.Nodes.Add(child);
                        }

                        InsertClassInNsHierarchy(type, typeId);

                        clTypeIdHt[typeId] = node;
                        dobTypeHt[type] = node;
                        type = type.BaseType;
                    }

                    if (node != null)
                    {
                        ((ClassNode) dobTypeHt[node.DobType.BaseType]).Nodes.Add(node);
                    }
                }
            }

            treeViewClassHierarchy.Nodes.Add(objNode);

            treeViewClassHierarchy.Sort();
            treeViewNsHierarchy.Sort();

            treeViewNsHierarchy.EndUpdate();
            treeViewClassHierarchy.EndUpdate();
            treeViewNsHierarchy.HideSelection = false;
            treeViewClassHierarchy.HideSelection = false;
        }

        private void InsertNamespace(Type type)
        {
            NamespaceNode node = null;
            var ns = type.Namespace.Split('.');
            var rest = type.Namespace + ".";
            for (var i = ns.Length - 1; i >= 0; i--)
            {
                rest = rest.Substring(0, rest.LastIndexOf('.'));
                if (nsHt[rest] == null)
                {
                    if (node == null)
                    {
                        node = new NamespaceNode(ns[i], type.Namespace);
                    }
                    else
                    {
                        var child = node;
                        node = new NamespaceNode(ns[i], type.Namespace);
                        node.Nodes.Add(child);
                    }
                    nsHt[rest] = node;
                }
                else
                {
                    if (node != null)
                    {
                        var parent = (NamespaceNode) nsHt[rest];

                        var inserted = false;
                        for (var nix = 0; nix < parent.Nodes.Count; nix++)
                        {
                            if (!(parent.Nodes[nix] is NamespaceNode))
                            {
                                parent.Nodes.Insert(nix, node);
                                inserted = true;
                                break;
                            }
                        }

                        if (!inserted)
                            parent.Nodes.Add(node);
                    }
                    return;
                }
            }
            nsHt[rest] = node;
            treeViewNsHierarchy.Nodes.Add(node);
        }

        private void InsertClassInNsHierarchy(Type type, long typeId)
        {
            InsertNamespace(type);
            var classNode = new ClassNode(type, typeId, false);
            nsTypeIdHt[typeId] = classNode;
            ((NamespaceNode) nsHt[type.Namespace]).Nodes.Add(classNode);
        }

        private void treeViewClassHierarchy_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right)
            {
                var clickedNode = treeViewClassHierarchy.GetNodeAt(e.X, e.Y);
                if (clickedNode != null)
                {
                    treeViewClassHierarchy.SelectedNode = clickedNode;
                }
            }
        }


        private void treeViewClassHierarchy_DoubleClick(object sender, EventArgs e)
        {
            Cursor = Cursors.WaitCursor;
            if (treeViewClassHierarchy.SelectedNode is ObjectNode)
            {
                var n = (ObjectNode) treeViewClassHierarchy.SelectedNode;

                try
                {
                    using (var entityProxy = MainForm.Instance.Dose.Read(n.EntityId))
                    {
                        var entityInfo = new EntityInfo();
                        entityInfo.Obj = entityProxy.Entity;
                        entityInfo.SetInstanceId(entityProxy.InstanceId);
                        entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                }
                catch
                {
                }
            }
            else if (treeViewClassHierarchy.SelectedNode is ClassNode)
            {
                var n = (ClassNode) treeViewClassHierarchy.SelectedNode;
                ObjectInfo objInfo = null;
                objInfo = new ObjectInfo();
                if (n == treeViewClassHierarchy.Nodes[0]) //root node is object
                {
                    objInfo.Obj = new Object();
                }
                else
                {
                    var dobType = MainForm.Instance.GetType(n.TypeId);
                    MessageInfo msgInfo = null;
                    ServiceHandlerInfo srvInfo = null;
                    EntityInfo entityInfo = null;

                    if (dobType.IsSubclassOf(typeof(Message)))
                    {
                        msgInfo = new MessageInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        msgInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(msgInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Service)))
                    {
                        srvInfo = new ServiceHandlerInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        srvInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(srvInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Entity)))
                    {
                        entityInfo = new EntityInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        entityInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                    else
                    {
                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        objInfo.Obj = (Object) constr.Invoke(null);
                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(objInfo));
                    }
                }
            }

            Cursor = Cursors.Default;
        }

        private void treeViewNsHierarchy_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right)
            {
                var clickedNode = treeViewNsHierarchy.GetNodeAt(e.X, e.Y);
                if (clickedNode != null)
                {
                    treeViewNsHierarchy.SelectedNode = clickedNode;
                }
            }
        }

        private void treeViewNsHierarchy_DoubleClick(object sender, EventArgs e)
        {
            Cursor = Cursors.WaitCursor;
            if (treeViewNsHierarchy.SelectedNode is ObjectNode)
            {
                var n = (ObjectNode) treeViewNsHierarchy.SelectedNode;

                try
                {
                    var entityInfo = new EntityInfo();
                    using (var entityProxy = MainForm.Instance.Dose.Read(n.EntityId))
                    {
                        entityInfo.Obj = entityProxy.Entity;
                        entityInfo.SetInstanceId(entityProxy.InstanceId);
                        entityInfo.setHandlerId(entityProxy.OwnerWithStringRepresentation);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                }
                catch
                {
                }
            }
            else if (treeViewNsHierarchy.SelectedNode is ClassNode)
            {
                var n = (ClassNode) treeViewNsHierarchy.SelectedNode;
                ObjectInfo objInfo = null;
                objInfo = new ObjectInfo();

                if (n == treeViewNsHierarchy.Nodes[0])
                {
                    objInfo.Obj = new Object();
                }
                else
                {
                    var dobType = MainForm.Instance.GetType(n.TypeId);
                    MessageInfo msgInfo = null;
                    ServiceHandlerInfo srvInfo = null;
                    EntityInfo entityInfo = null;

                    if (dobType.IsSubclassOf(typeof(Message)))
                    {
                        msgInfo = new MessageInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        msgInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(msgInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Service)))
                    {
                        srvInfo = new ServiceHandlerInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        srvInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(srvInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Entity)))
                    {
                        entityInfo = new EntityInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        entityInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                    else
                    {
                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        objInfo.Obj = (Object) constr.Invoke(null);
                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(objInfo));
                    }
                }
            }
            Cursor = Cursors.Default;
        }

        private long GetTypeId(Type type)
        {
            return (long) type.GetField("ClassTypeId").GetValue(null);
        }

        private void subscribeOptionsMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();
            long typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;

                if (Operations.IsOfType(typeId, Entity.ClassTypeId))
                {
                    var sf = new SubscribeForm(typeId);
                    sf.InitEntitySubForm();
                    if (sf.ShowDialog() == DialogResult.OK)
                    {
                        if (sf.AllInstancesSub)
                        {
                            var subInfo = new SubInfo(typeId, null, null, sf.DataUpdSub, sf.SubClassesSub, sf.RestartSub);
                            SubscribeEntity(subInfo);

                            if (sf.PermanentSub)
                            {
                                //Subscriptions
                                Settings.Sate.AddSubscription(subInfo);
                                Settings.Save();
                            }
                        }
                        else
                        {
                            var entityId = new EntityId(((DobUnit) node).TypeId, sf.Instance);
                            var subInfo = new SubInfo(0, new EntityIdSerializeable(entityId), null, sf.DataUpdSub,
                                sf.SubClassesSub, sf.RestartSub);
                            SubscribeEntity(subInfo);

                            if (sf.PermanentSub)
                            {
                                //Subscriptions
                                Settings.Sate.AddSubscription(subInfo);
                                Settings.Save();
                            }
                        }
                    }
                }
                else if (Operations.IsOfType(typeId, Message.ClassTypeId))
                {
                    var smf = new SubscribeMessageForm(typeId);
                    smf.InitMessageSubForm();

                    if (smf.ShowDialog() == DialogResult.OK)
                    {
                        ChannelId channelId;

                        if (smf.AllChannels)
                        {
                            channelId = ChannelId.ALL_CHANNELS;
                        }
                        else
                        {
                            /* try to parse as an long. O/w use the string representation */
                            try
                            {
                                var val = long.Parse(smf.ChannelTextBox);
                                channelId = new ChannelId(val);
                            }
                            catch
                            {
                                if (smf.ChannelTextBox != "")
                                {
                                    channelId = new ChannelId(smf.ChannelTextBox);
                                }
                                else
                                {
                                    channelId = new ChannelId();
                                }
                            }
                        }
                        var subInfo = new SubInfo();
                        subInfo.typeId = typeId;
                        subInfo.channelIdSer = new ChannelIdSerializable(channelId);
                        SubscribeMessage(subInfo);
                        if (smf.PermanentSub)
                        {
                            //Subscriptions
                            //  Settings.Sate.AddSubscription(new SubInfo(typeId, null, channelId, false, false, false));
                            Settings.Sate.AddSubscription(subInfo);
                            Settings.Save();
                        }
                    }
                }
            }
        }

        private void subscribeMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();
            long typeId;

            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;
            }
            else
            {
                return;
            }

            if (Operations.IsOfType(typeId, Entity.ClassTypeId))
            {
                SubscribeEntity(new SubInfo(typeId, null, null, true, true, true));
            }
            else if (Operations.IsOfType(typeId, Message.ClassTypeId))
            {
                var subInfo = new SubInfo();
                subInfo.typeId = typeId;
                subInfo.channelIdSer = new ChannelIdSerializable(ChannelId.ALL_CHANNELS);
                SubscribeMessage(subInfo);
                //SubscribeMessage(typeId, Safir.Dob.Typesystem.ChannelId.ALL_CHANNELS);
            }
        }

        private void subscribeRegistrationMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();
            long typeId;

            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;
            }
            else
            {
                return;
            }

            var handlerId = HandlerId.ALL_HANDLERS;

            if (Operations.IsOfType(typeId, Entity.ClassTypeId))
            {
                var subRegInfo = new SubRegInfo();
                subRegInfo.typeId = typeId;
                subRegInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                subRegInfo.includeSubClasses = true;
                subRegInfo.restartSubscription = true;
                SubscribeRegistration(subRegInfo);
                // save handlerid in node
                ((DobUnit) node).HandlerId = handlerId;
            }
            else if (Operations.IsOfType(typeId, Service.ClassTypeId))
            {
                var subRegInfo = new SubRegInfo();
                subRegInfo.typeId = typeId;
                subRegInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                subRegInfo.includeSubClasses = true;
                subRegInfo.restartSubscription = true;
                SubscribeRegistration(subRegInfo);
                // save handlerid in node
                ((DobUnit) node).HandlerId = handlerId;
            }
        }


        private void subscribeRegistrationOptionsMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();
            long typeId;

            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;

                var srf = new SubscribeRegistrationForm(typeId);
                srf.InitSubForm();

                if (Operations.IsOfType(typeId, Entity.ClassTypeId) ||
                    Operations.IsOfType(typeId, Service.ClassTypeId))
                {
                    if (srf.ShowDialog() == DialogResult.OK)
                    {
                        HandlerId handlerId;
                        // try to parse as an long. O/w use the string representation
                        try
                        {
                            var val = long.Parse(srf.HandlerIdTextBox);
                            handlerId = new HandlerId(val);
                        }
                        catch
                        {
                            if (srf.HandlerIdTextBox != "")
                            {
                                handlerId = new HandlerId(srf.HandlerIdTextBox);
                            }
                            else
                            {
                                handlerId = new HandlerId();
                            }
                        }
                        var subRegInfo = new SubRegInfo();
                        subRegInfo.typeId = typeId;
                        subRegInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                        subRegInfo.includeSubClasses = true;
                        subRegInfo.restartSubscription = true;
                        SubscribeRegistration(subRegInfo);
                        // save handlerid in node
                        ((DobUnit) node).HandlerId = handlerId;

                        if (srf.PermanentSub)
                        {
                            //Subscriptions
                            Settings.Sate.AddSubscriptionReg(new SubRegInfo(typeId,
                                new HandlerIdSerializeable(handlerId), srf.IncludeSubClassesSub, srf.RestartSub));
                            Settings.Save();
                        }
                    }
                }
            }
        }


        private void registerMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();
            long typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;
            }
            else
            {
                return;
            }
            var handlerId = new HandlerId();
            // save handlerid in node
            ((DobUnit) node).HandlerId = handlerId;

            if (Operations.IsOfType(typeId, Entity.ClassTypeId))
            {
                var regInfo = new RegInfo();
                regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                regInfo.typeId = typeId;
                regInfo.pending = false;
                regInfo.injection = false;
                regInfo.requestorDecides = false;
                RegisterEntity(regInfo);
            }
            else if (Operations.IsOfType(typeId, Service.ClassTypeId))
            {
                var regInfo = new RegInfo();
                regInfo.typeId = typeId;
                regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                regInfo.pending = false;
                RegisterService(regInfo);
            }
        }

        private void unregisterMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();
            long typeId;
            HandlerId handlerId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;
                handlerId = ((DobUnit) node).HandlerId;
                if (handlerId == null)
                {
                    handlerId = new HandlerId();
                }
            }
            else
            {
                return;
            }
            var regInfo = new RegInfo();
            regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
            regInfo.typeId = typeId;
            Unregister(regInfo);
        }

        private void registerOptionsMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();
            long typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;
                var rf = new RegisterForm(typeId);
                if (Operations.IsOfType(typeId, Entity.ClassTypeId))
                {
                    rf.InitEntitySubForm();
                }
                else
                {
                    rf.InitServiceSubForm();
                }

                if (rf.ShowDialog() == DialogResult.OK)
                {
                    HandlerId handlerId;
                    /* try to parse as an long. O/w use the string representation */
                    try
                    {
                        var val = long.Parse(rf.HandlerId);
                        handlerId = new HandlerId(val);
                    }
                    catch
                    {
                        if (rf.HandlerId != "")
                        {
                            handlerId = new HandlerId(rf.HandlerId);
                        }
                        else
                        {
                            handlerId = new HandlerId();
                        }
                    }

                    if (Operations.IsOfType(typeId, Entity.ClassTypeId))
                    {
                        var regInfo = new RegInfo();
                        regInfo.typeId = typeId;
                        regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                        regInfo.pending = rf.PendingReg;
                        regInfo.injection = rf.InjectionReg;
                        regInfo.requestorDecides = rf.RequestorDecides;
                        RegisterEntity(regInfo);
                    }
                    else if (Operations.IsOfType(typeId, Service.ClassTypeId))
                    {
                        var regInfo = new RegInfo();
                        regInfo.typeId = typeId;
                        regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                        regInfo.pending = rf.PendingReg;
                        RegisterService(regInfo);
                    }

                    if (rf.PermanentReg)
                    {
                        Settings.Sate.AddRegistration(new RegInfo(typeId, new HandlerIdSerializeable(handlerId),
                            rf.PendingReg, rf.InjectionReg, rf.RequestorDecides));
                        Settings.Save();
                    }
                }
            }
        }

        public void RegisterService(RegInfo regInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            var rec = new Action(DobAction.Register, 0, regInfo);
            ScenarioTabPage.Instance.Player.Record(rec);

            try
            {
                if (regInfo.pending)
                {
                    OutputPanel.Instance.LogEvent(
                        "- Request pending registration of service, " + regInfo.typeId + ", for handlerId " +
                        regInfo.handlerIdSer.HandlerId(), true);
                    MainForm.Instance.Dose.RegisterServiceHandlerPending(regInfo.typeId,
                        regInfo.handlerIdSer.HandlerId(), MainForm.Instance);
                    SetRegistered(regInfo.typeId);
                }
                else
                {
                    OutputPanel.Instance.LogEvent(
                        "- Request registration of service, " + regInfo.typeId + ", for handlerId " +
                        regInfo.handlerIdSer.HandlerId(), true);
                    MainForm.Instance.Dose.RegisterServiceHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId(),
                        MainForm.Instance);
                    SetRegistered(regInfo.typeId);
                }
            }
            catch (Exception excp)
            {
                MessageBox.Show("Register service failed.\n" + excp, "Register Failed", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        //--- Revoked Registration ---
        public void RevokedRegistration(RegInfo regInfo)
        {
            var name = Operations.GetName(regInfo.typeId);

            SetUnregistered(regInfo.typeId);
            OutputPanel.Instance.LogEvent(
                "- Revoked registration of handler '" + regInfo.handlerIdSer.HandlerId() + "'" + " for type " + name,
                true);

            // remove from either of these
            MainForm.Instance.requestorDecidesTypeIdList.Remove(regInfo.typeId);
            MainForm.Instance.handlerDecidesTypeIdList.Remove(regInfo.typeId);
        }

        //--- Unregister ---
        public void Unregister(RegInfo regInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.Unregister, regInfo);

            if (Operations.IsOfType(regInfo.typeId, Entity.ClassTypeId))
            {
                MainForm.Instance.Dose.UnregisterHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId());
                SetUnregistered(regInfo.typeId);
                OutputPanel.Instance.LogEvent(
                    "- Unregister registration for entity, " + regInfo.typeId + " for handler '" +
                    regInfo.handlerIdSer.HandlerId() + "'", true);
            }
            else if (Operations.IsOfType(regInfo.typeId, Service.ClassTypeId))
            {
                MainForm.Instance.Dose.UnregisterHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId());
                SetUnregistered(regInfo.typeId);
                OutputPanel.Instance.LogEvent(
                    "- Unregister registration for service, " + regInfo.typeId + " for handler '" +
                    regInfo.handlerIdSer.HandlerId() + "'", true);
            }

            // remove from either of these
            MainForm.Instance.requestorDecidesTypeIdList.Remove(regInfo.typeId);
            MainForm.Instance.handlerDecidesTypeIdList.Remove(regInfo.typeId);
        }

        //--- Subscribe ---
        public void SubscribeRegistration(SubRegInfo subRegInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            var rec = new Action(DobAction.SubscribeRegistration, 0, subRegInfo);
            ScenarioTabPage.Instance.Player.Record(rec);

            try
            {
                MainForm.Instance.Dose.SubscribeRegistration(subRegInfo.typeId, subRegInfo.handlerIdSer.HandlerId(),
                    subRegInfo.includeSubClasses, subRegInfo.restartSubscription, MainForm.Instance);
                SetSubscribed(subRegInfo.typeId, subRegInfo.handlerIdSer.HandlerId());
                OutputPanel.Instance.LogEvent(
                    "- Subscribe for registration of " + subRegInfo.typeId + " on handler '" +
                    subRegInfo.handlerIdSer.HandlerId() + "'", true);
            }
            catch (Exception excp)
            {
                MessageBox.Show("Subscribe registration failed.\n" + excp, "Subscribe Failed", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }


        public void SubscribeEntity(SubInfo subInfo) //Safir.Dob.Typesystem.EntityId entityId, bool upd, bool restart)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            var rec = new Action(DobAction.Subscribe, 0, subInfo);
            ScenarioTabPage.Instance.Player.Record(rec);

            try
            {
                if (subInfo.entityIdSer != null)
                {
                    MainForm.Instance.Dose.SubscribeEntity(subInfo.entityIdSer.EntityId(),
                        subInfo.upd, // Include updates
                        subInfo.restartSubscription,
                        MainForm.Instance);
                    SetSubscribed(subInfo.entityIdSer.EntityId());
                    OutputPanel.Instance.LogEvent("- Subscribe for entity, " + subInfo.entityIdSer.EntityId(), true);
                }
                else
                {
                    MainForm.Instance.Dose.SubscribeEntity(subInfo.typeId,
                        subInfo.upd, // Include updates
                        subInfo.includeSubClasses,
                        subInfo.restartSubscription,
                        MainForm.Instance);
                    SetSubscribed(subInfo.typeId);
                    OutputPanel.Instance.LogEvent("- Subscribe for typeId, " + subInfo.typeId, true);
                }
            }
            catch (Exception excp)
            {
                MessageBox.Show("Subscribe entity failed.\n" + excp, "Subscribe Failed", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        private void unsubscribeMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();
            long typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;
            }
            else
            {
                return;
            }
            var subInfo = new SubInfo();
            subInfo.typeId = typeId;
            if (Operations.IsOfType(typeId, Entity.ClassTypeId))
            {
                UnsubscribeEntity(subInfo);
            }
            else if (Operations.IsOfType(typeId, Message.ClassTypeId))
            {
                subInfo.channelIdSer = new ChannelIdSerializable(ChannelId.ALL_CHANNELS);
                subInfo.includeSubClasses = true;
                UnsubscribeMessage(subInfo);
            }
        }


        private void unsubscribeRegistrationMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();
            long typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;
            }
            else
            {
                return;
            }
            var subRegInfo = new SubRegInfo();
            subRegInfo.typeId = typeId;
            UnsubscribeRegistration(subRegInfo);
        }

        private void deleteRequestMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();

            if (node is ObjectNode)
            {
                var entityIdInfo = new EntityIdInfo();
                entityIdInfo.entityIdSer = new EntityIdSerializeable(((ObjectNode) node).EntityId);
                MainForm.Instance.DeleteRequest(entityIdInfo);
            }
        }

        private TreeNode GetSelectedNode()
        {
            if (treeViewClassHierarchy.Visible)
                return treeViewClassHierarchy.SelectedNode;
            if (treeViewNsHierarchy.Visible)
                return treeViewNsHierarchy.SelectedNode;
            return null;
        }

        private void contextMenu_Popup(object sender, EventArgs e)
        {
            openMenuItem.Visible = false;
            registerMenuItem.Visible = false; //reg
            registerOptionsMenuItem.Visible = false; //reg_inst
            unregisterMenuItem.Visible = false; //unreg
            separator1MenuItem.Visible = false; //sep
            subscribeMenuItem.Visible = false; //sub
            subscribeOptionsMenuItem.Visible = false; //sub_inst
            unsubscribeMenuItem.Visible = false; //unsub
            subscribeRegistrationMenuItem.Visible = false;
            subscribeRegistrationOptionsMenuItem.Visible = false;
            unsubscribeRegistrationMenuItem.Visible = false; //unsub reg
            separator2MenuItem.Visible = false; //sep
            deleteRequestMenuItem.Visible = false; //del
            separator3MenuItem.Visible = false; //sep
            iterateClassMenuItem.Visible = false; //dobRead
            dobUnitInfoMenuItem.Visible = false; //info
            viewDouFileMenuItem.Visible = false; //view
            changeImageMenuItem.Visible = true; //image

            var node = GetSelectedNode();

            long typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;
                viewDouFileMenuItem.Visible = true;
                openMenuItem.Visible = true;
            }
            else
            {
                return;
            }

            // No .dou file exists for Safir.Dob.Typesystem.Object
            if (typeId == Object.ClassTypeId)
            {
                viewDouFileMenuItem.Visible = false;
            }

            if (Operations.IsOfType(typeId, Entity.ClassTypeId))
            {
                registerMenuItem.Visible = true;
                registerOptionsMenuItem.Visible = true;
                unregisterMenuItem.Visible = true;
                separator1MenuItem.Visible = true;
                subscribeMenuItem.Visible = true;
                subscribeOptionsMenuItem.Visible = true;
                unsubscribeMenuItem.Visible = true;
                subscribeRegistrationMenuItem.Visible = true;
                subscribeRegistrationOptionsMenuItem.Visible = true;
                unsubscribeRegistrationMenuItem.Visible = true;
                separator3MenuItem.Visible = true;
                iterateClassMenuItem.Visible = true;
                if (node is ObjectNode)
                {
                    separator2MenuItem.Visible = true;
                    deleteRequestMenuItem.Visible = true;
                    iterateClassMenuItem.Visible = false;
                    changeImageMenuItem.Visible = false;
                }
            }
            else if (Operations.IsOfType(typeId, Message.ClassTypeId))
            {
                subscribeMenuItem.Visible = true;
                subscribeOptionsMenuItem.Visible = true;
                unsubscribeMenuItem.Visible = true;
                separator3MenuItem.Visible = true;
            }
            else if (Operations.IsOfType(typeId, Service.ClassTypeId))
            {
                registerMenuItem.Visible = true;
                registerOptionsMenuItem.Visible = true;
                unregisterMenuItem.Visible = true;
                separator1MenuItem.Visible = true;
                subscribeRegistrationMenuItem.Visible = true;
                subscribeRegistrationOptionsMenuItem.Visible = true;
                unsubscribeRegistrationMenuItem.Visible = true;
                separator3MenuItem.Visible = true;
            }
        }

        public void SubscribeMessage(SubInfo subInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            var rec = new Action(DobAction.Subscribe, 0, subInfo);
            ScenarioTabPage.Instance.Player.Record(rec);

            try
            {
                MainForm.Instance.Dose.SubscribeMessage(subInfo.typeId, subInfo.channelIdSer.ChannelId(),
                    subInfo.includeSubClasses, MainForm.Instance);
                SetSubscribed(subInfo.typeId, subInfo.channelIdSer.ChannelId());
                OutputPanel.Instance.LogEvent(
                    "- Subscribe for message, " + subInfo.typeId + ", on channel '" + subInfo.channelIdSer.ChannelId() +
                    "'", true);
            }
            catch (Exception excp)
            {
                MessageBox.Show("Subscribe message failed.\n" + excp, "Subscribe Failed", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        //--- Unsubscribe ---
        public void UnsubscribeEntity(SubInfo subInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.Unsubscribe, subInfo);

            if (Operations.IsOfType(subInfo.typeId, Entity.ClassTypeId))
            {
                MainForm.Instance.Dose.UnsubscribeEntity(subInfo.typeId, MainForm.Instance);
                SetUnsubscribed(subInfo.typeId);
                OutputPanel.Instance.LogEvent("- Unsubscribe entity, " + subInfo.typeId, true);
            }
        }

        public void UnsubscribeMessage(SubInfo subInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.Unsubscribe, subInfo);

            if (Operations.IsOfType(subInfo.typeId, Message.ClassTypeId))
            {
                MainForm.Instance.Dose.UnsubscribeMessage(subInfo.typeId, subInfo.channelIdSer.ChannelId(),
                    subInfo.includeSubClasses, MainForm.Instance);
                SetUnsubscribed(subInfo.typeId);
                OutputPanel.Instance.LogEvent(
                    "- Unsubscribe message, " + subInfo.typeId + ", on channel '" + subInfo.channelIdSer.ChannelId() +
                    "'", true);
            }
        }


        public void UnsubscribeRegistration(SubRegInfo subRegInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(DobAction.UnsubscribeRegistration, subRegInfo);

            if (Operations.IsOfType(subRegInfo.typeId, Entity.ClassTypeId))
            {
                MainForm.Instance.Dose.UnsubscribeRegistration(subRegInfo.typeId, HandlerId.ALL_HANDLERS, true,
                    MainForm.Instance);
                SetUnsubscribed(subRegInfo.typeId);
                OutputPanel.Instance.LogEvent("- Unsubscribe entity, " + subRegInfo.typeId, true);
            }
            else if (Operations.IsOfType(subRegInfo.typeId, Service.ClassTypeId))
            {
                MainForm.Instance.Dose.UnsubscribeRegistration(subRegInfo.typeId, HandlerId.ALL_HANDLERS, true,
                    MainForm.Instance);
                SetUnsubscribed(subRegInfo.typeId);
                OutputPanel.Instance.LogEvent("- Unsubscribe service registration, " + subRegInfo.typeId, true);
            }
        }


        private void iterateClassMenuItem_Click(object sender, EventArgs e)
        {
            var n = GetSelectedNode();
            if (n is ClassNode)
            {
                var typeId = ((ClassNode) n).TypeId;
                new IterateClassForm(typeId).ShowDialog();
            }
        }


        private void openMenuItem_Click(object sender, EventArgs e)
        {
            var node = GetSelectedNode();

            if (node is ObjectNode)
            {
                try
                {
                    var n = (ObjectNode) node;
                    using (var entityProxy = MainForm.Instance.Dose.Read(n.EntityId))
                    {
                        var entityInfo = new EntityInfo();
                        entityInfo.Obj = entityProxy.Entity;
                        entityInfo.SetInstanceId(entityProxy.InstanceId);
                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                }
                catch
                {
                }
            }
            else if (node is ClassNode)
            {
                var n = (ClassNode) node;
                var objInfo = new ObjectInfo();
                if (n == treeViewClassHierarchy.Nodes[0] ||
                    n == treeViewNsHierarchy.Nodes[0]) //root node is object
                {
                    objInfo.Obj = new Object();
                }
                else
                {
                    var dobType = MainForm.Instance.GetType(n.TypeId);
                    var constr = dobType.GetConstructor(Type.EmptyTypes);
                    objInfo.Obj = (Object) constr.Invoke(null);
                }

                MainForm.Instance.AddTabPage(new ObjectEditTabPage(objInfo));
            }
        }

        private void viewDouFileMenuItem_Click(object sender, EventArgs e)
        {
            var node = (DobUnit) GetSelectedNode();
            var fileName = InternalOperations.GetDouFilePath(node.TypeId);

            using (TextReader reader = new StreamReader(fileName))
            {
                var content = reader.ReadToEnd();
                reader.Close();

                MainForm.Instance.AddTabPage(new XmlTabPage(content, fileName));
            }
        }

        //------------------------------------------------------
        // Change icons in treeView
        //------------------------------------------------------
        public void AddObject(EntityId entityId)
        {
            var addNew = true;
            var typeId = entityId.TypeId;

            //class hierarchy
            var root = (ClassNode) clTypeIdHt[typeId];
            for (var i = 0; i < root.Nodes.Count; i++)
            {
                if (root.Nodes[i] is ObjectNode)
                {
                    // if object already exists, don't add it again
                    // sorting is handled in the container
                    if (((ObjectNode) root.Nodes[i]).EntityId.InstanceId.CompareTo(entityId.InstanceId) == 0)
                    {
                        addNew = false;
                        break;
                    }
                }
            }
            if (addNew)
            {
                root.Nodes.Add(new ObjectNode(entityId));
            }

            //namespace hierarchy
            root = (ClassNode) nsTypeIdHt[typeId];
            for (var i = 0; i < root.Nodes.Count; i++)
            {
                if (root.Nodes[i] is ObjectNode)
                {
                    // if object already exists, don't add it again
                    // sorting is handled in the container
                    if (((ObjectNode) root.Nodes[i]).EntityId.InstanceId.CompareTo(entityId.InstanceId) == 0)
                    {
                        addNew = false;
                        break;
                    }
                }
            }
            if (addNew)
            {
                root.Nodes.Add(new ObjectNode(entityId));
            }
        }

        public void DeleteObject(EntityId entityId)
        {
            //class hierarchy
            var root = (ClassNode) clTypeIdHt[entityId.TypeId];
            foreach (TreeNode n in root.Nodes)
            {
                if (n is ObjectNode)
                {
                    if (((ObjectNode) n).EntityId.InstanceId.CompareTo(entityId.InstanceId) == 0)
                    {
                        root.Nodes.Remove(n);
                        break;
                    }
                }
            }

            //namespace hierarchy
            root = (ClassNode) nsTypeIdHt[entityId.TypeId];
            foreach (TreeNode n in root.Nodes)
            {
                if (n is ObjectNode)
                {
                    if (((ObjectNode) n).EntityId.InstanceId.ToString().CompareTo(entityId.InstanceId.ToString()) == 0)
                    {
                        root.Nodes.Remove(n);
                        break;
                    }
                }
            }
        }

        //This application requests to be the owner, i.e pending registration
        public void SetPending(long typeId)
        {
            var cnode = (DobUnit) clTypeIdHt[typeId];
            var nnode = (DobUnit) nsTypeIdHt[typeId];
            switch (cnode.ImageType)
            {
                case TreeViewImageHandler.ImageType.Subscribed:
                case TreeViewImageHandler.ImageType.RegisteredSubscribed:
                case TreeViewImageHandler.ImageType.PendingSubscribed:
                case TreeViewImageHandler.ImageType.OwnedSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.PendingSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.PendingSubscribed;
                    break;
                default:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Pending;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Pending;
                    break;
            }
            cnode.ImageIndex = imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        //This application becomes the owner
        public void SetOwned(long typeId)
        {
            var cnode = (DobUnit) clTypeIdHt[typeId];
            var nnode = (DobUnit) nsTypeIdHt[typeId];
            switch (cnode.ImageType)
            {
                case TreeViewImageHandler.ImageType.Subscribed:
                case TreeViewImageHandler.ImageType.RegisteredSubscribed:
                case TreeViewImageHandler.ImageType.PendingSubscribed:
                case TreeViewImageHandler.ImageType.OwnedSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    break;
                default:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Owned;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Owned;
                    break;
            }
            cnode.ImageIndex = imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        //Registration received via subscription, other application
        public void SetRegistered(long typeId)
        {
            var cnode = (DobUnit) clTypeIdHt[typeId];
            var nnode = (DobUnit) nsTypeIdHt[typeId];
            switch (cnode.ImageType)
            {
                case TreeViewImageHandler.ImageType.Owned:
                case TreeViewImageHandler.ImageType.OwnedSubscribed:
                    break;
                case TreeViewImageHandler.ImageType.Subscribed:
                case TreeViewImageHandler.ImageType.RegisteredSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    break;
                default:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Registered;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Registered;
                    break;
            }
            cnode.ImageIndex = imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        public void SetUnregistered(long typeId)
        {
            var cnode = (DobUnit) clTypeIdHt[typeId];
            var nnode = (DobUnit) nsTypeIdHt[typeId];
            switch (cnode.ImageType)
            {
                case TreeViewImageHandler.ImageType.Subscribed:
                case TreeViewImageHandler.ImageType.RegisteredSubscribed:
                case TreeViewImageHandler.ImageType.OwnedSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    break;
                default:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Default;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Default;
                    break;
            }
            cnode.ImageIndex = imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }


        private void SetSubscribed(long typeId)
        {
            var cnode = (DobUnit) clTypeIdHt[typeId];
            var nnode = (DobUnit) nsTypeIdHt[typeId];
            switch (cnode.ImageType)
            {
                case TreeViewImageHandler.ImageType.Owned:
                case TreeViewImageHandler.ImageType.OwnedSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    break;
                case TreeViewImageHandler.ImageType.Registered:
                case TreeViewImageHandler.ImageType.RegisteredSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    break;
                default:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    break;
            }
            cnode.ImageIndex = imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        private void SetSubscribed(EntityId entityId)
        {
            var cnode = (DobUnit) clTypeIdHt[entityId.TypeId];
            var nnode = (DobUnit) nsTypeIdHt[entityId.TypeId];
            switch (cnode.ImageType)
            {
                case TreeViewImageHandler.ImageType.Owned:
                case TreeViewImageHandler.ImageType.OwnedSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    break;
                case TreeViewImageHandler.ImageType.Registered:
                case TreeViewImageHandler.ImageType.RegisteredSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    break;
                default:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    break;
            }
            cnode.ImageIndex = imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        private void SetSubscribed(long typeId, ChannelId channelId)
        {
            var cnode = (DobUnit) clTypeIdHt[typeId];
            var nnode = (DobUnit) nsTypeIdHt[typeId];
            switch (cnode.ImageType)
            {
                case TreeViewImageHandler.ImageType.Owned:
                case TreeViewImageHandler.ImageType.OwnedSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    break;
                case TreeViewImageHandler.ImageType.Registered:
                case TreeViewImageHandler.ImageType.RegisteredSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    break;
                default:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    break;
            }
            cnode.ImageIndex = imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        private void SetSubscribed(long typeId, HandlerId handlerId)
        {
            var cnode = (DobUnit) clTypeIdHt[typeId];
            var nnode = (DobUnit) nsTypeIdHt[typeId];
            switch (cnode.ImageType)
            {
                case TreeViewImageHandler.ImageType.Owned:
                case TreeViewImageHandler.ImageType.OwnedSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.OwnedSubscribed;
                    break;
                case TreeViewImageHandler.ImageType.Registered:
                case TreeViewImageHandler.ImageType.RegisteredSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.RegisteredSubscribed;
                    break;
                default:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Subscribed;
                    break;
            }
            cnode.ImageIndex = imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        private void SetUnsubscribed(long typeId)
        {
            var cnode = (DobUnit) clTypeIdHt[typeId];
            var nnode = (DobUnit) nsTypeIdHt[typeId];
            switch (cnode.ImageType)
            {
                case TreeViewImageHandler.ImageType.Owned:
                case TreeViewImageHandler.ImageType.OwnedSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Owned;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Owned;
                    break;
                case TreeViewImageHandler.ImageType.Registered:
                case TreeViewImageHandler.ImageType.RegisteredSubscribed:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Registered;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Registered;
                    break;
                default:
                    cnode.ImageType = TreeViewImageHandler.ImageType.Default;
                    nnode.ImageType = TreeViewImageHandler.ImageType.Default;
                    break;
            }
            cnode.ImageIndex = imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        public void LocateClass(long typeId)
        {
            tabControl.SelectedIndex = 0;
            treeViewClassHierarchy.Focus();
            treeViewClassHierarchy.SelectedNode = (ClassNode) clTypeIdHt[typeId];
        }


        //--- Register ---
        public void RegisterEntity(RegInfo regInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            var rec = new Action(DobAction.Register, 0, regInfo);
            ScenarioTabPage.Instance.Player.Record(rec);

            try
            {
                if (regInfo.injection)
                {
                    OutputPanel.Instance.LogEvent(
                        "- Request injection registration of entity, " + regInfo.typeId + " on handlerId '" +
                        regInfo.handlerIdSer.HandlerId() + "'", true);
                    if (regInfo.requestorDecides)
                    {
                        MainForm.Instance.Dose.RegisterEntityHandlerInjection(regInfo.typeId,
                            regInfo.handlerIdSer.HandlerId(), InstanceIdPolicy.Enumeration.RequestorDecidesInstanceId,
                            MainForm.Instance);
                        MainForm.Instance.requestorDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    else
                    {
                        MainForm.Instance.Dose.RegisterEntityHandlerInjection(regInfo.typeId,
                            regInfo.handlerIdSer.HandlerId(), InstanceIdPolicy.Enumeration.HandlerDecidesInstanceId,
                            MainForm.Instance);
                        MainForm.Instance.handlerDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    SetRegistered(regInfo.typeId);
                }
                else if (regInfo.pending)
                {
                    OutputPanel.Instance.LogEvent(
                        "- Request pending registration of entity, " + regInfo.typeId + " on handlerId " +
                        regInfo.handlerIdSer.HandlerId(), true);
                    if (regInfo.requestorDecides)
                    {
                        MainForm.Instance.Dose.RegisterEntityHandlerPending(regInfo.typeId,
                            regInfo.handlerIdSer.HandlerId(), InstanceIdPolicy.Enumeration.RequestorDecidesInstanceId,
                            MainForm.Instance);
                        MainForm.Instance.requestorDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    else
                    {
                        MainForm.Instance.Dose.RegisterEntityHandlerPending(regInfo.typeId,
                            regInfo.handlerIdSer.HandlerId(), InstanceIdPolicy.Enumeration.HandlerDecidesInstanceId,
                            MainForm.Instance);
                        MainForm.Instance.handlerDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    SetPending(regInfo.typeId);
                }
                else
                {
                    OutputPanel.Instance.LogEvent(
                        "- Request registration of entity, " + regInfo.typeId + " on handlerId '" +
                        regInfo.handlerIdSer.HandlerId() + "'", true);
                    if (regInfo.requestorDecides)
                    {
                        MainForm.Instance.Dose.RegisterEntityHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId(),
                            InstanceIdPolicy.Enumeration.RequestorDecidesInstanceId, MainForm.Instance);
                        MainForm.Instance.requestorDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    else
                    {
                        MainForm.Instance.Dose.RegisterEntityHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId(),
                            InstanceIdPolicy.Enumeration.HandlerDecidesInstanceId, MainForm.Instance);
                        MainForm.Instance.handlerDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    SetRegistered(regInfo.typeId);
                }
            }
            catch (Exception excp)
            {
                MessageBox.Show("Register entity failed.\n" + excp, "Register Failed", MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
            }
        }

        private class NamespaceNode : TreeNode
        {
            public readonly string FullName;

            public NamespaceNode(string shortName, string fullName)
            {
                FullName = fullName;
                Text = shortName;
                ImageIndex = imageHandler.NamespaceImageIndex;
                SelectedImageIndex = ImageIndex;
            }
        }


        private class DobUnit : TreeNode
        {
            public EntityId EntityId;
            public HandlerId HandlerId;
            public TreeViewImageHandler.ImageType ImageType = TreeViewImageHandler.ImageType.Default;

            public long TypeId;
        }

        private class ClassNode : DobUnit
        {
            public readonly Type DobType;

            public ClassNode(Type t, long typeId, bool fullName)
            {
                TypeId = typeId;
                DobType = t;

                if (fullName)
                    Text = t.FullName;
                else
                    Text = t.Name;
                ImageIndex = imageHandler.GetImageIndex(typeId, TreeViewImageHandler.ImageType.Default);
                SelectedImageIndex = ImageIndex;
            }
        }

        private class ObjectNode : DobUnit
        {
            public ObjectNode(EntityId entityId)
            {
                TypeId = entityId.TypeId;
                EntityId = entityId;
                ImageIndex = imageHandler.GetImageIndex(entityId, TreeViewImageHandler.ImageType.Default);
                SelectedImageIndex = ImageIndex;
                Text = entityId.InstanceId.ToString();
            }
        }

        private class TreeSorter : IComparer
        {
            public int Compare(object a, object b)
            {
                try
                {
                    if (a is ObjectNode && b is ObjectNode)
                    {
                        return long.Parse(((ObjectNode) a).Text).CompareTo(long.Parse(((ObjectNode) b).Text));
                    }
                    var n1 = (TreeNode) a;
                    var n2 = (TreeNode) b;
                    return n1.Text.CompareTo(n2.Text);
                }
                catch
                {
                }

                return 0;
            }
        }
    }
}