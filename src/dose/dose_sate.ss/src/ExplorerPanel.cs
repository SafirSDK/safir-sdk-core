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
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
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
        private static TreeViewImageHandler _imageHandler;

        private static ExplorerPanel _instance;
        private readonly MenuItem _changeImageMenuItem;

        // context menus
        private readonly MenuItem _deleteRequestMenuItem;
        private readonly MenuItem _dobUnitInfoMenuItem;
        private readonly MenuItem _iterateClassMenuItem;
        private readonly MenuItem _openMenuItem;
        private readonly MenuItem _registerMenuItem;
        private readonly MenuItem _registerOptionsMenuItem;
        private readonly MenuItem _separator1MenuItem;
        private readonly MenuItem _separator2MenuItem;
        private readonly MenuItem _separator3MenuItem;
        private readonly MenuItem _subscribeMenuItem;
        private readonly MenuItem _subscribeOptionsMenuItem;
        private readonly MenuItem _subscribeRegistrationMenuItem;
        private readonly MenuItem _subscribeRegistrationOptionsMenuItem;
        private readonly TabControl _tabControl = new TabControl();
        private readonly TabPage _tabInheritance = new TabPage("Inheritance");
        private readonly TabPage _tabNamespace = new TabPage("Namespace");
        private readonly TreeView _treeViewClassHierarchy;
        private readonly TreeView _treeViewNsHierarchy;
        private readonly MenuItem _unregisterMenuItem;
        private readonly MenuItem _unsubscribeMenuItem;
        private readonly MenuItem _unsubscribeRegistrationMenuItem;
        private readonly MenuItem _viewDouFileMenuItem;
        private readonly MenuItem _openInExternalMenuItem;
        private Hashtable _clTypeIdHt;
        private IContainer components;
        private ImageList _defaultImagesList;
        private Hashtable _dobTypeHt;
        private Panel _fillpanel;
        private ImageList _imageList;
        private Hashtable _nsHt;
        private Hashtable _nsTypeIdHt;
        private Panel _toppanel;


        private ExplorerPanel()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            SuspendLayout();

            //Load images for the treeview
            _imageHandler = new TreeViewImageHandler(_imageList, _defaultImagesList);
            _imageHandler.RefreshImageCollection();

            //context menu setup
            _openMenuItem = new MenuItem("Open");
            _registerMenuItem = new MenuItem("Register handler");
            _registerOptionsMenuItem = new MenuItem("Register handler with options...");
            _unregisterMenuItem = new MenuItem("Unregister handler");
            _separator1MenuItem = new MenuItem("-");
            _subscribeMenuItem = new MenuItem("Subscribe");
            _subscribeOptionsMenuItem = new MenuItem("Subscribe with options...");
            _unsubscribeMenuItem = new MenuItem("Unsubscribe");
            _subscribeRegistrationMenuItem = new MenuItem("Subscribe for registrations");
            _subscribeRegistrationOptionsMenuItem = new MenuItem("Subscribe for registrations with options...");
            _unsubscribeRegistrationMenuItem = new MenuItem("Unsubscribe registrations");
            _separator2MenuItem = new MenuItem("-");
            _deleteRequestMenuItem = new MenuItem("Send delete request");
            _separator3MenuItem = new MenuItem("-");
            _iterateClassMenuItem = new MenuItem("Iterate class...");
            _changeImageMenuItem = new MenuItem("Change image...");
            _dobUnitInfoMenuItem = new MenuItem("View details...");
            _viewDouFileMenuItem = new MenuItem("View dou-file...");

            _openInExternalMenuItem = new MenuItem("Open in external program");
            UpdateExternalProgramsSubmenu();
            _openMenuItem.Click += openMenuItem_Click;

            _registerMenuItem.Click += registerMenuItem_Click;
            _registerOptionsMenuItem.Click += registerOptionsMenuItem_Click;
            _unregisterMenuItem.Click += unregisterMenuItem_Click;

            _subscribeMenuItem.Click += subscribeMenuItem_Click;
            _subscribeOptionsMenuItem.Click += subscribeOptionsMenuItem_Click;
            _unsubscribeMenuItem.Click += unsubscribeMenuItem_Click;

            _subscribeRegistrationMenuItem.Click += subscribeRegistrationMenuItem_Click;
            _subscribeRegistrationOptionsMenuItem.Click += subscribeRegistrationOptionsMenuItem_Click;
            _unsubscribeRegistrationMenuItem.Click += unsubscribeRegistrationMenuItem_Click;

            _deleteRequestMenuItem.Click += deleteRequestMenuItem_Click;

            _iterateClassMenuItem.Click += iterateClassMenuItem_Click;

            _changeImageMenuItem.Click += changeImageMenuItem_Click;
            _dobUnitInfoMenuItem.Click += dobUnitInfoMenuItem_Click;
            _viewDouFileMenuItem.Click += viewDouFileMenuItem_Click;

            var contextMenu = new ContextMenu(new[]
            {
                _openMenuItem,
                _registerMenuItem,
                _registerOptionsMenuItem,
                _unregisterMenuItem,
                _separator1MenuItem,
                _subscribeMenuItem,
                _subscribeOptionsMenuItem,
                _unsubscribeMenuItem,
                _subscribeRegistrationMenuItem,
                _subscribeRegistrationOptionsMenuItem,
                _unsubscribeRegistrationMenuItem,
                _separator2MenuItem,
                _deleteRequestMenuItem,
                _separator3MenuItem,
                _iterateClassMenuItem,
                _dobUnitInfoMenuItem,
                _viewDouFileMenuItem,
                _openInExternalMenuItem,
                _changeImageMenuItem
            });

            contextMenu.Popup += contextMenu_Popup;

            //treeViewClassHierarchy
            _treeViewClassHierarchy = new TreeView();
            _treeViewClassHierarchy.ContextMenu = contextMenu;
            _treeViewClassHierarchy.Location = new Point(0, 40);
            _treeViewClassHierarchy.Dock = DockStyle.Fill;
            _treeViewClassHierarchy.ImageList = _imageList;

            _treeViewClassHierarchy.DoubleClick += treeViewClassHierarchy_DoubleClick;
            _treeViewClassHierarchy.MouseDown += treeViewClassHierarchy_MouseDown;
            _treeViewClassHierarchy.ItemDrag += treeViewClassHierarchy_ItemDrag;
            _treeViewClassHierarchy.TreeViewNodeSorter = new TreeSorter();

            //treeViewNsHierarchy
            _treeViewNsHierarchy = new TreeView();
            _treeViewNsHierarchy.ContextMenu = contextMenu;
            _treeViewNsHierarchy.Location = new Point(0, 40);
            _treeViewNsHierarchy.Dock = DockStyle.Fill;
            _treeViewNsHierarchy.ImageList = _imageList;
            _treeViewNsHierarchy.DoubleClick += treeViewNsHierarchy_DoubleClick;
            _treeViewNsHierarchy.MouseDown += treeViewNsHierarchy_MouseDown;
            _treeViewNsHierarchy.ItemDrag += treeViewNsHierarchy_ItemDrag;
            _treeViewNsHierarchy.TreeViewNodeSorter = new TreeSorter();

            //Tabs
            _tabControl.Dock = DockStyle.Fill;
            _tabInheritance.Controls.Add(_treeViewClassHierarchy);
            _tabNamespace.Controls.Add(_treeViewNsHierarchy);
            _fillpanel.Controls.Add(_tabControl);
            _tabControl.TabPages.AddRange(new[] { _tabInheritance, _tabNamespace });
            if (Settings.Sate.DefaultExplorerView)
                _tabControl.SelectedIndex = 0;
            else
                _tabControl.SelectedIndex = 1;
            _tabControl.SelectedIndex = 0;

            //TitleBar
            var titleLabel = new PanelLabelControl("Explorer");
            titleLabel.CloseEvent += titleLabel_CloseEvent;
            titleLabel.Height = 15;
            titleLabel.Dock = DockStyle.Top;
            _toppanel.Controls.Add(titleLabel);

            ResumeLayout(false);
        }
        

        public static ExplorerPanel Instance
        {
            get
            {
                if (_instance == null)
                    _instance = new ExplorerPanel();
                return _instance;
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

            var customImageForm = new CustomImageForm(_imageHandler, mappingName);
            customImageForm.ShowDialog();
            if (customImageForm.IsDirty)
            {
                UpdateAllImageIndices();
            }
        }

        private void UpdateAllImageIndices()
        {
            _imageHandler.RefreshImageCollection();

            //always go through every node is not the most efficient way but it's a simple
            //way to get correct image indices.
            foreach (DictionaryEntry de in _clTypeIdHt)
            {
                var tmp = de.Value as DobUnit;
                tmp.ImageIndex = _imageHandler.GetImageIndex(tmp.TypeId, tmp.ImageType);
                tmp.SelectedImageIndex = tmp.ImageIndex;
            }

            foreach (DictionaryEntry de in _nsTypeIdHt)
            {
                var tmp = de.Value as DobUnit;
                tmp.ImageIndex = _imageHandler.GetImageIndex(tmp.TypeId, tmp.ImageType);
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
            if (disposing && components != null)
            {
                components.Dispose();
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
            this._imageList = new System.Windows.Forms.ImageList(this.components);
            this._toppanel = new System.Windows.Forms.Panel();
            this._fillpanel = new System.Windows.Forms.Panel();
            this._defaultImagesList = new System.Windows.Forms.ImageList(this.components);
            this.SuspendLayout();
            //
            // imageList
            //
            this._imageList.ColorDepth = System.Windows.Forms.ColorDepth.Depth8Bit;
            this._imageList.ImageSize = new System.Drawing.Size(16, 16);
            this._imageList.TransparentColor = System.Drawing.Color.Transparent;
            //
            // toppanel
            //
            this._toppanel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this._toppanel.Dock = System.Windows.Forms.DockStyle.Top;
            this._toppanel.Location = new System.Drawing.Point(0, 0);
            this._toppanel.Name = "_toppanel";
            this._toppanel.Size = new System.Drawing.Size(224, 15);
            this._toppanel.TabIndex = 0;
            //
            // fillpanel
            //
            this._fillpanel.BackColor = System.Drawing.SystemColors.Control;
            this._fillpanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this._fillpanel.ForeColor = System.Drawing.SystemColors.Control;
            this._fillpanel.Location = new System.Drawing.Point(0, 0);
            this._fillpanel.Name = "_fillpanel";
            this._fillpanel.Padding = new System.Windows.Forms.Padding(0, 20, 0, 0);
            this._fillpanel.Size = new System.Drawing.Size(224, 504);
            this._fillpanel.TabIndex = 0;
            //
            // defaultImagesList
            //
            this._defaultImagesList.ImageStream =
                ((System.Windows.Forms.ImageListStreamer) (resources.GetObject("_defaultImagesList.ImageStream")));
            this._defaultImagesList.TransparentColor = System.Drawing.Color.Transparent;
            this._defaultImagesList.Images.SetKeyName(0, "package.bmp");
            this._defaultImagesList.Images.SetKeyName(1, "entity_default.bmp");
            this._defaultImagesList.Images.SetKeyName(2, "service_default.bmp");
            this._defaultImagesList.Images.SetKeyName(3, "object_default.bmp");
            this._defaultImagesList.Images.SetKeyName(4, "message_default.bmp");
            this._defaultImagesList.Images.SetKeyName(5, "response.bmp");
            this._defaultImagesList.Images.SetKeyName(6, "item.bmp");
            this._defaultImagesList.Images.SetKeyName(7, "struct.bmp");
            this._defaultImagesList.Images.SetKeyName(8, "parameters.bmp");
            this._defaultImagesList.Images.SetKeyName(9, "class_default.bmp");
            //
            // ExplorerPanel
            //
            this.Controls.Add(this._toppanel);
            this.Controls.Add(this._fillpanel);
            this.Size = new System.Drawing.Size(224, 504);
            this.Text = "Class Explorer";
            this.ResumeLayout(false);
        }

        #endregion

        public void LoadClassHierarchy()
        {
            _treeViewClassHierarchy.BeginUpdate();
            _treeViewNsHierarchy.BeginUpdate();

            _treeViewClassHierarchy.Nodes.Clear();
            _treeViewNsHierarchy.Nodes.Clear();

            var objType = typeof(Object);
            var objNode = new ClassNode(objType, Object.ClassTypeId, true);
            objNode.Expand();
            _dobTypeHt = new Hashtable();
            _dobTypeHt[objType] = objNode;
            _clTypeIdHt = new Hashtable();
            _clTypeIdHt[GetTypeId(objType)] = objNode;
            _nsTypeIdHt = new Hashtable();
            _nsHt = new Hashtable();

            InsertClassInNsHierarchy(objType, Object.ClassTypeId);
            foreach (var tid in Operations.GetAllTypeIds())
            {
                if (!Operations.IsClass(tid))
                {
                    continue;
                }

                Type type;
                try
                {
                    type = MainForm.Instance.GetType(tid);
                }
                catch (Safir.Dob.Typesystem.IllegalValueException)
                {
                    /* this is to handle the case where a type is in dots_kernel, but has
                       not been built. I.e. a parameter class */
                    continue;
                }
                if (type.IsSubclassOf(objType))
                {
                    ClassNode node = null;
                    while (type != objType && _dobTypeHt[type] == null)
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

                        _clTypeIdHt[typeId] = node;
                        _dobTypeHt[type] = node;
                        type = type.BaseType;
                    }

                    if (node != null)
                    {
                        ((ClassNode) _dobTypeHt[node.DobType.BaseType]).Nodes.Add(node);
                    }
                }
            }

            _treeViewClassHierarchy.Nodes.Add(objNode);

            _treeViewClassHierarchy.Sort();
            _treeViewNsHierarchy.Sort();

            _treeViewNsHierarchy.EndUpdate();
            _treeViewClassHierarchy.EndUpdate();
            _treeViewNsHierarchy.HideSelection = false;
            _treeViewClassHierarchy.HideSelection = false;
        }

        private void InsertNamespace(Type type)
        {
            NamespaceNode node = null;
            var ns = type.Namespace.Split('.');
            var rest = type.Namespace + ".";
            for (var i = ns.Length - 1; i >= 0; i--)
            {
                rest = rest.Substring(0, rest.LastIndexOf('.'));
                if (_nsHt[rest] == null)
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
                    _nsHt[rest] = node;
                }
                else
                {
                    if (node != null)
                    {
                        var parent = (NamespaceNode) _nsHt[rest];

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
            _nsHt[rest] = node;
            _treeViewNsHierarchy.Nodes.Add(node);
        }

        private void InsertClassInNsHierarchy(Type type, long typeId)
        {
            InsertNamespace(type);
            var classNode = new ClassNode(type, typeId, false);
            _nsTypeIdHt[typeId] = classNode;
            ((NamespaceNode) _nsHt[type.Namespace]).Nodes.Add(classNode);
        }

        private void treeViewClassHierarchy_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right)
            {
                var clickedNode = _treeViewClassHierarchy.GetNodeAt(e.X, e.Y);
                if (clickedNode != null)
                {
                    _treeViewClassHierarchy.SelectedNode = clickedNode;
                }
            }
        }


        private void treeViewClassHierarchy_DoubleClick(object sender, EventArgs e)
        {
            Cursor = Cursors.WaitCursor;
            if (_treeViewClassHierarchy.SelectedNode is ObjectNode)
            {
                var n = (ObjectNode) _treeViewClassHierarchy.SelectedNode;

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
            else if (_treeViewClassHierarchy.SelectedNode is ClassNode)
            {
                var n = (ClassNode) _treeViewClassHierarchy.SelectedNode;
                var objInfo = new ObjectInfo();
                if (n == _treeViewClassHierarchy.Nodes[0]) //root node is object
                {
                    objInfo.Obj = new Object();
                }
                else
                {
                    var dobType = MainForm.Instance.GetType(n.TypeId);

                    if (dobType.IsSubclassOf(typeof(Message)))
                    {
                        var msgInfo = new MessageInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        msgInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(msgInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Service)))
                    {
                        var srvInfo = new ServiceHandlerInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        srvInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(srvInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Entity)))
                    {
                        var entityInfo = new EntityInfo();

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
                var clickedNode = _treeViewNsHierarchy.GetNodeAt(e.X, e.Y);
                if (clickedNode != null)
                {
                    _treeViewNsHierarchy.SelectedNode = clickedNode;
                }
            }
        }

        private void treeViewNsHierarchy_DoubleClick(object sender, EventArgs e)
        {
            Cursor = Cursors.WaitCursor;
            if (_treeViewNsHierarchy.SelectedNode is ObjectNode)
            {
                var n = (ObjectNode) _treeViewNsHierarchy.SelectedNode;

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
            else if (_treeViewNsHierarchy.SelectedNode is ClassNode)
            {
                var n = (ClassNode) _treeViewNsHierarchy.SelectedNode;
                var objInfo = new ObjectInfo();

                if (n == _treeViewNsHierarchy.Nodes[0])
                {
                    objInfo.Obj = new Object();
                }
                else
                {
                    var dobType = MainForm.Instance.GetType(n.TypeId);

                    if (dobType.IsSubclassOf(typeof(Message)))
                    {
                        var msgInfo = new MessageInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        msgInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(msgInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Service)))
                    {
                        var srvInfo = new ServiceHandlerInfo();

                        var constr = dobType.GetConstructor(Type.EmptyTypes);
                        srvInfo.Obj = (Object) constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(srvInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Entity)))
                    {
                        var entityInfo = new EntityInfo();

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
                        var subInfo = new SubInfo
                        {
                            typeId = typeId,
                            channelIdSer = new ChannelIdSerializable(channelId)
                        };
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
                SetSubscribed(subRegInfo.typeId);
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
            if (_treeViewClassHierarchy.Visible)
                return _treeViewClassHierarchy.SelectedNode;
            if (_treeViewNsHierarchy.Visible)
                return _treeViewNsHierarchy.SelectedNode;
            return null;
        }

        private void contextMenu_Popup(object sender, EventArgs e)
        {
            _openMenuItem.Visible = false;
            _registerMenuItem.Visible = false; //reg
            _registerOptionsMenuItem.Visible = false; //reg_inst
            _unregisterMenuItem.Visible = false; //unreg
            _separator1MenuItem.Visible = false; //sep
            _subscribeMenuItem.Visible = false; //sub
            _subscribeOptionsMenuItem.Visible = false; //sub_inst
            _unsubscribeMenuItem.Visible = false; //unsub
            _subscribeRegistrationMenuItem.Visible = false;
            _subscribeRegistrationOptionsMenuItem.Visible = false;
            _unsubscribeRegistrationMenuItem.Visible = false; //unsub reg
            _separator2MenuItem.Visible = false; //sep
            _deleteRequestMenuItem.Visible = false; //del
            _separator3MenuItem.Visible = false; //sep
            _iterateClassMenuItem.Visible = false; //dobRead
            _dobUnitInfoMenuItem.Visible = false; //info
            _viewDouFileMenuItem.Visible = false; //view
            _openInExternalMenuItem.Visible = false; //external
            _changeImageMenuItem.Visible = true; //image

            var node = GetSelectedNode();

            long typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit) node).TypeId;
                _viewDouFileMenuItem.Visible = true;
                _openInExternalMenuItem.Visible = true;
                _openMenuItem.Visible = true;
            }
            else
            {
                return;
            }

            // No .dou file exists for Safir.Dob.Typesystem.Object
            if (typeId == Object.ClassTypeId)
            {
                _viewDouFileMenuItem.Visible = false;
            }

            if (Operations.IsOfType(typeId, Entity.ClassTypeId))
            {
                _registerMenuItem.Visible = true;
                _registerOptionsMenuItem.Visible = true;
                _unregisterMenuItem.Visible = true;
                _separator1MenuItem.Visible = true;
                _subscribeMenuItem.Visible = true;
                _subscribeOptionsMenuItem.Visible = true;
                _unsubscribeMenuItem.Visible = true;
                _subscribeRegistrationMenuItem.Visible = true;
                _subscribeRegistrationOptionsMenuItem.Visible = true;
                _unsubscribeRegistrationMenuItem.Visible = true;
                _separator3MenuItem.Visible = true;
                _iterateClassMenuItem.Visible = true;
                if (node is ObjectNode)
                {
                    _separator2MenuItem.Visible = true;
                    _deleteRequestMenuItem.Visible = true;
                    _iterateClassMenuItem.Visible = false;
                    _changeImageMenuItem.Visible = false;
                }
            }
            else if (Operations.IsOfType(typeId, Message.ClassTypeId))
            {
                _subscribeMenuItem.Visible = true;
                _subscribeOptionsMenuItem.Visible = true;
                _unsubscribeMenuItem.Visible = true;
                _separator3MenuItem.Visible = true;
            }
            else if (Operations.IsOfType(typeId, Service.ClassTypeId))
            {
                _registerMenuItem.Visible = true;
                _registerOptionsMenuItem.Visible = true;
                _unregisterMenuItem.Visible = true;
                _separator1MenuItem.Visible = true;
                _subscribeRegistrationMenuItem.Visible = true;
                _subscribeRegistrationOptionsMenuItem.Visible = true;
                _unsubscribeRegistrationMenuItem.Visible = true;
                _separator3MenuItem.Visible = true;
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
                SetSubscribed(subInfo.typeId);
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

        public void UpdateExternalProgramsSubmenu()
        {
            var apps = new List<MenuItem>();
            foreach (var app in ExternalApplicationSettings.Settings.Applications)
            {
                apps.Add(new MenuItem(app.Description + "...", openTypeExternalMenuItem_Click) { Tag = app });
            }

            _openInExternalMenuItem.MenuItems.Clear();
            _openInExternalMenuItem.MenuItems.AddRange(apps.ToArray());
        }
        

        private void openTypeExternalMenuItem_Click(object sender, EventArgs e)
        {
            var s = sender as MenuItem;

            if (s == null)
            {
                MessageBox.Show("Something went horribly wrong!");
                return;
            }

            var extInfo = s.Tag as Application;
            if (extInfo == null)
            {
                MessageBox.Show("Something went horribly wrong! (No command configured)");
                return;
            }
            var unit = GetSelectedNode() as DobUnit;
            if (unit == null)
            {
                MessageBox.Show("Something went horribly wrong! (Selection is not DobUnit)");
                return;
            }
            var info = new ProcessStartInfo(extInfo.Name, extInfo.Arguments);
            var typeId = unit.TypeId;
            info.Arguments = info.Arguments.Replace("%t", typeId.ToString());
            info.Arguments = info.Arguments.Replace("%n", Operations.GetName(typeId));
            info.Arguments = info.Arguments.Replace("%d", InternalOperations.GetDouFilePath(typeId));

            var obj = unit as ObjectNode;
            if (obj != null)
            {
                info.Arguments = info.Arguments.Replace("%i", obj.EntityId.InstanceId.ToString());
            }

            try
            {
                var proc = Process.Start(info);
                if (proc == null)
                {
                    MessageBox.Show("Failed to run command '" + info.FileName + "'\nwith arguments '" + info.Arguments + "'");
                }
            }
            catch (Exception exc)
            {
                MessageBox.Show("Failed to run command '" + info.FileName + "'\nwith arguments '" + info.Arguments + "'\n\n" + exc);
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
                if (n == _treeViewClassHierarchy.Nodes[0] ||
                    n == _treeViewNsHierarchy.Nodes[0]) //root node is object
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
            var root = (ClassNode) _clTypeIdHt[typeId];
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
            root = (ClassNode) _nsTypeIdHt[typeId];
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
            var root = (ClassNode) _clTypeIdHt[entityId.TypeId];
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
            root = (ClassNode) _nsTypeIdHt[entityId.TypeId];
            foreach (TreeNode n in root.Nodes)
            {
                if (n is ObjectNode)
                {
                    if (string.Compare(((ObjectNode) n).EntityId.InstanceId.ToString(), entityId.InstanceId.ToString(), StringComparison.Ordinal) == 0)
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
            var cnode = (DobUnit) _clTypeIdHt[typeId];
            var nnode = (DobUnit) _nsTypeIdHt[typeId];
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
            cnode.ImageIndex = _imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = _imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        //This application becomes the owner
        public void SetOwned(long typeId)
        {
            var cnode = (DobUnit) _clTypeIdHt[typeId];
            var nnode = (DobUnit) _nsTypeIdHt[typeId];
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
            cnode.ImageIndex = _imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = _imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        //Registration received via subscription, other application
        public void SetRegistered(long typeId)
        {
            var cnode = (DobUnit) _clTypeIdHt[typeId];
            var nnode = (DobUnit) _nsTypeIdHt[typeId];
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
            cnode.ImageIndex = _imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = _imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        public void SetUnregistered(long typeId)
        {
            var cnode = (DobUnit) _clTypeIdHt[typeId];
            var nnode = (DobUnit) _nsTypeIdHt[typeId];
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
            cnode.ImageIndex = _imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = _imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }


        private void SetSubscribed(long typeId)
        {
            var cnode = (DobUnit) _clTypeIdHt[typeId];
            var nnode = (DobUnit) _nsTypeIdHt[typeId];
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
            cnode.ImageIndex = _imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = _imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        private void SetSubscribed(EntityId entityId)
        {
            SetSubscribed(entityId.TypeId);
        }
        

        private void SetUnsubscribed(long typeId)
        {
            var cnode = (DobUnit) _clTypeIdHt[typeId];
            var nnode = (DobUnit) _nsTypeIdHt[typeId];
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
            cnode.ImageIndex = _imageHandler.GetImageIndex(cnode.TypeId, cnode.ImageType);
            nnode.ImageIndex = _imageHandler.GetImageIndex(nnode.TypeId, nnode.ImageType);
            cnode.SelectedImageIndex = cnode.ImageIndex;
            nnode.SelectedImageIndex = nnode.ImageIndex;
        }

        public void LocateClass(long typeId)
        {
            _tabControl.SelectedIndex = 0;
            _treeViewClassHierarchy.Focus();
            _treeViewClassHierarchy.SelectedNode = (ClassNode) _clTypeIdHt[typeId];
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
                ImageIndex = _imageHandler.NamespaceImageIndex;
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

                Text = fullName ? t.FullName : t.Name;
                ImageIndex = _imageHandler.GetImageIndex(typeId, TreeViewImageHandler.ImageType.Default);
                SelectedImageIndex = ImageIndex;
            }
        }

        private class ObjectNode : DobUnit
        {
            public ObjectNode(EntityId entityId)
            {
                TypeId = entityId.TypeId;
                EntityId = entityId;
                ImageIndex = _imageHandler.GetImageIndex(entityId, TreeViewImageHandler.ImageType.Default);
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
                    return string.Compare(n1.Text, n2.Text, StringComparison.Ordinal);
                }
                catch
                {
                }

                return 0;
            }
        }
    }
}
