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
    /// Summary description for ClassViewPanel.
    /// </summary>
    public class ExplorerPanel : System.Windows.Forms.Panel
    {
        private System.Collections.Hashtable dobTypeHt = null;
        private System.Collections.Hashtable clTypeIdHt = null;
        private System.Collections.Hashtable nsTypeIdHt = null;
        private System.Collections.Hashtable nsHt = null;
        private System.Windows.Forms.ImageList imageList;
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.TreeView treeViewClassHierarchy;
        private System.Windows.Forms.TreeView treeViewNsHierarchy;
        private System.Windows.Forms.Panel toppanel;

        // context menus
        private System.Windows.Forms.ContextMenu contextMenu;
        private System.Windows.Forms.MenuItem openMenuItem;
        private System.Windows.Forms.MenuItem registerMenuItem;
        private System.Windows.Forms.MenuItem registerOptionsMenuItem;
        private System.Windows.Forms.MenuItem unregisterMenuItem;
        private System.Windows.Forms.MenuItem separator1MenuItem;
        private System.Windows.Forms.MenuItem subscribeMenuItem;
        private System.Windows.Forms.MenuItem subscribeOptionsMenuItem;
        private System.Windows.Forms.MenuItem unsubscribeMenuItem;
        private System.Windows.Forms.MenuItem subscribeRegistrationMenuItem;
        private System.Windows.Forms.MenuItem subscribeRegistrationOptionsMenuItem;
        private System.Windows.Forms.MenuItem unsubscribeRegistrationMenuItem;
        private System.Windows.Forms.MenuItem separator2MenuItem;
        private System.Windows.Forms.MenuItem deleteRequestMenuItem;
        private System.Windows.Forms.MenuItem separator3MenuItem;
        private System.Windows.Forms.MenuItem dobUnitInfoMenuItem;
        private System.Windows.Forms.MenuItem viewDouFileMenuItem;
        private System.Windows.Forms.MenuItem iterateClassMenuItem;
        private System.Windows.Forms.MenuItem changeImageMenuItem;
        private Panel fillpanel;
        private TabControl tabControl = new TabControl();
        private TabPage tabInheritance = new TabPage("Inheritance");
        private TabPage tabNamespace = new TabPage("Namespace");
        private static TreeViewImageHandler imageHandler;
        private ImageList defaultImagesList;

        private static ExplorerPanel instance = null;

        class NamespaceNode : TreeNode
        {
            public string FullName;

            public NamespaceNode(string shortName, string fullName)
            {
                FullName = fullName;
                this.Text = shortName;
                this.ImageIndex = imageHandler.NamespaceImageIndex;
                this.SelectedImageIndex = this.ImageIndex;
            }
        }


        class DobUnit : TreeNode
        {
            public TreeViewImageHandler.ImageType ImageType = TreeViewImageHandler.ImageType.Default;

            public Int64 TypeId;
            public Safir.Dob.Typesystem.EntityId EntityId;
            public Safir.Dob.Typesystem.HandlerId HandlerId;
        }

        class ClassNode : DobUnit
        {
            public Type DobType;

            public ClassNode(Type t, long typeId, bool fullName)
            {
                TypeId = typeId;
                DobType = t;

                if (fullName)
                    this.Text = t.FullName;
                else
                    this.Text = t.Name;
                this.ImageIndex = imageHandler.GetImageIndex(typeId, TreeViewImageHandler.ImageType.Default);
                this.SelectedImageIndex = this.ImageIndex;
            }
        }

        class ObjectNode : DobUnit
        {
            public ObjectNode(Safir.Dob.Typesystem.EntityId entityId)
            {
                this.TypeId = entityId.TypeId;
                this.EntityId = entityId;
                this.ImageIndex = imageHandler.GetImageIndex(entityId, TreeViewImageHandler.ImageType.Default);
                this.SelectedImageIndex = this.ImageIndex;
                this.Text = entityId.InstanceId.ToString();
            }
        }

        class TreeSorter : IComparer
        {
            public int Compare(object a, object b)
            {
                try
                {
                    if (a is ObjectNode && b is ObjectNode)
                    {
                        return Int64.Parse(((ObjectNode)a).Text).CompareTo(Int64.Parse(((ObjectNode)b).Text));
                    }
                    else
                    {
                        TreeNode n1 = (TreeNode)a;
                        TreeNode n2 = (TreeNode)b;
                        return n1.Text.CompareTo(n2.Text);
                    }
                }
                catch { }

                return 0;
            }
        }


        private ExplorerPanel()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            this.SuspendLayout();

            //Load images for the treeview
            imageHandler = new TreeViewImageHandler(this.imageList, this.defaultImagesList);
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

            openMenuItem.Click += new EventHandler(openMenuItem_Click);
            
            registerMenuItem.Click += new EventHandler(registerMenuItem_Click);
            registerOptionsMenuItem.Click += new EventHandler(registerOptionsMenuItem_Click);
            unregisterMenuItem.Click += new EventHandler(unregisterMenuItem_Click);

            subscribeMenuItem.Click += new EventHandler(subscribeMenuItem_Click);
            subscribeOptionsMenuItem.Click += new EventHandler(subscribeOptionsMenuItem_Click);
            unsubscribeMenuItem.Click += new EventHandler(unsubscribeMenuItem_Click);

            subscribeRegistrationMenuItem.Click += new EventHandler(subscribeRegistrationMenuItem_Click);
            subscribeRegistrationOptionsMenuItem.Click += new EventHandler(subscribeRegistrationOptionsMenuItem_Click);
            unsubscribeRegistrationMenuItem.Click += new EventHandler(unsubscribeRegistrationMenuItem_Click);
            
            deleteRequestMenuItem.Click += new EventHandler(deleteRequestMenuItem_Click);

            iterateClassMenuItem.Click += new EventHandler(iterateClassMenuItem_Click);

            changeImageMenuItem.Click += new EventHandler(changeImageMenuItem_Click);
            dobUnitInfoMenuItem.Click += new EventHandler(dobUnitInfoMenuItem_Click);
            viewDouFileMenuItem.Click += new EventHandler(viewDouFileMenuItem_Click);

            contextMenu = new ContextMenu(new MenuItem[]{   openMenuItem,
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
                                                            changeImageMenuItem});

            contextMenu.Popup += new EventHandler(contextMenu_Popup);

            //treeViewClassHierarchy
            this.treeViewClassHierarchy = new System.Windows.Forms.TreeView();
            this.treeViewClassHierarchy.ContextMenu = contextMenu;
            this.treeViewClassHierarchy.Location = new System.Drawing.Point(0, 40);
            this.treeViewClassHierarchy.Dock = System.Windows.Forms.DockStyle.Fill;
            this.treeViewClassHierarchy.ImageList = this.imageList;
            
            this.treeViewClassHierarchy.DoubleClick += new EventHandler(treeViewClassHierarchy_DoubleClick);
            this.treeViewClassHierarchy.MouseDown += new System.Windows.Forms.MouseEventHandler(this.treeViewClassHierarchy_MouseDown);
            this.treeViewClassHierarchy.ItemDrag += new ItemDragEventHandler(treeViewClassHierarchy_ItemDrag);
            this.treeViewClassHierarchy.TreeViewNodeSorter = new TreeSorter();

            //treeViewNsHierarchy
            this.treeViewNsHierarchy = new TreeView();
            this.treeViewNsHierarchy.ContextMenu = contextMenu;
            this.treeViewNsHierarchy.Location = new System.Drawing.Point(0, 40);
            this.treeViewNsHierarchy.Dock = System.Windows.Forms.DockStyle.Fill;
            this.treeViewNsHierarchy.ImageList = this.imageList;
            this.treeViewNsHierarchy.DoubleClick += new EventHandler(treeViewNsHierarchy_DoubleClick);
            this.treeViewNsHierarchy.MouseDown += new System.Windows.Forms.MouseEventHandler(this.treeViewNsHierarchy_MouseDown);
            this.treeViewNsHierarchy.ItemDrag += new ItemDragEventHandler(treeViewNsHierarchy_ItemDrag);
            this.treeViewNsHierarchy.TreeViewNodeSorter = new TreeSorter();

            //Tabs
            tabControl.Dock = DockStyle.Fill;
            tabInheritance.Controls.Add(treeViewClassHierarchy);
            tabNamespace.Controls.Add(treeViewNsHierarchy);
            this.fillpanel.Controls.Add(tabControl);
            tabControl.TabPages.AddRange(new TabPage[] { this.tabInheritance, this.tabNamespace });
            if (Settings.Sate.DefaultExplorerView)
                tabControl.SelectedIndex = 0;
            else
                tabControl.SelectedIndex = 1;
            tabControl.SelectedIndex = 0;

            //TitleBar
            PanelLabelControl titleLabel = new PanelLabelControl("Explorer");
            titleLabel.CloseEvent += new PanelLabelControl.OnCloseEventHandler(titleLabel_CloseEvent);
            titleLabel.Height = 15;
            titleLabel.Dock = DockStyle.Top;
            this.toppanel.Controls.Add(titleLabel);

            this.ResumeLayout(false);
        }


        void dobUnitInfoMenuItem_Click(object sender, EventArgs e)
        {
            DobUnit node = (DobUnit)GetSelectedNode();
            DobUnitDetailsForm detailsForm = new DobUnitDetailsForm(node.TypeId); //, node.InstanceId);
            detailsForm.ShowDialog();
        }

        void changeImageMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();

            string mappingName;

            if (node is DobUnit)
            {
                DobUnit du = node as DobUnit;
                Int64 tid = du.TypeId;
                mappingName = tid.ToString();

            }
            else if (node is NamespaceNode)
            {
                NamespaceNode nn = node as NamespaceNode;
                mappingName = nn.FullName;
            }
            else
            {
                return;
            }

            CustomImageForm customImageForm = new CustomImageForm(imageHandler, mappingName);
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
                DobUnit tmp = de.Value as DobUnit;
                tmp.ImageIndex = imageHandler.GetImageIndex(tmp.TypeId, tmp.ImageType);
                tmp.SelectedImageIndex = tmp.ImageIndex;
            }

            foreach (DictionaryEntry de in nsTypeIdHt)
            {
                DobUnit tmp = de.Value as DobUnit;
                tmp.ImageIndex = imageHandler.GetImageIndex(tmp.TypeId, tmp.ImageType);
                tmp.SelectedImageIndex = tmp.ImageIndex;
            }
        }

        void treeViewNsHierarchy_ItemDrag(object sender, ItemDragEventArgs e)
        {
            DobUnit n = (DobUnit)e.Item;
            DoDragDrop(n.TypeId, DragDropEffects.Move);
        }

        void treeViewClassHierarchy_ItemDrag(object sender, ItemDragEventArgs e)
        {
            DobUnit n = (DobUnit)e.Item;
            DoDragDrop(n.TypeId, DragDropEffects.Move);
        }

        void titleLabel_CloseEvent(object sender, EventArgs e)
        {
            MainForm.Instance.ShowHideExplorer(false);
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

        /// <summary>
        /// Clean up any resources being used.
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
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ExplorerPanel));
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
            this.defaultImagesList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("defaultImagesList.ImageStream")));
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
            this.treeViewClassHierarchy.BeginUpdate();

            this.treeViewNsHierarchy.BeginUpdate();
            this.treeViewClassHierarchy.Nodes.Clear();
            this.treeViewNsHierarchy.Nodes.Clear();

            Type objType = typeof(Safir.Dob.Typesystem.Object);
            ClassNode objNode = new ClassNode(objType, Safir.Dob.Typesystem.Object.ClassTypeId, true);
            objNode.Expand();
            dobTypeHt = new Hashtable();
            dobTypeHt[objType] = objNode;
            clTypeIdHt = new Hashtable();
            nsTypeIdHt = new Hashtable();
            nsHt = new Hashtable();
            clTypeIdHt[GetTypeId(objType)] = objNode;
            Type[] types = MainForm.Instance.DotsGenerated.GetTypes();

            InsertClassInNsHierarchy(objType, Safir.Dob.Typesystem.Object.ClassTypeId);
            foreach (Type t in types)
            {
                Type type = t;
                if (type.IsSubclassOf(objType))
                {
                    ClassNode node = null;
                    while (type != objType && dobTypeHt[type] == null)
                    {
                        long typeId = GetTypeId(type);
                        if (node == null)
                        {
                            node = new ClassNode(type, typeId, true);
                        }
                        else
                        {
                            ClassNode child = node;
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
                        ((ClassNode)dobTypeHt[node.DobType.BaseType]).Nodes.Add(node);
                    }
                }
            }

            this.treeViewClassHierarchy.Nodes.Add(objNode);

            treeViewClassHierarchy.Sort();
            treeViewNsHierarchy.Sort();

            this.treeViewNsHierarchy.EndUpdate();
            this.treeViewClassHierarchy.EndUpdate();
            this.treeViewNsHierarchy.HideSelection = false;
            this.treeViewClassHierarchy.HideSelection = false;
        }

        private void InsertNamespace(Type type)
        {
            NamespaceNode node = null;
            string[] ns = type.Namespace.Split('.');
            string rest = type.Namespace + ".";
            for (int i = ns.Length - 1; i >= 0; i--)
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
                        NamespaceNode child = node;
                        node = new NamespaceNode(ns[i], type.Namespace);
                        node.Nodes.Add(child);
                    }
                    nsHt[rest] = node;
                }
                else
                {
                    if (node != null)
                    {
                        NamespaceNode parent = (NamespaceNode)nsHt[rest];

                        bool inserted = false;
                        for (int nix = 0; nix < parent.Nodes.Count; nix++)
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
            this.treeViewNsHierarchy.Nodes.Add(node);
        }

        private void InsertClassInNsHierarchy(Type type, long typeId)
        {
            InsertNamespace(type);
            ClassNode classNode = new ClassNode(type, typeId, false);
            nsTypeIdHt[typeId] = classNode;
            ((NamespaceNode)nsHt[type.Namespace]).Nodes.Add(classNode);
        }

        private void treeViewClassHierarchy_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Right)
            {
                TreeNode clickedNode = treeViewClassHierarchy.GetNodeAt(e.X, e.Y);
                if (clickedNode != null)
                {
                    this.treeViewClassHierarchy.SelectedNode = clickedNode;
                }
            }
        }


        private void treeViewClassHierarchy_DoubleClick(object sender, EventArgs e)
        {
            Cursor = Cursors.WaitCursor;
            if (treeViewClassHierarchy.SelectedNode is ObjectNode)
            {
                ObjectNode n = (ObjectNode)treeViewClassHierarchy.SelectedNode;

                try
                {
                    using (Safir.Dob.EntityProxy entityProxy = MainForm.Instance.Dose.Read(n.EntityId))
                    {
                        EntityInfo entityInfo = new EntityInfo();
                        entityInfo.Obj = entityProxy.Entity;
                        entityInfo.setInstanceId(entityProxy.InstanceId);
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
                ClassNode n = (ClassNode)treeViewClassHierarchy.SelectedNode;
                ObjectInfo objInfo = null;
                objInfo = new ObjectInfo();
                if (n == treeViewClassHierarchy.Nodes[0]) //root node is object
                {
                    objInfo.Obj = new Safir.Dob.Typesystem.Object();
                }
                else
                {
                    string name = Safir.Dob.Typesystem.Operations.GetName(n.TypeId);
                    System.Type dobType = MainForm.Instance.DotsGenerated.GetType(name);
                    MessageInfo msgInfo = null;
                    ServiceHandlerInfo srvInfo = null;
                    EntityInfo entityInfo = null;

                    if (dobType.IsSubclassOf(typeof(Safir.Dob.Message)))
                    {
                        msgInfo = new MessageInfo();

                        System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                        msgInfo.Obj = (Safir.Dob.Typesystem.Object)constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(msgInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Safir.Dob.Service)))
                    {
                        srvInfo = new ServiceHandlerInfo();

                        System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                        srvInfo.Obj = (Safir.Dob.Typesystem.Object)constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(srvInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Safir.Dob.Entity)))
                    {
                        entityInfo = new EntityInfo();

                        System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                        entityInfo.Obj = (Safir.Dob.Typesystem.Object)constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                    else
                    {
                        System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                        objInfo.Obj = (Safir.Dob.Typesystem.Object)constr.Invoke(null);
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
                TreeNode clickedNode = treeViewNsHierarchy.GetNodeAt(e.X, e.Y);
                if (clickedNode != null)
                {
                    this.treeViewNsHierarchy.SelectedNode = clickedNode;
                }
            }
        }

        private void treeViewNsHierarchy_DoubleClick(object sender, EventArgs e)
        {
            Cursor = Cursors.WaitCursor;
            if (treeViewNsHierarchy.SelectedNode is ObjectNode)
            {
                ObjectNode n = (ObjectNode)treeViewNsHierarchy.SelectedNode;

                try
                {
                    EntityInfo entityInfo = new EntityInfo();
                    using (Safir.Dob.EntityProxy entityProxy = MainForm.Instance.Dose.Read(n.EntityId))
                    {
                        entityInfo.Obj = entityProxy.Entity;
                        entityInfo.setInstanceId(entityProxy.InstanceId);
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
                ClassNode n = (ClassNode)treeViewNsHierarchy.SelectedNode;
                ObjectInfo objInfo = null;
                objInfo = new ObjectInfo();

                if (n == treeViewNsHierarchy.Nodes[0])
                {
                    objInfo.Obj = new Safir.Dob.Typesystem.Object();
                }
                else
                {
                    string name = Safir.Dob.Typesystem.Operations.GetName(n.TypeId);
                    System.Type dobType = MainForm.Instance.DotsGenerated.GetType(name);
                    MessageInfo msgInfo = null;
                    ServiceHandlerInfo srvInfo = null;
                    EntityInfo entityInfo = null;

                    if (dobType.IsSubclassOf(typeof(Safir.Dob.Message)))
                    {
                        msgInfo = new MessageInfo();

                        System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                        msgInfo.Obj = (Safir.Dob.Typesystem.Object)constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(msgInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Safir.Dob.Service)))
                    {
                        srvInfo = new ServiceHandlerInfo();

                        System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                        srvInfo.Obj = (Safir.Dob.Typesystem.Object)constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(srvInfo));
                    }
                    else if (dobType.IsSubclassOf(typeof(Safir.Dob.Entity)))
                    {
                        entityInfo = new EntityInfo();

                        System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                        entityInfo.Obj = (Safir.Dob.Typesystem.Object)constr.Invoke(null);

                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                    else
                    {
                        System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                        objInfo.Obj = (Safir.Dob.Typesystem.Object)constr.Invoke(null);
                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(objInfo));
                    }
                }
            }
            Cursor = Cursors.Default;
        }

        private long GetTypeId(Type type)
        {
            return (long)type.GetField("ClassTypeId").GetValue(null);
        }

        private void subscribeOptionsMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();
            Int64 typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;

                if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId))
                {
                    SubscribeForm sf = new SubscribeForm(typeId);
                    sf.InitEntitySubForm();
                    if (sf.ShowDialog() == DialogResult.OK)
                    {
                        if (sf.AllInstancesSub)
                        {
                            SubInfo subInfo = new SubInfo(typeId, null, null, sf.DataUpdSub, sf.SubClassesSub, sf.RestartSub);
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
                            Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId(((DobUnit)node).TypeId, sf.Instance);
                            SubInfo subInfo = new SubInfo(0, new EntityIdSerializeable(entityId), null, sf.DataUpdSub, sf.SubClassesSub, sf.RestartSub);
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
                else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Message.ClassTypeId))
                {
                    SubscribeMessageForm smf = new SubscribeMessageForm(typeId);
                    smf.InitMessageSubForm();

                    if (smf.ShowDialog() == DialogResult.OK)
                    {
                        Safir.Dob.Typesystem.ChannelId channelId;

                        if (smf.AllChannels)
                        {
                            channelId = Safir.Dob.Typesystem.ChannelId.ALL_CHANNELS;
                        }
                        else
                        {
                            /* try to parse as an long. O/w use the string representation */
                            try
                            {
                                long val = long.Parse(smf.ChannelTextBox);
                                channelId = new Safir.Dob.Typesystem.ChannelId(val);
                            }
                            catch
                            {
                                if (smf.ChannelTextBox != "")
                                {
                                    channelId = new Safir.Dob.Typesystem.ChannelId(smf.ChannelTextBox);
                                }
                                else
                                {
                                    channelId = new Safir.Dob.Typesystem.ChannelId();
                                }
                            }
                        }
                        SubInfo subInfo = new SubInfo();
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
            System.Windows.Forms.TreeNode node = GetSelectedNode();
            Int64 typeId;

            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;
            }
            else
            {
                return;
            }

            if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId))
            {

                SubscribeEntity(new SubInfo(typeId, null, null, true, true, true));
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Message.ClassTypeId))
            {
                SubInfo subInfo = new SubInfo();
                subInfo.typeId = typeId;
                subInfo.channelIdSer = new ChannelIdSerializable(Safir.Dob.Typesystem.ChannelId.ALL_CHANNELS);
                SubscribeMessage(subInfo);
                //SubscribeMessage(typeId, Safir.Dob.Typesystem.ChannelId.ALL_CHANNELS);
            }
        }

        private void subscribeRegistrationMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();
            Int64 typeId;

            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;
            }
            else
            {
                return;
            }

            Safir.Dob.Typesystem.HandlerId handlerId = Safir.Dob.Typesystem.HandlerId.ALL_HANDLERS;

            if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId))
            {
                SubRegInfo subRegInfo = new SubRegInfo();
                subRegInfo.typeId = typeId;
                subRegInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                subRegInfo.includeSubClasses = true;
                subRegInfo.restartSubscription = true;
                SubscribeRegistration(subRegInfo);
                // save handlerid in node
                ((DobUnit)node).HandlerId = handlerId;
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Service.ClassTypeId))
            {
                SubRegInfo subRegInfo = new SubRegInfo();
                subRegInfo.typeId = typeId;
                subRegInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                subRegInfo.includeSubClasses = true;
                subRegInfo.restartSubscription = true;
                SubscribeRegistration(subRegInfo);
                // save handlerid in node
                ((DobUnit)node).HandlerId = handlerId;
            }
        }

      
        private void subscribeRegistrationOptionsMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();
            Int64 typeId;
        
            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;

                SubscribeRegistrationForm srf = new SubscribeRegistrationForm(typeId);
                srf.InitSubForm();
             
                if ((Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId)) ||
                    (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Service.ClassTypeId)))
                {
                  
                    if (srf.ShowDialog() == DialogResult.OK)
                    {
                        Safir.Dob.Typesystem.HandlerId handlerId;
                        //TODO: add support for " " to force string representation
                        // try to parse as an long. O/w use the string representation
                        try
                        {
                            long val = long.Parse(srf.HandlerIdTextBox);
                            handlerId = new Safir.Dob.Typesystem.HandlerId(val);
                        }
                        catch
                        {
                            if (srf.HandlerIdTextBox != "")
                            {
                                handlerId = new Safir.Dob.Typesystem.HandlerId(srf.HandlerIdTextBox);
                            }
                            else
                            {
                                handlerId = new Safir.Dob.Typesystem.HandlerId();
                            }
                        }
                        SubRegInfo subRegInfo = new SubRegInfo();
                        subRegInfo.typeId = typeId;
                        subRegInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                        subRegInfo.includeSubClasses = true;
                        subRegInfo.restartSubscription = true;
                        SubscribeRegistration(subRegInfo);
                        // save handlerid in node
                        ((DobUnit)node).HandlerId = handlerId;

                        if (srf.PermanentSub)
                        {
                            //Subscriptions
                            Settings.Sate.AddSubscriptionReg(new SubRegInfo(typeId, new HandlerIdSerializeable(handlerId), srf.IncludeSubClassesSub, srf.RestartSub));
                            Settings.Save();
                        }
                    }
                  
                }
                 
            } 
        }
       
    

                    
                    

        private void registerMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();
            Int64 typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;
            }
            else
            {
                return;
            }
            Safir.Dob.Typesystem.HandlerId handlerId = new Safir.Dob.Typesystem.HandlerId();
            // save handlerid in node
            ((DobUnit)node).HandlerId = handlerId;

            if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId))
            {
                RegInfo regInfo = new RegInfo();
                regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                regInfo.typeId = typeId;
                regInfo.pending = false;
                regInfo.injection = false;
                regInfo.requestorDecides = false;
                RegisterEntity(regInfo);
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Service.ClassTypeId))
            {
                RegInfo regInfo = new RegInfo();
                regInfo.typeId = typeId;
                regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                regInfo.pending = false;
                RegisterService(regInfo);
            }
        }

        private void unregisterMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();
            Int64 typeId;
            Safir.Dob.Typesystem.HandlerId handlerId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;
                handlerId = ((DobUnit)node).HandlerId;
                if (handlerId == null)
                {
                    handlerId = new Safir.Dob.Typesystem.HandlerId();
                }
            }
            else
            {
                return;
            }
            RegInfo regInfo = new RegInfo();
            regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
            regInfo.typeId = typeId;
            Unregister(regInfo);
        }

        private void registerOptionsMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();
            Int64 typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;
                RegisterForm rf = new RegisterForm(typeId);
                if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId))
                {
                    rf.InitEntitySubForm();
                }
                else
                {
                    rf.InitServiceSubForm();
                }

                if (rf.ShowDialog() == DialogResult.OK)
                {
                    Safir.Dob.Typesystem.HandlerId handlerId;
                    // TODO: " " representation support to add
                    /* try to parse as an long. O/w use the string representation */
                    try
                    {
                        long val = long.Parse(rf.HandlerId);
                        handlerId = new Safir.Dob.Typesystem.HandlerId(val);
                    }
                    catch
                    {
                        if (rf.HandlerId != "")
                        {
                            handlerId = new Safir.Dob.Typesystem.HandlerId(rf.HandlerId);
                        }
                        else
                        {
                            handlerId = new Safir.Dob.Typesystem.HandlerId();
                        }
                    }

                    if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId))
                    {
                        RegInfo regInfo = new RegInfo();
                        regInfo.typeId = typeId;
                        regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                        regInfo.pending = rf.PendingReg;
                        regInfo.injection = rf.InjectionReg;
                        regInfo.requestorDecides = rf.RequestorDecides;
                        RegisterEntity(regInfo);
                    }
                    else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Service.ClassTypeId))
                    {
                        RegInfo regInfo = new RegInfo();
                        regInfo.typeId = typeId;
                        regInfo.handlerIdSer = new HandlerIdSerializeable(handlerId);
                        regInfo.pending = rf.PendingReg;
                        RegisterService(regInfo);
                    }

                    if (rf.PermanentReg)
                    {
                        Settings.Sate.AddRegistration(new RegInfo(typeId, new HandlerIdSerializeable(handlerId), rf.PendingReg, rf.InjectionReg, rf.RequestorDecides));
                        Settings.Save();
                    }
                }
            }
        }
    
        public void RegisterService(RegInfo regInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;
            
            Scenarios.Action rec = new Sate.Scenarios.Action(Scenarios.DobAction.Register, 0, regInfo);
            ScenarioTabPage.Instance.Player.Record(rec);

            try
            {
                if (regInfo.pending)
                {
                    OutputPanel.Instance.LogEvent("- Request pending registration of service, " + regInfo.typeId + ", for handlerId " + regInfo.handlerIdSer.HandlerId().ToString(), true);
                    MainForm.Instance.Dose.RegisterServiceHandlerPending(regInfo.typeId, regInfo.handlerIdSer.HandlerId(), MainForm.Instance);
                    SetRegistered(regInfo.typeId);
                }
                else
                {
                    OutputPanel.Instance.LogEvent("- Request registration of service, " + regInfo.typeId + ", for handlerId " + regInfo.handlerIdSer.HandlerId().ToString(), true);
                    MainForm.Instance.Dose.RegisterServiceHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId(), MainForm.Instance);
                    SetRegistered(regInfo.typeId);
                }
            }
            catch (Exception excp)
            {
                MessageBox.Show("Register service failed.\n" + excp, "Register Failed", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        //--- Revoked Registration ---
        public void RevokedRegistration(RegInfo regInfo)
        {
            String name = Safir.Dob.Typesystem.Operations.GetName(regInfo.typeId);

            SetUnregistered(regInfo.typeId);
            OutputPanel.Instance.LogEvent("- Revoked registration of handler '" + regInfo.handlerIdSer.HandlerId().ToString() + "'" + " for type " + name, true);

            // remove from either of these
            MainForm.Instance.requestorDecidesTypeIdList.Remove(regInfo.typeId);
            MainForm.Instance.handlerDecidesTypeIdList.Remove(regInfo.typeId);
        }

        //--- Unregister ---
        public void Unregister(RegInfo regInfo)
        {    
            if (!MainForm.Instance.CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.Unregister, regInfo);

            if (Safir.Dob.Typesystem.Operations.IsOfType(regInfo.typeId, Safir.Dob.Entity.ClassTypeId))
            {
                MainForm.Instance.Dose.UnregisterHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId());
                SetUnregistered(regInfo.typeId);
                OutputPanel.Instance.LogEvent("- Unregister registration for entity, " + regInfo.typeId + " for handler '" + regInfo.handlerIdSer.HandlerId().ToString() + "'",  true);
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(regInfo.typeId, Safir.Dob.Service.ClassTypeId))
            {
                MainForm.Instance.Dose.UnregisterHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId());
                SetUnregistered(regInfo.typeId);
                OutputPanel.Instance.LogEvent("- Unregister registration for service, " + regInfo.typeId + " for handler '" + regInfo.handlerIdSer.HandlerId().ToString() + "'", true);
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

            Scenarios.Action rec = new Sate.Scenarios.Action(Scenarios.DobAction.SubscribeRegistration, 0, subRegInfo);
            ScenarioTabPage.Instance.Player.Record(rec);

            try
            {
                MainForm.Instance.Dose.SubscribeRegistration(subRegInfo.typeId, subRegInfo.handlerIdSer.HandlerId(), subRegInfo.includeSubClasses, subRegInfo.restartSubscription, MainForm.Instance);
                SetSubscribed(subRegInfo.typeId, subRegInfo.handlerIdSer.HandlerId());
                OutputPanel.Instance.LogEvent("- Subscribe for registration of " + subRegInfo.typeId + " on handler '" + subRegInfo.handlerIdSer.HandlerId().ToString() + "'", true);
            }
            catch (Exception excp)
            {
                MessageBox.Show("Subscribe registration failed.\n" + excp, "Subscribe Failed", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }


        public void SubscribeEntity(SubInfo subInfo) //Safir.Dob.Typesystem.EntityId entityId, bool upd, bool restart)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            Scenarios.Action rec = new Sate.Scenarios.Action(Scenarios.DobAction.Subscribe, 0, subInfo);
            ScenarioTabPage.Instance.Player.Record(rec);

            try
            {
                if (subInfo.entityIdSer != null)
                {
                    MainForm.Instance.Dose.SubscribeEntity(subInfo.entityIdSer.EntityId(),
                                                           subInfo.upd,  // Include updates
                                                           subInfo.restartSubscription,
                                                           MainForm.Instance);
                    SetSubscribed(subInfo.entityIdSer.EntityId());
                    OutputPanel.Instance.LogEvent("- Subscribe for entity, " + subInfo.entityIdSer.EntityId().ToString(), true);
                }
                else
                {
                    MainForm.Instance.Dose.SubscribeEntity(subInfo.typeId,
                                                           subInfo.upd,  // Include updates
                                                           subInfo.includeSubClasses,
                                                           subInfo.restartSubscription,
                                                           MainForm.Instance);
                    SetSubscribed(subInfo.typeId);
                    OutputPanel.Instance.LogEvent("- Subscribe for typeId, " + subInfo.typeId.ToString(), true);
                }
            }
            catch (Exception excp)
            {
                MessageBox.Show("Subscribe entity failed.\n" + excp, "Subscribe Failed", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        private void unsubscribeMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();
            Int64 typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;
            }
            else
            {
                return;
            }
            SubInfo subInfo = new SubInfo();
            subInfo.typeId = typeId;
            if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId))
            {
                UnsubscribeEntity(subInfo);
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Message.ClassTypeId))
            {
                subInfo.channelIdSer = new ChannelIdSerializable(Safir.Dob.Typesystem.ChannelId.ALL_CHANNELS);
                subInfo.includeSubClasses = true;
                UnsubscribeMessage(subInfo);
            }
        }


        private void unsubscribeRegistrationMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();
            Int64 typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;
            }
            else
            {
                return;
            }
            SubRegInfo subRegInfo = new SubRegInfo();
            subRegInfo.typeId = typeId;
            UnsubscribeRegistration(subRegInfo);
            
        }

        private void deleteRequestMenuItem_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.TreeNode node = GetSelectedNode();

            if (node is ObjectNode)
            {
                EntityIdInfo entityIdInfo = new EntityIdInfo();
                entityIdInfo.entityIdSer = new EntityIdSerializeable(((ObjectNode)node).EntityId);
                MainForm.Instance.DeleteRequest(entityIdInfo);
            }
        }

        private TreeNode GetSelectedNode()
        {
            if (this.treeViewClassHierarchy.Visible)
                return treeViewClassHierarchy.SelectedNode;
            else if (this.treeViewNsHierarchy.Visible)
                return treeViewNsHierarchy.SelectedNode;
            else
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

            System.Windows.Forms.TreeNode node = GetSelectedNode();

            Int64 typeId;
            if (node is DobUnit)
            {
                typeId = ((DobUnit)node).TypeId;
                viewDouFileMenuItem.Visible = true;
                openMenuItem.Visible = true;
            }
            else
            {
                return;
            }

            // No .dou file exists for Safir.Dob.Typesystem.Object
            if (typeId == Safir.Dob.Typesystem.Object.ClassTypeId)
            {
                viewDouFileMenuItem.Visible = false;
            }

            if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId))
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
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Message.ClassTypeId))
            {
                subscribeMenuItem.Visible = true;
                subscribeOptionsMenuItem.Visible = true;
                unsubscribeMenuItem.Visible = true;
                separator3MenuItem.Visible = true;
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Service.ClassTypeId))
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
            
            Scenarios.Action rec = new Sate.Scenarios.Action(Scenarios.DobAction.Subscribe, 0, subInfo);
            ScenarioTabPage.Instance.Player.Record(rec);
            
            try
            {
                MainForm.Instance.Dose.SubscribeMessage(subInfo.typeId, subInfo.channelIdSer.ChannelId(), subInfo.includeSubClasses, MainForm.Instance);
                SetSubscribed(subInfo.typeId, subInfo.channelIdSer.ChannelId());
                OutputPanel.Instance.LogEvent("- Subscribe for message, " + subInfo.typeId + ", on channel '" + subInfo.channelIdSer.ChannelId().ToString() + "'", true);
            }
            catch (Exception excp)
            {
                MessageBox.Show("Subscribe message failed.\n" + excp, "Subscribe Failed", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }

        //--- Unsubscribe ---
        public void UnsubscribeEntity(SubInfo subInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.Unsubscribe, subInfo);

            if (Safir.Dob.Typesystem.Operations.IsOfType(subInfo.typeId, Safir.Dob.Entity.ClassTypeId))
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

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.Unsubscribe, subInfo);

            if (Safir.Dob.Typesystem.Operations.IsOfType(subInfo.typeId, Safir.Dob.Message.ClassTypeId))
            {
                MainForm.Instance.Dose.UnsubscribeMessage(subInfo.typeId, subInfo.channelIdSer.ChannelId(), subInfo.includeSubClasses, MainForm.Instance);
                SetUnsubscribed(subInfo.typeId);
                OutputPanel.Instance.LogEvent("- Unsubscribe message, " + subInfo.typeId + ", on channel '" + subInfo.channelIdSer.ChannelId().ToString() + "'", true);
            }
        }


        public void UnsubscribeRegistration(SubRegInfo subRegInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            ScenarioTabPage.Instance.Player.Record(Scenarios.DobAction.UnsubscribeRegistration, subRegInfo);

            if (Safir.Dob.Typesystem.Operations.IsOfType(subRegInfo.typeId, Safir.Dob.Entity.ClassTypeId))
            {
                MainForm.Instance.Dose.UnsubscribeRegistration(subRegInfo.typeId, Safir.Dob.Typesystem.HandlerId.ALL_HANDLERS, true, MainForm.Instance);
                SetUnsubscribed(subRegInfo.typeId);
                OutputPanel.Instance.LogEvent("- Unsubscribe entity, " + subRegInfo.typeId, true);
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(subRegInfo.typeId, Safir.Dob.Service.ClassTypeId))
            {
                MainForm.Instance.Dose.UnsubscribeRegistration(subRegInfo.typeId, Safir.Dob.Typesystem.HandlerId.ALL_HANDLERS, true, MainForm.Instance);
                SetUnsubscribed(subRegInfo.typeId);
                OutputPanel.Instance.LogEvent("- Unsubscribe service registration, " + subRegInfo.typeId, true);
            }
        }



        private void iterateClassMenuItem_Click(object sender, EventArgs e)
        {
            TreeNode n = this.GetSelectedNode();
            if (n is ClassNode)
            {
                Int64 typeId = ((ClassNode)n).TypeId;
                new IterateClassForm(typeId).ShowDialog();
            }
        }


        private void openMenuItem_Click(object sender, EventArgs e)
        {
            TreeNode node = GetSelectedNode();
            
            if (node is ObjectNode)
            {
                try
                {
                    ObjectNode n = (ObjectNode)node;
                    using (Safir.Dob.EntityProxy entityProxy = MainForm.Instance.Dose.Read(n.EntityId))
                    {
                        EntityInfo entityInfo = new EntityInfo();
                        entityInfo.Obj = entityProxy.Entity;
                        entityInfo.setInstanceId(entityProxy.InstanceId);
                        MainForm.Instance.AddTabPage(new ObjectEditTabPage(entityInfo));
                    }
                }
                catch
                {

                }
            }
            else if (node is ClassNode)
            {
                ClassNode n = (ClassNode)node;
                ObjectInfo objInfo = new ObjectInfo();
                if (n == treeViewClassHierarchy.Nodes[0] ||
                    n == treeViewNsHierarchy.Nodes[0]) //root node is object
                {
                    objInfo.Obj = new Safir.Dob.Typesystem.Object();
                }
                else
                {
                    string name = Safir.Dob.Typesystem.Operations.GetName(n.TypeId);
                    System.Type dobType = MainForm.Instance.DotsGenerated.GetType(name);
                    System.Reflection.ConstructorInfo constr = dobType.GetConstructor(System.Type.EmptyTypes);
                    objInfo.Obj = (Safir.Dob.Typesystem.Object)constr.Invoke(null);
                }

                MainForm.Instance.AddTabPage(new ObjectEditTabPage(objInfo));
            }

        }

        private void viewDouFileMenuItem_Click(object sender, EventArgs e)
        {
            DobUnit node = (DobUnit)GetSelectedNode();
            string fileName = Safir.Dob.Typesystem.Operations.GetName(node.TypeId) + ".dou";
            string path1 = Environment.GetEnvironmentVariable(@"SAFIR_RUNTIME") + @"/data/text/dots/classes/";
            string path2 = Environment.GetEnvironmentVariable(@"SAFIR_USER") + @"/runtime/data/text/dots/classes/";

            String[] files = System.IO.Directory.GetFiles(path1, fileName, System.IO.SearchOption.AllDirectories);

            if (files.Length == 0)
            {
                files = System.IO.Directory.GetFiles(path2, fileName, System.IO.SearchOption.AllDirectories);
            }


            if (files.Length == 0)
            {
                MessageBox.Show("The dou-file could not be found, '" + fileName + "'", "File not found", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            using (System.IO.TextReader reader = new System.IO.StreamReader(files[0]))
            {
                string content = reader.ReadToEnd();
                reader.Close();

                MainForm.Instance.AddTabPage(new XmlTabPage(content, fileName));

            }
        }

        //------------------------------------------------------
        // Change icons in treeView
        //------------------------------------------------------
        public void AddObject(Safir.Dob.Typesystem.EntityId entityId)
        {
            bool addNew = true;
            Int64 typeId = entityId.TypeId;

            //class hierarchy
            ClassNode root = (ClassNode)clTypeIdHt[typeId];
            for (int i = 0; i < root.Nodes.Count; i++)
            {
                if (root.Nodes[i] is ObjectNode)
                {
                    // if object already exists, don't add it again
                    // sorting is handled in the container
                    if (((ObjectNode)root.Nodes[i]).EntityId.InstanceId.CompareTo(entityId.InstanceId) == 0)
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
            root = (ClassNode)nsTypeIdHt[typeId];
            for (int i = 0; i < root.Nodes.Count; i++)
            {
                if (root.Nodes[i] is ObjectNode)
                {
                    // if object already exists, don't add it again
                    // sorting is handled in the container
                    if (((ObjectNode)root.Nodes[i]).EntityId.InstanceId.CompareTo(entityId.InstanceId) == 0)
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

        public void DeleteObject(Safir.Dob.Typesystem.EntityId entityId)
        {
            //class hierarchy
            ClassNode root = (ClassNode)clTypeIdHt[entityId.TypeId];
            foreach (TreeNode n in root.Nodes)
            {
                if (n is ObjectNode) 
                {
                    if (((ObjectNode)n).EntityId.InstanceId.CompareTo(entityId.InstanceId) == 0)
                    {
                        root.Nodes.Remove(n);
                        break;
                    }
                }
            }

            //namespace hierarchy
            root = (ClassNode)nsTypeIdHt[entityId.TypeId];
            foreach (TreeNode n in root.Nodes)
            {
                if (n is ObjectNode)
                {
                    if (((ObjectNode)n).EntityId.InstanceId.ToString().CompareTo(entityId.InstanceId.ToString()) == 0)
                    {
                        root.Nodes.Remove(n);
                        break;
                    }
                }
            }
        }

        //This application requests to be the owner, i.e pending registration
        public void SetPending(Int64 typeId)
        {
            DobUnit cnode = (DobUnit)clTypeIdHt[typeId];
            DobUnit nnode = (DobUnit)nsTypeIdHt[typeId];
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
        public void SetOwned(Int64 typeId)
        {
            DobUnit cnode = (DobUnit)clTypeIdHt[typeId];
            DobUnit nnode = (DobUnit)nsTypeIdHt[typeId];
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
        public void SetRegistered(System.Int64 typeId)
        {
            DobUnit cnode = (DobUnit)clTypeIdHt[typeId];
            DobUnit nnode = (DobUnit)nsTypeIdHt[typeId];
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

        public void SetUnregistered(System.Int64 typeId)
        {
            DobUnit cnode = (DobUnit)clTypeIdHt[typeId];
            DobUnit nnode = (DobUnit)nsTypeIdHt[typeId];
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


        private void SetSubscribed(Int64 typeId)
        {
            DobUnit cnode = (DobUnit)clTypeIdHt[typeId];
            DobUnit nnode = (DobUnit)nsTypeIdHt[typeId];
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

        private void SetSubscribed(Safir.Dob.Typesystem.EntityId entityId)
        {
            DobUnit cnode = (DobUnit)clTypeIdHt[entityId.TypeId];
            DobUnit nnode = (DobUnit)nsTypeIdHt[entityId.TypeId];
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

        private void SetSubscribed(Int64 typeId, Safir.Dob.Typesystem.ChannelId channelId)
        {
            DobUnit cnode = (DobUnit)clTypeIdHt[typeId];
            DobUnit nnode = (DobUnit)nsTypeIdHt[typeId];
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

        private void SetSubscribed(Int64 typeId, Safir.Dob.Typesystem.HandlerId handlerId)
        {
            DobUnit cnode = (DobUnit)clTypeIdHt[typeId];
            DobUnit nnode = (DobUnit)nsTypeIdHt[typeId];
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

        private void SetUnsubscribed(Int64 typeId)
        {
            DobUnit cnode = (DobUnit)clTypeIdHt[typeId];
            DobUnit nnode = (DobUnit)nsTypeIdHt[typeId];
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
            treeViewClassHierarchy.SelectedNode = ((ClassNode)clTypeIdHt[typeId]);
        }


     
        //--- Register ---
        public void RegisterEntity(RegInfo regInfo)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            Scenarios.Action rec = new Sate.Scenarios.Action(Scenarios.DobAction.Register, 0, regInfo);
            ScenarioTabPage.Instance.Player.Record(rec);

            try
            {
                if (regInfo.injection)
                {
                    OutputPanel.Instance.LogEvent("- Request injection registration of entity, " + regInfo.typeId + " on handlerId '" + regInfo.handlerIdSer.HandlerId().ToString() + "'", true);
                    if (regInfo.requestorDecides)
                    {
                        MainForm.Instance.Dose.RegisterEntityHandlerInjection(regInfo.typeId, regInfo.handlerIdSer.HandlerId(), Safir.Dob.InstanceIdPolicy.Enumeration.RequestorDecidesInstanceId, MainForm.Instance);
                        MainForm.Instance.requestorDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    else
                    {
                        MainForm.Instance.Dose.RegisterEntityHandlerInjection(regInfo.typeId, regInfo.handlerIdSer.HandlerId(), Safir.Dob.InstanceIdPolicy.Enumeration.HandlerDecidesInstanceId, MainForm.Instance);
                        MainForm.Instance.handlerDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    SetRegistered(regInfo.typeId);
                }
                else if (regInfo.pending)
                {
                    OutputPanel.Instance.LogEvent("- Request pending registration of entity, " + regInfo.typeId + " on handlerId " + regInfo.handlerIdSer.HandlerId().ToString(), true);
                    if (regInfo.requestorDecides)
                    {
                        MainForm.Instance.Dose.RegisterEntityHandlerPending(regInfo.typeId, regInfo.handlerIdSer.HandlerId(), Safir.Dob.InstanceIdPolicy.Enumeration.RequestorDecidesInstanceId, MainForm.Instance);
                        MainForm.Instance.requestorDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    else
                    {
                        MainForm.Instance.Dose.RegisterEntityHandlerPending(regInfo.typeId, regInfo.handlerIdSer.HandlerId(), Safir.Dob.InstanceIdPolicy.Enumeration.HandlerDecidesInstanceId, MainForm.Instance);
                        MainForm.Instance.handlerDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    SetPending(regInfo.typeId);
                }
                else
                {
                    OutputPanel.Instance.LogEvent("- Request registration of entity, " + regInfo.typeId + " on handlerId '" + regInfo.handlerIdSer.HandlerId().ToString() + "'", true);
                    if (regInfo.requestorDecides)
                    {
                        MainForm.Instance.Dose.RegisterEntityHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId(), Safir.Dob.InstanceIdPolicy.Enumeration.RequestorDecidesInstanceId, MainForm.Instance);
                        MainForm.Instance.requestorDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    else
                    {
                        MainForm.Instance.Dose.RegisterEntityHandler(regInfo.typeId, regInfo.handlerIdSer.HandlerId(), Safir.Dob.InstanceIdPolicy.Enumeration.HandlerDecidesInstanceId, MainForm.Instance);
                        MainForm.Instance.handlerDecidesTypeIdList.Add(regInfo.typeId);
                    }
                    SetRegistered(regInfo.typeId);
                }
            }
            catch (Exception excp)
            {
                MessageBox.Show("Register entity failed.\n" + excp, "Register Failed", MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
        }
    }
}
