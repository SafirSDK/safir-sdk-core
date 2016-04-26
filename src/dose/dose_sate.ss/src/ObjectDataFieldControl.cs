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
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Safir.Dob.Typesystem;
using Object = Safir.Dob.Typesystem.Object;

namespace Sate
{
    /// <summary>
    ///     Summary description for ObjectDataFieldControl.
    /// </summary>
    public abstract class ObjectDataFieldControl : UserControl
    {
        //Constants for positioning of controls
        protected const int Y_START = 5;
        protected const int Y_DEFAULT_HEIGHT = 23;
        protected const int Y_STEP = 5;
        protected const int X_STEP = 10;
        protected const int X_TYPE_START = 5;
        protected const int X_NAME_START = 200; //150;
        protected const int X_VALUE_START = 360; //280;
        protected const int X_NULL_START = 470; //390;
        protected const int X_DEFAULT_WIDTH = 600; //550;

        protected static Font font = new Font("Courier New", 8, FontStyle.Bold);
        protected static Font dataFont = new Font("Courier New", 8, FontStyle.Regular);

        /// <summary>
        ///     Required designer variable.
        /// </summary>
        private readonly Container components = null;

        protected List<bool> changed = new List<bool>();
        protected CollectionType collectionType;

        protected List<Label> fieldNameLabel = new List<Label>();
        protected List<Control> fieldValueControl = new List<Control>();

        private bool ignoreEvent;
        protected List<bool> isNotChanged = new List<bool>();
        protected List<CheckBox> isNullCheckBox = new List<CheckBox>();
        protected bool isSequenceChanged;
        protected MemberType keyType;
        protected long keyTypeId;
        protected int member;
        protected string memberName = "";
        protected ObjectEditPanel parentObjectEditPanel;

        protected Label typeLabel;
        protected LinkLabel typeLabelAddItem;
        protected string typeName = "";

        protected ObjectDataFieldControl()
        {
        }

        protected ObjectDataFieldControl(ObjectInfo objInfo, int member, string typeName, string memberName,
            CollectionType collectionType, int arraySize)
        {
            Init(objInfo, member, typeName, memberName, collectionType, arraySize);
        }

        public ObjectEditPanel ParentObjectEditPanel
        {
            get { return parentObjectEditPanel; }
            set { parentObjectEditPanel = value; }
        }

        private int TypeLabelWidth
        {
            get
            {
                if (collectionType == CollectionType.SequenceCollectionType ||
                    collectionType == CollectionType.DictionaryCollectionType)
                    return typeLabelAddItem.Width;
                return typeLabel.Width;
            }
        }

        public static Font DataFont
        {
            get { return dataFont; }
        }


        protected abstract bool ValidInput(int index, bool setVal);
        public abstract void SetFieldValues();
        protected abstract void InsertSequenceItem(int index);

        protected void Init(ObjectInfo objInfo, int member, string typeName, string memberName,
            CollectionType collectionType, int arraySize)
        {
            this.typeName = typeName;
            this.memberName = memberName;
            this.collectionType = collectionType;

            Controls.Clear();
            fieldValueControl.Clear();
            fieldNameLabel.Clear();
            isNullCheckBox.Clear();
            changed.Clear();
            isNotChanged.Clear();

            ignoreEvent = true;
            Tag = objInfo;
            this.member = member;
            SuspendLayout();

            Name = "ObjectDataFieldControl";
            BackColor = Color.LightGray;
            Width = X_DEFAULT_WIDTH;

            var location = new Point(X_TYPE_START, Y_START);

            switch (collectionType)
            {
                case CollectionType.SingleValueCollectionType:
                {
                    InitSingleOrArray(objInfo, member, typeName, arraySize, ref location);
                }
                    break;

                case CollectionType.ArrayCollectionType:
                {
                    InitSingleOrArray(objInfo, member, typeName, arraySize, ref location);
                }
                    break;

                case CollectionType.DictionaryCollectionType:
                {
                    InitDictionary(objInfo, member, typeName, ref location);
                }
                    break;

                case CollectionType.SequenceCollectionType:
                {
                    InitSequence(objInfo, member, typeName, ref location);
                }
                    break;
            }


            Controls.AddRange(fieldNameLabel.ToArray());
            Controls.AddRange(fieldValueControl.ToArray());
            Controls.AddRange(isNullCheckBox.ToArray());

            if (typeLabel != null)
                Controls.Add(typeLabel);
            if (typeLabelAddItem != null)
                Controls.Add(typeLabelAddItem);

            Height = location.Y;

            SetFieldValues();
            ignoreEvent = false;
            ResumeLayout(false);
        }

        protected void InitDictionary(ObjectInfo objInfo, int member, string typeName, ref Point location)
        {
            MemberType memberType;
            long complexType;
            int typeSize;
            CollectionType ct;
            int arrLength;
            var memberName = Members.GetInfo(objInfo.Obj.GetTypeId(),
                member,
                out memberType,
                out keyType,
                out complexType,
                out keyTypeId,
                out typeSize,
                out ct,
                out arrLength);

            var keyTypeName = ObjectDataFieldDictionaryKey.GetTypeName(keyType, keyTypeId);


            //Type label and AddItem link
            typeLabelAddItem = new LinkLabel();
            typeLabelAddItem.Width = X_NAME_START - X_TYPE_START - 2*X_STEP;
            typeLabelAddItem.AutoSize = false;
            typeLabelAddItem.Location = location;
            typeLabelAddItem.Font = font;
            typeLabelAddItem.LinkColor = ColorMap.ADD_LINK_DEFAULT;
            typeLabelAddItem.Text = string.Format("Dict<{0}, {1}>", keyTypeName, typeName);
            location.X += typeLabelAddItem.Width + X_STEP;
            typeLabelAddItem.Click += AddDictionaryItem_Click;
            var tsi = new ToolStripMenuItem();
            tsi.Text = "Is changed";
            tsi.Click += OnIsChangedMenuItem;
            typeLabelAddItem.ContextMenuStrip = new ContextMenuStrip();
            typeLabelAddItem.ContextMenuStrip.Items.Add(tsi);

            //set values
            var tmp = (ObjectInfo) Tag;
            var container = tmp.Obj.GetMember(member, 0);
            var containerType = container.GetType();
            var keys = (IEnumerable) containerType.GetProperty("Keys").GetValue(container, null);

            var index = 0;
            foreach (var k in keys)
            {
                //key
                var keyString = ObjectDataFieldDictionaryKey.KeyToString(keyType, k);
                var nameLabel = index == 0
                    ? CreateNameLabel(memberName + Environment.NewLine + string.Format("[{0}]", keyString))
                    : CreateNameLabel(string.Format("[{0}]", keyString));
                nameLabel.Tag = k;
                fieldNameLabel.Add(nameLabel);

                //value
                fieldValueControl.Add(CreateDictionaryItemValueControl());
                changed.Add(false);
                isNotChanged.Add(false);
                isNullCheckBox.Add(CreateIsNullCheckbox());

                PositionControls(index, ref location);
                index++;
            }

            if (index == 0)
            {
                fieldNameLabel.Add(CreateNameLabel(memberName));
                PositionControls(0, ref location);
            }
        }

        private void AddDictionaryItem_Click(object sender, EventArgs e)
        {
            var addDlg = new ObjectDataFieldDictionaryKey(keyType, keyTypeId);
            addDlg.StartPosition = FormStartPosition.Manual;
            addDlg.Location = PointToScreen(new Point(0, Y_DEFAULT_HEIGHT + 2*Y_STEP));
            if (addDlg.ShowDialog() == DialogResult.OK)
            {
                var key = addDlg.GetKey();
                if (fieldValueControl.Count == 0)
                {
                    fieldNameLabel[0].Text += string.Format(Environment.NewLine + "[{0}]", addDlg.KeyString);
                    fieldNameLabel[0].Tag = key;
                }
                else
                {
                    var keyIndex = FindDictionaryKeyIndex(key);
                    if (keyIndex < 0)
                    {
                        var keyLabel = CreateNameLabel(string.Format("[{0}]", addDlg.KeyString));
                        keyLabel.Tag = key;
                        fieldNameLabel.Add(keyLabel);
                        Controls.Add(keyLabel);
                    }
                    else
                    {
                        //duplicated key
                        fieldValueControl[keyIndex].Focus();
                        return;
                    }
                }

                var control = CreateDictionaryItemValueControl();
                Controls.Add(control);
                fieldValueControl.Add(control);
                changed.Add(true);
                isNotChanged.Add(false);
                var cb = CreateIsNullCheckbox();
                cb.Checked = true;
                Controls.Add(cb);
                isNullCheckBox.Add(cb);

                InsertDictionaryItem(key);

                SetDictionaryChanged(true, fieldValueControl.Count - 1);

                RePositioning();
                control.Focus();
                parentObjectEditPanel.ExpandCollapse(member);
            }
        }

        protected int FindDictionaryKeyIndex(object key)
        {
            for (var i = 0; i < fieldNameLabel.Count; i++)
            {
                if (key.Equals(fieldNameLabel[i].Tag))
                    return i;
            }

            return -1;
        }

        protected void InitSequence(ObjectInfo objInfo, int member, string typeName, ref Point location)
        {
            //Type label and AddItem link
            typeLabelAddItem = new LinkLabel();
            typeLabelAddItem.Width = X_NAME_START - X_TYPE_START - 2*X_STEP;
            typeLabelAddItem.Text = string.Format("Seq<{0}>", typeName);
            typeLabelAddItem.AutoSize = false;
            typeLabelAddItem.Location = location;
            typeLabelAddItem.Font = font;
            location.X = X_NAME_START;
            typeLabelAddItem.Click += AddSequenceItem_Click;
            var tsi = new ToolStripMenuItem();
            tsi.Text = "Is changed";
            tsi.Click += OnIsChangedMenuItem;
            typeLabelAddItem.ContextMenuStrip = new ContextMenuStrip();
            typeLabelAddItem.ContextMenuStrip.Items.Add(tsi);

            var tmp = (ObjectInfo) Tag;
            var container = tmp.Obj.GetMember(member, 0);
            var containerType = container.GetType();
            var numberOfValues = (int) containerType.GetProperty("Count").GetValue(container, null);

            for (var i = 0; i < numberOfValues; i++)
            {
                fieldNameLabel.Add(CreateNameLabel(string.Format("{0}[{1}]", memberName, i)));
                fieldValueControl.Add(CreateSequenceItemValueControl());
                PositionControls(i, ref location);
            }

            if (numberOfValues == 0)
            {
                fieldNameLabel.Add(CreateNameLabel(memberName));
                PositionControls(0, ref location);
            }
        }

        private Control CreateDictionaryItemValueControl()
        {
            var control = CreateValueControl();

            var deleteItem = new ToolStripMenuItem();
            deleteItem.Tag = control;
            deleteItem.Text = "Delete item";
            deleteItem.Click += DeleteDictionaryItem_Click;
            control.ContextMenuStrip.Items.Add(deleteItem);

            return control;
        }

        private Control CreateSequenceItemValueControl()
        {
            var control = CreateValueControl();

            var addAbove = new ToolStripMenuItem();
            addAbove.Tag = control;
            addAbove.Text = "Insert before";
            addAbove.Click += AddSequenceItem_Click;
            control.ContextMenuStrip.Items.Add(addAbove);

            var addBelow = new ToolStripMenuItem();
            addBelow.Tag = control;
            addBelow.Text = "Insert after";
            addBelow.Click += AddSequenceItem_Click;
            control.ContextMenuStrip.Items.Add(addBelow);

            var deleteItem = new ToolStripMenuItem();
            deleteItem.Tag = control;
            deleteItem.Text = "Delete item";
            deleteItem.Click += DeleteSequenceItem_Click;
            control.ContextMenuStrip.Items.Add(deleteItem);

            return control;
        }

        private void AddSequenceItem_Click(object sender, EventArgs e)
        {
            var insertAt = -1;

            var menuItem = sender as ToolStripMenuItem;
            if (menuItem != null) //if null then Add Item button has been clicked
            {
                for (var i = 0; i < fieldValueControl.Count; i++)
                {
                    if (fieldValueControl[i] == menuItem.Tag)
                    {
                        var insertAfter = menuItem.Text != "Insert before";
                        insertAt = insertAfter ? i + 1 : i;
                        break;
                    }
                }
            }

            //insert member name with index
            if (fieldValueControl.Count == 0)
            {
                fieldNameLabel[0].Text = memberName + "[0]";
            }
            else
            {
                var nameLabel = CreateNameLabel(string.Format("{0}[{1}]", memberName, fieldNameLabel.Count));
                fieldNameLabel.Add(nameLabel);
                Controls.Add(nameLabel);
            }

            //insert controls
            var control = CreateSequenceItemValueControl();
            Controls.Add(control);
            if (insertAt < 0)
            {
                fieldValueControl.Add(control);
                InsertSequenceItem(fieldValueControl.Count - 1);
            }
            else
            {
                fieldValueControl.Insert(insertAt, control);
                InsertSequenceItem(insertAt);
            }

            SetSequenceChanged(true);

            RePositioning();

            parentObjectEditPanel.ExpandCollapse(member);
            control.Focus(); //set input focus on the newly added control
        }


        private void DeleteDictionaryItem_Click(object sender, EventArgs e)
        {
            var removeAt = -1;
            for (var i = 0; i < fieldValueControl.Count; i++)
            {
                var menuItem = sender as ToolStripMenuItem;
                if (menuItem != null && fieldValueControl[i] == menuItem.Tag)
                {
                    removeAt = i;
                    break;
                }
            }

            if (removeAt >= 0)
            {
                var key = fieldNameLabel[removeAt].Tag;

                if (this is ObjectField)
                {
                    ((ObjectField) this).DeleteObjPanel(removeAt);
                }

                Controls.Remove(fieldValueControl[removeAt]);
                fieldValueControl.RemoveAt(removeAt);

                Controls.Remove(isNullCheckBox[removeAt]);
                isNullCheckBox.RemoveAt(removeAt);

                changed.RemoveAt(removeAt);
                isNotChanged.RemoveAt(removeAt);

                if (fieldValueControl.Count > 0)
                {
                    Controls.Remove(fieldNameLabel[removeAt]);
                    fieldNameLabel.RemoveAt(removeAt);
                }
                else
                {
                    fieldNameLabel[0].Text = memberName;
                }


                var tmp = (ObjectInfo) Tag;
                var container = tmp.Obj.GetMember(member, 0);
                var containerType = container.GetType();
                containerType.GetMethod("Remove", new[] {key.GetType()}).Invoke(container, new[] {key});

                SetDictionaryChanged(true, -1);

                RePositioning();

                parentObjectEditPanel.ExpandCollapse(member);
            }
        }

        private void DeleteSequenceItem_Click(object sender, EventArgs e)
        {
            var removeAt = -1;
            for (var i = 0; i < fieldValueControl.Count; i++)
            {
                var menuItem = sender as ToolStripMenuItem;
                if (menuItem != null && fieldValueControl[i] == menuItem.Tag)
                {
                    removeAt = i;
                    break;
                }
            }

            if (removeAt >= 0)
            {
                if (this is ObjectField)
                {
                    ((ObjectField) this).DeleteObjPanel(removeAt);
                }

                Controls.Remove(fieldValueControl[removeAt]);
                fieldValueControl.RemoveAt(removeAt);

                if (fieldValueControl.Count > 0)
                {
                    Controls.Remove(fieldNameLabel[fieldNameLabel.Count - 1]);
                    fieldNameLabel.RemoveAt(fieldNameLabel.Count - 1);
                }
                else
                {
                    fieldNameLabel[0].Text = memberName;
                }

                var tmp = (ObjectInfo) Tag;
                var container = tmp.Obj.GetMember(member, 0);
                var containerType = container.GetType();
                containerType.GetMethod("RemoveAt").Invoke(container, new object[] {removeAt});

                SetSequenceChanged(true);

                RePositioning();

                parentObjectEditPanel.ExpandCollapse(member);
            }
        }

        protected void SetDictionaryChanged(bool isChanged, int index)
        {
            var tmp = (ObjectInfo) Tag;
            var container = tmp.Obj.GetMember(member, 0);
            container.SetChanged(isChanged);

            typeLabelAddItem.LinkColor = isChanged ? ColorMap.ADD_LINK_CHANGED : ColorMap.ADD_LINK_DEFAULT;
            ((ToolStripMenuItem) typeLabelAddItem.ContextMenuStrip.Items[0]).Checked = isChanged;

            if (index >= 0)
            {
                changed[index] = isChanged;
                isNotChanged[index] = !isChanged;
                ((ToolStripMenuItem) fieldValueControl[index].ContextMenuStrip.Items[0]).Checked = isChanged;
                fieldValueControl[index].BackColor = isChanged ? ColorMap.CHANGED : ColorMap.ENABLED;

                var keyValue = fieldNameLabel[index].Tag;
                var containerType = container.GetType();
                var itemContainer =
                    (ContainerBase) containerType.GetProperty("Item").GetValue(container, new[] {keyValue});
                itemContainer.SetChanged(isChanged);
            }
        }

        protected void SetSequenceChanged(bool isChanged)
        {
            //if (fieldValueControl.Count==1 && fieldValueControl[0])
            isSequenceChanged = isChanged;
            var color = isChanged ? ColorMap.CHANGED : ColorMap.ENABLED;
            for (var i = 0; i < fieldValueControl.Count; i++)
            {
                ((ToolStripMenuItem) fieldValueControl[i].ContextMenuStrip.Items[0]).Checked = isChanged;

                if (ValidInput(i, false))
                    fieldValueControl[i].BackColor = color;
                else
                    fieldValueControl[i].BackColor = ColorMap.ERROR;
            }

            var tmp = (ObjectInfo) Tag;
            var cont = tmp.Obj.GetMember(member, 0);
            cont.SetChanged(isChanged);

            typeLabelAddItem.LinkColor = isChanged ? ColorMap.ADD_LINK_CHANGED : ColorMap.ADD_LINK_DEFAULT;
            ((ToolStripMenuItem) typeLabelAddItem.ContextMenuStrip.Items[0]).Checked = isChanged;
        }

        protected void InitSingleOrArray(ObjectInfo objInfo, int member, string typeName, int numberOfValues,
            ref Point location)
        {
            typeLabel = new Label();
            typeLabel.Width = X_NAME_START - X_TYPE_START - 2*X_STEP;
            typeLabel.Text = typeName;
            typeLabel.AutoSize = true;
            typeLabel.Location = location;
            typeLabel.Font = font;
            location.X = X_NAME_START;

            for (var i = 0; i < numberOfValues; i++)
            {
                fieldValueControl.Add(CreateValueControl());
                changed.Add(false);
                isNotChanged.Add(false);
                fieldNameLabel.Add(CreateNameLabel(numberOfValues > 1 ? memberName + "[" + i + "]" : memberName));
                isNullCheckBox.Add(CreateIsNullCheckbox());
                PositionControls(i, ref location);
            }
        }

        protected CheckBox CreateIsNullCheckbox()
        {
            var cb = new CheckBox();
            cb.Font = font;
            cb.Checked = false;
            cb.Text = "Is Null";
            cb.CheckedChanged += ObjectDataFieldControl_CheckedChanged;
            return cb;
        }

        protected Label CreateNameLabel(string name)
        {
            var label = new Label();
            label.Text = name;
            label.ForeColor = Color.Blue;
            label.Font = font;
            label.AutoSize = true;
            return label;
        }

        public bool ValidateMember()
        {
            var ok = true;

            //special sequence hanling
            if (collectionType == CollectionType.SequenceCollectionType)
            {
                for (var index = 0; index < fieldValueControl.Count; index++)
                {
                    if (!ValidInput(index, true))
                    {
                        ok = false;
                        fieldValueControl[index].BackColor = ColorMap.ERROR;
                    }
                }
                return ok;
            }

            //other collectionType than sequence
            for (var index = 0; index < fieldValueControl.Count; index++)
            {
                var cont = collectionType == CollectionType.DictionaryCollectionType
                    ? GetDictionaryValue(fieldNameLabel[index].Tag)
                    : ((ObjectInfo) Tag).Obj.GetMember(member, index);

                // set isChanged flag to false if this should be done
                if (isNotChanged[index])
                {
                    // before setting SetChanged(), check if null should be set.
                    // this should be set before as it changes the isChanged flag.
                    if (isNullCheckBox.Count > index && isNullCheckBox[index].Checked)
                    {
                        cont.SetNull();
                        // will set isChanged to true implicit
                        // no need to do anything about data when null is set
                        changed[index] = false;
                    }

                    cont.SetChanged(false);
                }


                if (changed[index])
                {
                    if (isNullCheckBox.Count > index && isNullCheckBox[index].Checked)
                    {
                        cont.SetNull();
                        // will set isChanged to true implicit
                        // no need to do anything about data when null is set
                        changed[index] = false;
                    }

                    if (changed[index] && !ValidInput(index, true))
                    {
                        ok = false;
                        fieldValueControl[index].BackColor = ColorMap.ERROR;
                    }
                }
            }
            return ok;
        }

        protected int ChangedValue<T>(IEnumerable<T> controls, object sender) where T : Control
        {
            var index = 0;
            foreach (Control c in controls)
            {
                if (c == sender)
                {
                    if (collectionType == CollectionType.SequenceCollectionType)
                    {
                        SetSequenceChanged(true);
                    }
                    else
                    {
                        ((ToolStripMenuItem) fieldValueControl[index].ContextMenuStrip.Items[0]).Checked = true;
                        fieldValueControl[index].BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                        isNotChanged[index] = false;
                    }

                    return index;
                }
                index++;
            }

            return -1;
        }

        protected int FindMenuItemOwnerControl(object sender)
        {
            var index = 0;
            foreach (var t in fieldValueControl)
            {
                foreach (ToolStripItem mi in t.ContextMenuStrip.Items)
                {
                    if (mi == sender)
                    {
                        return index;
                    }
                }

                index++;
            }
            return -1;
        }

        private void ObjectDataFieldControl_CheckedChanged(object sender, EventArgs e)
        {
            if (!ignoreEvent)
            {
                ignoreEvent = true;
                var i = ChangedValue(isNullCheckBox, sender);
                if (i >= 0)
                {
                    if (fieldValueControl[i] is TextBox)
                        fieldValueControl[i].Text = "";

                    parentObjectEditPanel.ChangedDataField();
                }
                ignoreEvent = false;
            }
        }

        protected void ObjectDataFieldControl_TextChanged(object sender, EventArgs e)
        {
            if (!ignoreEvent)
            {
                ignoreEvent = true;
                var i = ChangedValue(fieldValueControl, sender);

                if (i >= 0)
                {
                    if (isNullCheckBox.Count > i)
                    {
                        isNullCheckBox[i].Checked = false;
                    }
                    var tb = fieldValueControl[i] as TextBox;

                    if (tb != null)
                    {
                        //If it's a textbox then we do some extra stuff
                        TextBoxHandler(tb);
                    }

                    if (!ValidInput(i, true))
                    {
                        fieldValueControl[i].BackColor = ColorMap.ERROR;
                    }
                }

                parentObjectEditPanel.ChangedDataField();

                ignoreEvent = false;
            }
        }

        private void TextBoxHandler(TextBox tb)
        {
            if (tb.Text.Contains("\n"))
            {
                tb.ScrollBars = ScrollBars.Vertical;
                tb.Height = 30;
            }
            else
            {
                tb.ScrollBars = ScrollBars.None;
                tb.Height = 20;
            }
        }

        protected virtual Control CreateValueControl()
        {
            var textBox = new TextBox();
            textBox.Multiline = true;
            textBox.Font = dataFont;
            textBox.TextChanged += ObjectDataFieldControl_TextChanged;

            var tsi = new ToolStripMenuItem();
            tsi.Text = "Is changed";
            if (collectionType == CollectionType.SequenceCollectionType ||
                collectionType == CollectionType.DictionaryCollectionType)
            {
                tsi.Checked = true; //always changed when new items are added
            }
            tsi.Click += OnIsChangedMenuItem;
            textBox.ContextMenuStrip = new ContextMenuStrip();
            textBox.ContextMenuStrip.Items.Add(tsi);
            return textBox;
        }

        protected void OnIsChangedMenuItem(object sender, EventArgs e)
        {
            var mi = sender as ToolStripMenuItem;
            mi.Checked = !mi.Checked;
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var i = FindMenuItemOwnerControl(sender);
                // STSYLI 20080821 #265
                // Set flag if this checkbox is checked or not
                isNotChanged[i] = !mi.Checked;
                changed[i] = mi.Checked;
                if (mi.Checked)
                {
                    fieldValueControl[i].BackColor = ColorMap.CHANGED;
                }
                else
                {
                    ResetIndex(i);
                    ValidateMember();
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(mi.Checked);
                if (mi.Checked)
                    ValidateMember();
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var i = FindMenuItemOwnerControl(sender);
                SetDictionaryChanged(mi.Checked, i);
                if (mi.Checked)
                    ValidateMember();
            }
        }

        public virtual void ResetIndex(int i)
        {
            fieldValueControl[i].BackColor = ColorMap.ENABLED;
            (fieldValueControl[i].ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = false;
        }

        public void ResetChanged()
        {
            for (var i = 0; i < fieldValueControl.Count; i++)
            {
                ResetIndex(i);
            }

            ValidateMember();
        }

        public void RePositioning()
        {
            SuspendLayout();
            Width = X_DEFAULT_WIDTH;
            var location = new Point(X_TYPE_START, Y_START);
            var numControls = fieldValueControl.Count;

            var index = 0;
            do
            {
                PositionControls(index, ref location);
            } while (++index < numControls);

            Height = location.Y;
            ResumeLayout(false);
        }

        protected virtual void PositionControls(int index, ref Point location)
        {
            if (fieldNameLabel.Count > index)
            {
                location.X = X_NAME_START;
                fieldNameLabel[index].Location = location;
            }

            if (isNullCheckBox.Count > index)
            {
                location.X = X_NULL_START;
                isNullCheckBox[index].Location = location;
            }

            if (fieldValueControl.Count > index)
            {
                location.X = X_VALUE_START;
                fieldValueControl[index].Location = location;

                location.X = X_NAME_START;
                location.Y += fieldValueControl[index].Height + Y_STEP;
            }
            else
            {
                location.X = X_NAME_START;
                location.Y += Y_DEFAULT_HEIGHT + Y_STEP;
            }
        }

        /// <summary>
        ///     Clean up any resources being used.
        /// </summary>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                foreach (var c in fieldValueControl)
                {
                    c.Dispose();
                }

                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        protected virtual void InsertDictionaryItem(object key)
        {
            var tmp = (ObjectInfo) Tag;
            var container = tmp.Obj.GetMember(member, 0);
            var containerType = container.GetType();
            var method = containerType.GetMethod("Add", new[] {key.GetType()});
            method.Invoke(container, new[] {key});
        }

        protected ContainerBase GetDictionaryValue(object key)
        {
            var tmp = (ObjectInfo) Tag;
            var container = tmp.Obj.GetMember(member, 0);
            var containerType = container.GetType();
            var value = containerType.GetProperty("Item").GetValue(container, new[] {key});
            return value as ContainerBase;
        }

        //protected void InsertDictionaryItem(object key)
        //{
        //    ObjectInfo tmp = (ObjectInfo)Tag;
        //    var container = tmp.Obj.GetMember(member, 0);
        //    var containerType = container.GetType();
        //    object value = null;
        //    switch ()
        //    {
        //        case Safir.Dob.Typesystem.MemberType.Int32MemberType:
        //            {
        //                value = new Safir.Dob.Typesystem.Int32Container();
        //            }
        //            break;
        //        case Safir.Dob.Typesystem.MemberType.Int64MemberType:
        //            {
        //                value = new Safir.Dob.Typesystem.Int32Container();
        //            }
        //            break;
        //        case Safir.Dob.Typesystem.MemberType.StringMemberType:
        //            {
        //                value = new Safir.Dob.Typesystem.Int32Container();
        //            }
        //            break;
        //        case Safir.Dob.Typesystem.MemberType.TypeIdMemberType:
        //            {
        //                value = new Safir.Dob.Typesystem.Int32Container();
        //            }
        //            break;
        //        case Safir.Dob.Typesystem.MemberType.EntityIdMemberType:
        //            {
        //                value = new Safir.Dob.Typesystem.Int32Container();
        //            }
        //            break;
        //        case Safir.Dob.Typesystem.MemberType.InstanceIdMemberType:
        //            {
        //                value = new Safir.Dob.Typesystem.Int32Container();
        //            }
        //            break;
        //        case Safir.Dob.Typesystem.MemberType.HandlerIdMemberType:
        //            {
        //                value = new Safir.Dob.Typesystem.Int32Container();
        //            }
        //            break;
        //        case Safir.Dob.Typesystem.MemberType.ChannelIdMemberType:
        //            {
        //                value = new Safir.Dob.Typesystem.Int32Container();
        //            }
        //            break;
        //        case Safir.Dob.Typesystem.MemberType.EnumerationMemberType:
        //            {
        //                value = new Safir.Dob.Typesystem.Int32Container();
        //            }
        //            break;

        //        default:
        //            break;
        //    }

        //    containerType.GetProperty("Item").SetValue(container, value, new object[] { key });

        //}
    }

    //---------------------------------------------------
    // Bool control
    //---------------------------------------------------
    public class BoolField : ObjectDataFieldControl
    {
        public BoolField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "Boolean", name, ct, arraySize)
        {
        }

        protected override Control CreateValueControl()
        {
            var combo = new ComboBox();
            combo.Font = dataFont;
            combo.SelectedIndexChanged += ObjectDataFieldControl_TextChanged;
            combo.Items.AddRange(new[] {"True", "False"});
            combo.SelectedIndex = 0;
            combo.DropDownStyle = ComboBoxStyle.DropDownList;
            combo.Width = 100;
            combo.ContextMenuStrip = new ContextMenuStrip();
            combo.ContextMenuStrip.Items.Add(new ToolStripMenuItem("Is changed", null, OnIsChangedMenuItem));

            return combo;
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (BooleanSequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, true);
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            if (!setVal)
                return true;

            var c = (ComboBox) fieldValueControl[index];
            var tmp = (ObjectInfo) Tag;
            var val = c.Text == "True";
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (BooleanContainer) tmp.Obj.GetMember(member, index);
                cont.Val = val;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (BooleanContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Val = val;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var cont = (BooleanSequenceContainer) tmp.Obj.GetMember(member, 0);
                if (cont.Count > index)
                {
                    cont[index] = val;
                }
                else
                {
                    cont.Add(val);
                }
            }

            return true;
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (ComboBox c in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (BooleanContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                        {
                            isNullCheckBox[index].Checked = true;
                        }
                    }
                    else
                    {
                        c.Text = cont.Val.ToString();
                        c.BackColor = ColorMap.ENABLED;
                    }
                    if (cont.IsChanged())
                    {
                        (c.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        c.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (ComboBox c in fieldValueControl)
                {
                    var cont = (BooleanContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                        {
                            isNullCheckBox[index].Checked = true;
                        }
                    }
                    else
                    {
                        c.Text = cont.Val.ToString();
                        c.BackColor = ColorMap.ENABLED;
                    }
                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (BooleanSequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (ComboBox t in fieldValueControl)
                {
                    t.Text = cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }


    //---------------------------------------------------
    // Enum control
    //---------------------------------------------------
    public class EnumField : ObjectDataFieldControl
    {
        private readonly string[] enumValueNames;

        public EnumField(ObjectInfo objInfo, int member, long enumTypeId, string name, CollectionType ct, int arraySize)
        {
            var noEnumValues = Operations.GetNumberOfEnumerationValues(enumTypeId);
            enumValueNames = new string[noEnumValues];
            for (var ev = 0; ev < noEnumValues; ev++)
            {
                enumValueNames[ev] = Operations.GetEnumerationValueName(enumTypeId, ev);
            }

            var enumName = Operations.GetName(enumTypeId);
            Init(objInfo, member, enumName, name, ct, arraySize);
        }

        protected override Control CreateValueControl()
        {
            var combo = new ComboBox();
            combo.Font = dataFont;
            combo.SelectedIndexChanged += ObjectDataFieldControl_TextChanged;
            combo.Items.AddRange(enumValueNames);
            combo.SelectedIndex = 0;
            combo.DropDownStyle = ComboBoxStyle.DropDownList;
            combo.DropDownWidth = 400;
            combo.Width = 100;
            combo.ContextMenuStrip = new ContextMenuStrip();
            combo.ContextMenuStrip.Items.Add(new ToolStripMenuItem("Is changed", null, OnIsChangedMenuItem));
            return combo;
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var container = tmp.Obj.GetMember(member, 0);
            var containerType = container.GetType();
            var enumType =
                containerType.Assembly.GetType(containerType.ReflectedType.FullName).GetNestedType("Enumeration");
            var enumVal = Enum.GetValues(enumType).GetValue(0);
            containerType.GetMethod("Insert").Invoke(container, new[] {index, enumVal});
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            if (!setVal)
                return true;

            var c = (ComboBox) fieldValueControl[index];
            var tmp = (ObjectInfo) Tag;

            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (EnumerationContainerBase) tmp.Obj.GetMember(member, index);
                cont.Ordinal = c.SelectedIndex;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (EnumerationContainerBase) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Ordinal = c.SelectedIndex;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var container = tmp.Obj.GetMember(member, 0);
                var containerType = container.GetType();
                var enumType =
                    containerType.Assembly.GetType(containerType.ReflectedType.FullName).GetNestedType("Enumeration");
                var enumVal = Enum.GetValues(enumType).GetValue(c.SelectedIndex);

                var numValues = (int) containerType.GetProperty("Count").GetValue(container, null);
                if (numValues > index)
                {
                    containerType.GetProperty("Item").SetValue(container, enumVal, new object[] {index});
                }
                else
                {
                    containerType.GetMethod("Add").Invoke(container, new[] {enumVal});
                }
            }

            return true;
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (ComboBox c in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (EnumerationContainerBase) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        c.SelectedIndex = cont.Ordinal;
                        c.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (c.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        c.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (ComboBox c in fieldValueControl)
                {
                    var cont = (EnumerationContainerBase) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        c.SelectedIndex = cont.Ordinal;
                        c.BackColor = ColorMap.ENABLED;
                    }
                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);

                var tmp = (ObjectInfo) Tag;
                var container = tmp.Obj.GetMember(member, 0);
                var containerType = container.GetType();

                if ((int) containerType.GetProperty("Count").GetValue(container, null) != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                var ix = new object[1] {index};
                foreach (ComboBox t in fieldValueControl)
                {
                    ix[0] = index++;
                    var value = (int) containerType.GetProperty("Item").GetValue(container, ix);
                    t.SelectedIndex = value;
                    t.BackColor = ColorMap.ENABLED;
                }
            }
        }
    }

    //---------------------------------------------------
    // Int32 control
    //---------------------------------------------------
    public class Int32Field : ObjectDataFieldControl
    {
        public Int32Field(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "Int32", name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (Int32SequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, 0);
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                var c = (TextBox) fieldValueControl[index];
                var val = int.Parse(c.Text);

                if (!setVal)
                    return true;

                var tmp = (ObjectInfo) Tag;
                if (collectionType == CollectionType.SingleValueCollectionType ||
                    collectionType == CollectionType.ArrayCollectionType)
                {
                    changed[index] = false;
                    var cont = (Int32Container) tmp.Obj.GetMember(member, index);
                    cont.Val = val;
                    ;
                }
                else if (collectionType == CollectionType.DictionaryCollectionType)
                {
                    changed[index] = false;
                    var cont = (Int32Container) GetDictionaryValue(fieldNameLabel[index].Tag);
                    cont.Val = val;
                }
                else if (collectionType == CollectionType.SequenceCollectionType)
                {
                    var cont = (Int32SequenceContainer) tmp.Obj.GetMember(member, 0);
                    if (cont.Count > index)
                    {
                        cont[index] = val;
                    }
                    else
                    {
                        cont.Add(val);
                    }
                }

                return true;
            }
            catch
            {
                return false;
            }
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (Int32Container) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (Int32Container) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (Int32SequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }

    //---------------------------------------------------
    // Int64 control
    //---------------------------------------------------
    public class Int64Field : ObjectDataFieldControl
    {
        public Int64Field(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "Int64", name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (Int64SequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, 0);
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                var c = (TextBox) fieldValueControl[index];
                var val = long.Parse(c.Text);

                if (!setVal)
                    return true;

                var tmp = (ObjectInfo) Tag;
                if (collectionType == CollectionType.SingleValueCollectionType ||
                    collectionType == CollectionType.ArrayCollectionType)
                {
                    changed[index] = false;
                    var cont = (Int64Container) tmp.Obj.GetMember(member, index);
                    cont.Val = val;
                    ;
                }
                else if (collectionType == CollectionType.DictionaryCollectionType)
                {
                    changed[index] = false;
                    var cont = (Int64Container) GetDictionaryValue(fieldNameLabel[index].Tag);
                    cont.Val = val;
                }
                else if (collectionType == CollectionType.SequenceCollectionType)
                {
                    var cont = (Int64SequenceContainer) tmp.Obj.GetMember(member, 0);
                    if (cont.Count > index)
                    {
                        cont[index] = val;
                    }
                    else
                    {
                        cont.Add(val);
                    }
                }

                return true;
            }
            catch
            {
                return false;
            }
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (Int64Container) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (Int64Container) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (Int64SequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }

    //---------------------------------------------------
    // Float32 control
    //---------------------------------------------------
    public class Float32Field : ObjectDataFieldControl
    {
        public Float32Field(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize,
            string typeName) : base(objInfo, member, typeName, name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (Float32SequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, 0);
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                var c = (TextBox) fieldValueControl[index];
                var val = float.Parse(c.Text);

                if (!setVal)
                    return true;

                var tmp = (ObjectInfo) Tag;
                if (collectionType == CollectionType.SingleValueCollectionType ||
                    collectionType == CollectionType.ArrayCollectionType)
                {
                    changed[index] = false;
                    var cont = (Float32Container) tmp.Obj.GetMember(member, index);
                    cont.Val = val;
                }
                else if (collectionType == CollectionType.DictionaryCollectionType)
                {
                    changed[index] = false;
                    var cont = (Float32Container) GetDictionaryValue(fieldNameLabel[index].Tag);
                    cont.Val = val;
                }
                else if (collectionType == CollectionType.SequenceCollectionType)
                {
                    var cont = (Float32SequenceContainer) tmp.Obj.GetMember(member, 0);
                    if (cont.Count > index)
                    {
                        cont[index] = val;
                    }
                    else
                    {
                        cont.Add(val);
                    }
                }

                return true;
            }
            catch
            {
                return false;
            }
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (Float32Container) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (Float32Container) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (Float32SequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }

    //---------------------------------------------------
    // Float64 control
    //---------------------------------------------------
    public class Float64Field : ObjectDataFieldControl
    {
        public Float64Field(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize,
            string typeName)
            : base(objInfo, member, typeName, name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (Float64SequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, 0);
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                var c = (TextBox) fieldValueControl[index];
                var val = double.Parse(c.Text);

                if (!setVal)
                    return true;

                var tmp = (ObjectInfo) Tag;
                if (collectionType == CollectionType.SingleValueCollectionType ||
                    collectionType == CollectionType.ArrayCollectionType)
                {
                    changed[index] = false;
                    var cont = (Float64Container) tmp.Obj.GetMember(member, index);
                    cont.Val = val;
                    ;
                }
                else if (collectionType == CollectionType.DictionaryCollectionType)
                {
                    changed[index] = false;
                    var cont = (Float64Container) GetDictionaryValue(fieldNameLabel[index].Tag);
                    cont.Val = val;
                }
                else if (collectionType == CollectionType.SequenceCollectionType)
                {
                    var cont = (Float64SequenceContainer) tmp.Obj.GetMember(member, 0);
                    if (cont.Count > index)
                    {
                        cont[index] = val;
                    }
                    else
                    {
                        cont.Add(val);
                    }
                }

                return true;
            }
            catch
            {
                return false;
            }
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (Float64Container) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (Float64Container) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (Float64SequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }

    //---------------------------------------------------
    // String control
    //---------------------------------------------------
    public class StringField : ObjectDataFieldControl
    {
        private readonly int maxLength;

        public StringField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize, int maxLength)
        {
            this.maxLength = maxLength;
            Init(objInfo, member, "String", name, ct, arraySize);
        }

        protected override Control CreateValueControl()
        {
            var control = base.CreateValueControl() as TextBox;
            if (control != null)
            {
                control.MaxLength = maxLength;
            }
            return control;
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (StringSequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, "");
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            if (!setVal)
                return true;

            var c = (TextBox) fieldValueControl[index];
            var tmp = (ObjectInfo) Tag;

            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (StringContainer) tmp.Obj.GetMember(member, index);
                cont.Val = c.Text;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (StringContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Val = c.Text;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var cont = (StringSequenceContainer) tmp.Obj.GetMember(member, 0);
                if (cont.Count > index)
                {
                    cont[index] = c.Text;
                }
                else
                {
                    cont.Add(c.Text);
                }
            }

            return true;
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (StringContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        // STSYLI 20080821: Remove whitespace from the beginning and end of the string
                        // was problem when loading serializable data and lots of white spaces in beginning
                        // resulted in not showing the actual data
                        t.Text = cont.Val.Trim();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (StringContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.Trim();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (StringSequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = cont[index].Trim();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }

    //---------------------------------------------------
    // TypeId control
    //---------------------------------------------------
    public class TypeIdField : ObjectDataFieldControl
    {
        public TypeIdField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "TypeId", name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (TypeIdSequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, 0);
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            long val;
            var c = (TextBox) fieldValueControl[index];
            try
            {
                val = long.Parse(c.Text);
            }
            catch
            {
                val = Operations.GetTypeId(c.Text);
            }

            if (!setVal)
                return true;

            var tmp = (ObjectInfo) Tag;
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (TypeIdContainer) tmp.Obj.GetMember(member, index);
                cont.Val = val;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (TypeIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Val = val;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var cont = (TypeIdSequenceContainer) tmp.Obj.GetMember(member, 0);
                if (cont.Count > index)
                {
                    cont[index] = val;
                }
                else
                {
                    cont.Add(val);
                }
            }
            return true;
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (TypeIdContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = Operations.Exists(cont.Val) ? Operations.GetName(cont.Val) : cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }
                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (TypeIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = Operations.Exists(cont.Val) ? Operations.GetName(cont.Val) : cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (TypeIdSequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = Operations.Exists(cont[index]) ? Operations.GetName(cont[index]) : cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }


    //---------------------------------------------------
    // ChannelId control
    //---------------------------------------------------
    public class ChannelIdField : ObjectDataFieldControl
    {
        public ChannelIdField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "ChannelId", name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (ChannelIdSequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, new ChannelId());
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            var c = (TextBox) fieldValueControl[index];
            long val;
            string idString = null;

            if (!long.TryParse(c.Text, out val))
            {
                idString = c.Text.Trim().Replace("\"", "");
                if (idString == string.Empty)
                {
                    return false;
                }
            }

            if (!setVal)
                return true;

            var id = string.IsNullOrEmpty(idString) ? new ChannelId(val) : new ChannelId(idString);

            var tmp = (ObjectInfo) Tag;
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (ChannelIdContainer) tmp.Obj.GetMember(member, index);
                cont.Val = id;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (ChannelIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Val = id;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var cont = (ChannelIdSequenceContainer) tmp.Obj.GetMember(member, 0);
                if (cont.Count > index)
                {
                    cont[index] = id;
                }
                else
                {
                    cont.Add(id);
                }
            }
            return true;
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (ChannelIdContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (ChannelIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (ChannelIdSequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }

    //---------------------------------------------------
    // HandlerId control
    //---------------------------------------------------
    public class HandlerIdField : ObjectDataFieldControl
    {
        public HandlerIdField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "HandlerId", name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (HandlerIdSequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, new HandlerId());
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            var c = (TextBox) fieldValueControl[index];
            long val;
            string idString = null;

            if (!long.TryParse(c.Text, out val))
            {
                idString = c.Text.Trim().Replace("\"", "");
                if (idString == string.Empty)
                {
                    return false;
                }
            }

            if (!setVal)
                return true;

            var id = string.IsNullOrEmpty(idString) ? new HandlerId(val) : new HandlerId(idString);

            var tmp = (ObjectInfo) Tag;
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (HandlerIdContainer) tmp.Obj.GetMember(member, index);
                cont.Val = id;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (HandlerIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Val = id;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var cont = (HandlerIdSequenceContainer) tmp.Obj.GetMember(member, 0);
                if (cont.Count > index)
                {
                    cont[index] = id;
                }
                else
                {
                    cont.Add(id);
                }
            }
            return true;
        }


        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (HandlerIdContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (HandlerIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (HandlerIdSequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }

    //---------------------------------------------------
    // EntityId control
    //---------------------------------------------------
    public class EntityIdField : ObjectDataFieldControl
    {
        public EntityIdField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "EntityId", name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (EntityIdSequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, new EntityId());
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            var entityId = new EntityId();
            var c = (TextBox) fieldValueControl[index];
            var s = c.Text.Trim().Replace(':', ' ').Split(' ');
            if (s.Length < 2)
            {
                return false;
            }
            if (s.Length > 2)
            {
                for (var i = 1; i < s.Length - 1; i++)
                {
                    if (s[i] != "")
                        return false;
                }
            }

            try
            {
                entityId.TypeId = long.Parse(s[0]);
            }
            catch
            {
                entityId.TypeId = Operations.GetTypeId(s[0]);
            }

            try
            {
                entityId.InstanceId = new InstanceId(long.Parse(s[s.Length - 1]));
            }
            catch
            {
                string idString;
                if (s[s.Length - 1].StartsWith("\"") && s[s.Length - 1].EndsWith("\"") && s[s.Length - 1].Length > 2)
                {
                    // remove quotation
                    idString = s[s.Length - 1].Substring(1, s[s.Length - 1].Length - 2);
                }
                else
                {
                    idString = s[s.Length - 1];
                }

                if (idString == "")
                {
                    return false;
                }
                entityId.InstanceId = new InstanceId(idString);
            }

            if (!setVal)
                return true;

            var tmp = (ObjectInfo) Tag;
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (EntityIdContainer) tmp.Obj.GetMember(member, index);
                cont.Val = entityId;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (EntityIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Val = entityId;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var cont = (EntityIdSequenceContainer) tmp.Obj.GetMember(member, 0);
                if (cont.Count > index)
                {
                    cont[index] = entityId;
                }
                else
                {
                    cont.Add(entityId);
                }
            }
            return true;
        }


        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (EntityIdContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        var val = cont.Val;
                        var typename = "";
                        try
                        {
                            typename = Operations.GetName(val.TypeId);
                        }
                        catch
                        {
                            typename = val.TypeId.ToString();
                        }

                        t.Text = typename + " : " + val.InstanceId;
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (EntityIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        var val = cont.Val;
                        var typename = "";
                        try
                        {
                            typename = Operations.GetName(val.TypeId);
                        }
                        catch
                        {
                            typename = val.TypeId.ToString();
                        }

                        t.Text = typename + " : " + val.InstanceId;
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (EntityIdSequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var eid = cont[index];
                    var typename = Operations.Exists(eid.TypeId)
                        ? Operations.GetName(eid.TypeId)
                        : eid.TypeId.ToString();
                    t.Text = typename + " : " + eid.InstanceId;
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }

    //---------------------------------------------------
    // InstanceId control
    //---------------------------------------------------
    public class InstanceIdField : ObjectDataFieldControl
    {
        public InstanceIdField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "InstanceId", name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (InstanceIdSequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, new InstanceId());
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            var c = (TextBox) fieldValueControl[index];
            long val;
            string idString = null;

            if (!long.TryParse(c.Text, out val))
            {
                idString = c.Text.Trim().Replace("\"", "");
                if (idString == string.Empty)
                {
                    return false;
                }
            }

            if (!setVal)
                return true;

            var id = string.IsNullOrEmpty(idString) ? new InstanceId(val) : new InstanceId(idString);

            var tmp = (ObjectInfo) Tag;
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (InstanceIdContainer) tmp.Obj.GetMember(member, index);
                cont.Val = id;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (InstanceIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Val = id;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var cont = (InstanceIdSequenceContainer) tmp.Obj.GetMember(member, 0);
                if (cont.Count > index)
                {
                    cont[index] = id;
                }
                else
                {
                    cont.Add(id);
                }
            }
            return true;
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (InstanceIdContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (InstanceIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (InstanceIdSequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }


    //---------------------------------------------------
    // Object control
    //---------------------------------------------------
    public class ObjectField : ObjectDataFieldControl
    {
        private readonly long typeId;

        public ObjectField(ObjectInfo objInfo, int member, long typeId, string name, CollectionType ct, int arraySize)
        {
            this.typeId = typeId;
            Init(objInfo, member, Operations.GetName(typeId), name, ct, arraySize);
        }

        public void TypeChange(long newTypeId, ObjectEditPanel sender)
        {
            var index = IndexOfEditPanel(sender);
            var objButton = fieldValueControl[index] as ObjectExpandCollapseButton;
            ChangedValue(fieldValueControl, objButton);
            Controls.Remove(objButton.ObjPanel);
            var objInfo = new ObjectInfo();

            objInfo.Obj = ObjectFactory.Instance.CreateObject(newTypeId);

            objButton.Tag = objInfo;
            objButton.ObjPanel = new ObjectEditPanel(objInfo, typeId, true);
            objButton.ObjPanel.ParentObjectField = this;
            objButton.ObjPanel.AutoScroll = false;

            parentObjectEditPanel.ChangedDataField();

            parentObjectEditPanel.ExpandCollapse(member); //to force total resize
        }

        public void ExpandCollapse(ObjectEditPanel sender)
        {
            parentObjectEditPanel.ExpandCollapse(member);
        }

        public void ChangedEditPanel(ObjectEditPanel ep)
        {
            var index = IndexOfEditPanel(ep);
            ChangedValue(new[] {fieldValueControl[index]}, ep);
            parentObjectEditPanel.ChangedDataField();
        }

        public int IndexOfEditPanel(ObjectEditPanel ep)
        {
            for (var i = 0; i < fieldValueControl.Count; i++)
            {
                var objButton = fieldValueControl[i] as ObjectExpandCollapseButton;
                if (objButton != null && objButton.ObjPanel == ep)
                {
                    return i;
                }
            }
            return -1;
        }

        protected override Control CreateValueControl()
        {
            var button = new ObjectExpandCollapseButton();
            button.Appearance = Appearance.Button;
            button.Font = dataFont;
            button.BackColor = ColorMap.ENABLED;
            button.ForeColor = Color.Black;
            button.Text = "Expand";
            button.Height = 20;
            button.Width = 100;
            button.CheckedChanged += button_CheckedChanged;
            button.ContextMenuStrip = new ContextMenuStrip();
            if (collectionType == CollectionType.SequenceCollectionType)
            {
                button.ContextMenuStrip.Items.Add(new ToolStripMenuItem("Is changed", null, OnIsChangedMenuItem));
            }
            else
            {
                button.ContextMenuStrip.Items.AddRange(
                    new ToolStripItem[]
                    {
                        new ToolStripMenuItem("Is changed", null, OnIsChangedMenuItem),
                        new ToolStripMenuItem("Is changed here", null, OnIsChangedHereMenuItem)
                    });
            }


            var expandAllMenuItem = new MenuItem("Expand all", OnExpandAll_ContextMenu);
            var collapseAllMenuItem = new MenuItem("Collapse all", OnCollapseAll_ContextMenu);
            ContextMenu = new ContextMenu(new[] {expandAllMenuItem, collapseAllMenuItem});

            return button;
        }

        protected void OnIsChangedHereMenuItem(object sender, EventArgs e)
        {
            var i = FindMenuItemOwnerControl(sender);

            var changedHere = sender as ToolStripMenuItem;
            var changed = (ToolStripMenuItem) changedHere.GetCurrentParent().Items[0];
            changedHere.Checked = !changedHere.Checked;
            if (changedHere.Checked)
            {
                changed.Checked = true;
            }
            if (changed.Checked)
            {
                fieldValueControl[i].BackColor = ColorMap.CHANGED;
            }
            else
            {
                fieldValueControl[i].BackColor = ColorMap.ENABLED;
                ValidateMember();
            }

            parentObjectEditPanel.ChangedDataField();
        }

        private void OnExpandAll_ContextMenu(object o, EventArgs e)
        {
            if (collectionType == CollectionType.SequenceCollectionType)
            {
                foreach (ObjectExpandCollapseButton b in fieldValueControl)
                {
                    b.Checked = true;
                }
            }
            else
            {
                for (var i = 0; i < isNullCheckBox.Count; i++)
                {
                    if (!isNullCheckBox[i].Checked)
                    {
                        ((ObjectExpandCollapseButton) fieldValueControl[i]).Checked = true;
                    }
                }
            }
        }

        private void OnCollapseAll_ContextMenu(object o, EventArgs e)
        {
            if (collectionType == CollectionType.SequenceCollectionType)
            {
                foreach (ObjectExpandCollapseButton b in fieldValueControl)
                {
                    b.Checked = false;
                }
            }
            else
            {
                for (var i = 0; i < isNullCheckBox.Count; i++)
                {
                    var cb = fieldValueControl[i] as ObjectExpandCollapseButton;
                    if (cb.Checked)
                    {
                        cb.Checked = false;
                    }
                }
            }
        }

        public void DeleteObjPanel(int index)
        {
            var b = fieldValueControl[index] as ObjectExpandCollapseButton;
            if (b != null && b.ObjPanel != null)
            {
                Controls.Remove(b.ObjPanel);
            }
        }

        protected override void InsertSequenceItem(int index)
        {
            var b = fieldValueControl[index] as ObjectExpandCollapseButton;

            if (b.Tag == null)
            {
                b.Tag = new ObjectInfo();
                ((ObjectInfo) b.Tag).Obj = ObjectFactory.Instance.CreateObject(typeId);
                b.ObjPanel = new ObjectEditPanel((ObjectInfo) b.Tag, typeId, true);
                b.ObjPanel.ParentObjectField = this;
                b.ObjPanel.AutoScroll = false;
                fieldValueControl[index].BackColor = ColorMap.CHANGED;
                ((ToolStripMenuItem) fieldValueControl[index].ContextMenuStrip.Items[0]).Checked = true; //isChanged
            }

            var tmp = (ObjectInfo) Tag;
            var obj = ((ObjectInfo) b.Tag).Obj;
            var container = tmp.Obj.GetMember(member, 0);
            var containerType = container.GetType();
            containerType.GetMethod("Insert").Invoke(container, new object[] {index, obj});
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            var objButton = fieldValueControl[index] as ObjectExpandCollapseButton;
            if (objButton == null)
                return false;

            if (objButton.Tag == null)
                return false;

            if (!setVal)
                return true;

            if (!objButton.ObjPanel.SetObjectMembers())
                return false;

            var tmp = (ObjectInfo) Tag;
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (ObjectContainerBase) tmp.Obj.GetMember(member, index);
                cont.InternalObj = ((ObjectInfo) objButton.ObjPanel.Tag).Obj;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (ObjectContainerBase) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.InternalObj = ((ObjectInfo) objButton.ObjPanel.Tag).Obj;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var obj = ((ObjectInfo) objButton.Tag).Obj;
                var container = tmp.Obj.GetMember(member, 0);
                var containerType = container.GetType();

                var numValues = (int) containerType.GetProperty("Count").GetValue(container, null);
                if (numValues > index)
                {
                    containerType.GetProperty("Item").SetValue(container, obj, new object[] {index});
                }
                else
                {
                    containerType.GetMethod("Add").Invoke(container, new object[] {obj});
                }
            }
            return true;
        }


        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType ||
                collectionType == CollectionType.DictionaryCollectionType)
            {
                var forceResize = false;
                var tmp = (ObjectInfo) Tag;
                var index = 0;
                foreach (ObjectExpandCollapseButton b in fieldValueControl)
                {
                    var cont = collectionType != CollectionType.DictionaryCollectionType
                        ? (ObjectContainerBase) tmp.Obj.GetMember(member, index)
                        : (ObjectContainerBase) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;

                        b.Tag = null;
                        b.Checked = false;
                    }
                    else
                    {
                        var objectInfo = new ObjectInfo();
                        objectInfo.Obj = cont.InternalObj;
                        b.Tag = objectInfo;

                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = false;

                        b.BackColor = ColorMap.ENABLED;
                        var objButton = fieldValueControl[index] as ObjectExpandCollapseButton;
                        if (objButton.ObjPanel != null)
                        {
                            //if (((Safir.Dob.Typesystem.Object)(objPanel[index].Tag)).GetTypeId() == cont.InternalObj.GetTypeId())
                            if (((ObjectInfo) objButton.ObjPanel.Tag).Obj.GetTypeId() == cont.InternalObj.GetTypeId())
                            {
                                objButton.ObjPanel.UpdateData(objectInfo);
                            }
                            else //Dynamic type has changed (inheritance)
                            {
                                Controls.Remove(objButton.ObjPanel);
                                objButton.ObjPanel = new ObjectEditPanel(objectInfo, typeId, true);
                                objButton.ObjPanel.ParentObjectField = this;
                                objButton.ObjPanel.AutoScroll = false;
                                Controls.Add(objButton.ObjPanel);
                                forceResize = true;
                            }
                        }
                        else
                        {
                            objButton.ObjPanel = new ObjectEditPanel(objectInfo, typeId, true);
                            objButton.ObjPanel.ParentObjectField = this;
                            objButton.ObjPanel.AutoScroll = false;
                        }

                        var w = X_VALUE_START + objButton.ObjPanel.Width + X_STEP;
                        if (w > Width)
                        {
                            Width = w;
                        }
                    }

                    if (cont.IsChanged())
                    {
                        if (collectionType == CollectionType.DictionaryCollectionType)
                        {
                            SetDictionaryChanged(cont.IsChanged(), index);
                        }
                        else
                        {
                            (b.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                            b.BackColor = ColorMap.CHANGED;
                            changed[index] = true;
                        }
                    }

                    index++;
                }

                if (forceResize)
                {
                    parentObjectEditPanel.ExpandCollapse(member);
                    Invalidate();
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var container = tmp.Obj.GetMember(member, 0);
                var containerType = container.GetType();

                if ((int) containerType.GetProperty("Count").GetValue(container, null) != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var forceResize = false;
                var index = 0;
                var ix = new object[] {index};
                foreach (ObjectExpandCollapseButton b in fieldValueControl)
                {
                    ix[0] = index++;
                    var obj = (Object) containerType.GetProperty("Item").GetValue(container, ix);
                    b.BackColor = ColorMap.ENABLED;

                    var objectInfo = new ObjectInfo();
                    objectInfo.Obj = obj;
                    b.Tag = objectInfo;

                    if (b.ObjPanel != null)
                    {
                        if (((ObjectInfo) b.ObjPanel.Tag).Obj.GetTypeId() == obj.GetTypeId())
                        {
                            b.ObjPanel.UpdateData(objectInfo);
                        }
                        else //Dynamic type has changed (inheritance)
                        {
                            Controls.Remove(b.ObjPanel);
                            b.ObjPanel = new ObjectEditPanel(objectInfo, typeId, true);
                            b.ObjPanel.ParentObjectField = this;
                            b.ObjPanel.AutoScroll = false;
                            Controls.Add(b.ObjPanel);
                            forceResize = true;
                        }
                    }
                    else
                    {
                        b.ObjPanel = new ObjectEditPanel(objectInfo, typeId, true);
                        b.ObjPanel.ParentObjectField = this;
                        b.ObjPanel.AutoScroll = false;
                    }

                    var w = X_VALUE_START + b.ObjPanel.Width + X_STEP;
                    if (w > Width)
                    {
                        Width = w;
                    }
                }

                if (forceResize)
                {
                    parentObjectEditPanel.ExpandCollapse(member);
                    Invalidate();
                }
            }
        }

        private void button_CheckedChanged(object sender, EventArgs e)
        {
            var index = 0;
            foreach (var c in fieldValueControl)
            {
                if (c == sender)
                    break;

                index++;
            }

            var b = (ObjectExpandCollapseButton) sender;

            if (b.Checked)
            {
                b.Text = "Collapse";

                if (b.Tag == null)
                {
                    b.Tag = new ObjectInfo();
                    ((ObjectInfo) b.Tag).Obj = ObjectFactory.Instance.CreateObject(typeId);
                    b.ObjPanel = new ObjectEditPanel((ObjectInfo) b.Tag, typeId, true);
                    b.ObjPanel.ParentObjectField = this;
                    b.ObjPanel.AutoScroll = false;

                    fieldValueControl[index].BackColor = ColorMap.CHANGED;

                    ((ToolStripMenuItem) fieldValueControl[index].ContextMenuStrip.Items[0]).Checked = true; //isChanged
                    ((ToolStripMenuItem) fieldValueControl[index].ContextMenuStrip.Items[1]).Checked = true;
                    //isChangedHere
                    fieldValueControl[index].BackColor = ColorMap.CHANGED;

                    if (collectionType != CollectionType.SequenceCollectionType)
                        changed[index] = true;
                }

                if (collectionType == CollectionType.SingleValueCollectionType ||
                    collectionType == CollectionType.ArrayCollectionType)
                {
                    var cont = (ObjectContainerBase) ((ObjectInfo) Tag).Obj.GetMember(member, index);
                    cont.InternalObj = ((ObjectInfo) b.Tag).Obj;

                    //Safir.Dob.Typesystem.ObjectContainerBase cont = (Safir.Dob.Typesystem.ObjectContainerBase)((Safir.Dob.Typesystem.Object)((ObjectInfo)Tag).Obj).GetMember(member, index);
                    //cont.InternalObj = (Safir.Dob.Typesystem.Object)((ObjectInfo)b.Tag).Obj;
                }
                else if (collectionType == CollectionType.DictionaryCollectionType)
                {
                    var cont = (ObjectContainerBase) GetDictionaryValue(fieldNameLabel[index].Tag);
                    cont.InternalObj = ((ObjectInfo) b.Tag).Obj;
                }
            }
            else
            {
                b.Text = "Expand";
            }

            parentObjectEditPanel.ExpandCollapse(member);

            if (collectionType != CollectionType.SequenceCollectionType)
            {
                if (isNullCheckBox.Count > index)
                    isNullCheckBox[index].Checked = false;
            }

            if (!ValidInput(index, false))
                fieldValueControl[index].BackColor = ColorMap.ERROR;

            parentObjectEditPanel.ChangedDataField();

            b.ObjPanel.Focus();
        }

        protected override void PositionControls(int index, ref Point location)
        {
            base.PositionControls(index, ref location);
            if (fieldValueControl.Count <= index)
                return;

            var b = fieldValueControl[index] as ObjectExpandCollapseButton;

            if (b == null)
                return;

            if (b.Checked)
            {
                var w = X_VALUE_START + b.ObjPanel.Width + X_STEP;
                if (w > Width)
                    Width = w;

                location.X = X_VALUE_START;


                b.ObjPanel.Location = location;

                location.X = X_NAME_START;
                location.Y += b.ObjPanel.Height + Y_STEP;

                Controls.Add(b.ObjPanel);
            }
            else
            {
                if (b.ObjPanel != null)
                {
                    Controls.Remove(b.ObjPanel);
                }
                location.X = X_NAME_START;
            }
        }

        public override void ResetIndex(int i)
        {
            base.ResetIndex(i);
            var objButton = fieldValueControl[i] as ObjectExpandCollapseButton;
            (objButton.ContextMenuStrip.Items[1] as ToolStripMenuItem).Checked = false;
            if (objButton.ObjPanel != null)
                objButton.ObjPanel.ResetChanged();
        }

        private class ObjectExpandCollapseButton : CheckBox
        {
            public ObjectEditPanel ObjPanel { get; set; }
        }
    }

    //---------------------------------------------------
    // Binary control
    //---------------------------------------------------
    public class BinaryField : ObjectDataFieldControl
    {
        public BinaryField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "Binary", name, ct, arraySize)
        {
        }

        protected override Control CreateValueControl()
        {
            var control = base.CreateValueControl() as TextBox;
            if (control != null)
            {
                control.ContextMenuStrip.Items.AddRange(new ToolStripItem[]
                {
                    new ToolStripSeparator(),
                    new ToolStripMenuItem("Save to file...", null, OnSaveToFile),
                    new ToolStripMenuItem("Load from file...", null, OnLoadFromFile)
                });
            }
            return control;
        }

        private void OnSaveToFile(object sender, EventArgs e)
        {
            var i = FindMenuItemOwnerControl(sender);

            var sfd = new SaveFileDialog();
            if (sfd.ShowDialog() == DialogResult.OK)
            {
                using (TextWriter writer = new StreamWriter(sfd.FileName))
                {
                    writer.Write(fieldValueControl[i].Text);
                    writer.Flush();
                    writer.Close();
                }
            }
        }

        private void OnLoadFromFile(object sender, EventArgs e)
        {
            var i = FindMenuItemOwnerControl(sender);

            var ofd = new OpenFileDialog();

            if (ofd.ShowDialog() == DialogResult.OK)
            {
                using (TextReader reader = new StreamReader(ofd.FileName))
                {
                    fieldValueControl[i].Text = reader.ReadToEnd();
                    reader.Close();
                }
            }
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (BinarySequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, new byte[] {});
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                var c = (TextBox) fieldValueControl[index];
                var val = Convert.FromBase64String(c.Text);

                if (!setVal)
                    return true;

                var tmp = (ObjectInfo) Tag;

                if (collectionType == CollectionType.SingleValueCollectionType ||
                    collectionType == CollectionType.ArrayCollectionType)
                {
                    changed[index] = false;
                    var cont = (BinaryContainer) tmp.Obj.GetMember(member, index);
                    cont.Val = val;
                }
                else if (collectionType == CollectionType.DictionaryCollectionType)
                {
                    changed[index] = false;
                    var cont = (BinaryContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                    cont.Val = val;
                }
                else if (collectionType == CollectionType.SequenceCollectionType)
                {
                    var cont = (BinarySequenceContainer) tmp.Obj.GetMember(member, 0);
                    if (cont.Count > index)
                    {
                        cont[index] = val;
                    }
                    else
                    {
                        cont.Add(val);
                    }
                }
                return true;
            }
            catch
            {
                return false;
            }
        }

        public override void SetFieldValues()
        {
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var tmp = (ObjectInfo) Tag;
                    var cont = (BinaryContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = Convert.ToBase64String(cont.Val);
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        (t.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                        t.BackColor = ColorMap.CHANGED;
                        changed[index] = true;
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    var cont = (BinaryContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = cont.Val.ToString();
                        t.BackColor = ColorMap.ENABLED;
                    }

                    if (cont.IsChanged())
                    {
                        SetDictionaryChanged(cont.IsChanged(), index);
                    }

                    index++;
                }
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                SetSequenceChanged(false);
                var tmp = (ObjectInfo) Tag;
                var cont = (BinarySequenceContainer) tmp.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                foreach (TextBox t in fieldValueControl)
                {
                    t.Text = Convert.ToBase64String(cont[index]);
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }
}