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
using System.Windows.Forms;
using Safir.Dob.Typesystem;

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
        
    }
    
}