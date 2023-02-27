using System;
using System.Drawing;
using System.Windows.Forms;
using Safir.Dob.Typesystem;
using Object = Safir.Dob.Typesystem.Object;

namespace Sate
{
    public class ObjectField : ObjectDataFieldControl
    {
        private readonly long typeId;

        public ObjectField(ObjectInfo objInfo, int member, long typeId, string name, CollectionType ct, int arraySize)
        {
            Tag = objInfo;
            this.typeId = typeId;
            Init(member, Operations.GetName(typeId), name, ct, arraySize);
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
                var container = ObjInfo.Obj.GetMember(member, 0);
                var containerType = container.GetType();

                if ((int) containerType.GetProperty("Count").GetValue(container, null) != fieldValueControl.Count)
                {
                    Init(member, typeName, memberName, collectionType, 1);
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
}