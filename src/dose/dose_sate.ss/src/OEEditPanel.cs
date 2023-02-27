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
using System.Drawing;
using System.Windows.Forms;
using Safir.Dob.Typesystem;
using Object = Safir.Dob.Typesystem.Object;

namespace Sate
{
    //*****************************************************************************************************
    // Object Edit Panel
    //*****************************************************************************************************
    public class ObjectEditPanel : UserControl
    {
        private readonly Label deletedLabel = new Label();
        private readonly TextBox instanceTextBox = new TextBox();

        private readonly Panel subject = new Panel();
        private Control classNameControl;
        private ObjectDataFieldControl[] dataMembers;
        private int noMembers;
        private ObjectField _parentObjectField = null;
        private readonly long _baseTypeId;

        public ObjectEditPanel(ObjectInfo objInfo, bool classComboBox)
            : this(objInfo, objInfo.Obj.GetTypeId(), classComboBox)
        {
        }

        public ObjectEditPanel(ObjectInfo objInfo, long baseTypeId, bool classComboBox)
        {
            if (objInfo == null)
                return;

            _baseTypeId = baseTypeId;
            Tag = objInfo;

            CreateSubject(objInfo, classComboBox);

            //------- Create deleted label, used for notification of deleted live objects ----------
            deletedLabel.ForeColor = Color.Red;
            deletedLabel.Font = new Font("", 8, FontStyle.Bold);
            deletedLabel.Text = "(OBJECT HAS BEEN DELETED)";
            deletedLabel.Location = new Point(20, 50);
            deletedLabel.AutoSize = true;
            deletedLabel.Visible = false;

            CreateDataFields();
            PositionControls();
        }

        public ObjectField ParentObjectField
        {
            get { return _parentObjectField; }
            set { _parentObjectField = value; }
        }

        public long BaseTypeId
        {
            get { return _baseTypeId; }
        }

        private void CreateSubject(ObjectInfo objInfo, bool combo)
        {
            var typeId = objInfo.Obj.GetTypeId();

            //--- Create Subject panel ---
            BackColor = Color.White;

            subject.BackColor = Color.GhostWhite;
            subject.Height = 40;
            subject.Dock = DockStyle.Top;

            var classLabel = new Label();
            classLabel.Location = new Point(20, 10);
            classLabel.AutoSize = false;
            classLabel.Size = new Size(60, 30);
            classLabel.Font = new Font("Courier New", 10, FontStyle.Bold);
            classLabel.Text = "class: ";

            var instanceLabel = new Label();
            instanceLabel.AutoSize = true;
            instanceLabel.Font = new Font("Courier New", 10, FontStyle.Bold);
            instanceLabel.Text = "Instance: ";

            if (combo)
            {
                var cb = new ComboBox();
                cb.DropDownStyle = ComboBoxStyle.DropDownList;
                var baseType = MainForm.Instance.GetType(BaseTypeId);
                cb.Sorted = true;
                cb.Items.Add(baseType);
                foreach (var tid in Operations.GetAllTypeIds())
                {
                    if (Operations.IsClass(tid))
                    {
                        try
                        {
                            var type = MainForm.Instance.GetType(tid);
                            if (type.IsSubclassOf(baseType))
                            {
                                cb.Items.Add(type);
                            }
                        }
                        catch (Safir.Dob.Typesystem.IllegalValueException)
                        {
                            /* this is to handle the case where a type is in dots_kernel, but has
                               not been built. I.e. a parameter class */
                            continue;
                        }
                    }
                }
                if (cb.Items.Count < 2)
                {
                    classNameControl = new Label();
                    classNameControl.Text = Operations.GetName(typeId);
                }
                else
                {
                    cb.SelectedItem = objInfo.Obj.GetType();
                    cb.DropDownWidth = 500;
                    cb.SelectedIndexChanged += classCombo_SelectedIndexChanged;
                    classNameControl = cb;
                }
            }
            else
            {
                classNameControl = new Label();
                classNameControl.Text = Operations.GetName(typeId);
                if (objInfo.Blobsize > 0)
                {
                    classNameControl.Text += "\n(size: " + objInfo.Blobsize + ")";
                }
            }
            classNameControl.Location = new Point(classLabel.Location.X + classLabel.Width, 10);
            classNameControl.AutoSize = false;
            classNameControl.Size = new Size(250, 30);
            classNameControl.ForeColor = Color.Blue;
            classNameControl.Font = new Font("Courier New", 10, FontStyle.Bold);

            if (objInfo is EntityInfo)
            {
                var entityInfo = (EntityInfo) objInfo;
                if (entityInfo.GetInstanceId() == null)
                {
                    instanceTextBox.Text = "";
                }
                else
                {
                    instanceTextBox.Text = entityInfo.GetInstanceId().ToString();
                }
            }
            else
            {
                instanceTextBox.Hide();
                instanceLabel.Hide();
            }

            instanceLabel.Location = new Point(classNameControl.Location.X + classNameControl.Width, 10);

            instanceTextBox.Location = new Point(instanceLabel.Location.X + instanceLabel.Width, 10);
            instanceTextBox.Width = 150;
            instanceTextBox.TextChanged += instanceTextBox_TextChanged;

            subject.Controls.Add(classLabel);
            subject.Controls.Add(classNameControl);
            subject.Controls.Add(instanceLabel);
            subject.Controls.Add(instanceTextBox);

            subject.ContextMenu = new ContextMenu(new[]
            {
                new MenuItem("Locate in class explorer", OnLocateInClassExplorer_Click),
                new MenuItem("Reset all change flags", OnResetAllChangeFlags_Click)
            });


            subject.AllowDrop = true;
            subject.DragEnter += subject_DragEnter;
            subject.DragDrop += subject_DragDrop;
        }

        private void classCombo_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (ParentObjectField != null)
            {
                var typename = ((ComboBox) classNameControl).SelectedItem.ToString();
                if (typename == "Safir.Dob.Typesystem.Object")
                {
                    typename = "Object";
                }
                var newType = Operations.GetTypeId(typename);
                ParentObjectField.TypeChange(newType, this);
            }
        }

        public void ChangedDataField()
        {
            if (ParentObjectField != null)
            {
                ParentObjectField.ChangedEditPanel(this);
            }
        }

        public void ExpandCollapse(int member)
        {
            dataMembers[member].ShowElements();
            dataMembers[member].RePositioning();

            if (dataMembers[member].Width > Width)
            {
                Width = dataMembers[member].Width + 50;
            }

            var location = dataMembers[member].Location;
            location.Y += dataMembers[member].Height + 10;

            for (var i = member + 1; i < noMembers; i++)
            {
                dataMembers[i].Location = location;
                location.Y += dataMembers[i].Height + 10;
            }

            Height = location.Y + 10;

            if (ParentObjectField != null)
            {
                ParentObjectField.ExpandCollapse(this);
            }
        }

        private void subject_DragDrop(object sender, DragEventArgs e)
        {
            var newType = (long) e.Data.GetData(typeof(long));
            if (Operations.IsOfType(newType, BaseTypeId))
            {
                if (ParentObjectField != null)
                {
                    ParentObjectField.TypeChange(newType, this);
                }
            }
        }

        private void subject_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.Move;
        }


        private void OnLocateInClassExplorer_Click(object sender, EventArgs e)
        {            
            var objInfo = Tag as ObjectInfo;
            var typeId = objInfo.Obj.GetTypeId();
            var entityInfo = objInfo as EntityInfo;
            var instanceId = entityInfo?.GetInstanceId().RawValue;
            ExplorerPanel.Instance.LocateInInheritanceTree(typeId, instanceId);
        }

        private void OnResetAllChangeFlags_Click(object sender, EventArgs e)
        {
            if (ParentObjectField != null)
            {
                var i = ParentObjectField.IndexOfEditPanel(this);
                if (i > -1)
                {
                    ParentObjectField.ResetIndex(i);
                }
            }
            else
            {
                ResetChanged();
            }
        }

        private void CreateDataFields()
        {
            //--- Create member data controls ---
            var objInfo = (ObjectInfo) Tag;

            var typeId = objInfo.Obj.GetTypeId();
            noMembers = Members.GetNumberOfMembers(objInfo.Obj.GetTypeId());
            dataMembers = new ObjectDataFieldControl[noMembers];

            for (var member = 0; member < noMembers; member++)
            {
                dataMembers[member] = CreateControl(typeId, member, objInfo);
            }
        }

        private void PositionControls()
        {
            Controls.Clear();

            Width = 500;

            var location = new Point(20, 70);

            for (var member = 0; member < noMembers; member++)
            {
                dataMembers[member].Location = location;
                location.Y = 10 + dataMembers[member].Location.Y + dataMembers[member].Height;

                if (dataMembers[member].Width > Width)
                {
                    Width = dataMembers[member].Width + 50;
                }
            }
            Height = location.Y + 10;

            Controls.Add(subject);
            Controls.Add(deletedLabel);
            Controls.AddRange(dataMembers);
        }

        private ObjectDataFieldControl CreateControl(long typeId, int member, ObjectInfo objInfo)
        {
            ObjectDataFieldControl c = null;
            MemberType memberType;
            MemberType keyType;
            long complexType;
            long keyTypeId;
            int typeSize;
            CollectionType ct;
            int arrLength;
            var memberName = Members.GetInfo(typeId,
                member,
                out memberType,
                out keyType,
                out complexType,
                out keyTypeId,
                out typeSize,
                out ct,
                out arrLength);


            if (ct != CollectionType.ArrayCollectionType)
            {
                arrLength = 1;
            }

            switch (memberType)
            {
                case MemberType.EnumerationMemberType:
                {
                    c = new EnumField(objInfo, member, complexType, memberName, ct, arrLength);
                    break;
                }
                case MemberType.BooleanMemberType:
                {
                    c = new BoolField(objInfo, member, memberName, ct, arrLength);
                    break;
                }
                case MemberType.Int32MemberType:
                {
                    c = new Int32Field(objInfo, member, memberName, ct, arrLength);
                    break;
                }
                case MemberType.Int64MemberType:
                {
                    c = new Int64Field(objInfo, member, memberName, ct, arrLength);
                    break;
                }
                case MemberType.ChannelIdMemberType:
                {
                    c = new ChannelIdField(objInfo, member, memberName, ct, arrLength);
                    break;
                }
                case MemberType.HandlerIdMemberType:
                {
                    c = new HandlerIdField(objInfo, member, memberName, ct, arrLength);
                    break;
                }
                case MemberType.EntityIdMemberType:
                {
                    c = new EntityIdField(objInfo, member, memberName, ct, arrLength);
                    break;
                }
                case MemberType.InstanceIdMemberType:
                {
                    c = new InstanceIdField(objInfo, member, memberName, ct, arrLength);
                    break;
                }
                case MemberType.StringMemberType:
                {
                    c = new StringField(objInfo, member, memberName, ct, arrLength, typeSize);
                    break;
                }
                case MemberType.TypeIdMemberType:
                {
                    c = new TypeIdField(objInfo, member, memberName, ct, arrLength);
                    break;
                }
                case MemberType.ObjectMemberType:
                {
                    c = new ObjectField(objInfo, member, complexType, memberName, ct, arrLength);
                    break;
                }
                case MemberType.BinaryMemberType:
                {
                    c = new BinaryField(objInfo, member, memberName, ct, arrLength);
                    break;
                }
                case MemberType.Float32MemberType:
                case MemberType.Ampere32MemberType:
                case MemberType.CubicMeter32MemberType:
                case MemberType.Hertz32MemberType:
                case MemberType.Joule32MemberType:
                case MemberType.Kelvin32MemberType:
                case MemberType.Kilogram32MemberType:
                case MemberType.Meter32MemberType:
                case MemberType.MeterPerSecond32MemberType:
                case MemberType.MeterPerSecondSquared32MemberType:
                case MemberType.Newton32MemberType:
                case MemberType.Pascal32MemberType:
                case MemberType.Radian32MemberType:
                case MemberType.RadianPerSecond32MemberType:
                case MemberType.RadianPerSecondSquared32MemberType:
                case MemberType.Second32MemberType:
                case MemberType.SquareMeter32MemberType:
                case MemberType.Steradian32MemberType:
                case MemberType.Volt32MemberType:
                case MemberType.Watt32MemberType:
                {
                    c = new Float32Field(objInfo, member, memberName, ct, arrLength,
                        Members.GetTypeName(typeId, member));
                    break;
                }
                case MemberType.Float64MemberType:
                case MemberType.Ampere64MemberType:
                case MemberType.CubicMeter64MemberType:
                case MemberType.Hertz64MemberType:
                case MemberType.Joule64MemberType:
                case MemberType.Kelvin64MemberType:
                case MemberType.Kilogram64MemberType:
                case MemberType.Meter64MemberType:
                case MemberType.MeterPerSecond64MemberType:
                case MemberType.MeterPerSecondSquared64MemberType:
                case MemberType.Newton64MemberType:
                case MemberType.Pascal64MemberType:
                case MemberType.Radian64MemberType:
                case MemberType.RadianPerSecond64MemberType:
                case MemberType.RadianPerSecondSquared64MemberType:
                case MemberType.Second64MemberType:
                case MemberType.SquareMeter64MemberType:
                case MemberType.Steradian64MemberType:
                case MemberType.Volt64MemberType:
                case MemberType.Watt64MemberType:
                {
                    c = new Float64Field(objInfo, member, memberName, ct, arrLength,
                        Members.GetTypeName(typeId, member));
                    break;
                }
            }
            if (c != null)
            {
                c.ParentObjectEditPanel = this;
            }
            return c;
        }

        private bool SetInstance(bool setVal)
        {
            /* instance number textbox isn't used as before */
            return true;
        }

        private void instanceTextBox_TextChanged(object sender, EventArgs e)
        {
            //SetInstance(false);

            resulttype result;
            long val = 0;
            /* try to parse as an long. O/w use the string representation */

            try
            {
                val = long.Parse(instanceTextBox.Text);
                result = resulttype.longvalue;
            }
            catch
            {
                if (instanceTextBox.Text != "")
                {
                    result = resulttype.stringvalue;
                }
                else
                {
                    result = resulttype.novalue;
                }
            }

            if (Tag is MessageInfo)
            {
                switch (result)
                {
                    case resulttype.longvalue:
                        ((MessageInfo) Tag).setChannelId(new ChannelId(val));
                        break;
                    case resulttype.stringvalue:
                        ((MessageInfo) Tag).setChannelId(new ChannelId(instanceTextBox.Text));
                        break;
                    default:
                        ((MessageInfo) Tag).setChannelId(new ChannelId());
                        break;
                }
            }
            else if (Tag is ServiceHandlerInfo)
            {
                switch (result)
                {
                    case resulttype.longvalue:
                        ((ServiceHandlerInfo) Tag).setHandlerId(new HandlerId(val));
                        break;
                    case resulttype.stringvalue:
                        ((ServiceHandlerInfo) Tag).setHandlerId(new HandlerId(instanceTextBox.Text));
                        break;
                    default:
                        ((ServiceHandlerInfo) Tag).setHandlerId(new HandlerId());
                        break;
                }
            }
            else if (Tag is EntityInfo)
            {
                switch (result)
                {
                    case resulttype.longvalue:
                        ((EntityInfo) Tag).SetInstanceId(new InstanceId(val));
                        break;
                    case resulttype.stringvalue:
                        ((EntityInfo) Tag).SetInstanceId(new InstanceId(instanceTextBox.Text));
                        break;
                    default:
                        ((EntityInfo) Tag).SetInstanceId(InstanceId.GenerateRandom());
                        break;
                }
            }
            instanceTextBox.BackColor = ColorMap.ENABLED;
        }


        public bool SetObjectMembers()
        {
            var instanceOk = SetInstance(true);
            var membersOk = true;
            for (var i = 0; i < noMembers; i++)
            {
                membersOk = membersOk && dataMembers[i].ValidateMember();
            }

            if (!instanceOk)
            {
                instanceTextBox.BackColor = ColorMap.ERROR;
                MessageBox.Show("Some of the values in the form are invalid due to the data type!", "Invalid Values",
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

            return instanceOk && membersOk;
        }

        public void UpdateData(ObjectInfo objInfo)
        {
            deletedLabel.Visible = false;
            Tag = objInfo;

            for (var member = 0; member < noMembers; member++)
            {
                dataMembers[member].Tag = objInfo;
                dataMembers[member].SetFieldValues();
            }

            if (objInfo is MessageInfo)
            {
                var msgInfo = (MessageInfo) objInfo;
                instanceTextBox.Text = msgInfo.getChannelId().ToString();
            }
            else if (objInfo is ServiceHandlerInfo)
            {
                var serviceInfo = (ServiceHandlerInfo) objInfo;
                instanceTextBox.Text = serviceInfo.getHandlerId().ToString();
            }
            else if (objInfo is EntityInfo)
            {
                var entityInfo = (EntityInfo) objInfo;
                instanceTextBox.Text = entityInfo.GetInstanceId().ToString();
            }
        }

        public void SetDeleted(bool deleted) //if deleted=false, then removed
        {
            if (deleted)
            {
                deletedLabel.Text = "(OBJECT HAS BEEN DELETED)";
            }
            else
            {
                deletedLabel.Text = "(OBJECT HAS BEEN REMOVED)";
            }
            deletedLabel.Visible = true;
        }

        public void ResetChanged()
        {
            foreach (var c in dataMembers)
            {
                c.ResetChanged();
            }
        }

        private enum resulttype
        {
            longvalue,
            stringvalue,
            novalue
        }
    }
}
