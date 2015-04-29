/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace Sate
{
    //*****************************************************************************************************
    // Object Edit Panel
    //*****************************************************************************************************
    public class ObjectEditPanel : System.Windows.Forms.UserControl
    {
        private Label deletedLabel = new Label();
        private Control classNameControl = null;
        private System.Windows.Forms.TextBox instanceTextBox = new TextBox();
        private Panel subject = new Panel();
        private ObjectDataFieldControl[] dataMembers;
        private int noMembers;
        private long baseTypeId;

        private ObjectField parentObjectField = null;

        public ObjectEditPanel(ObjectInfo objInfo, bool classComboBox) : this(objInfo, objInfo.Obj.GetTypeId(), classComboBox)
        {
        }

        public ObjectEditPanel(ObjectInfo objInfo, long baseTypeId, bool classComboBox)
        {
            if (objInfo == null)
                return;

            this.baseTypeId = baseTypeId;
            this.Tag = objInfo;

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

        private void CreateSubject(ObjectInfo objInfo, bool combo)
        {
            long typeId = objInfo.Obj.GetTypeId();

            //--- Create Subject panel ---
            BackColor = Color.White;

            subject.BackColor = Color.GhostWhite;
            subject.Height = 40;
            subject.Dock = DockStyle.Top;

            Label classLabel = new System.Windows.Forms.Label();
            classLabel.Location = new Point(20, 10);
            classLabel.AutoSize = false;
            classLabel.Size = new Size(60, 30);
            classLabel.Font = new Font("Courier New", 10, System.Drawing.FontStyle.Bold);
            classLabel.Text = "class: ";

            Label instanceLabel = new Label();
            instanceLabel.AutoSize = true;
            instanceLabel.Font = new Font("Courier New", 10, System.Drawing.FontStyle.Bold);
            instanceLabel.Text = "Instance: ";

            if (combo)
            {
                ComboBox cb = new ComboBox();
                cb.DropDownStyle = ComboBoxStyle.DropDownList;
                Type baseType = MainForm.Instance.GetType(baseTypeId);
                cb.Sorted = true;
                cb.Items.Add(baseType);
                foreach (Int64 tid in Safir.Dob.Typesystem.Operations.GetAllTypeIds())
                {
                    if (Safir.Dob.Typesystem.Operations.IsClass(tid))
                    {
                        Type type = MainForm.Instance.GetType(tid);
                        if (type.IsSubclassOf(baseType))
                        {
                            cb.Items.Add(type);
                        }
                    }
                }
                if (cb.Items.Count < 2)
                {
                    classNameControl = new Label();
                    classNameControl.Text = Safir.Dob.Typesystem.Operations.GetName(typeId);
                }
                else
                {
                    cb.SelectedItem = objInfo.Obj.GetType();
                    cb.DropDownWidth = 500;
                    cb.SelectedIndexChanged += new EventHandler(classCombo_SelectedIndexChanged);
                    classNameControl = cb;
                }
            }
            else
            {
                classNameControl = new Label();
                classNameControl.Text = Safir.Dob.Typesystem.Operations.GetName(typeId);
                if (objInfo.Blobsize > 0)
                {
                    classNameControl.Text += "\n(size: " + objInfo.Blobsize.ToString() + ")";
                }
            }
            classNameControl.Location = new Point(classLabel.Location.X + classLabel.Width, 10);
            classNameControl.AutoSize = false;
            classNameControl.Size = new Size(250, 30);
            classNameControl.ForeColor = Color.Blue;
            classNameControl.Font = new Font("Courier New", 10, System.Drawing.FontStyle.Bold);

            if (objInfo is EntityInfo)
            {
                EntityInfo entityInfo = (EntityInfo)objInfo;
                if (entityInfo.getInstanceId() == null)
                {
                    instanceTextBox.Text = "";
                }
                else
                {
                    instanceTextBox.Text = entityInfo.getInstanceId().ToString();
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
            instanceTextBox.TextChanged += new EventHandler(instanceTextBox_TextChanged);

            subject.Controls.Add(classLabel);
            subject.Controls.Add(classNameControl);
            subject.Controls.Add(instanceLabel);
            subject.Controls.Add(instanceTextBox);

            subject.ContextMenu = new ContextMenu(new MenuItem[] {
                                        new MenuItem("Locate in class explorer", new EventHandler(OnLocateInClassExplorer_Click)),
                                        new MenuItem("Reset all change flags", new EventHandler(OnResetAllChangeFlags_Click)) });


            subject.AllowDrop = true;
            subject.DragEnter += new DragEventHandler(subject_DragEnter);
            subject.DragDrop += new DragEventHandler(subject_DragDrop);
        }

        void classCombo_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (parentObjectField != null)
            {
                string typename = ((ComboBox)classNameControl).SelectedItem.ToString();
                if (typename == "Safir.Dob.Typesystem.Object")
                {
                    typename = "Object";
                }
                long newType = Safir.Dob.Typesystem.Operations.GetTypeId(typename);
                parentObjectField.TypeChange(newType, this);
            }
        }

        public ObjectField ParentObjectField
        {
            get { return parentObjectField; }
            set { parentObjectField = value; }
        }

        public void ChangedDataField()
        {
            if (parentObjectField != null)
            {
                parentObjectField.ChangedEditPanel(this);
            }
        }

        public void ExpandCollapse(int member)
        {
            dataMembers[member].RePositioning();
            if (dataMembers[member].Width > this.Width)
            {
                this.Width = dataMembers[member].Width + 50;
            }

            Point location = dataMembers[member].Location;
            location.Y += dataMembers[member].Height + 10;

            for (int i = member + 1; i < noMembers; i++)
            {
                dataMembers[i].Location = location;
                location.Y += dataMembers[i].Height + 10;
            }

            Height = location.Y + 10;

            if (parentObjectField != null)
            {
                parentObjectField.ExpandCollapse(this);
            }
        }

        void subject_DragDrop(object sender, DragEventArgs e)
        {
            long newType=(long)e.Data.GetData(typeof(long));
            if (Safir.Dob.Typesystem.Operations.IsOfType(newType, baseTypeId))
            {
                if (parentObjectField != null)
                {
                    parentObjectField.TypeChange(newType, this);
                }
            }
        }

        void subject_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.Move;
        }


        void OnLocateInClassExplorer_Click(object sender, EventArgs e)
        {
            ExplorerPanel.Instance.LocateClass(((Safir.Dob.Typesystem.Object)Tag).GetTypeId());
        }

        void OnResetAllChangeFlags_Click(object sender, EventArgs e)
        {
            if (parentObjectField != null)
            {
                int i = parentObjectField.IndexOfEditPanel(this);
                if (i > -1)
                {
                    parentObjectField.ResetIndex(i);
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
            ObjectInfo objInfo = (ObjectInfo)Tag;

            long typeId = objInfo.Obj.GetTypeId();
            noMembers = Safir.Dob.Typesystem.Members.GetNumberOfMembers(objInfo.Obj.GetTypeId());
            dataMembers = new ObjectDataFieldControl[noMembers];

            for (int member = 0; member < noMembers; member++)
            {
                dataMembers[member] = CreateControl(typeId, member, objInfo);
            }
        }

        private void PositionControls()
        {
            Controls.Clear();

            this.Width = 500;

            Point location = new Point(20, 70);

            for (int member = 0; member < noMembers; member++)
            {
                dataMembers[member].Location = location;
                location.Y = 10 + dataMembers[member].Location.Y + dataMembers[member].Height;

                if (dataMembers[member].Width > this.Width)
                {
                    this.Width = dataMembers[member].Width + 50;
                }
            }
            this.Height = location.Y + 10;

            Controls.Add(subject);
            Controls.Add(deletedLabel);
            Controls.AddRange(dataMembers);
        }

        private ObjectDataFieldControl CreateControl(long typeId, int member, ObjectInfo objInfo)
        {
            ObjectDataFieldControl c = null;
            Safir.Dob.Typesystem.MemberType memberType;
            long complexType;
            int typeSize;
            Safir.Dob.Typesystem.CollectionType ct;
            int arrLength;
            string memberName = Safir.Dob.Typesystem.Members.GetInfo(typeId, member, out memberType, out complexType, out typeSize, out ct, out arrLength);

            if (ct!=Safir.Dob.Typesystem.CollectionType.ArrayCollectionType)
            {
                arrLength = 1;
            }

            switch (memberType)
            {
                case Safir.Dob.Typesystem.MemberType.EnumerationMemberType:
                    {
                        c = new EnumField(objInfo, member, complexType, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.BooleanMemberType:
                    {
                        c = new BoolField(objInfo, member, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.Int32MemberType:
                    {
                        c = new Int32Field(objInfo, member, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.Int64MemberType:
                    {
                        c = new Int64Field(objInfo, member, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.ChannelIdMemberType:
                    {
                        c = new ChannelIdField(objInfo, member, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.HandlerIdMemberType:
                    {
                        c = new HandlerIdField(objInfo, member, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.EntityIdMemberType:
                    {
                        c = new EntityIdField(objInfo, member, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.InstanceIdMemberType:
                    {
                        c = new InstanceIdField(objInfo, member, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.StringMemberType:
                    {
                        c = new StringField(objInfo, member, memberName, arrLength, typeSize);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.TypeIdMemberType:
                    {
                        c = new TypeIdField(objInfo, member, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.ObjectMemberType:
                    {
                        c = new ObjectField(objInfo, member, complexType, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.BinaryMemberType:
                    {
                        c = new BinaryField(objInfo, member, memberName, arrLength);
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.Float32MemberType:
                case Safir.Dob.Typesystem.MemberType.Ampere32MemberType:
                case Safir.Dob.Typesystem.MemberType.CubicMeter32MemberType:
                case Safir.Dob.Typesystem.MemberType.Hertz32MemberType:
                case Safir.Dob.Typesystem.MemberType.Joule32MemberType:
                case Safir.Dob.Typesystem.MemberType.Kelvin32MemberType:
                case Safir.Dob.Typesystem.MemberType.Kilogram32MemberType:
                case Safir.Dob.Typesystem.MemberType.Meter32MemberType:
                case Safir.Dob.Typesystem.MemberType.MeterPerSecond32MemberType:
                case Safir.Dob.Typesystem.MemberType.MeterPerSecondSquared32MemberType:
                case Safir.Dob.Typesystem.MemberType.Newton32MemberType:
                case Safir.Dob.Typesystem.MemberType.Pascal32MemberType:
                case Safir.Dob.Typesystem.MemberType.Radian32MemberType:
                case Safir.Dob.Typesystem.MemberType.RadianPerSecond32MemberType:
                case Safir.Dob.Typesystem.MemberType.RadianPerSecondSquared32MemberType:
                case Safir.Dob.Typesystem.MemberType.Second32MemberType:
                case Safir.Dob.Typesystem.MemberType.SquareMeter32MemberType:
                case Safir.Dob.Typesystem.MemberType.Steradian32MemberType:
                case Safir.Dob.Typesystem.MemberType.Volt32MemberType:
                case Safir.Dob.Typesystem.MemberType.Watt32MemberType:
                    {
                        c = new Float32Field(objInfo, member, memberName, arrLength,
                            Safir.Dob.Typesystem.Members.GetTypeName(typeId, member));
                        break;
                    }
                case Safir.Dob.Typesystem.MemberType.Float64MemberType:
                case Safir.Dob.Typesystem.MemberType.Ampere64MemberType:
                case Safir.Dob.Typesystem.MemberType.CubicMeter64MemberType:
                case Safir.Dob.Typesystem.MemberType.Hertz64MemberType:
                case Safir.Dob.Typesystem.MemberType.Joule64MemberType:
                case Safir.Dob.Typesystem.MemberType.Kelvin64MemberType:
                case Safir.Dob.Typesystem.MemberType.Kilogram64MemberType:
                case Safir.Dob.Typesystem.MemberType.Meter64MemberType:
                case Safir.Dob.Typesystem.MemberType.MeterPerSecond64MemberType:
                case Safir.Dob.Typesystem.MemberType.MeterPerSecondSquared64MemberType:
                case Safir.Dob.Typesystem.MemberType.Newton64MemberType:
                case Safir.Dob.Typesystem.MemberType.Pascal64MemberType:
                case Safir.Dob.Typesystem.MemberType.Radian64MemberType:
                case Safir.Dob.Typesystem.MemberType.RadianPerSecond64MemberType:
                case Safir.Dob.Typesystem.MemberType.RadianPerSecondSquared64MemberType:
                case Safir.Dob.Typesystem.MemberType.Second64MemberType:
                case Safir.Dob.Typesystem.MemberType.SquareMeter64MemberType:
                case Safir.Dob.Typesystem.MemberType.Steradian64MemberType:
                case Safir.Dob.Typesystem.MemberType.Volt64MemberType:
                case Safir.Dob.Typesystem.MemberType.Watt64MemberType:
                    {
                        c = new Float64Field(objInfo, member, memberName, arrLength,
                            Safir.Dob.Typesystem.Members.GetTypeName(typeId, member));
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
            /*
            try
            {
                int instance = int.Parse(instanceTextBox.Text);
                if (setVal)
                    ((Safir.Dob.Typesystem.Object)Tag).InstanceNumber = instance;
                instanceTextBox.BackColor = ColorMap.ENABLED;
                return true;
            }
            catch
            {
                instanceTextBox.BackColor = ColorMap.ERROR;
                return false;
            }
             */
            /* instance number textbox isn't used as before */
            return true;
        }
        private enum resulttype { longvalue, stringvalue, novalue }
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
                        ((MessageInfo)Tag).setChannelId(new Safir.Dob.Typesystem.ChannelId(val));
                        break;
                    case resulttype.stringvalue:
                        ((MessageInfo)Tag).setChannelId(new Safir.Dob.Typesystem.ChannelId(instanceTextBox.Text));
                        break;
                    default:
                        ((MessageInfo)Tag).setChannelId(new Safir.Dob.Typesystem.ChannelId());
                        break;
                }
            }
            else if (Tag is ServiceHandlerInfo)
            {
                switch (result)
                {
                    case resulttype.longvalue:
                        ((ServiceHandlerInfo)Tag).setHandlerId(new Safir.Dob.Typesystem.HandlerId(val));
                        break;
                    case resulttype.stringvalue:
                        ((ServiceHandlerInfo)Tag).setHandlerId(new Safir.Dob.Typesystem.HandlerId(instanceTextBox.Text));
                        break;
                    default:
                        ((ServiceHandlerInfo)Tag).setHandlerId(new Safir.Dob.Typesystem.HandlerId());
                        break;
                }
            }
            else if (Tag is EntityInfo)
            {
                switch (result)
                {
                    case resulttype.longvalue:
                        ((EntityInfo)Tag).setInstanceId(new Safir.Dob.Typesystem.InstanceId(val));
                        break;
                    case resulttype.stringvalue:
                        ((EntityInfo)Tag).setInstanceId(new Safir.Dob.Typesystem.InstanceId(instanceTextBox.Text));
                        break;
                    default:
                        ((EntityInfo)Tag).setInstanceId(Safir.Dob.Typesystem.InstanceId.GenerateRandom());
                        break;
                }
            }
            instanceTextBox.BackColor = ColorMap.ENABLED;
        }


        
        public bool SetObjectMembers()
        {
            bool instanceOk = SetInstance(true);
            bool membersOk = true;
            for (int i = 0; i < noMembers; i++)
            {
                membersOk = membersOk && dataMembers[i].ValidateMember();
            }

            if (!instanceOk)
            {
                instanceTextBox.BackColor = ColorMap.ERROR;
                MessageBox.Show("Some of the values in the form are invalid due to the data type!", "Invalid Values", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

            return instanceOk && membersOk;
        }

        public void UpdateData(ObjectInfo objInfo)
        {
            deletedLabel.Visible = false;
            Tag = objInfo;

            for (int member = 0; member < noMembers; member++)
            {
                dataMembers[member].Tag = objInfo;
                dataMembers[member].SetFieldValues();
            }

            if (objInfo is MessageInfo)
            {
                MessageInfo msgInfo = (MessageInfo)objInfo;
                instanceTextBox.Text = msgInfo.getChannelId().ToString();
            }
            else if (objInfo is ServiceHandlerInfo)
            {
                ServiceHandlerInfo serviceInfo = (ServiceHandlerInfo)objInfo;
                instanceTextBox.Text = serviceInfo.getHandlerId().ToString();
            }
            else if (objInfo is EntityInfo)
            {
                EntityInfo entityInfo = (EntityInfo)objInfo;
                instanceTextBox.Text = entityInfo.getInstanceId().ToString();
            }
        }

        public void SetDeleted(bool deleted)//if deleted=false, then removed
        {
            if (deleted)
            {
                deletedLabel.Text="(OBJECT HAS BEEN DELETED)";
            }
            else
            {
                deletedLabel.Text="(OBJECT HAS BEEN REMOVED)";
            }
            deletedLabel.Visible = true;
        }

        public long BaseTypeId
        {
            get { return baseTypeId; }
        }

        public void ResetChanged()
        {
            foreach (ObjectDataFieldControl c in dataMembers)
            {
                c.ResetChanged();
            }
        }
    }

}
