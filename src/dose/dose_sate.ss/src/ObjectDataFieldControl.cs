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
using System.Data;
using System.Windows.Forms;

namespace Sate
{
    /// <summary>
    /// Summary description for ObjectDataFieldControl.
    /// </summary>
    public abstract class ObjectDataFieldControl : System.Windows.Forms.UserControl
    {
        protected ObjectEditPanel parentObjectEditPanel = null;

        protected static System.Drawing.Font font=new Font("Courier New", 8, System.Drawing.FontStyle.Bold);
        protected static System.Drawing.Font dataFont=new Font("Courier New", 8, System.Drawing.FontStyle.Regular);

        protected System.Windows.Forms.Label typeLabel;
        protected System.Windows.Forms.Label[] fieldNameLabel;
        protected System.Windows.Forms.CheckBox[] isNullCheckBox;
        protected System.Windows.Forms.Control[] fieldValueControl;
        protected bool[] changed;
        protected int member;
        protected bool[] isNotChanged;

        //Constants for positioning of controls
        protected const int START_Y = 5;
        protected const int Y_STEP = 5;
        protected const int X_STEP = 10;
        protected const int X_TYPE_START = 5;
        protected const int X_NAME_START = 115;
        protected const int X_VALUE_START = 245;
        protected const int X_NULL_START = 355;
        protected const int X_DEFAULT_WIDTH = 470;

        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.Container components = null;
        private ToolTip m_toolTip = new ToolTip();


        protected abstract bool ValidInput(int index, bool setVal);
        public abstract void SetFieldValues();

        public bool ValidateMember()
        {
            bool ok=true;
            for (int index = 0; index < this.fieldValueControl.Length; index++)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.ContainerBase cont = tmp.Obj.GetMember(member, index);

                // set isChanged flag to false if this should be done
                if (isNotChanged[index])
                {
                    // before setting SetChanged(), check if null should be set.
                    // this should be set before as it changes the isChanged flag.
                    if (isNullCheckBox[index].Checked)
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
                    if (isNullCheckBox[index].Checked)
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

        protected int ChangedValue(Control[] controls, object sender)
        {
            int index=0;
            foreach (Control c in controls)
            {
                if (c==sender)
                {
                    ((ToolStripMenuItem)fieldValueControl[index].ContextMenuStrip.Items[0]).Checked = true;
                    fieldValueControl[index].BackColor = ColorMap.CHANGED;
                    changed[index]=true;
                    isNotChanged[index] = false;
                    return index;
                }
                index++;
            }
            return -1;
        }

        protected int FindMenuItemOwnerControl(object sender)
        {
            int index = 0;
            foreach (Control t in fieldValueControl)
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

        private bool ignoreEvent = false;
        private void ObjectDataFieldControl_CheckedChanged(object sender, EventArgs e)
        {
            if (!ignoreEvent)
            {
                ignoreEvent=true;
                int i=ChangedValue(isNullCheckBox, sender);
                if (i>=0)
                {
                    if (fieldValueControl[i] is TextBox)
                        fieldValueControl[i].Text="";

                    parentObjectEditPanel.ChangedDataField();
                }
                ignoreEvent=false;
            }
        }

        protected void ObjectDataFieldControl_MouseHover(object sender, EventArgs e)
        {
            int index = 0;
            foreach (Control c in fieldValueControl)
            {
                if (c == sender)
                {
                    m_toolTip.SetToolTip(c, c.Text);
                }
                index++;
            }
        }

        protected void ObjectDataFieldControl_TextChanged(object sender, EventArgs e)
        {
            if (!ignoreEvent)
            {
                ignoreEvent = true;
                int i = ChangedValue(fieldValueControl, sender);

                if (i >= 0)
                {
                    isNullCheckBox[i].Checked = false;
                    TextBox tb = this.fieldValueControl[i] as TextBox;
                    
                    if (tb != null)
                    {
                        //If it's a textbox then we do some extra stuff
                        m_toolTip.Show(tb.Text, tb, tb.Left, tb.Top, 5000);
                        TextBoxHandler(ref tb);
                    }

                    if (!ValidInput(i, true))
                    {
                        this.fieldValueControl[i].BackColor = ColorMap.ERROR;
                    }
                }

                parentObjectEditPanel.ChangedDataField();

                ignoreEvent = false;
            }
        }

        private void TextBoxHandler(ref TextBox tb)
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
        
        protected virtual void CreateValueControls(int arraySize)
        {
            //Default control is TextBox
            TextBox textBox;
            fieldValueControl = new System.Windows.Forms.TextBox[arraySize];

            for (int i=0; i<arraySize; i++)
            {
                textBox= new TextBox();
                textBox.Multiline = true;
                fieldValueControl[i] = textBox;
                fieldValueControl[i].Font=dataFont;
                fieldValueControl[i].TextChanged+=new EventHandler(ObjectDataFieldControl_TextChanged);
                fieldValueControl[i].MouseHover += new EventHandler(ObjectDataFieldControl_MouseHover);

                ToolStripMenuItem tsi=new ToolStripMenuItem();
                tsi.Text="Is changed";
                tsi.Click+=new EventHandler(OnIsChangedMenuItem);
                fieldValueControl[i].ContextMenuStrip = new ContextMenuStrip();
                fieldValueControl[i].ContextMenuStrip.Items.Add(tsi);
            }
        }

        protected void OnIsChangedMenuItem(object sender, EventArgs e)
        {
            int i=FindMenuItemOwnerControl(sender);
            ToolStripMenuItem mi = sender as ToolStripMenuItem;
            mi.Checked = !mi.Checked;
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

        public virtual void ResetIndex(int i)
        {
            fieldValueControl[i].BackColor = ColorMap.ENABLED;
            (fieldValueControl[i].ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = false;
        }

        public void ResetChanged()
        {
            for (int i = 0; i<fieldValueControl.Length; i++)
            {
                ResetIndex(i);
            }

            ValidateMember();
        }

        public ObjectEditPanel ParentObjectEditPanel
        {
            get { return parentObjectEditPanel; }
            set { parentObjectEditPanel = value; }
        }

        protected ObjectDataFieldControl()
        {
        }

        protected ObjectDataFieldControl(ObjectInfo objInfo, int member, string typeName, string fieldName, int arraySize)
        {
            Init(objInfo, member, typeName, fieldName, arraySize);
            m_toolTip.InitialDelay = 10;
            m_toolTip.AutoPopDelay = 5000;
            m_toolTip.ReshowDelay = 10;
        }

        protected void Init(ObjectInfo objInfo, int member, string typeName, string fieldName, int arraySize)
        {
            if (arraySize==0)
                return;

            ignoreEvent=true;
            this.Tag=objInfo;
            this.member=member;

            this.SuspendLayout();

            CreateValueControls(arraySize);

            this.Name = "ObjectDataFieldControl";
            this.BackColor=System.Drawing.Color.LightGray;
            this.Width = X_DEFAULT_WIDTH;

            typeLabel=new Label();
            typeLabel.AutoSize=false;
            typeLabel.Text=typeName;
            fieldNameLabel=new System.Windows.Forms.Label[arraySize];
            isNullCheckBox=new System.Windows.Forms.CheckBox[arraySize];
            changed=new bool[arraySize];
            isNotChanged = new bool[arraySize];
            System.Drawing.Point location=new Point(X_TYPE_START, START_Y);

            typeLabel.Location=location;
            typeLabel.Font=font;
            location.X=X_NAME_START;
            this.Controls.Add(typeLabel);

            for (int i=0; i<arraySize; i++)
            {
                changed[i]=false;

                fieldNameLabel[i]=new Label();
                fieldNameLabel[i].ForeColor=Color.Blue;
                fieldNameLabel[i].Font=font;
                fieldNameLabel[i].AutoSize=false;
                if (arraySize > 1)
                {
                    fieldNameLabel[i].Text = fieldName + "[" + i + "]";
                }
                else
                {
                    fieldNameLabel[i].Text = fieldName;
                }
                isNullCheckBox[i]=new CheckBox();
                isNullCheckBox[i].Font=font;
                isNullCheckBox[i].Checked=false;
                isNullCheckBox[i].Text="Is Null";
                isNullCheckBox[i].CheckedChanged+=new EventHandler(ObjectDataFieldControl_CheckedChanged);

                isNotChanged[i] = false;

                PositionControls(i, ref location);
            }

            this.Height = location.Y;

            SetFieldValues();
            ignoreEvent=false;
            this.ResumeLayout(false);
        }

        public void RePositioning()
        {
            this.SuspendLayout();
            this.Width = X_DEFAULT_WIDTH;
            System.Drawing.Point location = new Point(X_TYPE_START, START_Y);
            int arraySize = fieldValueControl.Length;
            for (int i = 0; i < arraySize; i++)
            {
                PositionControls(i, ref location);
            }
            this.Height = location.Y;
            this.ResumeLayout(false);
        }

        protected virtual void PositionControls(int index, ref Point location)
        {
            location.X = X_NAME_START;
            fieldNameLabel[index].Location = location;
            location.X = X_VALUE_START;
            fieldValueControl[index].Location = location;
            location.X = X_NULL_START;
            isNullCheckBox[index].Location = location;
            this.Controls.AddRange(new System.Windows.Forms.Control[] { fieldNameLabel[index],
                                                                        fieldValueControl[index],
                                                                        isNullCheckBox[index] });
            location.X = X_NAME_START;
            location.Y += fieldValueControl[index].Height + Y_STEP;
        }

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        protected override void Dispose( bool disposing )
        {
            if( disposing )
            {
                foreach (Control c in fieldValueControl)
                {
                    c.Dispose();
                }
                
                if(components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose( disposing );
        }

        public static Font DataFont
        {
            get {return dataFont;}
        }
    }

    //---------------------------------------------------
    // Bool control
    //---------------------------------------------------
    public class BoolField : ObjectDataFieldControl
    {
        public BoolField(ObjectInfo objInfo, int member, string name, int arraySize) : base(objInfo, member, "Boolean", name, arraySize)
        {
        }

        protected override void CreateValueControls(int arraySize)
        {
            fieldValueControl=new System.Windows.Forms.ComboBox[arraySize];
            for (int i=0; i<arraySize; i++)
            {
                ComboBox combo=new ComboBox();
                combo.Font=dataFont;
                combo.SelectedIndexChanged+=new EventHandler(ObjectDataFieldControl_TextChanged);
                combo.Items.AddRange(new string[]{"True", "False"});
                combo.SelectedIndex=0;
                combo.DropDownStyle=ComboBoxStyle.DropDownList;
                combo.Width=100;
                fieldValueControl[i]=combo;
                fieldValueControl[i].ContextMenuStrip = new ContextMenuStrip();
                fieldValueControl[i].ContextMenuStrip.Items.Add(new ToolStripMenuItem("Is changed", null, new EventHandler(OnIsChangedMenuItem)));
            }
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            ComboBox c=(ComboBox)fieldValueControl[index];
            if (setVal)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.BooleanContainer cont = (Safir.Dob.Typesystem.BooleanContainer)tmp.Obj.GetMember(member, index);
                cont.Val = (c.Text == "True");
                changed[index]=false;
            }
            return true;
        }

        public override void SetFieldValues()
        {
            int index=0;
            foreach (ComboBox c in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.BooleanContainer cont = (Safir.Dob.Typesystem.BooleanContainer)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                }
                else
                {
                    c.Text=cont.Val.ToString();
                    c.BackColor=ColorMap.ENABLED;
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
    }


    //---------------------------------------------------
    // Enum control
    //---------------------------------------------------
    public class EnumField : ObjectDataFieldControl
    {
        private long enumTypeId;

        public EnumField(ObjectInfo objInfo, int member, long enumTypeId, string name, int arraySize) : base()
        {
            this.enumTypeId=enumTypeId;
            string enumName=Safir.Dob.Typesystem.Operations.GetName(enumTypeId);
            base.Init(objInfo, member, enumName, name, arraySize);
        }

        protected override void CreateValueControls(int arraySize)
        {
            fieldValueControl=new System.Windows.Forms.ComboBox[arraySize];
            int noEnumValues=Safir.Dob.Typesystem.Operations.GetNumberOfEnumerationValues(enumTypeId);
            string[] enumValueNames=new string[noEnumValues];
            for (int ev=0; ev<noEnumValues; ev++)
            {
                enumValueNames[ev]=Safir.Dob.Typesystem.Operations.GetEnumerationValueName(enumTypeId, ev);
            }

            for (int i=0; i<arraySize; i++)
            {
                ComboBox combo=new ComboBox();
                combo.Font=dataFont;
                combo.SelectedIndexChanged+=new EventHandler(ObjectDataFieldControl_TextChanged);
                combo.MouseHover += new EventHandler(ObjectDataFieldControl_MouseHover);
                combo.Items.AddRange(enumValueNames);
                combo.SelectedIndex=0;
                combo.DropDownStyle=ComboBoxStyle.DropDownList;
                combo.DropDownWidth = 400;
                combo.Width=100;
                fieldValueControl[i]=combo;
                fieldValueControl[i].ContextMenuStrip = new ContextMenuStrip();
                fieldValueControl[i].ContextMenuStrip.Items.Add(new ToolStripMenuItem("Is changed", null, new EventHandler(OnIsChangedMenuItem)));
            }
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            ComboBox c=(ComboBox)fieldValueControl[index];
            if (setVal)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.EnumerationContainerBase cont = (Safir.Dob.Typesystem.EnumerationContainerBase)tmp.Obj.GetMember(member, index);
                cont.Ordinal = c.SelectedIndex;
                changed[index]=false;
            }
            return true;
        }

        public override void SetFieldValues()
        {
            int index=0;
            foreach (ComboBox c in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.EnumerationContainerBase cont = (Safir.Dob.Typesystem.EnumerationContainerBase)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                }
                else
                {
                    c.SelectedIndex = cont.Ordinal;
                    c.BackColor=ColorMap.ENABLED;
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
    }

    //---------------------------------------------------
    // Int32 control
    //---------------------------------------------------
    public class Int32Field : ObjectDataFieldControl
    {
        public Int32Field(ObjectInfo objInfo, int member, string name, int arraySize) : base(objInfo, member, "Int32", name, arraySize)
        {
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                TextBox c=(TextBox)fieldValueControl[index];
                int val=int.Parse(c.Text);
                if (setVal)
                {
                    ObjectInfo tmp = (ObjectInfo)Tag;
                    Safir.Dob.Typesystem.Int32Container cont = (Safir.Dob.Typesystem.Int32Container)tmp.Obj.GetMember(member, index);
                    cont.Val = val;
                    changed[index]=false;
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
            int index=0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.Int32Container cont = (Safir.Dob.Typesystem.Int32Container)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                }
                else
                {
                    t.Text=cont.Val.ToString();
                    t.BackColor=ColorMap.ENABLED;
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
    }

    //---------------------------------------------------
    // Int64 control
    //---------------------------------------------------
    public class Int64Field : ObjectDataFieldControl
    {
        public Int64Field(ObjectInfo objInfo, int member, string name, int arraySize) : base(objInfo, member, "Int64", name, arraySize)
        {
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                TextBox c=(TextBox)fieldValueControl[index];
                long val=long.Parse(c.Text);
                if (setVal)
                {
                    ObjectInfo tmp = (ObjectInfo)Tag;
                    Safir.Dob.Typesystem.Int64Container cont = (Safir.Dob.Typesystem.Int64Container)tmp.Obj.GetMember(member, index);
                    cont.Val = val;
                    changed[index]=false;
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
            int index=0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.Int64Container cont = (Safir.Dob.Typesystem.Int64Container)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                }
                else
                {
                    t.Text=cont.Val.ToString();
                    t.BackColor=ColorMap.ENABLED;
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
    }

    //---------------------------------------------------
    // Float32 control
    //---------------------------------------------------
    public class Float32Field : ObjectDataFieldControl
    {
        public Float32Field(ObjectInfo objInfo, int member, string name, int arraySize, string typeName) : base(objInfo, member, typeName, name, arraySize)
        {
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.Float32Container cont = (Safir.Dob.Typesystem.Float32Container)tmp.Obj.GetMember(member, index);

                TextBox c=(TextBox)fieldValueControl[index];
                float val=float.Parse(c.Text);
                if (setVal)
                {
                    cont.Val = val;
                    changed[index]=false;
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
            int index=0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.Float32Container cont = (Safir.Dob.Typesystem.Float32Container)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                }
                else
                {
                    t.Text=cont.Val.ToString();
                    t.BackColor=ColorMap.ENABLED;
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
    }

    //---------------------------------------------------
    // Float64 control
    //---------------------------------------------------
    public class Float64Field : ObjectDataFieldControl
    {
        public Float64Field(ObjectInfo objInfo, int member, string name, int arraySize, string typeName)
            : base(objInfo, member, typeName, name, arraySize)
        {
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                TextBox c=(TextBox)fieldValueControl[index];
                double val=double.Parse(c.Text);
                if (setVal)
                {
                    ObjectInfo tmp = (ObjectInfo)Tag;
                    Safir.Dob.Typesystem.Float64Container cont = (Safir.Dob.Typesystem.Float64Container)tmp.Obj.GetMember(member, index);
                    cont.Val = val;
                    changed[index]=false;
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
            int index=0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.Float64Container cont = (Safir.Dob.Typesystem.Float64Container)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                }
                else
                {
                    t.Text=cont.Val.ToString();
                    t.BackColor=ColorMap.ENABLED;
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
    }

    //---------------------------------------------------
    // String control
    //---------------------------------------------------
    public class StringField : ObjectDataFieldControl
    {
        public StringField(ObjectInfo objInfo, int member, string name, int arraySize, int maxLength)
            : base(objInfo, member, "String", name, arraySize)
        {
            foreach (TextBox t in fieldValueControl)
            {
                t.MaxLength = maxLength;
            }
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            TextBox c=(TextBox)fieldValueControl[index];
            if (setVal)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.StringContainer cont = (Safir.Dob.Typesystem.StringContainer)tmp.Obj.GetMember(member, index);
                cont.Val = c.Text;

                changed[index]=false;
            }
            return true;
        }

        public override void SetFieldValues()
        {
            int index=0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.StringContainer cont = (Safir.Dob.Typesystem.StringContainer)tmp.Obj.GetMember(member, index);
                
                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                }
                else
                {
                    // STSYLI 20080821: Remove whitespace from the beginning and end of the string
                    // was problem when loading serializable data and lots of white spaces in beginning
                    // resulted in not showing the actual data
                    t.Text=cont.Val.Trim();
                    t.BackColor=ColorMap.ENABLED;
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
    }

    //---------------------------------------------------
    // TypeId control
    //---------------------------------------------------
    public class TypeIdField : ObjectDataFieldControl
    {
        public TypeIdField(ObjectInfo objInfo, int member, string name, int arraySize) : base(objInfo, member, "TypeId", name, arraySize)
        {
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            long val;
            TextBox c=(TextBox)fieldValueControl[index];
            try
            {
                val=long.Parse(c.Text);
            }
            catch
            {
                val=Safir.Dob.Typesystem.Operations.GetTypeId(c.Text);
            }

            if (Safir.Dob.Typesystem.Operations.Exists(val))
            {
                c.Text=Safir.Dob.Typesystem.Operations.GetName(val);
            }

            if (setVal)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.TypeIdContainer cont = (Safir.Dob.Typesystem.TypeIdContainer)tmp.Obj.GetMember(member, index);
                cont.Val = val;

                changed[index]=false;
            }
            return true;
        }

        public override void SetFieldValues()
        {
            int index=0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.TypeIdContainer cont = (Safir.Dob.Typesystem.TypeIdContainer)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                }
                else
                {
                    t.Text=cont.Val.ToString();
                    t.BackColor=ColorMap.ENABLED;
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
    }


    //---------------------------------------------------
    // ChannelId control
    //---------------------------------------------------
    public class ChannelIdField : ObjectDataFieldControl
    {
        public ChannelIdField(ObjectInfo objInfo, int member, string name, int arraySize)
            : base(objInfo, member, "ChannelId", name, arraySize)
        {
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            TextBox c=(TextBox)fieldValueControl[index];
            long val;

            try
            {
                val = long.Parse(c.Text);
                if (setVal)
                {
                    ObjectInfo tmp = (ObjectInfo)Tag;
                    Safir.Dob.Typesystem.ChannelIdContainer cont = (Safir.Dob.Typesystem.ChannelIdContainer)tmp.Obj.GetMember(member, index);
                    cont.Val = new Safir.Dob.Typesystem.ChannelId(val);
                    changed[index] = false;
                }
                return true;
            }
            catch
            {
                string idString;
                if (c.Text.StartsWith("\"") && (c.Text.EndsWith("\"")) && c.Text.Length > 2)
                {
                    // remove quotation
                    idString = c.Text.Substring(1, c.Text.Length - 2);
                }
                else
                {
                    idString = c.Text;
                }

                if (idString == "")
                {
                    return false;
                }

                if (setVal)
                {
                    ObjectInfo tmp = (ObjectInfo)Tag;
                    Safir.Dob.Typesystem.ChannelIdContainer cont = (Safir.Dob.Typesystem.ChannelIdContainer)tmp.Obj.GetMember(member, index);
                    cont.Val = new Safir.Dob.Typesystem.ChannelId(idString);
                    changed[index] = false;
                }
                return true;
            }
        }

        public override void SetFieldValues()
        {
            int index=0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.ChannelIdContainer cont = (Safir.Dob.Typesystem.ChannelIdContainer)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                }
                else
                {
                    t.Text=cont.Val.ToString();
                    t.BackColor=ColorMap.ENABLED;
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
    }

    //---------------------------------------------------
    // HandlerId control
    //---------------------------------------------------
    public class HandlerIdField : ObjectDataFieldControl
    {
        public HandlerIdField(ObjectInfo objInfo, int member, string name, int arraySize)
            : base(objInfo, member, "HandlerId", name, arraySize)
        {
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            TextBox c = (TextBox)fieldValueControl[index];
            long val;
            try
            {
                val = long.Parse(c.Text);
                if (setVal)
                {
                    ObjectInfo tmp = (ObjectInfo)Tag;
                    Safir.Dob.Typesystem.HandlerIdContainer cont = (Safir.Dob.Typesystem.HandlerIdContainer)tmp.Obj.GetMember(member, index);
                    cont.Val = new Safir.Dob.Typesystem.HandlerId(val);
                    changed[index] = false;
                }
                return true;
            }
            catch
            {
                string idString;
                if (c.Text.StartsWith("\"") && (c.Text.EndsWith("\"")) && c.Text.Length > 2)
                {
                    // remove quotation
                    idString = c.Text.Substring(1, c.Text.Length - 2);
                }
                else
                {
                    idString = c.Text;
                }

                if (idString == "")
                {
                    return false;
                }

                if (setVal)
                {
                    ObjectInfo tmp = (ObjectInfo)Tag;
                    Safir.Dob.Typesystem.HandlerIdContainer cont = (Safir.Dob.Typesystem.HandlerIdContainer)tmp.Obj.GetMember(member, index);
                    cont.Val = new Safir.Dob.Typesystem.HandlerId(idString);
                    changed[index] = false;
                }
                return true;
            }
        }


        public override void SetFieldValues()
        {
            int index = 0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.HandlerIdContainer cont = (Safir.Dob.Typesystem.HandlerIdContainer)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked = true;
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
    }

    //---------------------------------------------------
    // EntityId control
    //---------------------------------------------------
    public class EntityIdField : ObjectDataFieldControl
    {
        public EntityIdField(ObjectInfo objInfo, int member, string name, int arraySize)
            : base(objInfo, member, "EntityId", name, arraySize)
        {
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId();
            TextBox c = (TextBox)fieldValueControl[index];
            string[] s = c.Text.Trim().Replace(':', ' ').Split(' ');
            if (s.Length < 2)
            {
                return false;
            }
            else if (s.Length > 2)
            {
                for (int i = 1; i < s.Length - 1; i++)
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
                entityId.TypeId = Safir.Dob.Typesystem.Operations.GetTypeId(s[0]);
            }

            try
            {
                entityId.InstanceId = new Safir.Dob.Typesystem.InstanceId(Int64.Parse(s[s.Length - 1]));
            }
            catch
            {
                string idString;
                if (s[s.Length - 1].StartsWith("\"") && (s[s.Length - 1].EndsWith("\"")) && s[s.Length - 1].Length > 2)
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
                entityId.InstanceId = new Safir.Dob.Typesystem.InstanceId(idString);
            }

            //if (Safir.Dob.Typesystem.Operations.Exists(entityId.TypeId))
            //{
                //c.Text = entityId.ToString();
                //c.Text = s[0] + " " + s[s.Length - 1];
            //}

            if (setVal)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.EntityIdContainer cont = (Safir.Dob.Typesystem.EntityIdContainer)tmp.Obj.GetMember(member, index);
                cont.Val = entityId;
                changed[index] = false;
            }
            return true;
        }


        public override void SetFieldValues()
        {
            int index = 0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.EntityIdContainer cont = (Safir.Dob.Typesystem.EntityIdContainer)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked = true;
                }
                else
                {
                    Safir.Dob.Typesystem.EntityId val = cont.Val;
                    string typename = "";
                    try
                    {
                        typename = Safir.Dob.Typesystem.Operations.GetName(val.TypeId);
                    }
                    catch
                    {
                        typename = val.TypeId.ToString();
                    }

                    t.Text = typename + " : " + val.InstanceId.ToString();
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


    }

    //---------------------------------------------------
    // InstanceId control
    //---------------------------------------------------
    public class InstanceIdField : ObjectDataFieldControl
    {
        public InstanceIdField(ObjectInfo objInfo, int member, string name, int arraySize)
            : base(objInfo, member, "InstanceId", name, arraySize)
        {
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            TextBox c = (TextBox)fieldValueControl[index];
            Safir.Dob.Typesystem.InstanceId instanceId;

            try
            {
                instanceId = new Safir.Dob.Typesystem.InstanceId(Int64.Parse(c.Text));
            }
            catch
            {
                string idString;
                if (c.Text.StartsWith("\"") && (c.Text.EndsWith("\"")) && c.Text.Length > 2)
                {
                    // remove quotation
                    idString = c.Text.Substring(1, c.Text.Length - 2);
                }
                else
                {
                    idString = c.Text;
                }

                if (idString == "")
                {
                    return false;
                }
                instanceId = new Safir.Dob.Typesystem.InstanceId(idString);
            }


            if (setVal)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.InstanceIdContainer cont = (Safir.Dob.Typesystem.InstanceIdContainer)tmp.Obj.GetMember(member, index);
                cont.Val = instanceId;
                changed[index] = false;
            }

            return true;
        }

        public override void SetFieldValues()
        {
            int index = 0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.InstanceIdContainer cont = (Safir.Dob.Typesystem.InstanceIdContainer)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked = true;
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
    }
        

    //---------------------------------------------------
    // Object control
    //---------------------------------------------------
    public class ObjectField : ObjectDataFieldControl
    {
        private ObjectEditPanel[] objPanel = null;
        private long typeId;

        public ObjectField(ObjectInfo objInfo, int member, long typeId, string name, int arraySize) : base()
        {
            this.typeId=typeId;
            base.Init(objInfo, member, Safir.Dob.Typesystem.Operations.GetName(typeId), name, arraySize);
        }

        public void TypeChange(long newTypeId, ObjectEditPanel sender)
        {
            int index = ChangedValue(objPanel, sender);
            Controls.Remove(objPanel[index]);
            ObjectInfo objInfo = new ObjectInfo();

            objInfo.Obj = Safir.Dob.Typesystem.ObjectFactory.Instance.CreateObject(newTypeId);

            fieldValueControl[index].Tag = objInfo;
            objPanel[index] = new ObjectEditPanel(objInfo, typeId, true);
            objPanel[index].ParentObjectField = this;
            objPanel[index].AutoScroll = false;

            parentObjectEditPanel.ChangedDataField();

            parentObjectEditPanel.ExpandCollapse(member); //to force total resize
        }

        public void ExpandCollapse(ObjectEditPanel sender)
        {
            parentObjectEditPanel.ExpandCollapse(member);
        }

        public void ChangedEditPanel(ObjectEditPanel ep)
        {
            ChangedValue(objPanel, ep);
            parentObjectEditPanel.ChangedDataField();
        }

        public int IndexOfEditPanel(ObjectEditPanel ep)
        {
            for (int i = 0; i < objPanel.Length; i++)
            {
                if (objPanel[i] == ep)
                    return i;
            }
            return -1;
        }

        protected override void CreateValueControls(int arraySize)
        {
            fieldValueControl=new System.Windows.Forms.CheckBox[arraySize];
            objPanel = new ObjectEditPanel[arraySize];

            for (int i=0; i<arraySize; i++)
            {
                CheckBox button = new CheckBox();
                button.Appearance = Appearance.Button;
                button.Font=dataFont;
                button.BackColor=ColorMap.ENABLED;
                button.ForeColor=Color.Black;
                button.Text="Expand";
                button.Height=20;
                button.Width = 100;
                button.CheckedChanged += new EventHandler(button_CheckedChanged);
                fieldValueControl[i]=button;

                fieldValueControl[i].ContextMenuStrip = new ContextMenuStrip();
                fieldValueControl[i].ContextMenuStrip.Items.AddRange(
                new ToolStripItem[]{new ToolStripMenuItem("Is changed", null, new EventHandler(OnIsChangedMenuItem)),
                                    new ToolStripMenuItem("Is changed here", null, new EventHandler(OnIsChangedHereMenuItem))});

            }

            if (arraySize > 1)
            {
                MenuItem expandAllMenuItem = new MenuItem("Expand all", new EventHandler(OnExpandAll_ContextMenu));
                MenuItem collapseAllMenuItem = new MenuItem("Collapse all", new EventHandler(OnCollapseAll_ContextMenu));
                this.ContextMenu = new ContextMenu(new MenuItem[] { expandAllMenuItem, collapseAllMenuItem });
            }
        }

        protected void OnIsChangedHereMenuItem(object sender, EventArgs e)
        {
            int i = FindMenuItemOwnerControl(sender);

            ToolStripMenuItem changedHere = sender as ToolStripMenuItem;
            ToolStripMenuItem changed=(ToolStripMenuItem)changedHere.GetCurrentParent().Items[0];
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
            for (int i=0; i<isNullCheckBox.Length; i++)
            {
                if (!isNullCheckBox[i].Checked)
                {
                    ((CheckBox)fieldValueControl[i]).Checked = true;
                }
            }
        }

        private void OnCollapseAll_ContextMenu(object o, EventArgs e)
        {
            for (int i = 0; i < isNullCheckBox.Length; i++)
            {
                CheckBox cb = fieldValueControl[i] as CheckBox;
                if (cb.Checked)
                {
                    cb.Checked = false;
                }
            }
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            if (fieldValueControl[index].Tag!=null)
            {
                if (setVal)
                {
                    if (!objPanel[index].SetObjectMembers())
                    {
                        return false;
                    }

                    ObjectInfo tmp = (ObjectInfo)Tag;
                    Safir.Dob.Typesystem.ObjectContainerBase cont = (Safir.Dob.Typesystem.ObjectContainerBase)tmp.Obj.GetMember(member, index);
                    cont.InternalObj = (Safir.Dob.Typesystem.Object)((ObjectInfo)(objPanel[index].Tag)).Obj;

                    changed[index]=false;
                }
                return true;
            }
            else
                return false;
        }


        public override void SetFieldValues()
        {
            bool forceResize = false;
            ObjectInfo tmp = (ObjectInfo)Tag;
            int index=0;
            foreach (CheckBox b in fieldValueControl)
            {
                Safir.Dob.Typesystem.ObjectContainerBase cont = (Safir.Dob.Typesystem.ObjectContainerBase)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked=true;
                    b.Tag = null;
                    b.Checked = false;
                }
                else
                {
                    ObjectInfo objectInfo = new ObjectInfo();
                    objectInfo.Obj = cont.InternalObj;
                    //b.Tag=cont.InternalObj;
                    b.Tag = objectInfo;
                    this.isNullCheckBox[index].Checked = false;
                    b.BackColor=ColorMap.ENABLED;
                    if (objPanel[index] != null)
                    {

                        //if (((Safir.Dob.Typesystem.Object)(objPanel[index].Tag)).GetTypeId() == cont.InternalObj.GetTypeId())
                        if (((ObjectInfo)(objPanel[index].Tag)).Obj.GetTypeId() == cont.InternalObj.GetTypeId())
                        {
                            objPanel[index].UpdateData(objectInfo);
                        }
                        else //Dynamic type has changed (inheritance)
                        {
                            Controls.Remove(objPanel[index]);
                            objPanel[index] = new ObjectEditPanel(objectInfo, typeId, true);
                            objPanel[index].ParentObjectField = this;
                            objPanel[index].AutoScroll = false;
                            Controls.Add(objPanel[index]);
                            forceResize = true;
                        }
                    }
                    else
                    {
                        objPanel[index] = new ObjectEditPanel(objectInfo, typeId, true);
                        objPanel[index].ParentObjectField = this;
                        objPanel[index].AutoScroll = false;
                    }

                    int w = X_VALUE_START + objPanel[index].Width + X_STEP;
                    if (w > this.Width)
                    {
                        this.Width = w;
                    }
                }

                if (cont.IsChanged())
                {
                    (b.ContextMenuStrip.Items[0] as ToolStripMenuItem).Checked = true;
                    b.BackColor = ColorMap.CHANGED;
                    changed[index] = true;
                }

                index++;
            }

            if (forceResize)
            {
                parentObjectEditPanel.ExpandCollapse(member);
                Invalidate();
            }
        }

        private void button_CheckedChanged(object sender, EventArgs e)
        {
            int index = 0;
            foreach (Control c in fieldValueControl)
            {
                if (c == sender)
                    break;

                index++;
            }

            CheckBox b = (CheckBox)sender;

            if (b.Checked)
            {
                b.Text = "Collapse";

                if (b.Tag == null)
                {
                    b.Tag = new ObjectInfo();
                    ((ObjectInfo)b.Tag).Obj = Safir.Dob.Typesystem.ObjectFactory.Instance.CreateObject(typeId);
                    objPanel[index] = new ObjectEditPanel((ObjectInfo)b.Tag, typeId, true);
                    objPanel[index].ParentObjectField = this;
                    objPanel[index].AutoScroll = false;

                    fieldValueControl[index].BackColor = ColorMap.CHANGED;

                    ((ToolStripMenuItem)fieldValueControl[index].ContextMenuStrip.Items[0]).Checked = true; //isChanged
                    ((ToolStripMenuItem)fieldValueControl[index].ContextMenuStrip.Items[1]).Checked = true; //isChangedHere
                    fieldValueControl[index].BackColor = ColorMap.CHANGED;
                    changed[index]=true;
                }

                Safir.Dob.Typesystem.ObjectContainerBase cont=(Safir.Dob.Typesystem.ObjectContainerBase)((Safir.Dob.Typesystem.Object)((ObjectInfo)Tag).Obj).GetMember(member, index);
                cont.InternalObj = (Safir.Dob.Typesystem.Object)((ObjectInfo)b.Tag).Obj;
            }
            else
            {
                b.Text = "Expand";
            }

            parentObjectEditPanel.ExpandCollapse(member);


            isNullCheckBox[index].Checked = false;
            if (!ValidInput(index, false))
                this.fieldValueControl[index].BackColor = ColorMap.ERROR;
            parentObjectEditPanel.ChangedDataField();

            objPanel[index].Focus();
        }

        protected override void PositionControls(int index, ref Point location)
        {
            base.PositionControls(index, ref location);
            CheckBox b = (CheckBox)fieldValueControl[index];

            if (b.Checked)
            {
                int w = X_VALUE_START + objPanel[index].Width + X_STEP;
                if (w > this.Width)
                    this.Width = w;

                location.X = X_VALUE_START;


                objPanel[index].Location = location;

                location.X = X_NAME_START;
                location.Y += objPanel[index].Height + Y_STEP;

                this.Controls.Add(objPanel[index]);
            }
            else
            {
                if (objPanel[index] != null)
                {
                    Controls.Remove(objPanel[index]);
                }
                location.X = X_NAME_START;
            }
        }

        public override void ResetIndex(int i)
        {
            base.ResetIndex(i);
            (fieldValueControl[i].ContextMenuStrip.Items[1] as ToolStripMenuItem).Checked = false;
            if (objPanel[i]!=null)
                objPanel[i].ResetChanged();
        }
    }

    //---------------------------------------------------
    // Binary control
    //---------------------------------------------------
    public class BinaryField : ObjectDataFieldControl
    {
        public BinaryField(ObjectInfo objInfo, int member, string name, int arraySize)
            : base(objInfo, member, "Binary", name, arraySize)
        {
            foreach (TextBox t in fieldValueControl)
            {
                t.ContextMenuStrip.Items.AddRange(new ToolStripItem[]{
                    new ToolStripSeparator(),
                    new ToolStripMenuItem("Save to file...", null, OnSaveToFile),
                    new ToolStripMenuItem("Load from file...", null, OnLoadFromFile)});
            }
        }

        private void OnSaveToFile(object sender, EventArgs e)
        {
            int i = FindMenuItemOwnerControl(sender);

            SaveFileDialog sfd = new SaveFileDialog();
            if (sfd.ShowDialog() == DialogResult.OK)
            {
                using (System.IO.TextWriter writer = new System.IO.StreamWriter(sfd.FileName))
                {
                    writer.Write(fieldValueControl[i].Text);
                    writer.Flush();
                    writer.Close();
                }
            }
        }

        private void OnLoadFromFile(object sender, EventArgs e)
        {
            int i = FindMenuItemOwnerControl(sender);

            OpenFileDialog ofd = new OpenFileDialog();

            if (ofd.ShowDialog() == DialogResult.OK)
            {
                using (System.IO.TextReader reader = new System.IO.StreamReader(ofd.FileName))
                {
                    fieldValueControl[i].Text = reader.ReadToEnd();
                    reader.Close();
                }
            }
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.BinaryContainer cont = (Safir.Dob.Typesystem.BinaryContainer)tmp.Obj.GetMember(member, index);
                TextBox c = (TextBox)fieldValueControl[index];
                byte[] val = System.Convert.FromBase64String(c.Text);

                if (setVal)
                {

                    cont.Val = val;
                    changed[index] = false;
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
            int index = 0;
            foreach (TextBox t in fieldValueControl)
            {
                ObjectInfo tmp = (ObjectInfo)Tag;
                Safir.Dob.Typesystem.BinaryContainer cont = (Safir.Dob.Typesystem.BinaryContainer)tmp.Obj.GetMember(member, index);

                if (cont.IsNull())
                {
                    this.isNullCheckBox[index].Checked = true;
                }
                else
                {
                    t.Text = System.Convert.ToBase64String(cont.Val);
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
    }

}
