using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
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
                var cont = (BooleanSequenceContainer) ObjInfo.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(member, typeName, memberName, collectionType, 1);
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
}