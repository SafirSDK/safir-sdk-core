using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
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
}