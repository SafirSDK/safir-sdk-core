using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
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
                var cont = (Float32SequenceContainer) ObjInfo.Obj.GetMember(member, 0);

                if (cont.Count != fieldValueControl.Count)
                {
                    Init(member, typeName, memberName, collectionType, 1);
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
}