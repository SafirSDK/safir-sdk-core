using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
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
                var cont = (TypeIdSequenceContainer) ObjInfo.Obj.GetMember(member, 0);

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
                    t.Text = Operations.Exists(cont[index]) ? Operations.GetName(cont[index]) : cont[index].ToString();
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }
}