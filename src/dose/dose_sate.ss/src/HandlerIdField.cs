using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
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
                var cont = (HandlerIdSequenceContainer)ObjInfo.Obj.GetMember(member, 0);

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