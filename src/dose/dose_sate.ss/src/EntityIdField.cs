using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
    public class EntityIdField : ObjectDataFieldControl
    {
        public EntityIdField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "EntityId", name, ct, arraySize)
        {
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (EntityIdSequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, new EntityId());
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            var entityId = new EntityId();
            var c = (TextBox) fieldValueControl[index];
            var s = c.Text.Trim().Replace(':', ' ').Split(' ');
            if (s.Length < 2)
            {
                return false;
            }
            if (s.Length > 2)
            {
                for (var i = 1; i < s.Length - 1; i++)
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
                entityId.TypeId = Operations.GetTypeId(s[0]);
            }

            try
            {
                entityId.InstanceId = new InstanceId(long.Parse(s[s.Length - 1]));
            }
            catch
            {
                string idString;
                if (s[s.Length - 1].StartsWith("\"") && s[s.Length - 1].EndsWith("\"") && s[s.Length - 1].Length > 2)
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
                entityId.InstanceId = new InstanceId(idString);
            }

            if (!setVal)
                return true;

            var tmp = (ObjectInfo) Tag;
            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (EntityIdContainer) tmp.Obj.GetMember(member, index);
                cont.Val = entityId;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (EntityIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Val = entityId;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var cont = (EntityIdSequenceContainer) tmp.Obj.GetMember(member, 0);
                if (cont.Count > index)
                {
                    cont[index] = entityId;
                }
                else
                {
                    cont.Add(entityId);
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
                    var cont = (EntityIdContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        var val = cont.Val;
                        var typename = "";
                        try
                        {
                            typename = Operations.GetName(val.TypeId);
                        }
                        catch
                        {
                            typename = val.TypeId.ToString();
                        }

                        t.Text = typename + " : " + val.InstanceId;
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
                    var cont = (EntityIdContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        var val = cont.Val;
                        var typename = "";
                        try
                        {
                            typename = Operations.GetName(val.TypeId);
                        }
                        catch
                        {
                            typename = val.TypeId.ToString();
                        }

                        t.Text = typename + " : " + val.InstanceId;
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
                var cont = (EntityIdSequenceContainer) ObjInfo.Obj.GetMember(member, 0);

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
                    var eid = cont[index];
                    var typename = Operations.Exists(eid.TypeId)
                        ? Operations.GetName(eid.TypeId)
                        : eid.TypeId.ToString();
                    t.Text = typename + " : " + eid.InstanceId;
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }
}