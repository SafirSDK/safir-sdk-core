using System;
using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
    public class EnumField : ObjectDataFieldControl
    {
        private readonly string[] enumValueNames;

        public EnumField(ObjectInfo objInfo, int member, long enumTypeId, string name, CollectionType ct, int arraySize)
        {
            var noEnumValues = Operations.GetNumberOfEnumerationValues(enumTypeId);
            enumValueNames = new string[noEnumValues];
            for (var ev = 0; ev < noEnumValues; ev++)
            {
                enumValueNames[ev] = Operations.GetEnumerationValueName(enumTypeId, ev);
            }

            var enumName = Operations.GetName(enumTypeId);
            Init(objInfo, member, enumName, name, ct, arraySize);
        }

        protected override Control CreateValueControl()
        {
            var combo = new ComboBox();
            combo.Font = dataFont;
            combo.SelectedIndexChanged += ObjectDataFieldControl_TextChanged;
            combo.Items.AddRange(enumValueNames);
            combo.SelectedIndex = 0;
            combo.DropDownStyle = ComboBoxStyle.DropDownList;
            combo.DropDownWidth = 400;
            combo.Width = 100;
            combo.ContextMenuStrip = new ContextMenuStrip();
            combo.ContextMenuStrip.Items.Add(new ToolStripMenuItem("Is changed", null, OnIsChangedMenuItem));
            return combo;
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var container = tmp.Obj.GetMember(member, 0);
            var containerType = container.GetType();
            var enumType =
                containerType.Assembly.GetType(containerType.ReflectedType.FullName).GetNestedType("Enumeration");
            var enumVal = Enum.GetValues(enumType).GetValue(0);
            containerType.GetMethod("Insert").Invoke(container, new[] {index, enumVal});
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            if (!setVal)
                return true;

            var c = (ComboBox) fieldValueControl[index];
            var tmp = (ObjectInfo) Tag;

            if (collectionType == CollectionType.SingleValueCollectionType ||
                collectionType == CollectionType.ArrayCollectionType)
            {
                changed[index] = false;
                var cont = (EnumerationContainerBase) tmp.Obj.GetMember(member, index);
                cont.Ordinal = c.SelectedIndex;
            }
            else if (collectionType == CollectionType.DictionaryCollectionType)
            {
                changed[index] = false;
                var cont = (EnumerationContainerBase) GetDictionaryValue(fieldNameLabel[index].Tag);
                cont.Ordinal = c.SelectedIndex;
            }
            else if (collectionType == CollectionType.SequenceCollectionType)
            {
                var container = tmp.Obj.GetMember(member, 0);
                var containerType = container.GetType();
                var enumType =
                    containerType.Assembly.GetType(containerType.ReflectedType.FullName).GetNestedType("Enumeration");
                var enumVal = Enum.GetValues(enumType).GetValue(c.SelectedIndex);

                var numValues = (int) containerType.GetProperty("Count").GetValue(container, null);
                if (numValues > index)
                {
                    containerType.GetProperty("Item").SetValue(container, enumVal, new object[] {index});
                }
                else
                {
                    containerType.GetMethod("Add").Invoke(container, new[] {enumVal});
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
                    var cont = (EnumerationContainerBase) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        c.SelectedIndex = cont.Ordinal;
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
                    var cont = (EnumerationContainerBase) GetDictionaryValue(fieldNameLabel[index].Tag);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        c.SelectedIndex = cont.Ordinal;
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

                var tmp = (ObjectInfo) Tag;
                var container = tmp.Obj.GetMember(member, 0);
                var containerType = container.GetType();

                if ((int) containerType.GetProperty("Count").GetValue(container, null) != fieldValueControl.Count)
                {
                    Init(tmp, member, typeName, memberName, collectionType, 1);
                    parentObjectEditPanel.ExpandCollapse(member);
                    SetSequenceChanged(true);
                    return;
                }

                var index = 0;
                var ix = new object[1] {index};
                foreach (ComboBox t in fieldValueControl)
                {
                    ix[0] = index++;
                    var value = (int) containerType.GetProperty("Item").GetValue(container, ix);
                    t.SelectedIndex = value;
                    t.BackColor = ColorMap.ENABLED;
                }
            }
        }
    }
}