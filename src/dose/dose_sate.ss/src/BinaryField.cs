using System;
using System.IO;
using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
    public class BinaryField : ObjectDataFieldControl
    {
        public BinaryField(ObjectInfo objInfo, int member, string name, CollectionType ct, int arraySize)
            : base(objInfo, member, "Binary", name, ct, arraySize)
        {
        }

        protected override Control CreateValueControl()
        {
            var control = base.CreateValueControl() as TextBox;
            if (control != null)
            {
                control.ContextMenuStrip.Items.AddRange(new ToolStripItem[]
                {
                    new ToolStripSeparator(),
                    new ToolStripMenuItem("Save to file...", null, OnSaveToFile),
                    new ToolStripMenuItem("Load from file...", null, OnLoadFromFile)
                });
            }
            return control;
        }

        private void OnSaveToFile(object sender, EventArgs e)
        {
            var i = FindMenuItemOwnerControl(sender);

            var sfd = new SaveFileDialog();
            if (sfd.ShowDialog() == DialogResult.OK)
            {
                using (TextWriter writer = new StreamWriter(sfd.FileName))
                {
                    writer.Write(fieldValueControl[i].Text);
                    writer.Flush();
                    writer.Close();
                }
            }
        }

        private void OnLoadFromFile(object sender, EventArgs e)
        {
            var i = FindMenuItemOwnerControl(sender);

            var ofd = new OpenFileDialog();

            if (ofd.ShowDialog() == DialogResult.OK)
            {
                using (TextReader reader = new StreamReader(ofd.FileName))
                {
                    fieldValueControl[i].Text = reader.ReadToEnd();
                    reader.Close();
                }
            }
        }

        protected override void InsertSequenceItem(int index)
        {
            var tmp = (ObjectInfo) Tag;
            var cont = (BinarySequenceContainer) tmp.Obj.GetMember(member, 0);
            cont.Insert(index, new byte[] {});
        }

        protected override bool ValidInput(int index, bool setVal)
        {
            try
            {
                var c = (TextBox) fieldValueControl[index];
                var val = Convert.FromBase64String(c.Text);

                if (!setVal)
                    return true;

                var tmp = (ObjectInfo) Tag;

                if (collectionType == CollectionType.SingleValueCollectionType ||
                    collectionType == CollectionType.ArrayCollectionType)
                {
                    changed[index] = false;
                    var cont = (BinaryContainer) tmp.Obj.GetMember(member, index);
                    cont.Val = val;
                }
                else if (collectionType == CollectionType.DictionaryCollectionType)
                {
                    changed[index] = false;
                    var cont = (BinaryContainer) GetDictionaryValue(fieldNameLabel[index].Tag);
                    cont.Val = val;
                }
                else if (collectionType == CollectionType.SequenceCollectionType)
                {
                    var cont = (BinarySequenceContainer) tmp.Obj.GetMember(member, 0);
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
                    var cont = (BinaryContainer) tmp.Obj.GetMember(member, index);

                    if (cont.IsNull())
                    {
                        if (isNullCheckBox.Count > index)
                            isNullCheckBox[index].Checked = true;
                    }
                    else
                    {
                        t.Text = Convert.ToBase64String(cont.Val);
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
                    var cont = (BinaryContainer) GetDictionaryValue(fieldNameLabel[index].Tag);

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
                var tmp = (ObjectInfo) Tag;
                var cont = (BinarySequenceContainer) tmp.Obj.GetMember(member, 0);

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
                    t.Text = Convert.ToBase64String(cont[index]);
                    t.BackColor = ColorMap.ENABLED;
                    index++;
                }
            }
        }
    }
}