using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace Sate
{
    public partial class ObjectDataFieldDictionaryKey : Form
    {
        private Safir.Dob.Typesystem.MemberType keyType;
        public ObjectDataFieldDictionaryKey(Safir.Dob.Typesystem.MemberType keyType, long enumTypeId = 0)
        {
            InitializeComponent();

            this.keyType = keyType;
            BackColor = Color.LightSkyBlue;

            this.typeLabel.Text = GetTypeName(keyType, enumTypeId);

            if (keyType== Safir.Dob.Typesystem.MemberType.EnumerationMemberType)
            {
                keyTextBox.Hide();

                int noEnumValues = Safir.Dob.Typesystem.Operations.GetNumberOfEnumerationValues(enumTypeId);
                var enumValueNames = new string[noEnumValues];
                for (int ev = 0; ev < noEnumValues; ev++)
                {
                    enumValueNames[ev] = Safir.
                        Dob.Typesystem.Operations.GetEnumerationValueName(enumTypeId, ev);
                }

                keyComboBox.Items.AddRange(enumValueNames);
                keyComboBox.SelectedIndex = 0;
                keyComboBox.DropDownStyle = ComboBoxStyle.DropDownList;
            }
            else
            {
                keyComboBox.Hide();
                keyTextBox.TextChanged += KeyTextBox_TextChanged;
            }
        }

        private void KeyTextBox_TextChanged(object sender, EventArgs e)
        {
            keyTextBox.BackColor = ValidInput() ? Color.White : ColorMap.ERROR;
        }

        private bool ValidInput()
        {
            try
            {
                switch (keyType)
                {
                    case Safir.Dob.Typesystem.MemberType.Int32MemberType:
                        {
                            var dummy = Int32Key;
                            return true;
                        }
                    case Safir.Dob.Typesystem.MemberType.Int64MemberType:
                        {
                            var dummy = Int64Key;
                            return true;
                        }
                    case Safir.Dob.Typesystem.MemberType.StringMemberType:
                        {
                            //always valid
                            return true;
                        }
                    case Safir.Dob.Typesystem.MemberType.TypeIdMemberType:
                        {
                            var dummy = TypeIdKey;
                            return true;
                        }
                    case Safir.Dob.Typesystem.MemberType.EntityIdMemberType:
                        {
                            var dummy = EntityIdKey;
                            return true;
                        }
                    case Safir.Dob.Typesystem.MemberType.InstanceIdMemberType:
                        {
                            var dummy = InstanceIdKey;
                            return true;
                        }
                    case Safir.Dob.Typesystem.MemberType.HandlerIdMemberType:
                        {
                            var dummy = HandlerIdKey;
                            return true;
                        }
                    case Safir.Dob.Typesystem.MemberType.ChannelIdMemberType:
                        {
                            var dummy = ChannelIdKey;
                            return true;
                        }
                    case Safir.Dob.Typesystem.MemberType.EnumerationMemberType:
                        {
                            //always valid
                            return true;
                        }

                    default:
                        return false;
                }
            }
            catch
            {
                return false;
            }
        }

        private void cancelbutton_Click(object sender, EventArgs e)
        {
            this.DialogResult = DialogResult.Cancel;
        }

        private void addButton_Click(object sender, EventArgs e)
        {
            if (ValidInput())
                DialogResult = DialogResult.OK;
        }

        public static string GetTypeName(Safir.Dob.Typesystem.MemberType keyType, long typeId)
        {
            switch (keyType)
            {
                case Safir.Dob.Typesystem.MemberType.Int32MemberType:
                    {
                        return "Int32";
                    }
                case Safir.Dob.Typesystem.MemberType.Int64MemberType:
                    {
                        return "Int64";
                    }
                case Safir.Dob.Typesystem.MemberType.StringMemberType:
                    {
                        return "String";
                    }
                case Safir.Dob.Typesystem.MemberType.TypeIdMemberType:
                    {
                        return "TypeId";
                    }
                case Safir.Dob.Typesystem.MemberType.EntityIdMemberType:
                    {
                        return "EntityId";
                    }
                case Safir.Dob.Typesystem.MemberType.InstanceIdMemberType:
                    {
                        return "InstanceId";
                    }
                case Safir.Dob.Typesystem.MemberType.HandlerIdMemberType:
                    {
                        return "HandlerId";
                    }
                case Safir.Dob.Typesystem.MemberType.ChannelIdMemberType:
                    {
                        return "ChannelId";
                    }
                case Safir.Dob.Typesystem.MemberType.EnumerationMemberType:
                    {
                        return Safir.Dob.Typesystem.Operations.GetName(typeId);
                    }

                default:
                    throw new ArgumentException("GetTypeName of unknown memberType");
            }
        }

        public string StringKey { get { return keyTextBox.Text; } }
        public int Int32Key { get { return int.Parse(keyTextBox.Text); } }
        public long Int64Key { get { return long.Parse(keyTextBox.Text); } }

        public int EnumKey
        {
            get
            {
                return keyComboBox.SelectedIndex;
            }
        }

        public long TypeIdKey
        {
            get
            {
                if (keyTextBox.Text == string.Empty)
                    throw new ArgumentException();

                long result;
                if (!long.TryParse(keyTextBox.Text, out result))
                {
                    result = Safir.Dob.Typesystem.Operations.GetTypeId(keyTextBox.Text);
                }
                return result;
            }
        }

        public Safir.Dob.Typesystem.EntityId EntityIdKey
        {
            get
            {
                if (keyTextBox.Text == string.Empty)
                    throw new ArgumentException();

                Safir.Dob.Typesystem.EntityId entityId = new Safir.Dob.Typesystem.EntityId();                
                string[] s = keyTextBox.Text.Trim().Replace(':', ' ').Split(' ');
                if (s.Length < 2)
                {
                    throw new ArgumentException();
                }
                else if (s.Length > 2)
                {
                    for (int i = 1; i < s.Length - 1; i++)
                    {
                        if (s[i] != "")
                            throw new ArgumentException();
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
                        throw new ArgumentException();
                    }
                    entityId.InstanceId = new Safir.Dob.Typesystem.InstanceId(idString);
                }
                return entityId;
            }
        }


        public Safir.Dob.Typesystem.InstanceId InstanceIdKey
        {
            get
            {
                if (keyTextBox.Text == string.Empty)
                    throw new ArgumentException();

                long val;
                if (long.TryParse(keyTextBox.Text, out val))
                    return new Safir.Dob.Typesystem.InstanceId(val);

                return new Safir.Dob.Typesystem.InstanceId(keyTextBox.Text.Trim().Replace("\"", ""));
            }
        }

        public Safir.Dob.Typesystem.HandlerId HandlerIdKey
        {
            get
            {
                if (keyTextBox.Text == string.Empty)
                    throw new ArgumentException();

                long val;
                if (long.TryParse(keyTextBox.Text, out val))
                    return new Safir.Dob.Typesystem.HandlerId(val);

                return new Safir.Dob.Typesystem.HandlerId(keyTextBox.Text.Trim().Replace("\"", ""));
            }
        }

        public Safir.Dob.Typesystem.ChannelId ChannelIdKey
        {
            get
            {
                if (keyTextBox.Text == string.Empty)
                    throw new ArgumentException();

                long val;
                if (long.TryParse(keyTextBox.Text, out val))
                    return new Safir.Dob.Typesystem.ChannelId(val);

                return new Safir.Dob.Typesystem.ChannelId(keyTextBox.Text.Trim().Replace("\"", ""));
            }
        }

        public object GetKey()
        {
            switch (keyType)
            {
                case Safir.Dob.Typesystem.MemberType.Int32MemberType:
                    {
                        return Int32Key;
                    }
                case Safir.Dob.Typesystem.MemberType.Int64MemberType:
                    {
                        return Int64Key;
                    }
                case Safir.Dob.Typesystem.MemberType.StringMemberType:
                    {
                        return StringKey;
                    }
                case Safir.Dob.Typesystem.MemberType.TypeIdMemberType:
                    {
                        return TypeIdKey;
                    }
                case Safir.Dob.Typesystem.MemberType.EntityIdMemberType:
                    {
                        return EntityIdKey;
                    }
                case Safir.Dob.Typesystem.MemberType.InstanceIdMemberType:
                    {
                        return InstanceIdKey;
                    }
                case Safir.Dob.Typesystem.MemberType.HandlerIdMemberType:
                    {
                        return HandlerIdKey;
                    }
                case Safir.Dob.Typesystem.MemberType.ChannelIdMemberType:
                    {
                        return ChannelIdKey;
                    }
                case Safir.Dob.Typesystem.MemberType.EnumerationMemberType:
                    {
                        return EnumKey;
                    }

                default:
                    return null;
            }
        }

        private static string TypeIdToString(long typeId)
        {
            if (Safir.Dob.Typesystem.Operations.Exists(typeId))
                return Safir.Dob.Typesystem.Operations.GetName(typeId);
            return typeId.ToString();
        }

        public static string KeyToString(Safir.Dob.Typesystem.MemberType keyType, object key)
        {
            switch (keyType)
            {
                case Safir.Dob.Typesystem.MemberType.TypeIdMemberType:
                    {
                        return TypeIdToString((long)key);
                    }
                case Safir.Dob.Typesystem.MemberType.EntityIdMemberType:
                    {
                        var eid = (Safir.Dob.Typesystem.EntityId)key;
                        return string.Format("{0} : {1}", TypeIdToString(eid.TypeId), eid.InstanceId.ToString());
                    }
                default:
                    return key.ToString();
            }
        }

        public string KeyString
        {
            get
            {
                if (keyType == Safir.Dob.Typesystem.MemberType.EnumerationMemberType)
                {
                    return keyComboBox.SelectedItem.ToString();
                }
                else if (keyType == Safir.Dob.Typesystem.MemberType.EntityIdMemberType)
                {
                    var eid = EntityIdKey;
                    return string.Format("{0} : {1}", TypeIdToString(eid.TypeId), eid.InstanceId.ToString());
                }
                else if (keyType == Safir.Dob.Typesystem.MemberType.TypeIdMemberType)
                {
                    return TypeIdToString(TypeIdKey);
                }

                return keyTextBox.Text.Trim();
            }
        }

        private void OnKeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode==Keys.Enter)
            {
                addButton_Click(sender, e);
            }
        }
    }
}
