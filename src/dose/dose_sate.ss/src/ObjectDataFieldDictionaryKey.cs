using System;
using System.Drawing;
using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
    public partial class ObjectDataFieldDictionaryKey : Form
    {
        private readonly MemberType keyType;

        public ObjectDataFieldDictionaryKey(MemberType keyType, long enumTypeId = 0)
        {
            InitializeComponent();

            this.keyType = keyType;
            BackColor = Color.LightSkyBlue;

            typeLabel.Text = GetTypeName(keyType, enumTypeId);

            if (keyType == MemberType.EnumerationMemberType)
            {
                keyTextBox.Hide();

                var noEnumValues = Operations.GetNumberOfEnumerationValues(enumTypeId);
                var enumValueNames = new string[noEnumValues];
                for (var ev = 0; ev < noEnumValues; ev++)
                {
                    enumValueNames[ev] = Operations.GetEnumerationValueName(enumTypeId, ev);
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

        public string StringKey
        {
            get { return keyTextBox.Text; }
        }

        public int Int32Key
        {
            get { return int.Parse(keyTextBox.Text); }
        }

        public long Int64Key
        {
            get { return long.Parse(keyTextBox.Text); }
        }

        public int EnumKey
        {
            get { return keyComboBox.SelectedIndex; }
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
                    result = Operations.GetTypeId(keyTextBox.Text);
                }
                return result;
            }
        }

        public EntityId EntityIdKey
        {
            get
            {
                if (keyTextBox.Text == string.Empty)
                    throw new ArgumentException();

                var entityId = new EntityId();
                var s = keyTextBox.Text.Trim().Replace(':', ' ').Split(' ');
                if (s.Length < 2)
                {
                    throw new ArgumentException();
                }
                if (s.Length > 2)
                {
                    for (var i = 1; i < s.Length - 1; i++)
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
                        throw new ArgumentException();
                    }
                    entityId.InstanceId = new InstanceId(idString);
                }
                return entityId;
            }
        }


        public InstanceId InstanceIdKey
        {
            get
            {
                if (keyTextBox.Text == string.Empty)
                    throw new ArgumentException();

                long val;
                if (long.TryParse(keyTextBox.Text, out val))
                    return new InstanceId(val);

                return new InstanceId(keyTextBox.Text.Trim().Replace("\"", ""));
            }
        }

        public HandlerId HandlerIdKey
        {
            get
            {
                if (keyTextBox.Text == string.Empty)
                    throw new ArgumentException();

                long val;
                if (long.TryParse(keyTextBox.Text, out val))
                    return new HandlerId(val);

                return new HandlerId(keyTextBox.Text.Trim().Replace("\"", ""));
            }
        }

        public ChannelId ChannelIdKey
        {
            get
            {
                if (keyTextBox.Text == string.Empty)
                    throw new ArgumentException();

                long val;
                if (long.TryParse(keyTextBox.Text, out val))
                    return new ChannelId(val);

                return new ChannelId(keyTextBox.Text.Trim().Replace("\"", ""));
            }
        }

        public string KeyString
        {
            get
            {
                if (keyType == MemberType.EnumerationMemberType)
                {
                    return keyComboBox.SelectedItem.ToString();
                }
                if (keyType == MemberType.EntityIdMemberType)
                {
                    var eid = EntityIdKey;
                    return string.Format("{0} : {1}", TypeIdToString(eid.TypeId), eid.InstanceId);
                }
                if (keyType == MemberType.TypeIdMemberType)
                {
                    return TypeIdToString(TypeIdKey);
                }

                return keyTextBox.Text.Trim();
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
                    case MemberType.Int32MemberType:
                    {
                        var dummy = Int32Key;
                        return true;
                    }
                    case MemberType.Int64MemberType:
                    {
                        var dummy = Int64Key;
                        return true;
                    }
                    case MemberType.StringMemberType:
                    {
                        //always valid
                        return true;
                    }
                    case MemberType.TypeIdMemberType:
                    {
                        var dummy = TypeIdKey;
                        return true;
                    }
                    case MemberType.EntityIdMemberType:
                    {
                        var dummy = EntityIdKey;
                        return true;
                    }
                    case MemberType.InstanceIdMemberType:
                    {
                        var dummy = InstanceIdKey;
                        return true;
                    }
                    case MemberType.HandlerIdMemberType:
                    {
                        var dummy = HandlerIdKey;
                        return true;
                    }
                    case MemberType.ChannelIdMemberType:
                    {
                        var dummy = ChannelIdKey;
                        return true;
                    }
                    case MemberType.EnumerationMemberType:
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
            DialogResult = DialogResult.Cancel;
        }

        private void addButton_Click(object sender, EventArgs e)
        {
            if (ValidInput())
                DialogResult = DialogResult.OK;
        }

        public static string GetTypeName(MemberType keyType, long typeId)
        {
            switch (keyType)
            {
                case MemberType.Int32MemberType:
                {
                    return "Int32";
                }
                case MemberType.Int64MemberType:
                {
                    return "Int64";
                }
                case MemberType.StringMemberType:
                {
                    return "String";
                }
                case MemberType.TypeIdMemberType:
                {
                    return "TypeId";
                }
                case MemberType.EntityIdMemberType:
                {
                    return "EntityId";
                }
                case MemberType.InstanceIdMemberType:
                {
                    return "InstanceId";
                }
                case MemberType.HandlerIdMemberType:
                {
                    return "HandlerId";
                }
                case MemberType.ChannelIdMemberType:
                {
                    return "ChannelId";
                }
                case MemberType.EnumerationMemberType:
                {
                    return Operations.GetName(typeId);
                }

                default:
                    throw new ArgumentException("GetTypeName of unknown memberType");
            }
        }

        public object GetKey()
        {
            switch (keyType)
            {
                case MemberType.Int32MemberType:
                {
                    return Int32Key;
                }
                case MemberType.Int64MemberType:
                {
                    return Int64Key;
                }
                case MemberType.StringMemberType:
                {
                    return StringKey;
                }
                case MemberType.TypeIdMemberType:
                {
                    return TypeIdKey;
                }
                case MemberType.EntityIdMemberType:
                {
                    return EntityIdKey;
                }
                case MemberType.InstanceIdMemberType:
                {
                    return InstanceIdKey;
                }
                case MemberType.HandlerIdMemberType:
                {
                    return HandlerIdKey;
                }
                case MemberType.ChannelIdMemberType:
                {
                    return ChannelIdKey;
                }
                case MemberType.EnumerationMemberType:
                {
                    return EnumKey;
                }

                default:
                    return null;
            }
        }

        private static string TypeIdToString(long typeId)
        {
            if (Operations.Exists(typeId))
                return Operations.GetName(typeId);
            return typeId.ToString();
        }

        public static string KeyToString(MemberType keyType, object key)
        {
            switch (keyType)
            {
                case MemberType.TypeIdMemberType:
                {
                    return TypeIdToString((long) key);
                }
                case MemberType.EntityIdMemberType:
                {
                    var eid = (EntityId) key;
                    return string.Format("{0} : {1}", TypeIdToString(eid.TypeId), eid.InstanceId);
                }
                default:
                    return key.ToString();
            }
        }

        private void OnKeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                addButton_Click(sender, e);
            }
        }
    }
}