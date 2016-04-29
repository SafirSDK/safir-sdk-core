/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
* 
* Created by: Joel Ottosson / stjoot
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/

using System;
using System.Drawing;
using System.Windows.Forms;
using Safir.Dob;
using Safir.Dob.Typesystem;
using Message = Safir.Dob.Message;
using Object = Safir.Dob.Typesystem.Object;

namespace Sate
{
    //*****************************************************************************************************
    // Object Edit Control
    //*****************************************************************************************************
    public class ObjectEditControl : UserControl
    {
        private readonly Label channelIdInputLabel;
        private readonly TextBox channelIdInputTextBox;
        private readonly LinkLabel createEntityHandlerDecidesReqLink;
        private readonly LinkLabel createEntityReqLink;
        private readonly LinkLabel deleteEntityLink;
        private readonly LinkLabel deleteEntityReqLink;

        private readonly Label handlerIdInputLabel;
        private readonly TextBox handlerIdInputTextBox;

        private readonly LinkLabel injectChangesLink;
        private readonly LinkLabel injectDeleteLink;
        private readonly LinkLabel injectInitialSetLink;
        private readonly Label injectTimestampLabel;
        private readonly TextBox injectTimestampTextBox;
        private readonly CheckBox liveDataCheckBox = new CheckBox();
        private readonly ObjectEditPanel objectPanel;

        private readonly LinkLabel sendMessageLink;
        private readonly LinkLabel sendServiceReqLink;
        private readonly LinkLabel setAllEntityLink;

        private readonly LinkLabel setChangesEntityLink;
        private readonly LinkLabel toJsonLink;
        private readonly LinkLabel toXmlLink;
        private readonly LinkLabel updateEntityReqLink;
        private readonly LinkLabel useReplyLink;
        private readonly Panel _rightPanel = new Panel();

        public ObjectEditControl(ObjectInfo objInfo)
        {
            if (objInfo == null)
                return;

            objectPanel = new ObjectEditPanel(objInfo, false);
            objectPanel.AutoScroll = true;
            objectPanel.Dock = DockStyle.Fill;
            RightPanel.Dock = DockStyle.Right;

            Controls.AddRange(new Control[] {objectPanel, RightPanel});

            AutoScroll = true;
            BackColor = Color.White;

            //--- Create operations panel ---

            var operations = new Panel();
            operations.BackColor = Color.LightYellow;
            var operationLabel = new Label();
            operationLabel.Font = new Font(FontFamily.GenericSansSerif, 10, FontStyle.Bold | FontStyle.Underline);
            operationLabel.Text = "Operations";
            operationLabel.ForeColor = Color.Black;
            operationLabel.Location = new Point(10, 10);
            operations.Controls.Add(operationLabel);

            toXmlLink = new LinkLabel();
            toXmlLink.AutoSize = true;
            toXmlLink.Text = "Serialize to XML";
            toXmlLink.Location = new Point(10, 40);
            toXmlLink.Click += toXmlLink_Click;
            operations.Controls.Add(toXmlLink);

            toJsonLink = new LinkLabel();
            toJsonLink.AutoSize = true;
            toJsonLink.Text = "Serialize to JSON";
            toJsonLink.Location = new Point(10, 60);
            toJsonLink.Click += toJsonLink_Click;
            operations.Controls.Add(toJsonLink);


            if (Operations.IsOfType(objInfo.Obj.GetTypeId(), Entity.ClassTypeId))
            {
                setChangesEntityLink = new LinkLabel();
                setChangesEntityLink.AutoSize = true;
                setChangesEntityLink.Text = "SetChanges";
                setChangesEntityLink.Location = new Point(10, 80);
                setChangesEntityLink.Click += setChangesEntityLink_Click;

                setAllEntityLink = new LinkLabel();
                setAllEntityLink.AutoSize = true;
                setAllEntityLink.Text = "SetAll";
                setAllEntityLink.Location = new Point(10, 100);
                setAllEntityLink.Click += setAllEntityLink_Click;

                deleteEntityLink = new LinkLabel();
                deleteEntityLink.AutoSize = true;
                deleteEntityLink.Text = "Delete";
                deleteEntityLink.Location = new Point(10, 120);
                deleteEntityLink.Click += deleteEntityLink_Click;

                createEntityReqLink = new LinkLabel();
                createEntityReqLink.AutoSize = true;
                createEntityReqLink.Text = "Create Request (requestor dec.)";
                createEntityReqLink.Location = new Point(10, 140);
                createEntityReqLink.Click += createEntityReqLink_Click;

                createEntityHandlerDecidesReqLink = new LinkLabel();
                createEntityHandlerDecidesReqLink.AutoSize = true;
                createEntityHandlerDecidesReqLink.Text = "Create Request (handler dec.)";
                createEntityHandlerDecidesReqLink.Location = new Point(10, 160);
                createEntityHandlerDecidesReqLink.Click += createEntityHandlerDecidesReqLink_Click;

                updateEntityReqLink = new LinkLabel();
                updateEntityReqLink.AutoSize = true;
                updateEntityReqLink.Text = "Update Request";
                updateEntityReqLink.Location = new Point(10, 180);
                updateEntityReqLink.Click += updateEntityReqLink_Click;

                deleteEntityReqLink = new LinkLabel();
                deleteEntityReqLink.AutoSize = true;
                deleteEntityReqLink.Text = "Delete Request";
                deleteEntityReqLink.Location = new Point(10, 200);
                deleteEntityReqLink.Click += deleteEntityReqLink_Click;

                handlerIdInputLabel = new Label();
                handlerIdInputLabel.AutoSize = true;
                handlerIdInputLabel.Text = "HandlerId:";
                handlerIdInputLabel.Location = new Point(10, 220);
                operations.Controls.Add(handlerIdInputLabel);

                handlerIdInputTextBox = new TextBox();
                var gotValue = false;
                if (objInfo is EntityInfo)
                {
                    var entityInfo = (EntityInfo) objInfo;
                    if (entityInfo.getHandlerId() != null)
                    {
                        handlerIdInputTextBox.Text = ((EntityInfo) objInfo).getHandlerId().ToString();
                        gotValue = true;
                    }
                }
                if (!gotValue)
                {
                    handlerIdInputTextBox.Text = "DEFAULT_HANDLER";
                }
                handlerIdInputTextBox.Location = new Point(10, 240);
                //new Point(idInputLabel.Location.X + idInputLabel.Width, 10);
                handlerIdInputTextBox.Width = 120;
                handlerIdInputTextBox.TextChanged += handlerIdInputTextBox_TextChanged;
                operations.Controls.Add(handlerIdInputTextBox);

                operations.Controls.AddRange(new Control[]
                {
                    setChangesEntityLink,
                    setAllEntityLink,
                    deleteEntityLink,
                    createEntityReqLink,
                    createEntityHandlerDecidesReqLink,
                    updateEntityReqLink,
                    deleteEntityReqLink
                });


                if (InjectionProperty.HasProperty(objInfo.Obj) &&
                    InjectionProperty.GetInjection(objInfo.Obj) == InjectionKind.Enumeration.Injectable)
                {
                    injectTimestampLabel = new Label();
                    injectTimestampLabel.AutoSize = true;
                    injectTimestampLabel.Text = "Timestamp:";
                    injectTimestampLabel.Location = new Point(10, 340);
                    operations.Controls.Add(injectTimestampLabel);

                    injectTimestampTextBox = new TextBox();
                    injectTimestampTextBox.Text = "0";
                    injectTimestampTextBox.Location = new Point(10, 360);
                    //new Point(idInputLabel.Location.X + idInputLabel.Width, 10);
                    injectTimestampTextBox.Width = 120;
                    injectTimestampTextBox.TextChanged += injectTimestampTextBox_TextChanged;
                    operations.Controls.Add(injectTimestampTextBox);

                    injectChangesLink = new LinkLabel();
                    injectChangesLink.AutoSize = true;
                    injectChangesLink.Text = "InjectChanges";
                    injectChangesLink.Location = new Point(10, 280);
                    injectChangesLink.Click += injectChangesLink_Click;

                    injectInitialSetLink = new LinkLabel();
                    injectInitialSetLink.AutoSize = true;
                    injectInitialSetLink.Text = "Inject InitialSet";
                    injectInitialSetLink.Location = new Point(10, 300);
                    injectInitialSetLink.Click += injectInitialSetLink_Click;

                    injectDeleteLink = new LinkLabel();
                    injectDeleteLink.AutoSize = true;
                    injectDeleteLink.Text = "InjectDelete";
                    injectDeleteLink.Location = new Point(10, 320);
                    injectDeleteLink.Click += injectDeleteLink_Click;


                    operations.Controls.AddRange(new Control[]
                    {
                        injectInitialSetLink,
                        injectChangesLink,
                        injectDeleteLink
                    });
                }
            }

            if (Operations.IsOfType(objInfo.Obj.GetTypeId(), Message.ClassTypeId))
            {
                sendMessageLink = new LinkLabel();
                sendMessageLink.AutoSize = true;
                sendMessageLink.Text = "Send Message";
                sendMessageLink.Location = new Point(10, 80);
                sendMessageLink.Click += sendMessageLink_Click;
                operations.Controls.Add(sendMessageLink);

                channelIdInputLabel = new Label();
                channelIdInputLabel.AutoSize = true;
                channelIdInputLabel.Text = "ChannelId:";
                channelIdInputLabel.Location = new Point(10, 200);
                operations.Controls.Add(channelIdInputLabel);

                channelIdInputTextBox = new TextBox();
                var gotValue = false;
                if (objInfo is MessageInfo)
                {
                    var messageInfo = (MessageInfo) objInfo;
                    if (messageInfo.getChannelId() != null)
                    {
                        channelIdInputTextBox.Text = ((MessageInfo) objInfo).getChannelId().ToString();
                        gotValue = true;
                    }
                }
                if (!gotValue)
                {
                    channelIdInputTextBox.Text = "DEFAULT_CHANNEL";
                }
                channelIdInputTextBox.Location = new Point(10, 220);
                //new Point(idInputLabel.Location.X + idInputLabel.Width, 10);
                channelIdInputTextBox.Width = 120;
                channelIdInputTextBox.TextChanged += channelIdInputTextBox_TextChanged;
                operations.Controls.Add(channelIdInputTextBox);
            }

            else if (Operations.IsOfType(objInfo.Obj.GetTypeId(), Service.ClassTypeId))
            {
                sendServiceReqLink = new LinkLabel();
                sendServiceReqLink.AutoSize = true;
                sendServiceReqLink.Text = "Send Service Request";
                sendServiceReqLink.Location = new Point(10, 80);
                sendServiceReqLink.Click += sendServiceReqLink_Click;
                operations.Controls.Add(sendServiceReqLink);

                handlerIdInputLabel = new Label();
                handlerIdInputLabel.AutoSize = true;
                handlerIdInputLabel.Text = "HandlerId:";
                handlerIdInputLabel.Location = new Point(10, 200);
                operations.Controls.Add(handlerIdInputLabel);

                handlerIdInputTextBox = new TextBox();
                var gotValue = false;
                if (objInfo is EntityInfo)
                {
                    var entityInfo = (EntityInfo) objInfo;
                    if (entityInfo.getHandlerId() != null)
                    {
                        handlerIdInputTextBox.Text = ((EntityInfo) objInfo).getHandlerId().ToString();
                        gotValue = true;
                    }
                }
                if (!gotValue)
                {
                    handlerIdInputTextBox.Text = "DEFAULT_HANDLER";
                }
                handlerIdInputTextBox.Location = new Point(10, 220);
                //new Point(idInputLabel.Location.X + idInputLabel.Width, 10);
                handlerIdInputTextBox.Width = 120;
                handlerIdInputTextBox.TextChanged += handlerIdInputTextBox_TextChanged;
                operations.Controls.Add(handlerIdInputTextBox);
            }

            else if (Operations.IsOfType(objInfo.Obj.GetTypeId(), Response.ClassTypeId))
            {
                useReplyLink = new LinkLabel();
                useReplyLink.AutoSize = true;
                useReplyLink.Text = "Use as default response";
                useReplyLink.Location = new Point(10, 80);
                useReplyLink.Click += useReplyLink_Click;
                operations.Controls.Add(useReplyLink);
            }

            //live data check box
            liveDataCheckBox.Location = new Point(10, 400);
            liveDataCheckBox.Text = "Live data";
            operations.Controls.Add(liveDataCheckBox);

            operations.Size = new Size(170, 420);
            operations.Location = new Point(20, 40);

            RightPanel.Controls.Add(operations);

            Tag = objInfo;
        }

        public Panel RightPanel
        {
            get { return _rightPanel; }
        }

        public bool LiveData
        {
            get { return liveDataCheckBox.Checked; }
        }

        private void useReplyLink_Click(object sender, EventArgs e)
        {
            if (objectPanel.SetObjectMembers())
            {
                var responseInfo = (ResponseInfo) Tag;
                Settings.Sate.AutoResponse = (Response) responseInfo.Obj;
                Settings.Sate.XmlReplyObject = Serialization.ToXml(Settings.Sate.AutoResponse);
                Settings.Save();
            }
            else
            {
                MessageBox.Show("Failed to serialize the response to xml.");
            }
        }

        private void setChangesEntityLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                if (entityInfo.GetInstanceId() == null)
                {
                    entityInfo.SetInstanceId(new InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new HandlerId());
                }

                MainForm.Instance.SetChangesEntity(entityInfo);
            }
        }

        private void setAllEntityLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                if (entityInfo.GetInstanceId() == null)
                {
                    entityInfo.SetInstanceId(InstanceId.GenerateRandom());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new HandlerId());
                }

                MainForm.Instance.SetAllEntity(entityInfo);
            }
        }

        private void deleteEntityLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                if (entityInfo.GetInstanceId() == null)
                {
                    entityInfo.SetInstanceId(new InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new HandlerId());
                }

                MainForm.Instance.DeleteEntity(entityInfo);
            }
        }

        private void createEntityReqLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                entityInfo.RequestorDecides = true;
                if (entityInfo.GetInstanceId() == null)
                {
                    entityInfo.SetInstanceId(new InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new HandlerId());
                }

                MainForm.Instance.CreateRequest(entityInfo);
            }
        }

        private void createEntityHandlerDecidesReqLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                entityInfo.RequestorDecides = false;
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new HandlerId());
                }

                MainForm.Instance.CreateRequest(entityInfo);
            }
        }

        private void updateEntityReqLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                if (entityInfo.GetInstanceId() == null)
                {
                    entityInfo.SetInstanceId(InstanceId.GenerateRandom());
                }

                MainForm.Instance.UpdateRequest(entityInfo);
            }
        }

        private void deleteEntityReqLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                if (entityInfo.GetInstanceId() == null)
                {
                    throw new SoftwareViolationException("Error doing delete. No InstanceId set in entityInfo!");
                }
                MainForm.Instance.DeleteRequest(entityInfo);
            }
        }

        private void injectChangesLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                if (entityInfo.GetInstanceId() == null)
                {
                    entityInfo.SetInstanceId(new InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new HandlerId());
                }

                MainForm.Instance.InjectionSetChanges(entityInfo);
            }
        }

        private void injectInitialSetLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                if (entityInfo.GetInstanceId() == null)
                {
                    entityInfo.SetInstanceId(new InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new HandlerId());
                }

                MainForm.Instance.InjectionInitialSet(entityInfo);
            }
        }

        private void injectDeleteLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var entityInfo = (EntityInfo) Tag;
                if (entityInfo.GetInstanceId() == null)
                {
                    entityInfo.SetInstanceId(new InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new HandlerId());
                }

                MainForm.Instance.InjectionDelete(entityInfo);
            }
        }

        private void sendMessageLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var msgInfo = (MessageInfo) Tag;
                // if no channelid set, set to default
                if (msgInfo.getChannelId() == null)
                {
                    msgInfo.setChannelId(new ChannelId());
                }
                MainForm.Instance.SendMessage(msgInfo);
            }
        }


        private void sendServiceReqLink_Click(object sender, EventArgs e)
        {
            if (!MainForm.Instance.CheckConnection())
                return;

            if (objectPanel.SetObjectMembers())
            {
                var srvInfo = (ServiceHandlerInfo) Tag;
                if (srvInfo.getHandlerId() == null)
                {
                    srvInfo.setHandlerId(new HandlerId());
                }
                MainForm.Instance.ServiceRequest(srvInfo);
            }
        }


        private void toXmlLink_Click(object sender, EventArgs e)
        {
            if (objectPanel.SetObjectMembers())
            {
                MainForm.Instance.AddTabPage(new XmlTabPage((ObjectInfo) Tag));
            }
        }

        private void toJsonLink_Click(object sender, EventArgs e)
        {
            if (objectPanel.SetObjectMembers())
            {
                MainForm.Instance.AddTabPage(new JsonTabPage((ObjectInfo) Tag));
            }
        }


        public Object GetObject()
        {
            objectPanel.SetObjectMembers();
            return ((ObjectInfo) objectPanel.Tag).Obj;
        }

        public ObjectInfo GetObjectInfo()
        {
            objectPanel.SetObjectMembers();
            return (ObjectInfo) objectPanel.Tag;
        }

        public void UpdateData(ObjectInfo objInfo)
        {
            Tag = objInfo;
            objectPanel.UpdateData(objInfo);
        }

        public void DeletedData(bool deleted) //if deleted=false, then removed
        {
            objectPanel.SetDeleted(deleted);
        }

        private void handlerIdInputTextBox_TextChanged(object sender, EventArgs e)
        {
            resulttype result;
            long val = 0;
            /* try to parse as an long. O/w use the string representation */

            try
            {
                val = long.Parse(handlerIdInputTextBox.Text);
                result = resulttype.longvalue;
            }
            catch
            {
                if (handlerIdInputTextBox.Text != "")
                {
                    result = resulttype.stringvalue;
                }
                else
                {
                    result = resulttype.novalue;
                }
            }

            if (Tag is EntityInfo)
            {
                switch (result)
                {
                    case resulttype.longvalue:
                        ((EntityInfo) Tag).setHandlerId(new HandlerId(val));
                        break;
                    case resulttype.stringvalue:
                        ((EntityInfo) Tag).setHandlerId(new HandlerId(handlerIdInputTextBox.Text));
                        break;
                    default:
                        ((EntityInfo) Tag).setHandlerId(new HandlerId());
                        break;
                }
            }
            else if (Tag is ServiceHandlerInfo)
            {
                switch (result)
                {
                    case resulttype.longvalue:
                        ((ServiceHandlerInfo) Tag).setHandlerId(new HandlerId(val));
                        break;
                    case resulttype.stringvalue:
                        ((ServiceHandlerInfo) Tag).setHandlerId(new HandlerId(handlerIdInputTextBox.Text));
                        break;
                    default:
                        ((ServiceHandlerInfo) Tag).setHandlerId(new HandlerId());
                        break;
                }
            }
            handlerIdInputTextBox.BackColor = ColorMap.ENABLED;
        }

        private void channelIdInputTextBox_TextChanged(object sender, EventArgs e)
        {
            /* only valid for messages */
            if (!(Tag is MessageInfo))
            {
                return;
            }

            try
            {
                ((MessageInfo) Tag).setChannelId(new ChannelId(long.Parse(channelIdInputTextBox.Text)));
            }
            catch
            {
                string idString;
                if (channelIdInputTextBox.Text.StartsWith("\"") && channelIdInputTextBox.Text.EndsWith("\"") &&
                    channelIdInputTextBox.Text.Length > 2)
                {
                    // remove quotation
                    idString = channelIdInputTextBox.Text.Substring(1, channelIdInputTextBox.Text.Length - 2);
                }
                else
                {
                    idString = channelIdInputTextBox.Text;
                }

                if (idString == "")
                {
                    channelIdInputTextBox.BackColor = ColorMap.ERROR;
                    return;
                }
                ((MessageInfo) Tag).setChannelId(new ChannelId(idString));
            }

            channelIdInputTextBox.BackColor = ColorMap.ENABLED;
        }

        private void injectTimestampTextBox_TextChanged(object sender, EventArgs e)
        {
            long val = 0;

            try
            {
                val = long.Parse(injectTimestampTextBox.Text);
            }
            catch
            {
                val = 0;
                injectTimestampTextBox.Text = "0";
            }

            if (Tag is EntityInfo)
            {
                ((EntityInfo) Tag).Timestamp = val;
            }

            injectTimestampTextBox.BackColor = ColorMap.ENABLED;
        }

        private enum resulttype
        {
            longvalue,
            stringvalue,
            novalue
        }
    }
}