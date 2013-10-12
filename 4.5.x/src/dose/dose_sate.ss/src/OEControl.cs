/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace Sate
{
    //*****************************************************************************************************
    // Object Edit Control
    //*****************************************************************************************************
    public class ObjectEditControl : UserControl
    {
        private System.Windows.Forms.CheckBox liveDataCheckBox = new CheckBox();

        private Panel rightPanel = new Panel();
        private ObjectEditPanel objectPanel = null;

        private System.Windows.Forms.LinkLabel setChangesEntityLink;
        private System.Windows.Forms.LinkLabel setAllEntityLink;
        private System.Windows.Forms.LinkLabel deleteEntityLink;
        private System.Windows.Forms.LinkLabel createEntityReqLink;
        private System.Windows.Forms.LinkLabel createEntityHandlerDecidesReqLink;
        private System.Windows.Forms.LinkLabel updateEntityReqLink;
        private System.Windows.Forms.LinkLabel deleteEntityReqLink;
        private System.Windows.Forms.LinkLabel sendMessageLink;
        private System.Windows.Forms.LinkLabel sendServiceReqLink;
        private System.Windows.Forms.LinkLabel toXmlLink;
        private System.Windows.Forms.LinkLabel useReplyLink;
        
        private System.Windows.Forms.Label handlerIdInputLabel;
        private System.Windows.Forms.TextBox handlerIdInputTextBox;
        private System.Windows.Forms.Label channelIdInputLabel;
        private System.Windows.Forms.TextBox channelIdInputTextBox;

        private System.Windows.Forms.LinkLabel injectChangesLink;
        private System.Windows.Forms.LinkLabel injectInitialSetLink;
        private System.Windows.Forms.LinkLabel injectDeleteLink;
        private System.Windows.Forms.Label injectTimestampLabel;
        private System.Windows.Forms.TextBox injectTimestampTextBox;

        public ObjectEditControl(ObjectInfo objInfo)
        {
            if (objInfo == null)
                return;
            
            objectPanel = new ObjectEditPanel(objInfo, false);
            objectPanel.AutoScroll = true;
            objectPanel.Dock = DockStyle.Fill;
            rightPanel.Dock = DockStyle.Right;

            this.Controls.AddRange(new Control[] { objectPanel, rightPanel });

            this.AutoScroll = true;
            this.BackColor = Color.White;

            //--- Create operations panel ---

            Panel operations = new Panel();
            operations.BackColor = Color.LightYellow;
            Label operationLabel = new Label();
            operationLabel.Font = new Font(FontFamily.GenericSansSerif, 10, FontStyle.Bold | FontStyle.Underline);
            operationLabel.Text = "Operations";
            operationLabel.ForeColor = Color.Black;
            operationLabel.Location = new Point(10, 10);
            operations.Controls.Add(operationLabel);

            toXmlLink = new LinkLabel();
            toXmlLink.AutoSize = true;
            toXmlLink.Text = "Serialize to XML";
            toXmlLink.Location = new Point(10, 40);
            toXmlLink.Click += new EventHandler(toXmlLink_Click);
            operations.Controls.Add(toXmlLink);


            if (Safir.Dob.Typesystem.Operations.IsOfType(objInfo.Obj.GetTypeId(), Safir.Dob.Entity.ClassTypeId))
            {
                setChangesEntityLink = new LinkLabel();
                setChangesEntityLink.AutoSize = true;
                setChangesEntityLink.Text = "SetChanges";
                setChangesEntityLink.Location = new Point(10, 60);
                setChangesEntityLink.Click += new EventHandler(setChangesEntityLink_Click);

                setAllEntityLink = new LinkLabel();
                setAllEntityLink.AutoSize = true;
                setAllEntityLink.Text = "SetAll";
                setAllEntityLink.Location = new Point(10, 80);
                setAllEntityLink.Click += new EventHandler(setAllEntityLink_Click);

                deleteEntityLink = new LinkLabel();
                deleteEntityLink.AutoSize = true;
                deleteEntityLink.Text = "Delete";
                deleteEntityLink.Location = new Point(10, 100);
                deleteEntityLink.Click += new EventHandler(deleteEntityLink_Click);

                createEntityReqLink = new LinkLabel();
                createEntityReqLink.AutoSize = true;
                createEntityReqLink.Text = "Create Request (requestor dec.)";
                createEntityReqLink.Location = new Point(10, 120);
                createEntityReqLink.Click += new EventHandler(createEntityReqLink_Click);

                createEntityHandlerDecidesReqLink = new LinkLabel();
                createEntityHandlerDecidesReqLink.AutoSize = true;
                createEntityHandlerDecidesReqLink.Text = "Create Request (handler dec.)";
                createEntityHandlerDecidesReqLink.Location = new Point(10, 140);
                createEntityHandlerDecidesReqLink.Click += new EventHandler(createEntityHandlerDecidesReqLink_Click);

                updateEntityReqLink = new LinkLabel();
                updateEntityReqLink.AutoSize = true;
                updateEntityReqLink.Text = "Update Request";
                updateEntityReqLink.Location = new Point(10, 160);
                updateEntityReqLink.Click += new EventHandler(updateEntityReqLink_Click);

                deleteEntityReqLink = new LinkLabel();
                deleteEntityReqLink.AutoSize = true;
                deleteEntityReqLink.Text = "Delete Request";
                deleteEntityReqLink.Location = new Point(10, 180);
                deleteEntityReqLink.Click += new EventHandler(deleteEntityReqLink_Click);

                handlerIdInputLabel = new Label();
                handlerIdInputLabel.AutoSize = true;
                handlerIdInputLabel.Text = "HandlerId:";
                handlerIdInputLabel.Location = new Point(10, 200);
                operations.Controls.Add(handlerIdInputLabel);

                handlerIdInputTextBox = new TextBox();
                bool gotValue = false;
                if (objInfo is EntityInfo)
                {
                    EntityInfo entityInfo = (EntityInfo)objInfo;
                    if (entityInfo.getHandlerId() != null)
                    {
                        handlerIdInputTextBox.Text = ((EntityInfo)objInfo).getHandlerId().ToString();
                        gotValue = true;
                    }
                }
                if (!gotValue)
                {
                    handlerIdInputTextBox.Text = "DEFAULT_HANDLER";
                }
                handlerIdInputTextBox.Location = new Point(10, 220); //new Point(idInputLabel.Location.X + idInputLabel.Width, 10);
                handlerIdInputTextBox.Width = 120;
                handlerIdInputTextBox.TextChanged += new EventHandler(handlerIdInputTextBox_TextChanged);
                operations.Controls.Add(handlerIdInputTextBox);

                operations.Controls.AddRange(new Control[] {   setChangesEntityLink,
                                                               setAllEntityLink,
                                                               deleteEntityLink,
                                                               createEntityReqLink,
                                                               createEntityHandlerDecidesReqLink,
                                                               updateEntityReqLink,
                                                               deleteEntityReqLink});

                 
                if (Safir.Dob.InjectionProperty.HasProperty(objInfo.Obj) && 
                    Safir.Dob.InjectionProperty.GetInjection(objInfo.Obj) == Safir.Dob.InjectionKind.Enumeration.Injectable)
                {
                    injectTimestampLabel = new Label();
                    injectTimestampLabel.AutoSize = true;
                    injectTimestampLabel.Text = "Timestamp:";
                    injectTimestampLabel.Location = new Point(10, 320);
                    operations.Controls.Add(injectTimestampLabel);

                    injectTimestampTextBox = new TextBox();
                    injectTimestampTextBox.Text = "0";
                    injectTimestampTextBox.Location = new Point(10, 340); //new Point(idInputLabel.Location.X + idInputLabel.Width, 10);
                    injectTimestampTextBox.Width = 120;
                    injectTimestampTextBox.TextChanged += new EventHandler(injectTimestampTextBox_TextChanged);
                    operations.Controls.Add(injectTimestampTextBox);

                    injectChangesLink = new LinkLabel();
                    injectChangesLink.AutoSize = true;
                    injectChangesLink.Text = "InjectChanges";
                    injectChangesLink.Location = new Point(10, 260);
                    injectChangesLink.Click += new EventHandler(injectChangesLink_Click);

                    injectInitialSetLink = new LinkLabel();
                    injectInitialSetLink.AutoSize = true;
                    injectInitialSetLink.Text = "Inject InitialSet";
                    injectInitialSetLink.Location = new Point(10, 280);
                    injectInitialSetLink.Click += new EventHandler(injectInitialSetLink_Click);

                    injectDeleteLink = new LinkLabel();
                    injectDeleteLink.AutoSize = true;
                    injectDeleteLink.Text = "InjectDelete";
                    injectDeleteLink.Location = new Point(10, 300);
                    injectDeleteLink.Click += new EventHandler(injectDeleteLink_Click);


                    operations.Controls.AddRange(new Control[] {injectInitialSetLink, 
                                                               injectChangesLink,
                                                               injectDeleteLink});
                }
                

            }

            if (Safir.Dob.Typesystem.Operations.IsOfType(objInfo.Obj.GetTypeId(), Safir.Dob.Message.ClassTypeId))
            {
                sendMessageLink = new LinkLabel();
                sendMessageLink.AutoSize = true;
                sendMessageLink.Text = "Send Message";
                sendMessageLink.Location = new Point(10, 60);
                sendMessageLink.Click += new EventHandler(sendMessageLink_Click);
                operations.Controls.Add(sendMessageLink);

                channelIdInputLabel = new Label();
                channelIdInputLabel.AutoSize = true;
                channelIdInputLabel.Text = "ChannelId:";
                channelIdInputLabel.Location = new Point(10, 180);
                operations.Controls.Add(channelIdInputLabel);

                channelIdInputTextBox = new TextBox();
                bool gotValue = false;
                if (objInfo is MessageInfo)
                {
                    MessageInfo messageInfo = (MessageInfo)objInfo;
                    if (messageInfo.getChannelId() != null)
                    {
                        channelIdInputTextBox.Text = ((MessageInfo)objInfo).getChannelId().ToString();
                        gotValue = true;
                    }
                }
                if (!gotValue)
                {
                    channelIdInputTextBox.Text = "DEFAULT_CHANNEL";
                }
                channelIdInputTextBox.Location = new Point(10, 200); //new Point(idInputLabel.Location.X + idInputLabel.Width, 10);
                channelIdInputTextBox.Width = 120;
                channelIdInputTextBox.TextChanged += new EventHandler(channelIdInputTextBox_TextChanged);
                operations.Controls.Add(channelIdInputTextBox);

            }

            else if (Safir.Dob.Typesystem.Operations.IsOfType(objInfo.Obj.GetTypeId(), Safir.Dob.Service.ClassTypeId))
            {
                sendServiceReqLink = new LinkLabel();
                sendServiceReqLink.AutoSize = true;
                sendServiceReqLink.Text = "Send Service Request";
                sendServiceReqLink.Location = new Point(10, 60);
                sendServiceReqLink.Click += new EventHandler(sendServiceReqLink_Click);
                operations.Controls.Add(sendServiceReqLink);

                handlerIdInputLabel = new Label();
                handlerIdInputLabel.AutoSize = true;
                handlerIdInputLabel.Text = "HandlerId:";
                handlerIdInputLabel.Location = new Point(10, 180);
                operations.Controls.Add(handlerIdInputLabel);

                handlerIdInputTextBox = new TextBox();
                bool gotValue = false;
                if (objInfo is EntityInfo)
                {
                    EntityInfo entityInfo = (EntityInfo)objInfo;
                    if (entityInfo.getHandlerId() != null)
                    {
                        handlerIdInputTextBox.Text = ((EntityInfo)objInfo).getHandlerId().ToString();
                        gotValue = true;
                    }
                }
                if (!gotValue)
                {
                    handlerIdInputTextBox.Text = "DEFAULT_HANDLER";
                }
                handlerIdInputTextBox.Location = new Point(10, 200); //new Point(idInputLabel.Location.X + idInputLabel.Width, 10);
                handlerIdInputTextBox.Width = 120;
                handlerIdInputTextBox.TextChanged += new EventHandler(handlerIdInputTextBox_TextChanged);
                operations.Controls.Add(handlerIdInputTextBox);

            }

            else if (Safir.Dob.Typesystem.Operations.IsOfType(objInfo.Obj.GetTypeId(), Safir.Dob.Response.ClassTypeId))
            {
                useReplyLink = new LinkLabel();
                useReplyLink.AutoSize = true;
                useReplyLink.Text = "Use as default response";
                useReplyLink.Location = new Point(10, 60);
                useReplyLink.Click += new EventHandler(useReplyLink_Click);
                operations.Controls.Add(useReplyLink);
            }

            //live data check box
            liveDataCheckBox.Location = new Point(10, 360);
            liveDataCheckBox.Text = "Live data";
            operations.Controls.Add(liveDataCheckBox);

            operations.Size = new Size(170, 380);
            operations.Location = new Point(20, 40);

            rightPanel.Controls.Add(operations);

            this.Tag = objInfo;
        }

        void useReplyLink_Click(object sender, EventArgs e)
        {
            if (objectPanel.SetObjectMembers())
            {
                ResponseInfo responseInfo = (ResponseInfo)Tag;
                Settings.Sate.AutoResponse = (Safir.Dob.Response)responseInfo.Obj;
                Settings.Sate.XmlReplyObject = Safir.Dob.Typesystem.Serialization.ToXml(Settings.Sate.AutoResponse);
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                if (entityInfo.getInstanceId() == null)
                {
                    entityInfo.setInstanceId(new Safir.Dob.Typesystem.InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new Safir.Dob.Typesystem.HandlerId());
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                if (entityInfo.getInstanceId() == null)
                {
                    entityInfo.setInstanceId(Safir.Dob.Typesystem.InstanceId.GenerateRandom());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new Safir.Dob.Typesystem.HandlerId());
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                if (entityInfo.getInstanceId() == null)
                {
                    entityInfo.setInstanceId(new Safir.Dob.Typesystem.InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new Safir.Dob.Typesystem.HandlerId());
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                entityInfo.RequestorDecides = true;
                if (entityInfo.getInstanceId() == null)
                {
                    entityInfo.setInstanceId(new Safir.Dob.Typesystem.InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new Safir.Dob.Typesystem.HandlerId());
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                entityInfo.RequestorDecides = false;
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new Safir.Dob.Typesystem.HandlerId());
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                if (entityInfo.getInstanceId() == null)
                {
                    entityInfo.setInstanceId(Safir.Dob.Typesystem.InstanceId.GenerateRandom());
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                if (entityInfo.getInstanceId() == null)
                {
                    throw new Safir.Dob.Typesystem.SoftwareViolationException("Error doing delete. No InstanceId set in entityInfo!");
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                if (entityInfo.getInstanceId() == null)
                {
                    entityInfo.setInstanceId(new Safir.Dob.Typesystem.InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new Safir.Dob.Typesystem.HandlerId());
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                if (entityInfo.getInstanceId() == null)
                {
                    entityInfo.setInstanceId(new Safir.Dob.Typesystem.InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new Safir.Dob.Typesystem.HandlerId());
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
                EntityInfo entityInfo = (EntityInfo)Tag;
                if (entityInfo.getInstanceId() == null)
                {
                    entityInfo.setInstanceId(new Safir.Dob.Typesystem.InstanceId());
                }
                if (entityInfo.getHandlerId() == null)
                {
                    entityInfo.setHandlerId(new Safir.Dob.Typesystem.HandlerId());
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
                MessageInfo msgInfo = (MessageInfo)Tag;
                // if no channelid set, set to default
                if (msgInfo.getChannelId() == null)
                {
                    msgInfo.setChannelId(new Safir.Dob.Typesystem.ChannelId());
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
                ServiceHandlerInfo srvInfo = (ServiceHandlerInfo)Tag;
                if (srvInfo.getHandlerId() == null)
                {
                    srvInfo.setHandlerId(new Safir.Dob.Typesystem.HandlerId());
                }
                MainForm.Instance.ServiceRequest(srvInfo);
            }
        }


        private void toXmlLink_Click(object sender, EventArgs e)
        {
            if (objectPanel.SetObjectMembers())
            {
                //MainForm.Instance.AddTabPage(new XmlTabPage((Safir.Dob.Typesystem.Object)Tag));
                MainForm.Instance.AddTabPage(new XmlTabPage((ObjectInfo)Tag));
            }
        }

        public Panel RightPanel
        {
            get { return rightPanel; }
        }

        public bool LiveData
        {
            get { return liveDataCheckBox.Checked; }
        }


        public Safir.Dob.Typesystem.Object GetObject()
        {
            objectPanel.SetObjectMembers();
            return ((ObjectInfo)objectPanel.Tag).Obj;
        }
        
        public ObjectInfo GetObjectInfo()
        {
            objectPanel.SetObjectMembers();
            return (ObjectInfo)objectPanel.Tag;
        }

        public void UpdateData(ObjectInfo objInfo)
        {
            Tag = objInfo;
            objectPanel.UpdateData(objInfo);
        }

        public void DeletedData(bool deleted)//if deleted=false, then removed
        {
            objectPanel.SetDeleted(deleted);
        }

        private enum resulttype { longvalue, stringvalue, novalue }

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
                        ((EntityInfo)Tag).setHandlerId(new Safir.Dob.Typesystem.HandlerId(val));
                        break;
                    case resulttype.stringvalue:
                        ((EntityInfo)Tag).setHandlerId(new Safir.Dob.Typesystem.HandlerId(handlerIdInputTextBox.Text));
                        break;
                    default:
                        ((EntityInfo)Tag).setHandlerId(new Safir.Dob.Typesystem.HandlerId());
                        break;
                }
            }
            else if (Tag is ServiceHandlerInfo)
            {
                switch (result)
                {
                    case resulttype.longvalue:
                        ((ServiceHandlerInfo)Tag).setHandlerId(new Safir.Dob.Typesystem.HandlerId(val));
                        break;
                    case resulttype.stringvalue:
                        ((ServiceHandlerInfo)Tag).setHandlerId(new Safir.Dob.Typesystem.HandlerId(handlerIdInputTextBox.Text));
                        break;
                    default:
                        ((ServiceHandlerInfo)Tag).setHandlerId(new Safir.Dob.Typesystem.HandlerId());
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
                ((MessageInfo)Tag).setChannelId(new Safir.Dob.Typesystem.ChannelId(Int64.Parse(channelIdInputTextBox.Text)));
            }
            catch
            {
                string idString;
                if (channelIdInputTextBox.Text.StartsWith("\"") && (channelIdInputTextBox.Text.EndsWith("\"")) &&
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
                ((MessageInfo)Tag).setChannelId(new Safir.Dob.Typesystem.ChannelId(idString));
            
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
                ((EntityInfo)Tag).Timestamp = val;
                        
            }
           
            injectTimestampTextBox.BackColor = ColorMap.ENABLED;
        }

    }
}
