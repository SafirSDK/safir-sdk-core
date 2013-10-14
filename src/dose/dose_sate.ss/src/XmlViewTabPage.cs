/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* 
* Created by: Lars Hagström / stlrha
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
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace Sate
{
    public class XmlTabPage : System.Windows.Forms.TabPage
    {
        private XmlRichEdit xmlRichEdit;
        private string fileName;

        public XmlTabPage(ObjectInfo objInfo)
        {
            string name=Safir.Dob.Typesystem.Operations.GetName(objInfo.Obj.GetTypeId());
            name=name.Substring(name.LastIndexOf('.')+1);
            //this.Text=name+" : "+obj.InstanceNumber;
            this.Text = name;

            //fileName=name+"_"+obj.InstanceNumber.ToString()+".xml";
            fileName = name + ".xml";
            xmlRichEdit = new XmlRichEdit();
            xmlRichEdit.Dock=DockStyle.Fill;
            this.Controls.Add(xmlRichEdit);
            xmlRichEdit.WriteXml(Safir.Dob.Typesystem.Serialization.ToXml(objInfo.Obj));
        }

        public XmlTabPage(string xmlContent, string fileName)
        {
            this.fileName=fileName;
            this.Text=fileName;
            xmlRichEdit = new XmlRichEdit();
            xmlRichEdit.Dock=DockStyle.Fill;
            this.Controls.Add(xmlRichEdit);
            xmlRichEdit.WriteXml(xmlContent);
            xmlRichEdit.ReadOnly=false;
        }

        public void Save(string path)
        {
            using (System.IO.TextWriter writer = new System.IO.StreamWriter(path))
            {
                writer.Write(xmlRichEdit.Text);
                writer.Flush();
                writer.Close();
            }
        }

        public string FileName
        {
            get { return fileName; }
        }

        public XmlRichEdit RichEdit
        {
            get { return xmlRichEdit; }
        }
    }
}
