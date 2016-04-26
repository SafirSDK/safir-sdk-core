/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

using System.IO;
using System.Windows.Forms;
using Safir.Dob.Typesystem;

namespace Sate
{
    public class XmlTabPage : TabPage
    {
        public XmlTabPage(ObjectInfo objInfo)
        {
            var name = Operations.GetName(objInfo.Obj.GetTypeId());
            name = name.Substring(name.LastIndexOf('.') + 1);
            Text = name;

            FileName = name + ".xml";
            RichEdit = new XmlRichEdit();
            RichEdit.Dock = DockStyle.Fill;
            Controls.Add(RichEdit);
            RichEdit.WriteXml(Serialization.ToXml(objInfo.Obj));
        }

        public XmlTabPage(string xmlContent, string fileName)
        {
            FileName = fileName;
            Text = fileName;
            RichEdit = new XmlRichEdit();
            RichEdit.Dock = DockStyle.Fill;
            Controls.Add(RichEdit);
            RichEdit.WriteXml(xmlContent);
            RichEdit.ReadOnly = false;
        }

        public string FileName { get; }

        public XmlRichEdit RichEdit { get; }

        public void Save(string path)
        {
            using (TextWriter writer = new StreamWriter(path))
            {
                writer.Write(RichEdit.Text);
                writer.Flush();
                writer.Close();
            }
        }
    }


    //Json
    public class JsonTabPage : TabPage
    {
        public JsonTabPage(ObjectInfo objInfo)
        {
            var name = Operations.GetName(objInfo.Obj.GetTypeId());
            name = name.Substring(name.LastIndexOf('.') + 1);
            Text = name;

            FileName = name + ".json";
            RichEdit = new XmlRichEdit();
            RichEdit.Dock = DockStyle.Fill;
            Controls.Add(RichEdit);
            RichEdit.Text = Serialization.ToJson(objInfo.Obj);
        }

        public JsonTabPage(string jsonContent, string fileName)
        {
            FileName = fileName;
            Text = fileName;
            RichEdit = new XmlRichEdit();
            RichEdit.Dock = DockStyle.Fill;
            Controls.Add(RichEdit);
            RichEdit.Text = jsonContent;
            RichEdit.ReadOnly = false;
        }

        public string FileName { get; }

        public RichTextBox RichEdit { get; }

        public void Save(string path)
        {
            using (TextWriter writer = new StreamWriter(path))
            {
                writer.Write(RichEdit.Text);
                writer.Flush();
                writer.Close();
            }
        }
    }
}