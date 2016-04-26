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

using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using System.Xml;

namespace Sate
{
    /// <summary>
    ///     Summary description for XmlPanel.
    /// </summary>
    public class XmlRichEdit : RichTextBox
    {
        public XmlRichEdit()
        {
            BackColor = Color.White;
            ReadOnly = true;
        }

        public void WriteXml(string xml)
        {
            Clear();

            var reader = new XmlTextReader(new StringReader(xml));
            var doc = new XmlDocument();
            doc.Load(reader);

            var sb = new StringBuilder();
            var settings = new XmlWriterSettings();
            settings.Indent = true;
            settings.IndentChars = "  ";
            settings.NewLineChars = "\r\n";
            settings.NewLineHandling = NewLineHandling.Replace;
            using (var writer = XmlWriter.Create(sb, settings))
            {
                doc.Save(writer);
            }
            AppendText(sb.ToString());

            reader.Close();
        }
    }
}