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
using System.IO;
using System.Xml;
using System.Drawing;

namespace Sate
{
    /// <summary>
    /// Summary description for XmlPanel.
    /// </summary>
    public class XmlRichEdit : System.Windows.Forms.RichTextBox
    {
        private Font elementFont=new Font("", 8, FontStyle.Regular);
        private Font contentFont=new Font("", 8, FontStyle.Bold);

        public XmlRichEdit()
        {
            this.BackColor = Color.White;
            this.ReadOnly=true;
        }

        public void WriteXml(string xml)
        {
            this.Clear();
            
            System.Xml.XmlTextReader reader=new System.Xml.XmlTextReader(new System.IO.StringReader(xml));
            System.Xml.XmlDocument doc=new System.Xml.XmlDocument();
            doc.Load(reader);
            
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            XmlWriterSettings settings = new XmlWriterSettings();
            settings.Indent = true;
            settings.IndentChars = "  ";
            settings.NewLineChars = "\r\n";
            settings.NewLineHandling = NewLineHandling.Replace;
            using (XmlWriter writer = XmlWriter.Create(sb, settings)) {
                doc.Save(writer);
            }
            AppendText(sb.ToString());

            reader.Close();
        }

        private const string BLANK = "                                                                                                    ";
        private const int IND = 5;
        private void Indent(int level)
        {
            int c=level*IND;
            if (c>0)
            {
                if (c<=BLANK.Length)
                {
                    AppendText(BLANK.Substring(0, c));
                }
                else
                {
                    AppendText(BLANK);
                    for (int i=0; i<c-BLANK.Length; i++)
                        AppendText(" ");
                }
            }
        }


    }
}
