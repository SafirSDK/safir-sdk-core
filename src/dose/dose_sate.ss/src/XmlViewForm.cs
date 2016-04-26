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

using System;
using System.ComponentModel;
using System.IO;
using System.Windows.Forms;
using Safir.Dob.Typesystem;
using Object = Safir.Dob.Typesystem.Object;

namespace Sate
{
    /// <summary>
    ///     Summary description for XmlViewForm1.
    /// </summary>
    public class XmlViewForm : Form
    {
        /// <summary>
        ///     Required designer variable.
        /// </summary>
        private readonly Container components = null;

        private readonly string fileName;
        private readonly XmlRichEdit xmlRichEdit;
        private MenuItem findmenuItem;
        private MainMenu mainMenu1;
        private MenuItem menuItem1;
        private MenuItem savemenuItem;

        public XmlViewForm(Object obj)
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();

            var name = Operations.GetName(obj.GetTypeId());
            name = name.Substring(name.LastIndexOf('.') + 1);
            Text = "XML Serialization of " + name; // +" instance " + obj.InstanceNumber;

            //fileName=name+"__inst_"+obj.InstanceNumber.ToString()+".xml";
            fileName = name + ".xml";

            xmlRichEdit = new XmlRichEdit();
            xmlRichEdit.Dock = DockStyle.Fill;
            Controls.Add(xmlRichEdit);
            xmlRichEdit.WriteXml(Serialization.ToXml(obj));
        }

        public XmlViewForm(string xmlContent, string fileName)
        {
            InitializeComponent();

            this.fileName = fileName;
            Text = fileName;
            xmlRichEdit = new XmlRichEdit();
            xmlRichEdit.Dock = DockStyle.Fill;
            Controls.Add(xmlRichEdit);
            xmlRichEdit.WriteXml(xmlContent);
            xmlRichEdit.ReadOnly = false;
        }

        /// <summary>
        ///     Clean up any resources being used.
        /// </summary>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        ///     Required method for Designer support - do not modify
        ///     the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            var resources = new System.Resources.ResourceManager(typeof(XmlViewForm));
            this.mainMenu1 = new System.Windows.Forms.MainMenu();
            this.menuItem1 = new System.Windows.Forms.MenuItem();
            this.savemenuItem = new System.Windows.Forms.MenuItem();
            this.findmenuItem = new System.Windows.Forms.MenuItem();
            //
            // mainMenu1
            //
            this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[]
            {
                this.menuItem1
            });
            //
            // menuItem1
            //
            this.menuItem1.Index = 0;
            this.menuItem1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[]
            {
                this.savemenuItem,
                this.findmenuItem
            });
            this.menuItem1.Text = "File";
            //
            // savemenuItem
            //
            this.savemenuItem.Index = 0;
            this.savemenuItem.Text = "Save to file...";
            this.savemenuItem.Click += new System.EventHandler(this.savemenuItem_Click);
            //
            // findmenuItem
            //
            this.findmenuItem.Index = 1;
            this.findmenuItem.Text = "Find...";
            this.findmenuItem.Click += new System.EventHandler(this.findmenuItem_Click);
            //
            // XmlViewForm
            //
            this.ClientSize = new System.Drawing.Size(492, 666);
            this.Icon = ((System.Drawing.Icon) (resources.GetObject("$this.Icon")));
            this.Menu = this.mainMenu1;
            this.Name = "XmlViewForm";
            this.Text = "XmlViewForm1";
        }

        #endregion

        private void savemenuItem_Click(object sender, EventArgs e)
        {
            var sf = new SaveFileDialog();
            sf.AddExtension = true;
            sf.Filter = "XML files (*.xml)|*.xml|All files (*.*)|*.*";
            sf.FileName = fileName;

            if (sf.ShowDialog() == DialogResult.OK)
            {
                TextWriter writer = new StreamWriter(sf.FileName);
                writer.Write(xmlRichEdit.Text);
                writer.Flush();
                writer.Close();
            }
        }

        private void findmenuItem_Click(object sender, EventArgs e)
        {
            var ff = new FindForm(xmlRichEdit);
            ff.Show();
        }
    }
}