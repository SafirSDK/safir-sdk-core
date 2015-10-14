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
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Sate
{
   
    public partial class CustomImageForm : Form
    {
        public enum ImageKindType { ClassImage, NamespaceImage };
#if REMOVED_CODE
        private TreeViewImageHandler treeViewImageHandler;
#endif
        private bool isDirty = false;
        private string mappingName;

        public CustomImageForm(TreeViewImageHandler imageHandler, string mappingName)
        {
            InitializeComponent();
            this.mappingName = mappingName;
#if REMOVED_CODE
            treeViewImageHandler = imageHandler;
#endif
            imageOpenFileDialog.InitialDirectory = @"C:\development\Pluto\dose_proj\DoseSate";
            imageOpenFileDialog.Filter = "bitmap (*.bmp) | *.bmp";
            imageOpenFileDialog.DefaultExt = "bmp";
            imageOpenFileDialog.ValidateNames = false;

            System.Int64 typeId;
            if (System.Int64.TryParse(mappingName, out typeId))
            {
                applyToChildclassescheckBox.Text = "Apply to child classes";
                cusomtImgaeForLabel.Text = "Custom image for class";
                classlabel.Text = Safir.Dob.Typesystem.Operations.GetName(typeId);
            }
            else
            {
                applyToChildclassescheckBox.Text = "Apply to child namespaces";
                cusomtImgaeForLabel.Text = "Custom image for namespace";
                classlabel.Text = mappingName;
            }

            LoadImages();
        }


        private void LoadImages()
        {
            foreach (ImageMapping im in Settings.Sate.ImageMappings)
            {
                AddImage(im.ImagePath);
            }
        }

        public bool IsDirty
        {
            get { return isDirty; }
        }

        private void addimagebutton_Click(object sender, EventArgs e)
        {
            if (imageOpenFileDialog.ShowDialog() == DialogResult.OK)
            {
                AddImage(imageOpenFileDialog.FileName);
            }
        }

        private void AddImage(string path)
        {
            //Avoid duplicates
            foreach (ListViewItem item in imagelistView.Items)
            {
                if ((string)item.Tag == path)
                {
                    item.Selected = true;
                    imagelistView.Focus();
                    return;
                }
            }

            //A unique image, add it
            Image image = System.Drawing.Image.FromFile(path);
            image.Tag = path;
            this.imageList.Images.Add(image);
            ListViewItem li = new ListViewItem("", imageList.Images.Count - 1);
            li.Tag = path;
            this.imagelistView.Items.Add(li);
            li.Selected = true;
            imagelistView.Focus();
        }

        private void removeimagebutton_Click(object sender, EventArgs e)
        {
            if (imagelistView.SelectedItems.Count == 0)
            {
                MessageBox.Show("No image selected.");
                return;
            }

            if (MessageBox.Show("This operation will remove all existing mappings to the selected image. Do you want to proceed?",
                                "Remove Image",
                                MessageBoxButtons.OKCancel,
                                MessageBoxIcon.Question) == DialogResult.OK)
            {
                //remove image and all mappings
                Settings.Sate.RemoveImageAndMappings(imagelistView.SelectedItems[0].Tag as string);
                Settings.Save();
                imagelistView.Items.Remove(imagelistView.SelectedItems[0]);
                isDirty = true;
            }
        }

        private void okbutton_Click(object sender, EventArgs e)
        {
            if (!defaultimagecheckBox.Checked && imagelistView.SelectedIndices.Count == 0)
            {
                MessageBox.Show("You must select an image in the list view, or use the cancel button.", "No image selected", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                return;
            }

            if (defaultimagecheckBox.Checked)
            {
                Settings.Sate.RemoveImageMapping(mappingName);
                Settings.Save();
            }
            else
            {
                ImageMapping im = new ImageMapping(mappingName,
                                                    imagelistView.SelectedItems[0].Tag as string,
                                                    applyToChildclassescheckBox.Checked);
                Settings.Sate.AddImageMapping(im);
                Settings.Save();
            }

            isDirty = true;
            DialogResult = DialogResult.OK;

        }
        private void cancelbutton_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.Cancel;
        }
    }
}
