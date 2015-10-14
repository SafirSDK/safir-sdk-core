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
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace Sate
{
    /// <summary>
    /// Summary description for TypeIdCalculatorForm.
    /// </summary>
    public class TypeIdCalculatorForm : System.Windows.Forms.Form
    {
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox inputtextBox;
        private System.Windows.Forms.Button calckbutton;
        private System.Windows.Forms.TextBox resulttextBox;
        private System.Windows.Forms.Button closebutton;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label resultlabel;
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.Container components = null;

        public TypeIdCalculatorForm()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();
        }

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        protected override void Dispose( bool disposing )
        {
            if( disposing )
            {
                if(components != null)
                {
                    components.Dispose();
                }
            }
            base.Dispose( disposing );
        }

        #region Windows Form Designer generated code
        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(TypeIdCalculatorForm));
            this.label1 = new System.Windows.Forms.Label();
            this.inputtextBox = new System.Windows.Forms.TextBox();
            this.calckbutton = new System.Windows.Forms.Button();
            this.resultlabel = new System.Windows.Forms.Label();
            this.resulttextBox = new System.Windows.Forms.TextBox();
            this.closebutton = new System.Windows.Forms.Button();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(8, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(232, 23);
            this.label1.TabIndex = 0;
            this.label1.Text = "Type string or typeId:";
            // 
            // inputtextBox
            // 
            this.inputtextBox.Location = new System.Drawing.Point(8, 40);
            this.inputtextBox.Name = "inputtextBox";
            this.inputtextBox.Size = new System.Drawing.Size(288, 20);
            this.inputtextBox.TabIndex = 1;
            // 
            // calckbutton
            // 
            this.calckbutton.Location = new System.Drawing.Point(304, 38);
            this.calckbutton.Name = "calckbutton";
            this.calckbutton.Size = new System.Drawing.Size(112, 23);
            this.calckbutton.TabIndex = 2;
            this.calckbutton.Text = "Calculate";
            this.calckbutton.Click += new System.EventHandler(this.calckbutton_Click);
            // 
            // resultlabel
            // 
            this.resultlabel.Location = new System.Drawing.Point(8, 72);
            this.resultlabel.Name = "resultlabel";
            this.resultlabel.Size = new System.Drawing.Size(280, 23);
            this.resultlabel.TabIndex = 3;
            this.resultlabel.Text = "Result:";
            // 
            // resulttextBox
            // 
            this.resulttextBox.Location = new System.Drawing.Point(8, 88);
            this.resulttextBox.Name = "resulttextBox";
            this.resulttextBox.ReadOnly = true;
            this.resulttextBox.Size = new System.Drawing.Size(280, 20);
            this.resulttextBox.TabIndex = 4;
            // 
            // closebutton
            // 
            this.closebutton.Location = new System.Drawing.Point(368, 144);
            this.closebutton.Name = "closebutton";
            this.closebutton.Size = new System.Drawing.Size(75, 23);
            this.closebutton.TabIndex = 5;
            this.closebutton.Text = "Close";
            this.closebutton.Click += new System.EventHandler(this.closebutton_Click);
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.calckbutton);
            this.groupBox1.Controls.Add(this.resulttextBox);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.inputtextBox);
            this.groupBox1.Controls.Add(this.resultlabel);
            this.groupBox1.Location = new System.Drawing.Point(16, 8);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(424, 128);
            this.groupBox1.TabIndex = 6;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Calculate TypeId";
            // 
            // TypeIdCalculatorForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(450, 176);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.closebutton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "TypeIdCalculatorForm";
            this.Text = "TypeId Calculator";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);

        }
        #endregion

        private void calckbutton_Click(object sender, System.EventArgs e)
        {
            long typeId;
            try
            {
                typeId=long.Parse(inputtextBox.Text);
                this.resultlabel.Text="Result (type name):";
                if (Safir.Dob.Typesystem.Operations.Exists(typeId))
                {
                    string name=Safir.Dob.Typesystem.Operations.GetName(typeId);
                    this.resulttextBox.Text=name;
                }
                else
                {
                    this.resultlabel.Text="Result (error):";
                    this.resulttextBox.Text="No type exists with the specified typeId!";
                }
                return;
            }
            catch {}

            this.resultlabel.Text="Result (typeId):";
            typeId=Safir.Dob.Typesystem.Operations.GetTypeId(this.inputtextBox.Text);
            this.resulttextBox.Text=typeId.ToString();
        }

        private void closebutton_Click(object sender, System.EventArgs e)
        {
            DialogResult=DialogResult.OK;
        }
    }
}
