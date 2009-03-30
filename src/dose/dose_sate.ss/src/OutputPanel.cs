/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
using System.Text;

namespace Sate
{
    class OutputPanel : System.Windows.Forms.Panel
    {
        private System.Windows.Forms.Panel fillpanel;
        private System.Windows.Forms.Panel toppanel;
        private System.Windows.Forms.RichTextBox richTextBox;
        private PanelLabelControl titleLabel;

        private static OutputPanel instance = new OutputPanel();

        public static OutputPanel Instance
        {
            get { return instance; }
        }

        public void LogEvent(string text, bool newLine)
        {
            text = DateTime.Now.ToLongTimeString() + " " + text;
            if (newLine) text += "\n";
            //this.richTextBox.Focus();
            this.richTextBox.AppendText(text);
            if (richTextBox.Text.Length > 80 * 1000)
            {
                richTextBox.Text.Remove(0, 80 * 500);
            }
            // replaced Focus() with this one. To not steal focus.
            this.richTextBox.ScrollToCaret();
            this.Invalidate();
        }

        private OutputPanel()
        {
            this.toppanel = new System.Windows.Forms.Panel();
            this.fillpanel = new System.Windows.Forms.Panel();
            this.richTextBox = new System.Windows.Forms.RichTextBox();
            this.titleLabel = new PanelLabelControl("Output");
            this.SuspendLayout();
            // 
            // toppanel
            // 
            this.toppanel.Dock = System.Windows.Forms.DockStyle.Top;
            toppanel.Height = 15;
            // 
            // fillpanel
            // 
            this.fillpanel.Dock = System.Windows.Forms.DockStyle.Fill;
            toppanel.Controls.Add(titleLabel);
            richTextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            fillpanel.Controls.Add(richTextBox);
            this.Controls.AddRange(new System.Windows.Forms.Control[] { toppanel, fillpanel });
            this.ResumeLayout(false);

            titleLabel.CloseEvent += new PanelLabelControl.OnCloseEventHandler(titleLabel_CloseEvent);
        }

        void titleLabel_CloseEvent(object sender, EventArgs e)
        {
            MainForm.Instance.ShowHideOutput(false);
        }
    }
}
