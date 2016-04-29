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
using System.Windows.Forms;

namespace Sate
{
    internal class OutputPanel : Panel
    {
        private readonly Panel fillpanel;
        private readonly RichTextBox richTextBox;
        private readonly PanelLabelControl titleLabel;
        private readonly Panel toppanel;
        private static readonly OutputPanel Instance1 = new OutputPanel();

        private OutputPanel()
        {
            toppanel = new Panel();
            fillpanel = new Panel();
            richTextBox = new RichTextBox();
            titleLabel = new PanelLabelControl("Output");
            SuspendLayout();
            // 
            // toppanel
            // 
            toppanel.Dock = DockStyle.Top;
            toppanel.Height = 15;
            // 
            // fillpanel
            // 
            fillpanel.Dock = DockStyle.Fill;
            toppanel.Controls.Add(titleLabel);
            richTextBox.Dock = DockStyle.Fill;
            fillpanel.Controls.Add(richTextBox);
            Controls.AddRange(new Control[] {toppanel, fillpanel});
            ResumeLayout(false);

            titleLabel.CloseEvent += titleLabel_CloseEvent;
        }

        public static OutputPanel Instance
        {
            get { return Instance1; }
        }

        public void LogEvent(string text, bool newLine)
        {
            text = DateTime.Now.ToLongTimeString() + " " + text;
            if (newLine) text += "\n";
            //this.richTextBox.Focus();
            richTextBox.AppendText(text);
            if (richTextBox.Text.Length > 80*1000)
            {
                richTextBox.Text.Remove(0, 80*500);
            }
            // replaced Focus() with this one. To not steal focus.
            richTextBox.ScrollToCaret();
            Invalidate();
        }

        private void titleLabel_CloseEvent(object sender, EventArgs e)
        {
            MainForm.Instance.ShowHideOutput(false);
        }
    }
}