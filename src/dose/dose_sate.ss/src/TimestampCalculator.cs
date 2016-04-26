/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
* 
* Created by: Stefan Lindström / stsyli
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
    public partial class TimestampCalculatorForm : Form
    {
        public TimestampCalculatorForm()
        {
            InitializeComponent();
        }

        private void convertToMsButton_Click(object sender, EventArgs e)
        {
            if (humanTextBox.Text != "")
            {
                if (int64RadioButton.Checked)
                {
                    try
                    {
                        var dateTime = DateTime.Parse(humanTextBox.Text);
                        var ticks2008 = new DateTime(2008, 01, 01);
                        dateTime = dateTime.Subtract(new TimeSpan(ticks2008.Ticks));
                        numberTextBox.Text = (dateTime.Ticks/10).ToString();
                    }
                    catch
                    {
                        numberTextBox.Text = "Enter date and time above.";
                    }
                }
                else /* Second64 */
                {
                    try
                    {
                        var dateTime = DateTime.Parse(humanTextBox.Text);
                        var ticks1970 = new DateTime(1970, 01, 01);
                        dateTime = dateTime.Subtract(new TimeSpan(ticks1970.Ticks));
                        numberTextBox.Text = (dateTime.Ticks/10000000).ToString();
                    }
                    catch
                    {
                        numberTextBox.Text = "Enter date and time above.";
                    }
                }
            }
            else
            {
                numberTextBox.Text = "Enter date and time above.";
            }
        }

        private void convertToHumanButton_Click(object sender, EventArgs e)
        {
            try
            {
                if (int64RadioButton.Checked)
                {
                    var us = long.Parse(numberTextBox.Text);
                    // Timestamp used in Int64 is number of micro seconds since 2008-01-01
                    var humanTime = new DateTime(2008, 1, 1, 0, 0, 0).AddSeconds(us/1000000);
                    humanTextBox.Text = humanTime.ToString();
                }
                else /* Second64 */
                {
                    var s = long.Parse(numberTextBox.Text);
                    var humanTime = new DateTime(1970, 1, 1, 0, 0, 0).AddSeconds(s);
                    humanTextBox.Text = humanTime.ToString();
                }
            }
            catch
            {
                humanTextBox.Text = "Invalid format in textbox.";
            }
        }

        private void closeButton_Click(object sender, EventArgs e)
        {
            DialogResult = DialogResult.OK;
        }
    }
}