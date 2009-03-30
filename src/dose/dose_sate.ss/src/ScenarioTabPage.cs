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
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Collections;

namespace Sate
{
    public class ScenarioTabPage : TabPage
    {
        private ScenarioControl sc = new ScenarioControl();
        private static ScenarioTabPage instance = new ScenarioTabPage();

        private ScenarioTabPage()
        {
            sc.Dock = DockStyle.Fill;
            this.Controls.Add(sc);
            this.Text = "Play/Record";
            this.ImageIndex = 2;
        }

        public static ScenarioTabPage Instance
        {
            get { return instance; }
        }

        public ScenarioControl Player
        {
            get { return sc; }
        }


    }
}
