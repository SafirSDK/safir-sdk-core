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

using System.Windows.Forms;

namespace Sate
{
    public class ScenarioTabPage : TabPage
    {
        private static readonly ScenarioTabPage Instance1 = new ScenarioTabPage();
        private readonly ScenarioControl _player = new ScenarioControl();

        private ScenarioTabPage()
        {
            Player.Dock = DockStyle.Fill;
            Controls.Add(Player);
            Text = "Play/Record";
            ImageIndex = 2;
        }

        public static ScenarioTabPage Instance
        {
            get { return Instance1; }
        }

        public ScenarioControl Player
        {
            get { return _player; }
        }
    }
}