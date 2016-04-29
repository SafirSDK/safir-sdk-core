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
using Safir.Dob.Typesystem;

namespace Sate
{
    //*****************************************************************************************************
    // Object Edit Tab Page
    //*****************************************************************************************************
    public class ObjectEditTabPage : TabPage
    {
        private readonly ObjectEditControl _objEditCtrl;

        public ObjectEditTabPage(ObjectInfo objInfo)
        {
            var name = Operations.GetName(objInfo.Obj.GetTypeId());
            name = name.Substring(name.LastIndexOf('.') + 1);
            Text = name;
            Tag = objInfo;
            ImageIndex = 4;

            _objEditCtrl = new ObjectEditControl(objInfo);
            ObjEditCtrl.Dock = DockStyle.Fill;
            Controls.Add(ObjEditCtrl);
        }

        public ObjectEditControl ObjEditCtrl
        {
            get { return _objEditCtrl; }
        }
    }
}