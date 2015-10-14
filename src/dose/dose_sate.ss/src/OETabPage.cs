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
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace Sate
{
    //*****************************************************************************************************
    // Object Edit Tab Page
    //*****************************************************************************************************
    public class ObjectEditTabPage : System.Windows.Forms.TabPage
    {
        private ObjectEditControl oec;

        public ObjectEditTabPage(ObjectInfo objInfo)
        {
            string name = Safir.Dob.Typesystem.Operations.GetName(objInfo.Obj.GetTypeId());
            name = name.Substring(name.LastIndexOf('.') + 1);
            this.Text = name;
            this.Tag = objInfo;
            this.ImageIndex = 4;

            oec = new ObjectEditControl(objInfo);
            oec.Dock = DockStyle.Fill;
            this.Controls.Add(oec);
        }

        public ObjectEditControl ObjEditCtrl
        {
            get { return oec; }
        }
    }
}
