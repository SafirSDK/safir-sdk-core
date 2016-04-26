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
    public partial class DobUnitDetailsForm : Form
    {
        private readonly long typeId;

        public DobUnitDetailsForm(long typeId) //, Safir.Dob.Typesystem.InstanceId instanceId)
        {
            InitializeComponent();
            //objId = oid;
            this.typeId = typeId;
            //this.instanceId = instanceId;
            //if (oid.Instance==Safir.Dob.Typesystem.Constants.WHOLE_CLASS)
            {
                ClassDetails();
            }
#if STSYLI
            else
            {
                ObjectDetails();
            }
#endif
        }

#if STSYLI
        private void ObjectDetails()
        {
            //window text
#if STSYLI
            Text = "Details [object " + Safir.Dob.Typesystem.Operations.GetName(objId.TypeId) + " : " + objId.Instance + "]";
#endif
            //size

            //reg owner

            //subscribers           

        }
#endif

        private void ClassDetails()
        {
            //window text
            Text = "Details [class " + Operations.GetName(typeId) + "]";

            //number of instances

            //owner

            //subscribers
        }
    }
}