/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include <boost/interprocess/detail/tmp_dir_helpers.hpp>
#include <boost/interprocess/detail/workaround.hpp>

#if (defined BOOST_INTERPROCESS_HAS_WINDOWS_KERNEL_BOOTTIME) || (defined BOOST_INTERPROCESS_HAS_KERNEL_BOOTTIME)
#  error "WMI is not disabled!"
#endif

int main()
{
    return 0;
}

