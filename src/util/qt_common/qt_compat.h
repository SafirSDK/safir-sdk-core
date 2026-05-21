/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
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
#pragma once

#include <QtGlobal>

// QSortFilterProxyModel::invalidateFilter() was deprecated in Qt 6.9 in favour of
// beginFilterChange()/endFilterChange(). Use these macros to support both APIs:
//
//   FILTER_CHANGE_BEGIN();
//   <modify filter state>;
//   FILTER_CHANGE_END();
//
#if QT_VERSION >= QT_VERSION_CHECK(6, 9, 0)
#  define FILTER_CHANGE_BEGIN() beginFilterChange()
#  define FILTER_CHANGE_END()   endFilterChange()
#else
#  define FILTER_CHANGE_BEGIN()
#  define FILTER_CHANGE_END()   invalidateFilter()
#endif
