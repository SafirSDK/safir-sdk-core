/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Petter Lönnstedt / stpeln
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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4251)
#endif

#include <QString>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace VehicleMmiCppQt
{

    /**
    * This class is a workaround to solve error described in Qt Task Tracker,
    * Entry 127244, new VS2005 Project will treat wchar_t as bultin, causing
    * the build to fail with unresolved symbol.
    * This class can be removed in future when error solved by Trolltech.
    * The methods in tis class is a complete copy of methods in QString class
    * and needs to copied into this project because it's impossible to link
    * the application when project setting "Treat wchar_t as Built-in Type"
    * is set to "Yes".
    * The "Treat wchar_t as Built-in Type" has to be set to "Yes" otherwise
    * it's impossible link Dob code.
    */
    class QtWorkaround
    {
    public:
        virtual ~QtWorkaround();

        static QString StdWStringToQString(const std::wstring& rcwstrString);
        static std::wstring QStringToStdWString(const QString& rcqstrString);

    protected:
        static int QStringToWCharArray(const QString& rcqstrString, wchar_t *pArray);
        static QString WCharArrayToQString(const wchar_t *pcString, int nSize);

    private:
        QtWorkaround();
};



} // namespace VehicleMmiCppQt

