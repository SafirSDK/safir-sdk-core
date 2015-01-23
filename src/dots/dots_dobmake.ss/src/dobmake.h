/******************************************************************************
*
* Copyright Consoden AB, 2014 (http://www.consoden.se)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#endif

#include <QDialog>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace Ui {
    class Dobmake;
}

class Dobmake : public QDialog
{
    Q_OBJECT

public:
    explicit Dobmake(QWidget *parent = 0);

    ~Dobmake();



    static bool CheckPython();
    static QString GetDobmakeBatchScript();
private slots:
    void on_douDirectoryBrowse_clicked();

    void on_douDirectory_textChanged(const QString &arg1);

    void on_installDirectory_textChanged(const QString &arg1);

    void on_installDirectoryBrowse_clicked();

    void on_build_clicked();

    void on_buildAndInstall_clicked();

    void on_debugRadioButton_clicked(bool checked);

    void on_releaseRadioButton_clicked(bool checked);

    void on_debugCheckButton_clicked(bool checked);

    void on_releaseCheckButton_clicked(bool checked);

    void BuildComplete(const bool result);

    void on_showLog_toggled(bool checked);

private:
    void UpdateInstallButton();
    void UpdateBuildButton();
    void OpenLog(const bool ignoreCheckbox = false);
    
    bool m_debug;
    bool m_release;
    bool m_buildRunning;

    Ui::Dobmake* ui;
};
