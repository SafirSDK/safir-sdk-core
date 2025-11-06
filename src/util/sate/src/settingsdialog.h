/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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

#include <QDialog>

namespace Ui {
class SettingsDialog;
}

struct Settings
{
    bool dispatch = true;
    bool sendResponse = true;
    bool createEntities = true;
    bool updateEntities = true;
    bool deleteEntities = true;
    QString startupScriptPath;
};

class SettingsDialog : public QDialog
{
    Q_OBJECT

public:    

    explicit SettingsDialog(const Settings& states, const std::function<void(bool /*reset*/)>& editResonse, QWidget *parent = nullptr);
    ~SettingsDialog();

    Settings getCheckboxStates() const;

private slots:
    void onOpenResponseClicked();
    void onRestoreResponseClicked();
    void onSelectStartupScriptClicked();
    void onClearStartupScriptClicked();

private:
    Ui::SettingsDialog *ui;
    std::function<void(bool /*reset*/)> m_editResonse;
};
